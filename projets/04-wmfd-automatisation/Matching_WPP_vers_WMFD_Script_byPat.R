library(tidyverse)
library(readxl)
library(writexl)

# =============================================================================
# CATALOGUE DES SOURCES WMFD
# =============================================================================
# Etapes :
#   1. importe WPP et WMFD, puis construit les cles composites pays-annéé-enquete ;

#   2. harmonise les plages d'annees WPP et les libelles GGS-GGP ;

#   3. identifie les correspondances exactes obtenues apres ces harmonisations ;

#   4. recherche les correspondances deterministes selon le pays, le nom de
#      l'enquete et l'egalite de Year avec ReferenceYearStart ou ReferenceYearEnd 

#   5. calcule un score probabiliste pour les lignes encore sans correspondance,
#      a partir des libelles, des annees, de la source et du type d'enquete ;

#   6. accepte automatiquement les candidats classes A - tres probable et
#      B - probable selon les seuils de score, de marge et d'unicite ;

#   7. applique les validations manuelles des candidats C - possible et resout
#      manuellement les quatre correspondances finales non 1-a-1 ;

#   8. construit un registre unique des correspondances et controle leur cardinalite ;

#   9. transfere les variables WPP vers WMFD uniquement pour les correspondances
#      1-a-1 ou explicitement resolues manuellement ;

#  10. exporte la base WMFD fusionnee et les resultats du matching probabiliste.
#
#Noter que je n'ai apporter aucune modification aux deux classeurs Excel sources, elle sont juste lus.
#Il suffit d'executer ce script.'

# =============================================================================
# 1. PREPARATION WPP ET WMFD
# =============================================================================
# Ouvrir R dans le dossier de ce projet (voir README.md).
fichier_WPP <- "WPP2024_F02_Metadata.xlsx"
fichier_WMFD <- "catalog_WMFD.xlsx"

#Fonction necessaires (En particulier pour le matching probabiliste)

normaliser <- function(x) {
  x <- replace_na(as.character(x), "")
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^a-z0-9]+", " ")
  str_squish(x)
}

sans_annees <- function(x) {
  normaliser(x) %>%
    str_remove_all("\\b(?:19|20)\\d{2}\\b") %>%
    str_squish()
}

similarite <- function(a, b) {
  if (is.na(a) || is.na(b) || a == "" || b == "") return(0)
  1 - as.numeric(adist(a, b)) / max(nchar(a), nchar(b))
}

similarite_tokens <- function(a, b) {
  trier <- function(x) {
    x %>%
      str_split("\\s+") %>%
      unlist() %>%
      unique() %>%
      sort() %>%
      paste(collapse = " ")
  }
  similarite(trier(a), trier(b))
}

score_annee <- function(annee, debut, fin) {
  valeurs <- c(debut, fin)
  valeurs <- valeurs[!is.na(valeurs)]

  if (is.na(annee) || length(valeurs) == 0) {
    return(tibble(score_annee = 0.35, distance_annee = NA_real_))
  }

  debut <- if_else(is.na(debut), fin, debut)
  fin <- if_else(is.na(fin), debut, fin)
  borne_min <- min(debut, fin)
  borne_max <- max(debut, fin)

  distance <- if (annee >= borne_min && annee <= borne_max) {
    0
  } else {
    min(abs(annee - borne_min), abs(annee - borne_max))
  }

  score <- case_when(
    distance == 0 ~ 1,
    distance == 1 ~ 0.82,
    distance == 2 ~ 0.62,
    distance <= 5 ~ 0.35,
    TRUE ~ 0)

  tibble(score_annee = score, distance_annee = distance)
}

appliquer_remplacements <- function(donnees_WPP, remplacements) {
  donnees_WPP %>%
    left_join(remplacements, by = "ligne_WPP") %>%
    mutate(Composite_Key = coalesce(Composite_Key_WMFD, Composite_Key)) %>%
    select(-Composite_Key_WMFD)
}


#Importation des deux catalogues et construction des cles composites pour le matching

wpp <- read_excel(fichier_WPP, sheet = "Overall") %>%
  mutate(
    ligne_WPP = row_number() + 1,
    plage_annees_normalisee = str_detect(DataCatalogShortme, "^\\d{4}-\\d{4}"),
    Composite_Key_original = str_c(
      str_squish(as.character(ISO3_Code)),
      str_squish(DataCatalogShortme),
      sep = "::"
    ),## Harmonisation des libellés commençant par une plage d'années.
    # Exemple :
    #   DataCatalogShortme = "2019-2021 DHS"
    #   ReferenceYearStart = 2019
    #   Résultat            = "2019 DHS"
    DataCatalogShortme_key = if_else(
      str_detect(DataCatalogShortme, "^\\d{4}-\\d{4}"),
      str_replace(
        DataCatalogShortme,
        "^\\d{4}-\\d{4}",
        as.character(ReferenceYearStart)
      ),
      DataCatalogShortme
    ),
    Composite_Key = str_c(
      str_squish(as.character(ISO3_Code)),
      str_squish(DataCatalogShortme_key),
      sep = "::"
    ),
    harmonisation_GGS_GGP = str_detect(Composite_Key, "\\bGGS\\b"),
    # Harmonisation connue entre les deux catalogues sur GGS et GGP (GGS est un outills de GGP)
    Composite_Key = str_replace_all(Composite_Key, "\\bGGS\\b", "GGP")
  )

wmfd <- read_excel(fichier_WMFD, sheet = "Sheet1") %>%
  mutate(
    ligne_WMFD = row_number() + 1,
    Composite_Key = str_c(
      str_squish(as.character(ISO3_code)),
      str_squish(DataCatalogShortme_WMFD),
      sep = "::"))


# =============================================================================
# 2. CONFIGURATION DES PARAMETRES
# =============================================================================
# Fichiers de sortie et variables WPP a transferer.

fichier_sortie_probabilites <- "matching_probabiliste_complet.xlsx"
fichier_sortie_fusion <- "Catalogue_WMFD_fusionne.xlsx"

# Anneees d'intervalles admises pour la correction deterministe (ReferenceYearEnd - ReferenceYearStart)

durees_autorisees <- c(0, 1, 2) #J'en aurai besoin plus bas

# Variables WPP a ajouter dans WMFD. 

variables_WPP_a_transferer <- c(
  "Location",
  "DataCatalogID",
  "DataCatalogShortme",
  "DataCatalogName",
  "ReferenceYearStart",
  "ReferenceYearEnd",
  "DataProcessTypeID",
  "DataProcessType",
  "DataProcessID",
  "DataProcess",
  "Population",
  "Fertility",
  "Child_Mortality",
  "Adult_Mortality",
  "Overall_Mortality",
  "Migration")

# =============================================================================
# 3. MATCHING DETERMINISTE
# =============================================================================
# Sur la clé pays, le type d enquete et les annees de reference.

remplacements_deterministes <- wpp %>%
  mutate(
    nom_enquete = DataCatalogShortme %>%
      str_remove("^\\d{4}(-\\d{4})?\\s*") %>%
      str_squish() %>%
      str_to_upper(),
    duree = coalesce(ReferenceYearEnd, ReferenceYearStart) - ReferenceYearStart
  ) %>%
  filter(duree %in% durees_autorisees) %>%
  pivot_longer(
    cols = c(ReferenceYearStart, ReferenceYearEnd),
    names_to = "type_annee",
    values_to = "annee_reference"
  ) %>%
  filter(!is.na(annee_reference)) %>%
  distinct(ligne_WPP, annee_reference, .keep_all = TRUE) %>%
  inner_join(
    wmfd %>%
      mutate(
        nom_enquete = DataCatalogShortme_WMFD %>%
          str_remove("^\\d{4}(-\\d{4})?\\s*") %>%
          str_squish() %>%
          str_to_upper(),
        Composite_Key_WMFD = Composite_Key
      ),
    by = c(
      "ISO3_Code" = "ISO3_code",
      "nom_enquete",
      "annee_reference" = "Year"
    )
  ) %>%
  add_count(ligne_WPP, name = "n_WMFD") %>%
  add_count(ligne_WMFD, name = "n_WPP") %>%
  filter(n_WMFD == 1, n_WPP == 1) %>%
  transmute(
    ligne_WPP,
    ligne_WMFD,
    type_annee,
    Composite_Key_avant = Composite_Key.x,
    Composite_Key_WMFD,
    methode = "deterministe"
  ) %>%
  filter(Composite_Key_avant != Composite_Key_WMFD)

wpp <- appliquer_remplacements(
  wpp,
  remplacements_deterministes %>%
    select(ligne_WPP, Composite_Key_WMFD))

# =============================================================================
# Lignes encore sans correspondance apres le matching deterministe
# =============================================================================

wpp_sans_correspondance <- wpp %>%
  anti_join(wmfd %>% distinct(Composite_Key), by = "Composite_Key")

wmfd_sans_correspondance <- wmfd %>%
  anti_join(wpp %>% distinct(Composite_Key), by = "Composite_Key")

# =============================================================================
# 4. MATCHING PROBABILISTE
# =============================================================================
# Calcul des scores, classement A à D et application automatique des niveaux A-B.

paires_probabilistes <- inner_join(
  wmfd_sans_correspondance %>%
    select(
      ligne_WMFD, ISO3_code, Country_name_std,
      DataCatalogShortme_WMFD, Year, Data_Source, Data_type
    ) %>%
    mutate(pays = normaliser(ISO3_code)),
  wpp_sans_correspondance %>%
    select(
      ligne_WPP, ISO3_Code, DataCatalogID, DataCatalogShortme,
      DataCatalogName, ReferenceYearStart, ReferenceYearEnd
    ) %>%
    mutate(pays = normaliser(ISO3_Code)),
  by = "pays",
  relationship = "many-to-many"
) %>%
  rowwise() %>%
  mutate(
    wmfd_complet = normaliser(DataCatalogShortme_WMFD),
    wpp_complet = normaliser(DataCatalogShortme),
    wmfd_sans_annee = sans_annees(DataCatalogShortme_WMFD),
    wpp_sans_annee = sans_annees(DataCatalogShortme),
    contexte_wpp = normaliser(str_c(DataCatalogShortme, DataCatalogName, sep = " ")),
    source_wmfd = sans_annees(Data_Source),
    type_wmfd = normaliser(Data_type),
    similarite_base = max(
      similarite(wmfd_sans_annee, wpp_sans_annee),
      similarite_tokens(wmfd_sans_annee, wpp_sans_annee)
    ),
    similarite_complete = max(
      similarite(wmfd_complet, wpp_complet),
      similarite_tokens(wmfd_complet, wpp_complet)
    ),
    similarite_type = max(
      similarite(type_wmfd, wpp_sans_annee),
      similarite_tokens(type_wmfd, wpp_sans_annee),
      similarite(source_wmfd, contexte_wpp),
      similarite_tokens(source_wmfd, contexte_wpp)
    ),
    type_exact = type_wmfd != "" && str_detect(
      contexte_wpp,
      regex(str_c("(^| )", str_escape(type_wmfd), "( |$)"))
    ),
    similarite_type = if_else(type_exact, 1, similarite_type),
    calcul_annee = list(score_annee(Year, ReferenceYearStart, ReferenceYearEnd)),
    score_annee = calcul_annee$score_annee,
    distance_annee = calcul_annee$distance_annee,
    score = 0.46 * similarite_base +
      0.27 * score_annee +
      0.17 * similarite_type +
      0.10 * similarite_complete
  ) %>%
  ungroup() %>%
  select(-calcul_annee)

candidats_probabilistes <- paires_probabilistes %>%
  group_by(ligne_WMFD) %>%
  arrange(desc(score), .by_group = TRUE) %>%
  mutate(
    rang = row_number(),
    deuxieme_score = nth(score, 2, default = 0),
    marge = score - deuxieme_score
  ) %>%
  filter(rang == 1) %>%
  ungroup() %>%
  add_count(ligne_WPP, name = "nombre_utilisations_WPP") %>%
  mutate(
    candidat_WPP_unique = nombre_utilisations_WPP == 1,
    niveau = case_when(
      score >= 0.90 & marge >= 0.10 & candidat_WPP_unique &
        score_annee == 1 & type_exact ~ "A - tres probable",
      score >= 0.82 & marge >= 0.06 & candidat_WPP_unique ~ "B - probable",
      score >= 0.72 & marge >= 0.03 ~ "C - possible",
      TRUE ~ "D - faible"))

# Export Excel de tous les meilleurs candidats probabilistes.
sortie_probabilites <- candidats_probabilistes %>%
  transmute(
    WMFD = ligne_WMFD,
    WPP = ligne_WPP,
    Pays = ISO3_code,
    `Libelle WMFD` = DataCatalogShortme_WMFD,
    `Libelle WPP` = DataCatalogShortme,
    `Ref. Start` = ReferenceYearStart,
    `Ref. End` = ReferenceYearEnd,
    Score = round(score, 3),
    Marge = round(marge, 3),
    Unique = if_else(candidat_WPP_unique, "Oui", "Non"),
    Niveau = niveau
  ) %>%
  arrange(
    factor(Niveau, levels = c(
      "A - tres probable", "B - probable",
      "C - possible", "D - faible"
    )),
    desc(Score), desc(Marge))

remplacements_probabilistes <- candidats_probabilistes %>%
  filter(niveau %in% c("A - tres probable", "B - probable")) %>%
  transmute(
    ligne_WPP,
    ligne_WMFD,
    Composite_Key_avant = str_c(
      str_squish(ISO3_Code),
      str_squish(DataCatalogShortme),
      sep = "::"
    ),
    Composite_Key_WMFD = str_c(
      str_squish(ISO3_code),
      str_squish(DataCatalogShortme_WMFD),
      sep = "::"
    ),
    methode = niveau
  ) %>%
  filter(Composite_Key_avant != Composite_Key_WMFD)

wpp <- appliquer_remplacements(
  wpp,
  remplacements_probabilistes %>%
    select(ligne_WPP, Composite_Key_WMFD))

# =============================================================================
# 5. DECISIONS MANUELLES
# =============================================================================
# Validations des cas C et choix d une ligne WPP pour les cas non 1-a-1.

# -----------------------------------------------------------------------------
# VALIDATION MANUELLE DES CAS "C - possible"
# -----------------------------------------------------------------------------
# J'ajoute ici les paires validees, une ligne a la fois.
# Les numeros correspondent aux colonnes ligne_WMFD et ligne_WPP de
# candidats_possibles. (ce sont les numeros des lignes dans les feuilles excel)
#

validations_manuelles <- tribble(
  ~ligne_WMFD, ~ligne_WPP, ~valider, ~commentaire,
  75L,          296L,       TRUE,     "meme recensement, meme date",
  875L,         2248L,      TRUE,     "meme recensement, meme date",
  2428L,        6676L,      TRUE,     "meme enquete, meme date",
  527L,         1504L,      TRUE,     "deux recensements en trois ans paraissent peu probables",
  158L,         505L,       TRUE,     "validation manuelle",
  1511L,        4378L,      TRUE,     "validation manuelle",
  1044L,        2761L,      TRUE,     "validation manuelle",
  2077L,        5873L,      TRUE,     "validation manuelle",
  2084L,        5876L,      TRUE,     "validation manuelle",
  216L,         517L,       TRUE,     "validation manuelle",
  2119L,        5884L,      TRUE,     "validation manuelle",
  1621L,        4539L,      TRUE,     "validation manuelle",
  1736L,        4860L,      TRUE,     "validation manuelle",
  192L,         497L,       TRUE,     "GGS est le dispositif de GGP, meme annee",
  1531L,        4368L,      TRUE,     "GGS est le dispositif de GGP, meme annee")

# Choix manuels pour les cles non 1-a-1
choix_manuels_non_1a1 <- tribble(
  ~Composite_Key,    ~ligne_WPP, ~commentaire,
  "BEN::2021 MICS",  549L,       "libelle WPP exact",
  "RUS::2004 GGP",   5202L,      "GGP exact plutot que GGS",
  "RUS::2007 GGP",   5204L,      "GGP exact plutot que GGS",
  "SEN::2012 DHS",   5555L,      "choix manuel valide")

candidats_possibles <- candidats_probabilistes %>%
  filter(niveau == "C - possible") %>%
  arrange(desc(score), desc(marge))

validations_acceptees <- validations_manuelles %>%
  filter(valider) %>%
  distinct(ligne_WMFD, ligne_WPP, .keep_all = TRUE) %>%
  inner_join(
    candidats_possibles,
    by = c("ligne_WMFD", "ligne_WPP"))

stopifnot(
  nrow(validations_acceptees) ==
    nrow(validations_manuelles %>% filter(valider) %>% distinct(ligne_WMFD, ligne_WPP)))

remplacements_manuels <- validations_acceptees %>%
  transmute(
    ligne_WPP,
    ligne_WMFD,
    Composite_Key_avant = str_c(
      str_squish(ISO3_Code),
      str_squish(DataCatalogShortme),
      sep = "::"
    ),
    Composite_Key_WMFD = str_c(
      str_squish(ISO3_code),
      str_squish(DataCatalogShortme_WMFD),
      sep = "::"
    ),
    methode = "C - validation manuelle")

wpp <- appliquer_remplacements(
  wpp,
  remplacements_manuels %>%
    select(ligne_WPP, Composite_Key_WMFD))

# =============================================================================
# 6. REGISTRE UNIQUE DES CORRESPONDANCES
# =============================================================================
# ce journal consolide des corrections deterministes, probabilistes et manuelles.

# Journal de toutes les cles remplacees avant la fusion.
journal_remplacements <- bind_rows(
  remplacements_deterministes,
  remplacements_probabilistes,
  remplacements_manuels
) %>%
  arrange(ligne_WMFD, ligne_WPP)

# =============================================================================
# 7. CONTROLES 1-A-1
# =============================================================================
# Comptage des cardinalites avant tout transfert de variables.

comptes_WPP <- wpp %>% count(Composite_Key, name = "n_WPP")
comptes_WMFD <- wmfd %>% count(Composite_Key, name = "n_WMFD")

correspondances_finales <- inner_join(
  comptes_WPP,
  comptes_WMFD,
  by = "Composite_Key")

cles_finales_1a1 <- correspondances_finales %>%
  filter(n_WPP == 1, n_WMFD == 1)

cles_finales_non_1a1 <- correspondances_finales %>%
  filter(n_WPP != 1 | n_WMFD != 1)

wmfd_sans_correspondance_final <- wmfd %>%
  anti_join(cles_finales_1a1, by = "Composite_Key")

# =============================================================================
# 8. FUSION WPP VERS WMFD
# =============================================================================
# Selection des cles autorisees, transfert WPP et resume final.

variables_absentes <- setdiff(variables_WPP_a_transferer, names(wpp))
stopifnot(length(variables_absentes) == 0)

choix_non_1a1_valides <- choix_manuels_non_1a1 %>%
  inner_join(
    wpp %>% select(ligne_WPP, Composite_Key),
    by = c("ligne_WPP", "Composite_Key")
  ) %>%
  semi_join(cles_finales_non_1a1, by = "Composite_Key")

stopifnot(nrow(choix_non_1a1_valides) == nrow(choix_manuels_non_1a1))
stopifnot(!anyDuplicated(choix_non_1a1_valides$Composite_Key))

cles_autorisees_transfert <- bind_rows(
  cles_finales_1a1 %>% select(Composite_Key),
  choix_non_1a1_valides %>% select(Composite_Key)
) %>%
  distinct()

wpp_a_transferer <- bind_rows(
  wpp %>% semi_join(cles_finales_1a1, by = "Composite_Key"),
  wpp %>% semi_join(
    choix_non_1a1_valides %>% select(ligne_WPP, Composite_Key),
    by = c("ligne_WPP", "Composite_Key")
  )
) %>%
  select(
    ligne_WPP, Composite_Key,
    plage_annees_normalisee, harmonisation_GGS_GGP,
    all_of(variables_WPP_a_transferer)
  )

types_fusion <- wmfd %>%
  left_join(
    wpp_a_transferer %>%
      select(
        ligne_WPP, Composite_Key,
        plage_annees_normalisee, harmonisation_GGS_GGP
      ),
    by = "Composite_Key",
    relationship = "one-to-one"
  ) %>%
  left_join(
    journal_remplacements %>%
      select(ligne_WMFD, type_annee, methode) %>%
      distinct(),
    by = "ligne_WMFD"
  ) %>%
  mutate(
    type_fusion = case_when(
      is.na(ligne_WPP) ~ "Sans correspondance",
      Composite_Key %in% choix_non_1a1_valides$Composite_Key ~
        "Manuelle - choix non 1-a-1",
      methode == "deterministe" & type_annee == "ReferenceYearStart" ~
        "Deterministe - Year correspond a ReferenceYearStart",
      methode == "deterministe" & type_annee == "ReferenceYearEnd" ~
        "Deterministe - Year correspond a ReferenceYearEnd",
      methode == "A - tres probable" ~
        "Probabiliste - A tres probable",
      methode == "B - probable" ~
        "Probabiliste - B probable",
      methode == "C - validation manuelle" ~
        "Manuelle - C possible valide",
      coalesce(plage_annees_normalisee, FALSE) &
        coalesce(harmonisation_GGS_GGP, FALSE) ~
        "Deterministe - plage d'annees et harmonisation GGS vers GGP",
      coalesce(plage_annees_normalisee, FALSE) ~
        "Deterministe - plage d'annees remplacee par ReferenceYearStart",
      coalesce(harmonisation_GGS_GGP, FALSE) ~
        "Deterministe - harmonisation GGS vers GGP",
      TRUE ~ "Deterministe - concatenation directe"
    )
  ) %>%
  select(ligne_WMFD, type_fusion)

wpp_a_transferer <- wpp_a_transferer %>%
  select(Composite_Key, all_of(variables_WPP_a_transferer))
stopifnot(!anyDuplicated(wpp_a_transferer$Composite_Key))

wmfd_fusionne <- wmfd %>%
  left_join(
    types_fusion,
    by = "ligne_WMFD",
    relationship = "one-to-one"
  ) %>%
  left_join(
    wpp_a_transferer,
    by = "Composite_Key",
    relationship = "one-to-one")

stopifnot(nrow(wmfd_fusionne) == nrow(wmfd))

wmfd_sans_correspondance_final <- wmfd %>%
  anti_join(cles_autorisees_transfert, by = "Composite_Key")

resume_fusion <- tibble(
  indicateur = c(
    "Lignes WPP",
    "Lignes WMFD",
    "Remplacements deterministes",
    "Remplacements probabilistes A+B",
    "Candidats C a examiner",
    "Validations manuelles C",
    "Correspondances finales 1-a-1",
    "Correspondances finales non 1-a-1",
    "Cles non 1-a-1 resolues manuellement",
    "Lignes WMFD sans correspondance utilisable"
  ),
  nombre = c(
    nrow(wpp),
    nrow(wmfd),
    nrow(remplacements_deterministes),
    nrow(remplacements_probabilistes),
    nrow(candidats_possibles),
    nrow(remplacements_manuels),
    nrow(cles_finales_1a1),
    nrow(cles_finales_non_1a1),
    nrow(choix_non_1a1_valides),
    nrow(wmfd_sans_correspondance_final)))

print(resume_fusion, n = Inf)

# =============================================================================
# 9. EXPORTS
# =============================================================================
# Export des candidats probabilistes, de WMFD fusionne et des objets R.

write_xlsx(
  list(
    Toutes = sortie_probabilites,
    A_tres_probable = filter(sortie_probabilites, Niveau == "A - tres probable"),
    B_probable = filter(sortie_probabilites, Niveau == "B - probable"),
    C_possible = filter(sortie_probabilites, Niveau == "C - possible"),
    D_faible = filter(sortie_probabilites, Niveau == "D - faible")
  ),
  path = fichier_sortie_probabilites)

writexl::write_xlsx(
  wmfd_fusionne,
  path = fichier_sortie_fusion)


#save(
  #wmfd_fusionne,
  #cles_finales_1a1,
  #cles_finales_non_1a1,
  #wmfd_sans_correspondance_final,
  #candidats_possibles,
  #validations_manuelles,
  #choix_manuels_non_1a1,
  #choix_non_1a1_valides,
  #journal_remplacements,
  #resume_fusion,
  #file = "fusion_WPP_vers_WMFD_envir_final.RData")
