#====================================================================
#By Ntwali Patrick

#2024-2025
#====================================================================================================================
#PROJET 1: ANALYSE DES DONNEES CLAIRANCE PLASMATIQUE DE L’IOHEXOL
#===========================================================================================================================
#Packages necessaires

library(readxl)
library(labelled)
library(expss)
library(summarytools)
library(tidyverse)
library(hrbrthemes)
library(gtsummary)
library(ggcorrplot)
library(e1071)
library(car)
library(patchwork)
#-----------------------------------------------------
#PRELIMINAIRES
#---------------------------------------------------------
# Ouvrir R dans le dossier de ce projet (voir README.md).
Iohexol <- read_xlsx("Classeur1.xlsx")
Iohexol <- Iohexol[!is.na(Iohexol$DFGindexé), ]

#Carateristiques sociodemograhiques de la popualation d'étude

vars <- c("Sexe", "Residence", "Education", "Avoir_Enfts", "Profession_bis", 
          "Assurance", "Tabagisme", "Nbre_Tabac", "Nbre_Alcool", "pays_origine")

Iohexol3r_up %>% 
  select(all_of(vars)) %>% 
  tbl_summary(by = Sexe, percent = "column", missing = "ifany") %>% 
  add_overall()

#Ajuster la nature et la mesure des varibles impliqués

Iohexol$Age <- as.numeric(Iohexol$Age)
Iohexol$Poids <- as.numeric(Iohexol$Poids)
Iohexol$Taille <- as.numeric(Iohexol$Taille)
Iohexol$BSA <- as.numeric(Iohexol$BSA)
Iohexol$Créatinine <- as.numeric(Iohexol$Créatinine)
Iohexol$CystatineC <-  as.numeric(Iohexol$CystatineC)
Iohexol$microalbcreat <-  as.numeric(Iohexol$microalbcreat)
Iohexol$CKDEPI2009 <-  as.numeric(Iohexol$CKDEPI2009)
Iohexol$CKDEPI2009avecrace <-  as.numeric(Iohexol$CKDEPI2009avecrace)
Iohexol$CKDEPI2021 <-  as.numeric(Iohexol$CKDEPI2021)
Iohexol$CKDEPIcys <-  as.numeric(Iohexol$CKDEPIcys)
Iohexol$CKDEPImix2009 <-  as.numeric(Iohexol$CKDEPImix2009)
Iohexol$CKDEPImix2009avecrace <-  as.numeric(Iohexol$CKDEPImix2009avecrace)
Iohexol$CKDEPImix2021 <-  as.numeric(Iohexol$CKDEPImix2021)
Iohexol$EKFCcre <-  as.numeric(Iohexol$EKFCcre)
Iohexol$EKFCcrePopulation <-  as.numeric(Iohexol$EKFCcrePopulation)
Iohexol$EKFCcys <-  as.numeric(Iohexol$EKFCcys)
Iohexol$EKFCmix <-  as.numeric(Iohexol$EKFCmix)
Iohexol$EKFCmixPS <-  as.numeric(Iohexol$EKFCmixPS)
Iohexol$DFGindexé <-  as.numeric(Iohexol$DFGindexé)

#Test de normalité des variables quantitatives

shapiro.test(Iohexol$Age)
shapiro.test(Iohexol$IMC) 
shapiro.test(Iohexol$Poids) 
shapiro.test(Iohexol$Taille)
shapiro.test(Iohexol$BSA)
shapiro.test(Iohexol$Créatinine)
shapiro.test(Iohexol$CystatineC)
shapiro.test(Iohexol$microalbcreat)
shapiro.test(Iohexol$CKDEPI2009)
shapiro.test(Iohexol$CKDEPI2009avecrace)
shapiro.test(Iohexol$CKDEPI2021)
shapiro.test(Iohexol$CKDEPIcys)
shapiro.test(Iohexol$CKDEPImix2009)
shapiro.test(Iohexol$CKDEPImix2009avecrace)
shapiro.test(Iohexol$CKDEPImix2021)
shapiro.test(Iohexol$EKFCcre)
shapiro.test(Iohexol$EKFCcrePopulation)
shapiro.test(Iohexol$EKFCcys)
shapiro.test(Iohexol$EKFCmix)
shapiro.test(Iohexol$EKFCmixPS)
shapiro.test(Iohexol$DFGindexé)
#-------------------------------------------------------------------------------------------------------------------
#Vu des toutes les distributions quantitatives de l'etude (analyse univarié)
#-------------------------------------------------------------------------------------------------------------------

# Vu de la distribution  du DFGindexé 

#QQ plot DFGindexé
qqnorm(Iohexol$DFGindexé) 
qqline(Iohexol$DFGindexé, col = "red") 

#Courbe de Gauss

hist(Iohexol3r_up$DFGindex, 
     breaks = 30, 
     probability = TRUE, 
     main = "Distribution du DFG mesuré",
     xlab = "DFG mesuré")

curve(dnorm(x, mean = mean(Iohexol$DFGindexé, na.rm = TRUE), 
            sd = sd(Iohexol$DFGindexé, na.rm = TRUE)), 
      col = "red", lwd = 2, add = TRUE)


#Figure 1 : Plan statistique des distributions de la normalité des caractéristiques de la population totale.

variables <- c("Age", "IMC", "BSA", "Créatinine", "CystatineC", "microalbcreat",
               "CKDEPI2009", "CKDEPI2009avecrace", "CKDEPI2021", "CKDEPIcys",
               "CKDEPImix2009", "CKDEPImix2009avecrace", "CKDEPImix2021",
               "EKFCcre", "EKFCcrePopulation", "EKFCcys", "EKFCmix", "EKFCmixPS", "DFGindexé")

# Organiser les graphiques dans une grille (par exemple, 3 lignes et 3 colonnes)
n_vars <- length(variables)
n_rows <- ceiling(sqrt(n_vars))  # Calculer le nombre de lignes nécessaires
n_cols <- ceiling(n_vars / n_rows)  # Calculer le nombre de colonnes nécessaires
par(mfrow = c(n_rows, n_cols))  # Configurer la grille

for (var in variables) {
  if (var %in% names(Iohexol)) {
    data <- Iohexol[[var]]
    if (is.numeric(data)) {
      dens <- density(data, na.rm = TRUE)
      plot(dens, main = var, xlab = "Valeurs", ylab = "Densité", col = "blue", lwd = 2)
    } else {
      plot.new()
      text(0.5, 0.5, paste(var, "\nnon numérique"), cex = 1.2)
    }
  } else {
    plot.new()
    text(0.5, 0.5, paste(var, "\nabsente"), cex = 1.2)
  }
}

par(mfrow = c(1, 1))
#--------------------------------------------------------------------------------------------------------------------
# TableauIII-b: Statistiques descriptives de la population d'étude sélon chaque formule par sexe 
#-----------------------------------------------------------------------------------------------------------------

Iohexol %>%
  tbl_summary(
    include = c(Age, Poids, Taille_CM,  IMC, BSA, Créatinine, CystatineC, microalbcreat, CKDEPI2009, CKDEPI2009avecrace, CKDEPI2021, CKDEPIcys, CKDEPImix2009, CKDEPImix2009avecrace, CKDEPImix2021,
                EKFCcre, EKFCcrePopulation, EKFCcys, EKFCmix, EKFCmixPS),
    by = Sexe,
    statistic = list(
      Age ~ "Moy. : {mean} ({sd})",
      Poids ~ "Moy. : {mean} ({sd})",
      Taille_CM ~ "Méd. : {median} ({p25} - {p75})",
      IMC ~ "Moy. : {mean} ({sd})",
      BSA ~ "Moy. : {mean} ({sd})",
      Créatinine ~ "Méd. : {median} [{p25} - {p75}]",
      CystatineC ~ "Méd. : {median} [{p25} - {p75}]",
      microalbcreat ~ "Méd. : {median} [{p25} - {p75}]",
      CKDEPI2009 ~ "Méd. : {median} [{p25} - {p75}]",
      CKDEPI2009avecrace ~ "Méd. : {median} [{p25} - {p75}]",
      CKDEPI2021 ~ "Méd. : {median} [{p25} - {p75}]",
      CKDEPIcys ~ "Méd. : {median} [{p25} - {p75}]",
      CKDEPImix2009 ~ "Méd. : {median} [{p25} - {p75}]",
      CKDEPImix2009avecrace ~ "Méd. : {median} [{p25} - {p75}]",
      CKDEPImix2021 ~ "Méd. : {median} [{p25} - {p75}]",
      EKFCcre ~ "Méd. : {median} [{p25} - {p75}]",
      EKFCcrePopulation ~ "Méd. : {median} [{p25} - {p75}]",
      EKFCcys ~ "Méd. : {median} [{p25} - {p75}]",
      EKFCmix ~ "Méd. : {median} [{p25} - {p75}]",
      EKFCmixPS ~ "Méd. : {median} [{p25} - {p75}]")) %>%
  modify_spanning_header(update = all_stat_cols() ~ "**Carateristiques de la population d'etude**") %>%
  add_overall(last = TRUE, col_label = "**Ensemble** (effectif total: {N})")

#-------------------------------------------------------------------------------
#Distribution du DFG mesuré par iohexol selon l’âge et le sexe 
#--------------------------------------------------------------------------------

#Figure 14: Distribution du DFG mesuré en fonction de l’âge (dans l'ensemble)

#Recodade de la variable Age en classes d'age

Iohexol$age_group[Iohexol$Age < 40] <- "18-40"
Iohexol$age_group[Iohexol$Age >= 40 & Iohexol$Age < 65] <- "40-65 ans"
Iohexol$age_group[Iohexol$Age >= 65] <- "65 ans et plus"

Iohexol$age_group <-  as.factor(Iohexol$age_group)
str(Iohexol$age_group)
table(Iohexol$age_group)

#Test d'homogeneité de variance su DFGindexé dans les groupes d'ages 

leveneTest(DFGindexé ~ age_group, data = Iohexol) ## L’hypothèse d’homogénéité des variances est acceptée (p > 0.05)

tapply(Iohexol$DFGindexé, Iohexol$age_group, median)
kruskal.test(DFGindexé ~ age_group, data = Iohexol)
options(scipen = 999)

ggplot(Iohexol, aes(x = factor(age_group), y = DFGindexé)) +
  stat_summary(fun = median, geom = "bar", fill = "orange", color = "black", na.rm = TRUE, width = 0.5) +
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = round(..y.., 2)), 
    vjust = -0.5, 
    color = "black",
    na.rm = TRUE) +
  labs(x = "Groupe d'âge", y = "DFG mesuré") +
  ggtitle("DFG median mesuré selon le groupe d'âge") +
  theme_bw() +
  annotate(
    "text", x = 2, 
    y = max(Iohexol$DFGindexé, na.rm = TRUE) + 2, 
    label = paste("p_value < 0.001"), 
    color = "red")
#-----------------------

#Figure 15 : Distribution du DFG mesuré en fonction de l’âge et du sexe
Iohexol_m <- Iohexol %>%
  filter(Sexe == "M")

Iohexol_f <- Iohexol %>%
  filter(Sexe == "F") 

#scatterplot hommes et femmes

plot_men_jitter0 <- ggplot(Iohexol_m, aes(x = factor(age_group), y = DFGindexé)) +
  geom_jitter(color = "blue", size = 2, width = 0.2, alpha = 0.7, na.rm = TRUE) +
  stat_summary(
    fun = median, 
    geom = "point", 
    shape = 23, 
    size = 4, 
    fill = "red", 
    color = "black", 
    na.rm = TRUE
  ) +
  labs(x = "Groupe d'âge", y = "DFG mesuré") +
  ggtitle("Répartition du DFG chez les hommes") +
  theme_bw() +
  annotate(
    "text", 
    x = 2, 
    y = max(Iohexol_m$DFGindexé, na.rm = TRUE) + 2, 
    label = paste(""), #j'ai enlevé les pvalue
    color = "red")


plot_women_jitter1 <- ggplot(Iohexol_f, aes(x = factor(age_group), y = DFGindexé)) +
  geom_jitter(color = "pink", size = 2, width = 0.2, alpha = 0.7, na.rm = TRUE) +
  stat_summary(
    fun = median, 
    geom = "point", 
    shape = 23, 
    size = 4, 
    fill = "red", 
    color = "black", 
    na.rm = TRUE
  ) +
  labs(x = "Groupe d'âge", y = "DFG mesuré") +
  ggtitle("Répartition du DFG chez les femmes") +
  theme_bw() +
  annotate(
    "text", 
    x = 2, 
    y = max(Iohexol_f$DFGindexé, na.rm = TRUE) + 2, 
    label = paste(""), #j'ai enlevé les pvalue
    color = "red")

# Combine scatter plots
combined_scatter1 <- plot_men_jitter0 + plot_women_jitter1

# Display the combined plot
print(combined_scatter1)


#Figure 15: Distribution du DFG mesuré en fonction de l’âge et le sexe

#bar graph

#hommes

plot_men <- ggplot(Iohexol_m, aes(x = factor(age_group), y = DFGindexé)) +
  stat_summary(fun = median, geom = "bar", fill = "blue", color = "black", na.rm = TRUE, width = 0.5) +
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = round(..y.., 2)), 
    vjust = -0.5, 
    color = "black",
    na.rm = TRUE) +
  labs(x = "Groupe d'âge", y = "DFG mesuré") +
  ggtitle("Chez les hommes") +
  theme_bw() +
  annotate(
    "text", 
    x = 2, 
    y = max(Iohexol_m$DFGindexé, na.rm = TRUE) + 2, 
    label = paste("p_value < 0.001"), 
    color = "red")

#femmes

plot_women <- ggplot(Iohexol_f, aes(x = factor(age_group), y = DFGindexé)) +
  stat_summary(fun = median, geom = "bar", fill = "red", color = "black", na.rm = TRUE, width = 0.5) +
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = round(..y.., 2)), 
    vjust = -0.5, 
    color = "black",
    na.rm = TRUE) +
  labs(x = "Groupe d'âge", y = "DFG mesuré") +
  ggtitle("Chez les femmes") +
  theme_bw() +
  annotate(
    "text", 
    x = 2, 
    y = max(Iohexol_f$DFGindexé, na.rm = TRUE) + 2, 
    label = paste("p_value < 0.001"), 
    color = "red")

# Combinaison des graphiques
combined_plot <- plot_men + plot_women

# Affichage
print(combined_plot)
#----------------------------------------------------------------------------------------------------
#Analyse chez les volontaires sains DFG > 60 ml/min/1.73 m2
#-----------------------------------------------------------------------------------------------------

class(Iohexol_60$DFGindex)

summary(Iohexol_60$DFGindex)

ggplot(Iohexol_60, aes(x = DFGindex)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
  geom_vline(aes(xintercept = median(Iohexol_60$DFGindex, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1) +
  stat_function(
    fun = dnorm,
    args = list(mean = mean(Iohexol_60$DFGindex, na.rm = TRUE), sd = sd(Iohexol_60$DFGindex, na.rm = TRUE)),
    color = "blue",
    size = 1
  ) +
  annotate(
    "text", 
    x = median(Iohexol_60$DFGindex, na.rm = TRUE), 
    y = 0.05,  # Ajustez `y` selon l'échelle de densité de vos données
    label = paste("Médiane :", round(median(Iohexol_60$DFGindex, na.rm = TRUE), 1)), 
    color = "red", 
    hjust = -0.1, 
    vjust = -0.5
  ) +
  ggtitle("Distribution de DFG chez les volontaires sains") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(50, 150, by = 20), limits = c(50, 150))


#Figure 17 : valeurs du DFG mesuré chez les volontaires sains

qqnorm(Iohexol_60$DFGindex) 
qqline(Iohexol_60$DFGindex, col = "red") 

#Tableau X : Test de normalité DFGm chez les volontaires sains

shapiro.test(Iohexol_60$DFGindex)
options(scipen = 999)

#Figure 18 : distribution des valeurs du DFG mesuré chez les volontaires sains Selon les groupes d’âges.


ggplot(Iohexol_60, aes(x = factor(age_group), y = DFGindex)) +
  stat_summary(fun = median, geom = "bar", fill = "green", color = "black", na.rm = TRUE, width = 0.5) +
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = round(..y.., 2)), 
    vjust = -0.5, 
    color = "black",
    na.rm = TRUE
  ) +
  labs(x = "Groupe d'âge", y = "DFG mesuré") +
  ggtitle("DFG médian mesuré selon le groupe d'âge chez les volontaires saines") +
  theme_bw() +
  annotate(
    "text", x = 2, 
    y = 120, 
    label = paste("p_value < 0.05"), 
    color = "red") +
  coord_cartesian(ylim = c(0, 120))

kruskal.test(DFGindex ~ age_group, data = Iohexol_60)

#Figure 20 : distribution des valeurs du DFG mesuré chez les volontaires sains, En fonction du sexe et des groupes d’âges

# Hommes

Iohexol_60_m <- Iohexol_60 %>%
  filter(Sexe == "Masculin") 

summary(Iohexol_60_m$DFGindex)

x_pos <- mean(as.numeric(factor(unique(Iohexol_60_m$age_group))))
# Limiter y pour que ça reste dans le cadre
y_pos <- min(mean(Iohexol_60_m$DFGindex, na.rm = TRUE) + 5, 120)

plot_men <- ggplot(Iohexol_60_m, aes(x = factor(age_group), y = DFGindex)) +
  stat_summary(fun = median, geom = "bar", fill = "grey", color = "black", na.rm = TRUE, width = 0.5) +
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = round(..y.., 2)), 
    vjust = -0.5, 
    color = "black",
    na.rm = TRUE
  ) +
  labs(x = "Groupe d'âge", y = "DFG mesuré") +
  ggtitle("DFG médian chez les hommes sains") +
  theme_bw() +
  annotate(
    "text", 
    x = x_pos, 
    y = y_pos, 
    label = "p_value > 0.05", 
    color = "red", 
    size = 4
  ) +
  coord_cartesian(ylim = c(0, 120))

#test

wilcox.test(DFGindex ~ age_group, data = Iohexol_60_m, exact = FALSE) #Mann-Whitney U

#femmes

Iohexol_60_f <- Iohexol_60 %>%
  filter(Sexe == "Féminin") 

summary(Iohexol_60_f$DFGindex)

plot_women <- ggplot(Iohexol_60_f, aes(x = factor(age_group), y = DFGindex)) +
  stat_summary(fun = median, geom = "bar", fill = "pink", color = "black", na.rm = TRUE, width = 0.5) +
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = round(..y.., 2)), 
    vjust = -0.5, 
    color = "black",
    na.rm = TRUE
  ) +
  labs(x = "Groupe d'âge", y = "DFG mesuré") +
  ggtitle("DFG médian chez les femmes saines") +
  theme_bw() +
  annotate(
    "text", 
    x = 3, 
    y = mean(Iohexol_60_f$DFGindex, na.rm = TRUE) + 2, 
    label = paste("p_value < 0.05"), 
    color = "red")  +
  coord_cartesian(ylim = c(0, 120))

#test

kruskal.test(DFGindex ~ age_group, data = Iohexol_60_f)# Kruskall waliis


# les graph cotes à cotes
combined_plot <- plot_men + plot_women
print(combined_plot)


#Figure 21 : distribution des valeurs du DFG mesuré chez les sujets sains (DFG>60 ml/min/1.73 m2), en fonction du sexe et des groupes d’âges.

#hommes 

plot_men_jitter <- ggplot(Iohexol_60_m, aes(x = factor(age_group), y = DFGindex)) +
  geom_jitter(color = "blue", size = 2, width = 0.2, alpha = 0.7, na.rm = TRUE) +
  stat_summary(
    fun = median, 
    geom = "point", 
    shape = 23, 
    size = 4, 
    fill = "red", 
    color = "black", 
    na.rm = TRUE
  ) +
  labs(x = "Groupe d'âge", y = "DFG mesuré") +
  ggtitle("Répartition du DFG chez les hommes sains") +
  theme_bw() +
  annotate(
    "text", 
    x = 2, 
    y = max(Iohexol_60_m$DFGindex, na.rm = TRUE) + 2, 
    label = paste(""), #j'ai enlevé les pvalue
    color = "red")

#femmes

plot_women_jitter <- ggplot(Iohexol_60_f, aes(x = factor(age_group), y = DFGindex)) +
  geom_jitter(color = "pink", size = 2, width = 0.2, alpha = 0.7, na.rm = TRUE) +
  stat_summary(
    fun = median, 
    geom = "point", 
    shape = 23, 
    size = 4, 
    fill = "red", 
    color = "black", 
    na.rm = TRUE
  ) +
  labs(x = "Groupe d'âge", y = "DFG mesuré") +
  ggtitle("Répartition du DFG chez les femmes saines") +
  theme_bw() +
  annotate(
    "text", 
    x = 2, 
    y = max(Iohexol_60_f$DFGindex, na.rm = TRUE) + 2, 
    label = paste(""), #j'ai enlevé les pvalue
    color = "red")

combined_scatter <- plot_men_jitter + plot_women_jitter
print(combined_scatter)

#====================================================================================================================================
#II-DETERMINATION DES MARQUEURS SERIQUES IMPLIQUES DANS L’ESTIMATION DU DFG.
#====================================================================================================================================
#1. CREATININE

#Figure 26 : QQ plot de distribution des valeurs de la créatinine dans l’ensemble de la population d’étude

ggplot(Iohexol, aes(sample = Créatinine)) +
  stat_qq(size = 1.5, color = "blue") +  # Points du nuage
  stat_qq_line(color = "red", linetype = "dashed", linewidth = 1) +  # Ligne de normalité
  labs(
    title = "Distribution de la créatinine en mg/dl",
    x = "Quantiles théoriques",
    y = "Quantiles des données"
  ) +
  theme_minimal()

#Tableau IX : Distribution des valeurs de la créatinine en fonction de l’âge et du sexe

# Femmes + Hommes
part1 <- Iohexol %>%
  group_by(age_group, Sexe) %>%
  summarise(
    Médiane = median(Créatinine, na.rm = TRUE),
    P25 = quantile(Créatinine, 0.25, na.rm = TRUE),
    P75 = quantile(Créatinine, 0.75, na.rm = TRUE),
    .groups = "drop")
part1
# Ensemble (tous sexes confondus)
part2 <- Iohexol %>%
  group_by(age_group) %>%
  summarise(
    sexe = "Ensemble",
    Médiane = median(Créatinine, na.rm = TRUE),
    P25 = quantile(Créatinine, 0.25, na.rm = TRUE),
    P75 = quantile(Créatinine, 0.75, na.rm = TRUE),
    .groups = "drop")
part2

#Figure X: Creatinine selon le DFG & creatinine selon  IMC

#categorisation du DFG et du BMI

Iohexol$DFGcat1[Iohexol$DFGindexé < 30 ] <- "<30"
Iohexol$DFGcat1[(Iohexol$DFGindexé >= 30) & (Iohexol$DFGindexé < 60)] <- "30-60"
Iohexol$DFGcat1[(Iohexol$DFGindexé >= 60) & (Iohexol$DFGindexé <  90)] <- "60-90"
Iohexol$DFGcat1[Iohexol$DFGindexé >= 90] <- "90 et plus"
Iohexol$DFGcat1 <- factor(Iohexol$DFGcat1, levels = c("<30", "30-60", "60-90", "90 et plus"))

table(Iohexol$DFGcat1)

Iohexol$BMI[Iohexol$IMC < 18 ] <- "<18"
Iohexol$BMI[(Iohexol$IMC >= 18) & (Iohexol$IMC < 25)] <- "18-25"
Iohexol$BMI[(Iohexol$IMC >= 25) & (Iohexol$IMC <  30)] <- "25-30"
Iohexol$BMI[Iohexol$IMC >= 30] <- "30 et plus"
Iohexol$BMI <- factor(Iohexol$BMI, levels = c("<18", "18-25", "25-30", "30 et plus"))

table(Iohexol$BMI)

#Graph

plot_DFG <- ggplot(Iohexol, aes(x = factor(DFGcat1), y = Créatinine)) +
  stat_summary(fun = median, geom = "bar", fill = "grey", color = "black", na.rm = TRUE, width = 0.5) +
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = round(..y.., 2)), 
    vjust = -0.5, 
    color = "black",
    na.rm = TRUE
  ) +
  labs(x = "Catégories de DFG (ml/mn/1,73m²)", y = "Créatinine mg/dL") +
  ggtitle("Créatinine par DFG") +
  theme_bw() +
  annotate(
    "text", 
    x = 2, 
    y = max(Iohexol$Créatinine, na.rm = TRUE) + 2, 
    label = paste("p_value < 0.001"), 
    color = "red")


plot_BMI <- ggplot(Iohexol, aes(x = factor(BMI), y = Créatinine)) +
  stat_summary(fun = median, geom = "bar", fill = "pink", color = "black", na.rm = TRUE, width = 0.5) +
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = round(..y.., 2)), 
    vjust = -0.5, 
    color = "black",
    na.rm = TRUE
  ) +
  labs(x = "Catégories de IMC (Kg/m²)", y = "Créatinine mg/dL") +
  ggtitle("Créatinine par IMC") +
  theme_bw() +
  annotate(
    "text", 
    x = 2, 
    y = max(Iohexol$Créatinine, na.rm = TRUE) + 2, 
    label = paste("p_value > 0.05"), 
    color = "red")

combined_plot <- plot_DFG + plot_BMI

print(combined_plot)

kruskal_test10 <- kruskal.test(Créatinine ~ factor(BMI), data = Iohexol)
options(scipen = 999)

kruskal_test <- kruskal.test(Créatinine ~ factor(DFGcat1), data = Iohexol)
options(scipen = 999)

#Creatinine====> Volontaires sains

#1. CREATININE

#Figure 26 : QQ plot de distribution des valeurs de la créatinine ches les volontaires sains

ggplot(Iohexol_60, aes(sample = Cratinine)) +
  stat_qq(size = 1.5, color = "blue") +  # Points du nuage
  stat_qq_line(color = "red", linetype = "dashed", linewidth = 1) +  # Ligne de normalité
  labs(
    title = "Distribution de la créatinine en mg/dL chez les volontaires sains",
    x = "Quantiles théoriques",
    y = "Quantiles des données"
  ) +
  theme_minimal()

#Tableau IX : Distribution des valeurs de la créatinine en fonction de l’âge et du sexe

# Femmes + Hommes
part1 <- Iohexol_60 %>%
  group_by(age_group, Sexe) %>%
  summarise(
    Médiane = median(Cratinine, na.rm = TRUE),
    P25 = quantile(Cratinine, 0.25, na.rm = TRUE),
    P75 = quantile(Cratinine, 0.75, na.rm = TRUE),
    .groups = "drop")
part1
# Ensemble (tous sexes confondus)
part2 <- Iohexol_60 %>%
  group_by(age_group) %>%
  summarise(
    sexe = "Ensemble",
    Médiane = median(Cratinine, na.rm = TRUE),
    P25 = quantile(Cratinine, 0.25, na.rm = TRUE),
    P75 = quantile(Cratinine, 0.75, na.rm = TRUE),
    .groups = "drop")
part2

#Figure X: Creatinine selon le DFG & creatinine selon  IMC

#categorisation du DFG et du BMI
Iohexol_60$taille_m <- Iohexol_60$Taillecm / 100
Iohexol_60$IMC <- Iohexol_60$PoidsKg / (Iohexol_60$taille_m)^2


Iohexol_60$DFGcat1[Iohexol_60$DFGindex < 30 ] <- "<30"
Iohexol_60$DFGcat1[(Iohexol_60$DFGindex >= 30) & (Iohexol_60$DFGindex < 60)] <- "30-60"
Iohexol_60$DFGcat1[(Iohexol_60$DFGindex >= 60) & (Iohexol_60$DFGindex <  90)] <- "60-90"
Iohexol_60$DFGcat1[Iohexol_60$DFGindex >= 90] <- "90 et plus"
Iohexol_60$DFGcat1 <- factor(Iohexol_60$DFGcat1, levels = c("<30", "30-60", "60-90", "90 et plus"))

table(Iohexol_60$DFGcat1)

Iohexol_60$BMI[Iohexol_60$IMC < 18 ] <- "<18"
Iohexol_60$BMI[(Iohexol_60$IMC >= 18) & (Iohexol_60$IMC < 25)] <- "18-25"
Iohexol_60$BMI[(Iohexol_60$IMC >= 25) & (Iohexol_60$IMC <  30)] <- "25-30"
Iohexol_60$BMI[Iohexol_60$IMC >= 30] <- "30 et plus"
Iohexol_60$BMI <- factor(Iohexol_60$BMI, levels = c("<18", "18-25", "25-30", "30 et plus"))

table(Iohexol_60$BMI)

#Graph

plot_DFG <- ggplot(Iohexol_60, aes(x = factor(DFGcat1), y = Cratinine)) +
  stat_summary(fun = median, geom = "bar", fill = "grey", color = "black", na.rm = TRUE, width = 0.5) +
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = round(..y.., 2)), 
    vjust = -0.5, 
    color = "black",
    na.rm = TRUE
  ) +
  labs(x = "Catégories de DFG (ml/mn/1,73m²)", y = "Créatinine mg/dL") +
  ggtitle("Créatinine par DFG chez les volontaires sains") +
  theme_bw() +
  annotate(
    "text", 
    x = 2, 
    y = max(Iohexol_60$Cratinine, na.rm = TRUE) + 2, 
    label = paste("p_value > 0.05"), 
    color = "red")


plot_BMI <- ggplot(Iohexol_60, aes(x = factor(BMI), y = Cratinine)) +
  stat_summary(fun = median, geom = "bar", fill = "pink", color = "black", na.rm = TRUE, width = 0.5) +
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = round(..y.., 2)), 
    vjust = -0.5, 
    color = "black",
    na.rm = TRUE
  ) +
  labs(x = "Catégories de IMC (Kg/m²)", y = "Créatinine mg/dL") +
  ggtitle("Créatinine par IMC chez les volontaires sains") +
  theme_bw() +
  annotate(
    "text", 
    x = 2, 
    y = max(Iohexol_60$Cratinine, na.rm = TRUE) + 2, 
    label = paste("p_value > 0.05"), 
    color = "red")

combined_plot <- plot_DFG + plot_BMI

kruskal_test <- kruskal.test(Cratinine ~ factor(DFGcat1), data = Iohexol_60)
options(scipen = 999)

kruskal_test10 <- kruskal.test(Cratinine ~ factor(BMI), data = Iohexol_60)
options(scipen = 999)



print(combined_plot)
------------------------------------------------------------
#2. CYSTATNE C

#Figure 31 : Distribution des valeurs de la cystatine C dans l’ensemble de la population (mg/L)

  ggplot(Iohexol, aes(sample = CystatineC)) +
  stat_qq(size = 1.5, color = "blue") +  # Points du nuage
  stat_qq_line(color = "red", linetype = "dashed", linewidth = 1) +  # Ligne de normalité
  labs(
    title = "Distribution de la Cystatine C en mg/L",
    x = "Quantiles théoriques",
    y = "Quantiles des données"
  ) +
  theme_minimal()

#Tableau X : distribution de la cystatine C selon l’âge et le sexe

# Femmes + Hommes
part3 <- Iohexol %>%
  group_by(age_group, Sexe) %>%
  summarise(
    Médiane = median(CystatineC, na.rm = TRUE),
    P25 = quantile(CystatineC, 0.25, na.rm = TRUE),
    P75 = quantile(CystatineC, 0.75, na.rm = TRUE),
    .groups = "drop")
part3
# Ensemble (tous sexes confondus)
part4 <- Iohexol %>%
  group_by(age_group) %>%
  summarise(
    sexe = "Ensemble",
    Médiane = median(CystatineC, na.rm = TRUE),
    P25 = quantile(CystatineC, 0.25, na.rm = TRUE),
    P75 = quantile(CystatineC, 0.75, na.rm = TRUE),
    .groups = "drop")
part4

#Figure X : distribution de la cystatine C selon le DFG mesuré et l’IMC

plot_DFG2 <- ggplot(Iohexol, aes(x = factor(DFGcat1), y = CystatineC)) +
  stat_summary(fun = median, geom = "bar", fill = "grey", color = "black", na.rm = TRUE, width = 0.5) +
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = round(..y.., 2)), 
    vjust = -0.5, 
    color = "black",
    na.rm = TRUE
  ) +
  labs(x = "Catégories de DFG (ml/mn/1,73m²)", y = "Créatinine mg/L") +
  ggtitle("CystatineC par DFG") +
  theme_bw() +
  annotate(
    "text", 
    x = 2, 
    y = max(Iohexol$CystatineC, na.rm = TRUE) + 2, 
    label = paste("p_value <0.001"), 
    color = "red")

kruskal_test20 <- kruskal.test(CystatineC ~ factor(DFGcat1), data = Iohexol)
options(scipen = 999)

plot_BMI2 <- ggplot(Iohexol, aes(x = factor(BMI), y = CystatineC)) +
  stat_summary(fun = median, geom = "bar", fill = "pink", color = "black", na.rm = TRUE, width = 0.5) +
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = round(..y.., 2)), 
    vjust = -0.5, 
    color = "black",
    na.rm = TRUE
  ) +
  labs(x = "Catégories de IMC (Kg/m²)", y = "Cystatine C mg/L") +
  ggtitle("Cystatine C par IMC") +
  theme_bw() +
  annotate(
    "text", 
    x = 2, 
    y = max(Iohexol$CystatineC, na.rm = TRUE) + 2, 
    label = paste("p_value > 0.05"), 
    color = "red")

kruskal_test30 <- kruskal.test(CystatineC ~ factor(BMI), data = Iohexol)
options(scipen = 999)

combined_plot58 <- plot_DFG2 + plot_BMI2

print(combined_plot58)

#Cystatine====> Volontaires sains

#2. CYSTATNE C

#Figure 31 : Distribution des valeurs de la cystatine C chez les sujets sains (mg/L)

ggplot(Iohexol_60, aes(sample = CystatineC)) +
  stat_qq(size = 1.5, color = "blue") +  # Points du nuage
  stat_qq_line(color = "red", linetype = "dashed", linewidth = 1) +  # Ligne de normalité
  labs(
    title = "Distribution de la Cystatine C en mg/L chez les volontaires sains",
    x = "Quantiles théoriques",
    y = "Quantiles des données"
  ) +
  theme_minimal()

#Tableau X : distribution de la cystatine C selon l’âge et le sexe

# Femmes + Hommes
part3 <- Iohexol_60 %>%
  group_by(age_group, Sexe) %>%
  summarise(
    Médiane = median(CystatineC, na.rm = TRUE),
    P25 = quantile(CystatineC, 0.25, na.rm = TRUE),
    P75 = quantile(CystatineC, 0.75, na.rm = TRUE),
    .groups = "drop")
part3
# Ensemble (tous sexes confondus)
part4 <- Iohexol_60 %>%
  group_by(age_group) %>%
  summarise(
    sexe = "Ensemble",
    Médiane = median(CystatineC, na.rm = TRUE),
    P25 = quantile(CystatineC, 0.25, na.rm = TRUE),
    P75 = quantile(CystatineC, 0.75, na.rm = TRUE),
    .groups = "drop")
part4

#Figure X : distribution de la cystatine C selon le DFG mesuré et l’IMC

plot_DFG2 <- ggplot(Iohexol_60, aes(x = factor(DFGcat1), y = CystatineC)) +
  stat_summary(fun = median, geom = "bar", fill = "grey", color = "black", na.rm = TRUE, width = 0.5) +
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = round(..y.., 2)), 
    vjust = -0.5, 
    color = "black",
    na.rm = TRUE
  ) +
  labs(x = "Catégories de DFG (ml/mn/1,73m²)", y = "Cystatine C mg/L") +
  ggtitle("CystatineC par DFG chez les volontaires sains") +
  theme_bw() +
  annotate(
    "text", 
    x = 2, 
    y = max(Iohexol_60$CystatineC, na.rm = TRUE) + 2, 
    label = paste("p_value < 0.001"), 
    color = "red")


plot_BMI2 <- ggplot(Iohexol_60, aes(x = factor(BMI), y = CystatineC)) +
  stat_summary(fun = median, geom = "bar", fill = "pink", color = "black", na.rm = TRUE, width = 0.5) +
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = round(..y.., 2)), 
    vjust = -0.5, 
    color = "black",
    na.rm = TRUE
  ) +
  labs(x = "Catégories de IMC (Kg/m²)", y = "Cystatine C mg/L") +
  ggtitle("Cystatine C par IMC chez les volontaires sains") +
  theme_bw() +
  annotate(
    "text", 
    x = 2, 
    y = max(Iohexol_60$CystatineC, na.rm = TRUE) + 2, 
    label = paste("p_value > 0.05"), 
    color = "red")

kruskal_test30 <- kruskal.test(CystatineC ~ factor(BMI), data = Iohexol_60)
options(scipen = 999)

kruskal_test20 <- kruskal.test(CystatineC ~ factor(DFGcat1), data = Iohexol_60)
options(scipen = 999)

combined_plot58 <- plot_DFG2 + plot_BMI2

print(combined_plot58)

#==================================================================================================================================================
#III-ESTIMATION DU DEBIT DE FILTRATION GLOMERULAIRE A PARTIR DES EQUATIONS
#==================================================================================================================================================

#Figure 13: Box plot des DGF estimé par les équations CKDEPI et EKFC, basées sur la créatinine dans l’ensemble de la population : 

#1. DFG estimé à partir des equations basé sur la creatinine


plot1 <- ggplot(Iohexol, aes(x = "", y = CKDEPI2009)) + # here we select the object and variables we want to plot
  geom_boxplot() +  #here we set the type of plot
  labs(title = "", # And we add a title and x and y-axis labels
       x = "",
       y = "CKD-EPI 2009")  

plot2 <- ggplot(Iohexol, aes(x = "", y = CKDEPI2009avecrace)) + # here we select the object and variables we want to plot
  geom_boxplot() +  #here we set the type of plot
  labs(title = "", # And we add a title and x and y-axis labels
       x = "",
       y = "CKD-EPI 2009 creat avec race")  


plot3 <- ggplot(Iohexol, aes(x = "", y = CKDEPI2021)) + # here we select the object and variables we want to plot
  geom_boxplot() +  #here we set the type of plot
  labs(title = "", # And we add a title and x and y-axis labels
       x = "",
       y = "CKD-EPI 2021")  

plot4 <- ggplot(Iohexol, aes(x = "", y = EKFCcre)) + # here we select the object and variables we want to plot
  geom_boxplot() +  #here we set the type of plot
  labs(title = "", # And we add a title and x and y-axis labels
       x = "",
       y = "EKFC cre") 

plot5 <- ggplot(Iohexol, aes(x = "", y = EKFCcrePopulation)) + # here we select the object and variables we want to plot
  geom_boxplot() +  #here we set the type of plot
  labs(title = "", # And we add a title and x and y-axis labels
       x = "",
       y = "EKFC cre Population") 

combined_plot <- plot1 + plot2 + plot3 + plot4 + plot5 
print(combined_plot)

#Figure 13-bis: Box plot des DGF estimé par les équations CKDEPI et EKFC, basées sur la cystatine C dans l’ensemble de la population.

#2. DFG estimé à partir des equations basé sur la cystatine C et mix

plot6 <- ggplot(Iohexol, aes(x = "", y = CKDEPIcys)) + # here we select the object and variables we want to plot
  geom_boxplot() +  #here we set the type of plot
  labs(title = "", # And we add a title and x and y-axis labels
       x = "",
       y = "CKD-EPI cys")  

plot7 <- ggplot(Iohexol, aes(x = "", y = CKDEPImix2009)) + # here we select the object and variables we want to plot
  geom_boxplot() +  #here we set the type of plot
  labs(title = "", # And we add a title and x and y-axis labels
       x = "",
       y = "CKD-EPI mix 2009")  

plot8 <- ggplot(Iohexol, aes(x = "", y = CKDEPImix2009avecrace)) + # here we select the object and variables we want to plot
  geom_boxplot() +  #here we set the type of plot
  labs(title = "", # And we add a title and x and y-axis labels
       x = "",
       y = "CKD-EPI mix 2009 avec race")  

plot9 <- ggplot(Iohexol, aes(x = "", y = CKDEPImix2021)) + # here we select the object and variables we want to plot
  geom_boxplot() +  #here we set the type of plot
  labs(title = "", # And we add a title and x and y-axis labels
       x = "",
       y = "CKD-EPI mix 2021")  

plot10 <- ggplot(Iohexol, aes(x = "", y = EKFCcys)) + # here we select the object and variables we want to plot
  geom_boxplot() +  #here we set the type of plot
  labs(title = "", # And we add a title and x and y-axis labels
       x = "",
       y = "EKFC cys") 

plot11 <- ggplot(Iohexol, aes(x = "", y = EKFCmix)) + # here we select the object and variables we want to plot
  geom_boxplot() +  #here we set the type of plot
  labs(title = "", # And we add a title and x and y-axis labels
       x = "",
       y = "EKFC mix") 

plot12 <- ggplot(Iohexol, aes(x = "", y = EKFCmixPS)) + # here we select the object and variables we want to plot
  geom_boxplot() +  #here we set the type of plot
  labs(title = "", # And we add a title and x and y-axis labels
       x = "",
       y = "EKFC mix PS") 

combined_plot89 <- (plot6 + plot7 + plot8 + plot9 + plot10 + plot11 + plot12) +
  plot_annotation(title = "DFG estimé à partir des équantions basées sur la cystatine C & mixte")

print(combined_plot89)

#======================================================================================================================================
#CHAPITRE IV-PERTINENCE DE L’APPLICATION DU FACTEUR ETHNIQUE AFRO-AMERICAIN CHEZ LES PERSONNES D’ORIGINE AFRICAINE VIVANT EN BELGIQUE.
#=====================================================================================================================================

#Figure : Courbes de régression linéaire de la clairance de l’iohexol avec les équations CKD-EPI 2009 sans facteur (A), CKD-EPI avec facteur (B), CKD-EPI mix 2009 sans facteur (C) et CKD-EPI mix 2009 avec facteur (D).

p75 <- ggplot(Iohexol, aes(DFGindexé, CKDEPI2009)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("DFG mesuré") +
  xlab("CKD-EPI 2009") +
  ggtitle("A") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p75

p80 <- ggplot(Iohexol, aes(DFGindexé, CKDEPI2009avecrace)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("DFG mesuré") +
  xlab("CKD-EPI 2009 avec CE") +
  ggtitle("B") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p80

p90 <- ggplot(Iohexol, aes(DFGindexé, CKDEPImix2009)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("DFG mesuré") +
  xlab("CKD-EPI mix 2009") +
  ggtitle("C") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p90

p100 <- ggplot(Iohexol, aes(DFGindexé, CKDEPImix2009avecrace)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("DFG mesuré") +
  xlab("CKD-EPI 2009 mix avec CE") +
  ggtitle("D") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p100

combined_plot600 <- p75 + p80 + p90 + p100
combined_plot600

--------------------
#Figure : Courbes de Bland-Altman : comparaison de la clairance plasmatique de l’iohexol avec les équations CKD-EPI 2009 sans facteur (A), CKD-EPI 2009 avec facteur (B), CKD-EPI mix 2009 sans facteur (C) et CKD-EPI mix 2009 avec facteur (D).
  
#Etape 1: Calcul des biais pour chaque formule
  
Iohexol$bias_CKDEPI2009_DGF <- (Iohexol$CKDEPI2009 - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_CKDEPI2009_DGF)

Iohexol$bias_CKDEPI2009avecrace_DGF <- (Iohexol$CKDEPI2009avecrace - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_CKDEPI2009avecrace_DGF)

Iohexol$bias_CKDEPI2021 <- (Iohexol$CKDEPI2021- Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_CKDEPI2021)

Iohexol$bias_CKDEPIcys <- (Iohexol$CKDEPIcys- Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_CKDEPIcys)

Iohexol$bias_CKDEPImix2009 <- (Iohexol$CKDEPImix2009 - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_CKDEPImix2009)

Iohexol$bias_CKDEPImix2009avecrace <- (Iohexol$CKDEPImix2009avecrace - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_CKDEPImix2009avecrace)

Iohexol$bias_CKDEPImix2021 <- (Iohexol$CKDEPImix2021 - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_CKDEPImix2021)

Iohexol$bias_EKFCcre <- (Iohexol$EKFCcre - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_EKFCcre)

Iohexol$bias_EKFCcrepop <- (Iohexol$EKFCcrePopulation - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_EKFCcrepop)

Iohexol$bias_EKFCcys <- (Iohexol$EKFCcys - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_EKFCcys)

Iohexol$bias_EKFCmix <- (Iohexol$EKFCmix - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_EKFCmix )

Iohexol$bias_EKFCmixpop <- (Iohexol$EKFCmixPS - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_EKFCmixpop)

#save(Iohexol, file = "Iohexol.Rda") #Enregistrement de la nouvelle BD avec les biais

#Etape 2: Rechape la BD en format longer

#Iohexol_long <- Iohexol %>%
# pivot_longer(
# cols = starts_with("bias"), # Colonnes contenant les bias de formules ( biai_EKFC, etc.)
# names_to = "Formules",          # Nouvelle colonne pour les noms des méthodes
#values_to = "Bias"  )            # Nouvelle colonne pour les valeurs des biais

#head(Iohexol_long)

#Iohexol_long$Formules <- as.factor(Iohexol_long$Formules)

#Etape 3: Filtre sur les formules sur lequelles on va travailler

#Iohexol_l1 <- Iohexol_long %>%
#filter(Formules %in% c("bias_CKDEPI2009_DGF", "bias_CKDEPI2009avecrace_DGF",
                         #"bias_CKDEPI2021", "bias_EKFCcre", "bias_EKFCcrepop"))
#unique(Iohexol_l1$Formules)
#table(Iohexol_l1$Formules)

#Etape 4 : Realisation de du plo de Bland-Altman

# Bland-Altman Plot 1: CKDEPI2009  vs DFGindexé

  par(mfrow = c(1, 2))  # 1 row, 2 columns

#Moyenne 

Iohexol <- Iohexol %>%
  mutate(Mean1 = (CKDEPI2009 + DFGindexé) / 2,
         Diff1 = CKDEPI2009 - DFGindexé)

# Calcul des statistiques
moyenne_diff <- mean(Iohexol$Diff1)
ecart_type_diff <- sd(Iohexol$Diff1)
n <- nrow(Iohexol)  
se_diff <- ecart_type_diff / sqrt(n)

limites_inferieures <- moyenne_diff - 1.96 * ecart_type_diff
limites_superieures <- moyenne_diff + 1.96 * ecart_type_diff

ic_biais_inf <- moyenne_diff - 1.96 * se_diff
ic_biais_sup <- moyenne_diff + 1.96 * se_diff

# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean1, Iohexol$Diff1, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + CKD-EPI 2009)/2",
  ylab = "Différence (CKD-EPI 2009 - DFG mesuré)",
  main = "A",
  ylim = c(-100, 100))

# Ajout de la ligne de biais
abline(h = moyenne_diff, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)

# Bland-Altman Plot 2: CKDEPI2009avecrace  vs DFGindexé

Iohexol <- Iohexol %>%
  mutate(Mean2 = (CKDEPI2009avecrace + DFGindexé) / 2,
         Diff2 = CKDEPI2009avecrace - DFGindexé)

# Calcul des statistiques
moyenne_diff2 <- mean(Iohexol$Diff2)
ecart_type_diff2 <- sd(Iohexol$Diff2)
n <- nrow(Iohexol)  
se_diff2 <- ecart_type_diff2 / sqrt(n)

limites_inferieures2 <- moyenne_diff2 - 1.96 * ecart_type_diff2
limites_superieures2 <- moyenne_diff2 + 1.96 * ecart_type_diff2

ic_biais_inf2 <- moyenne_diff2 - 1.96 * se_diff2
ic_biais_sup2 <- moyenne_diff2 + 1.96 * se_diff2


# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean2, Iohexol$Diff2, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + CKD-EPI 2009 avec CE)/2",
  ylab = "Différence (CKD-EPI 2009 avec CE - DFG mesuré)",
  main = "B",
  ylim = c(-100, 100))


# Ajout de la ligne de biais
abline(h = moyenne_diff2, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures2, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures2, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf2, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup2, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)

# reunitialiser les deux graph cote à cote

par(mfrow = c(1, 1))  
#-------------------------------------
  
# Bland-Altman Plot 3: CKDEPImix2009  vs DFGindexé

  
par(mfrow = c(1, 2))  # 1 row, 2 columns

#Moyenne 

Iohexol <- Iohexol %>%
  mutate(Mean3 = (CKDEPImix2009 + DFGindexé) / 2,
         Diff3 = CKDEPImix2009 - DFGindexé)

# Calcul des statistiques
moyenne_diff3 <- mean(Iohexol$Diff3)
ecart_type_diff3 <- sd(Iohexol$Diff3)
n <- nrow(Iohexol)  
se_diff3 <- ecart_type_diff3 / sqrt(n)

limites_inferieures3 <- moyenne_diff3 - 1.96 * ecart_type_diff3
limites_superieures3 <- moyenne_diff3 + 1.96 * ecart_type_diff3

ic_biais_inf3 <- moyenne_diff3 - 1.96 * se_diff3
ic_biais_sup3 <- moyenne_diff3 + 1.96 * se_diff3


# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean3, Iohexol$Diff3, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + CKD-EPI mix 2009)/2",
  ylab = "Différence (CKD-EPI mix 2009 - DFG mesuré)",
  main = "C",
  ylim = c(-100, 100))


# Ajout de la ligne de biais
abline(h = moyenne_diff3, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures3, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures3, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf3, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup3, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)


# Bland-Altman Plot 4: CKDEPImix2009avecrace  vs DFGindexé

Iohexol <- Iohexol %>%
  mutate(Mean4 = (CKDEPImix2009avecrace + DFGindexé) / 2,
         Diff4 = CKDEPImix2009avecrace - DFGindexé)

# Calcul des statistiques
moyenne_diff4 <- mean(Iohexol$Diff4)
ecart_type_diff4 <- sd(Iohexol$Diff4)
n <- nrow(Iohexol)  
se_diff4 <- ecart_type_diff4 / sqrt(n)

limites_inferieures4 <- moyenne_diff4 - 1.96 * ecart_type_diff4
limites_superieures4 <- moyenne_diff4 + 1.96 * ecart_type_diff4

ic_biais_inf4 <- moyenne_diff4 - 1.96 * se_diff4
ic_biais_sup4 <- moyenne_diff4 + 1.96 * se_diff4


# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean4, Iohexol$Diff4, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + CKD-EPI mix 2009 avec CE)/2",
  ylab = "Différence (CKD-EPI mix 2009 avec CE - DFG mesuré)",
  main = "D",
  ylim = c(-100, 100))


# Ajout de la ligne de biais
abline(h = moyenne_diff4, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures4, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures4, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf4, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup4, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)

# reunitialiser les deux graph cote à cote
par(mfrow = c(1, 1))  

#--------------------
#=========================================================================================================================================
#V-COMPARAISON DES EQUATIONS EKFC PAR RAPPORT EKFC POPULATION 
#=========================================================================================================================================

#Figure : Courbes de comparaisons de l’équation EKFC et EKFC cr population (A), EKFC cr et EKFC mix population (B), EKFC cys et EKFC  mix population (C), EKFC mix et EKFC mix population (D)

p581 <- ggplot(Iohexol, aes(EKFCcre, EKFCcrePopulation)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("EKFC cre") +
  xlab("EKFC cre PS") +
  ggtitle("A") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p581

p582 <- ggplot(Iohexol, aes(EKFCcre, EKFCmixPS)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("EKFC cre") +
  xlab("EKFC mix PS") +
  ggtitle("B") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p582

p583 <- ggplot(Iohexol, aes(EKFCcys, EKFCmixPS)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("EKFC cys C") +
  xlab("EKFC mix PS") +
  ggtitle("C") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p583

p584 <- ggplot(Iohexol, aes(EKFCmix, EKFCmixPS)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("EKFC mix") +
  xlab("EKFC mix PS") +
  ggtitle("D") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p584

combined_plot96 <- p581+ p582+ p583 + p584
combined_plot96
#=============================================================================================================================================
#CHAPITRE VI-COMPARAISON DES PERFORMANCES DES EQUATIONS D’ESTIMATION DU DFG
#=======================================================================================================================================================
#Figure 16: Courbes de régression linéaire ; comparaison de la clairance plasmatique de
#l’iohexol avec les équations basées sur la créatinine : CKD-EPI 2009, CKD-EPI 2009 avec facteur racial, CKD-EPI 2021, EKFC créatinine et EKFC créatinine population.

#12 figures :A-L

#Créatinine (A-E)

p25 <- ggplot(Iohexol, aes(DFGindexé, CKDEPI2009)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("DFG mesuré") +
  xlab("CKD-EPI 2009") +
  ggtitle("A") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p25

p26 <- ggplot(Iohexol, aes(DFGindexé, CKDEPI2009avecrace)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("DFG mesuré") +
  xlab("CKD-EPI 2009 avec CE") +
  ggtitle("B") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p26

p27 <- ggplot(Iohexol, aes(DFGindexé, CKDEPI2021)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("DFG mesuré") +
  xlab("CKD-EPI 2021") +
  ggtitle("C") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p27

p28 <- ggplot(Iohexol, aes(DFGindexé, EKFCcre)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("DFG mesuré") +
  xlab("EKFC cr") +
  ggtitle("D") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p28

p29 <- ggplot(Iohexol, aes(DFGindexé, EKFCcrePopulation)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("DFG mesuré") +
  xlab("EKFC cr PS") +
  ggtitle("E") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p29

combined_plot20x <- p25 + p26 + p27 + p28 + p29
combined_plot20x

---------------------
  #cystatine C (F-I)
  
  p30 <- ggplot(Iohexol, aes(DFGindexé, CKDEPIcys)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("DFG mesuré") +
  xlab("CKD-EPI cys C") +
  ggtitle("F") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p30

p31 <- ggplot(Iohexol, aes(DFGindexé, CKDEPImix2009)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("DFG mesuré") +
  xlab("CKD-EPI mix 2009") +
  ggtitle("G") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p31

p32 <- ggplot(Iohexol, aes(DFGindexé, CKDEPImix2009avecrace)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("DFG mesuré") +
  xlab("CKD-EPI 2009 mix avec CE") +
  ggtitle("H") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p32

p33 <- ggplot(Iohexol, aes(DFGindexé, CKDEPImix2021)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("DFG mesuré") +
  xlab("CKD-EPI mix 2021") +
  ggtitle("I") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p33

combined_plot30x <- p30 + p31 + p32 + p33
combined_plot30x
#-------------------------
  # (J-L)
  
  p40 <- ggplot(Iohexol, aes(DFGindexé,  EKFCcys)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("DFG mesuré") +
  xlab(" EKFC cys C") +
  ggtitle("J") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p40

p41 <- ggplot(Iohexol, aes(DFGindexé, EKFCmix)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("DFG mesuré") +
  xlab("EKFC mix ") +
  ggtitle("K") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p41

p42 <- ggplot(Iohexol, aes(DFGindexé, EKFCmixPS)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed") + # Courbe de régression linéaire
  geom_smooth(se = FALSE, color = "blue") + # Courbe de corrélation non paramétrique
  #stat_poly_eq(aes(label = paste("y = ", ..eq.label.., sep = "")), formula = y ~ x, 
  #color = "red", size = 5, parse = TRUE) + # Affichage de l'équation de régression
  ylab("DFG mesuré") +
  xlab("EKFC mix PS") +
  ggtitle("L") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) + 
  theme(legend.title = element_blank(), legend.position = "bottom") # Positionne la légende
p42

combined_plot40x <- p40 + p41 + p42
combined_plot40x

#======================================================================================
#Suite Brand Artman (12 equations)

#Figure : Courbes de Bland-Altman : comparaison de la clairance plasmatique de l’iohexol avec les équations CKD-EPI 2009 sans facteur (A), CKD-EPI 2009 avec facteur (B), CKD-EPI mix 2009 sans facteur (C) et CKD-EPI mix 2009 avec facteur (D).

#Etape 1: Calcul des biais pour chaque formule

Iohexol$bias_CKDEPI2009_DGF <- (Iohexol$CKDEPI2009 - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_CKDEPI2009_DGF)

Iohexol$bias_CKDEPI2009avecrace_DGF <- (Iohexol$CKDEPI2009avecrace - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_CKDEPI2009avecrace_DGF)

Iohexol$bias_CKDEPI2021 <- (Iohexol$CKDEPI2021- Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_CKDEPI2021)

Iohexol$bias_CKDEPIcys <- (Iohexol$CKDEPIcys- Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_CKDEPIcys)

Iohexol$bias_CKDEPImix2009 <- (Iohexol$CKDEPImix2009 - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_CKDEPImix2009)

Iohexol$bias_CKDEPImix2009avecrace <- (Iohexol$CKDEPImix2009avecrace - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_CKDEPImix2009avecrace)

Iohexol$bias_CKDEPImix2021 <- (Iohexol$CKDEPImix2021 - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_CKDEPImix2021)

Iohexol$bias_EKFCcre <- (Iohexol$EKFCcre - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_EKFCcre)

Iohexol$bias_EKFCcrepop <- (Iohexol$EKFCcrePopulation - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_EKFCcrepop)

Iohexol$bias_EKFCcys <- (Iohexol$EKFCcys - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_EKFCcys)

Iohexol$bias_EKFCmix <- (Iohexol$EKFCmix - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_EKFCmix )

Iohexol$bias_EKFCmixpop <- (Iohexol$EKFCmixPS - Iohexol$DFGindexé)
diff_median <- median(Iohexol$bias_EKFCmixpop)

#save(Iohexol, file = "Iohexol.Rda") #Enregistrement de la nouvelle BD avec les biais

#Etape 2: Rechape la BD en format longer

#Iohexol_long <- Iohexol %>%
# pivot_longer(
# cols = starts_with("bias"), # Colonnes contenant les bias de formules ( biai_EKFC, etc.)
# names_to = "Formules",          # Nouvelle colonne pour les noms des méthodes
#values_to = "Bias"  )            # Nouvelle colonne pour les valeurs des biais

#head(Iohexol_long)

#Iohexol_long$Formules <- as.factor(Iohexol_long$Formules)

#Etape 3: Filtre sur les formules sur lequelles on va travailler

#Iohexol_l1 <- Iohexol_long %>%
#filter(Formules %in% c("bias_CKDEPI2009_DGF", "bias_CKDEPI2009avecrace_DGF",
#"bias_CKDEPI2021", "bias_EKFCcre", "bias_EKFCcrepop"))
#unique(Iohexol_l1$Formules)
#table(Iohexol_l1$Formules)

#Etape 4 : Realisation de du plo de Bland-Altman

# Bland-Altman Plot 1: CKDEPI2009  vs DFGindexé

par(mfrow = c(1, 2))  # 1 row, 2 columns

#Moyenne 

Iohexol <- Iohexol %>%
  mutate(Mean1 = (CKDEPI2009 + DFGindexé) / 2,
         Diff1 = CKDEPI2009 - DFGindexé)

# Calcul des statistiques
moyenne_diff <- mean(Iohexol$Diff1)
ecart_type_diff <- sd(Iohexol$Diff1)
n <- nrow(Iohexol)  
se_diff <- ecart_type_diff / sqrt(n)

limites_inferieures <- moyenne_diff - 1.96 * ecart_type_diff
limites_superieures <- moyenne_diff + 1.96 * ecart_type_diff

ic_biais_inf <- moyenne_diff - 1.96 * se_diff
ic_biais_sup <- moyenne_diff + 1.96 * se_diff

# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean1, Iohexol$Diff1, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + CKD-EPI 2009)/2",
  ylab = "Différence (CKD-EPI 2009 - DFG mesuré)",
  main = "A",
  ylim = c(-100, 100))

# Ajout de la ligne de biais
abline(h = moyenne_diff, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)
-------------------------------------------
# Bland-Altman Plot 1: CKDEPI2009  vs DFGindexé

par(mfrow = c(1, 2))  # 1 row, 2 columns

#Moyenne 

Iohexol <- Iohexol %>%
  mutate(Mean1 = (CKDEPI2009 + DFGindexé) / 2,
         Diff1 = CKDEPI2009 - DFGindexé)

# Calcul des statistiques
moyenne_diff <- mean(Iohexol$Diff1)
ecart_type_diff <- sd(Iohexol$Diff1)
n <- nrow(Iohexol)  
se_diff <- ecart_type_diff / sqrt(n)

limites_inferieures <- moyenne_diff - 1.96 * ecart_type_diff
limites_superieures <- moyenne_diff + 1.96 * ecart_type_diff

ic_biais_inf <- moyenne_diff - 1.96 * se_diff
ic_biais_sup <- moyenne_diff + 1.96 * se_diff

# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean1, Iohexol$Diff1, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + CKD-EPI 2009)/2",
  ylab = "Différence (CKD-EPI 2009 - DFG mesuré)",
  main = "A",
  ylim = c(-100, 100))

# Ajout de la ligne de biais
abline(h = moyenne_diff, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)

# Bland-Altman Plot 2: CKDEPI2009avecrace  vs DFGindexé

Iohexol <- Iohexol %>%
  mutate(Mean2 = (CKDEPI2009avecrace + DFGindexé) / 2,
         Diff2 = CKDEPI2009avecrace - DFGindexé)

# Calcul des statistiques
moyenne_diff2 <- mean(Iohexol$Diff2)
ecart_type_diff2 <- sd(Iohexol$Diff2)
n <- nrow(Iohexol)  
se_diff2 <- ecart_type_diff2 / sqrt(n)

limites_inferieures2 <- moyenne_diff2 - 1.96 * ecart_type_diff2
limites_superieures2 <- moyenne_diff2 + 1.96 * ecart_type_diff2

ic_biais_inf2 <- moyenne_diff2 - 1.96 * se_diff2
ic_biais_sup2 <- moyenne_diff2 + 1.96 * se_diff2


# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean2, Iohexol$Diff2, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + CKD-EPI 2009 avec CE)/2",
  ylab = "Différence (CKD-EPI 2009 avec CE - DFG mesuré)",
  main = "B",
  ylim = c(-100, 100))


# Ajout de la ligne de biais
abline(h = moyenne_diff2, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures2, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures2, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf2, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup2, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)

# reunitialiser les deux graph cote à cote

par(mfrow = c(1, 1))  
#-------------------------------------

# Bland-Altman Plot 3: CKDEPImix2009  vs DFGindexé


par(mfrow = c(1, 2))  # 1 row, 2 columns

#Moyenne 

Iohexol <- Iohexol %>%
  mutate(Mean3 = (CKDEPImix2009 + DFGindexé) / 2,
         Diff3 = CKDEPImix2009 - DFGindexé)

# Calcul des statistiques
moyenne_diff3 <- mean(Iohexol$Diff3)
ecart_type_diff3 <- sd(Iohexol$Diff3)
n <- nrow(Iohexol)  
se_diff3 <- ecart_type_diff3 / sqrt(n)

limites_inferieures3 <- moyenne_diff3 - 1.96 * ecart_type_diff3
limites_superieures3 <- moyenne_diff3 + 1.96 * ecart_type_diff3

ic_biais_inf3 <- moyenne_diff3 - 1.96 * se_diff3
ic_biais_sup3 <- moyenne_diff3 + 1.96 * se_diff3


# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean3, Iohexol$Diff3, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + CKD-EPI mix 2009)/2",
  ylab = "Différence (CKD-EPI mix 2009 - DFG mesuré)",
  main = "C",
  ylim = c(-100, 100))


# Ajout de la ligne de biais
abline(h = moyenne_diff3, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures3, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures3, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf3, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup3, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)


# Bland-Altman Plot 4: CKDEPImix2009avecrace  vs DFGindexé

Iohexol <- Iohexol %>%
  mutate(Mean4 = (CKDEPImix2009avecrace + DFGindexé) / 2,
         Diff4 = CKDEPImix2009avecrace - DFGindexé)

# Calcul des statistiques
moyenne_diff4 <- mean(Iohexol$Diff4)
ecart_type_diff4 <- sd(Iohexol$Diff4)
n <- nrow(Iohexol)  
se_diff4 <- ecart_type_diff4 / sqrt(n)

limites_inferieures4 <- moyenne_diff4 - 1.96 * ecart_type_diff4
limites_superieures4 <- moyenne_diff4 + 1.96 * ecart_type_diff4

ic_biais_inf4 <- moyenne_diff4 - 1.96 * se_diff4
ic_biais_sup4 <- moyenne_diff4 + 1.96 * se_diff4


# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean4, Iohexol$Diff4, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + CKD-EPI mix 2009 avec CE)/2",
  ylab = "Différence (CKD-EPI mix 2009 avec CE - DFG mesuré)",
  main = "D",
  ylim = c(-100, 100))


# Ajout de la ligne de biais
abline(h = moyenne_diff4, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures4, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures4, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf4, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup4, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)

# reunitialiser les deux graph cote à cote
par(mfrow = c(1, 1))  
---------------------------
  
par(mfrow = c(1, 2))  # 1 row, 2 columns 
#Moyenne 

Iohexol <- Iohexol %>%
  mutate(Mean5 = (CKDEPI2021 + DFGindexé) / 2,
         Diff5 = CKDEPI2021 - DFGindexé)

# Calcul des statistiques
moyenne_diff5 <- mean(Iohexol$Diff5)
ecart_type_diff5 <- sd(Iohexol$Diff5)
n <- nrow(Iohexol)  
se_diff5 <- ecart_type_diff5 / sqrt(n)

limites_inferieures5 <- moyenne_diff5 - 1.96 * ecart_type_diff5
limites_superieures5 <- moyenne_diff5 + 1.96 * ecart_type_diff5

ic_biais_inf5 <- moyenne_diff5 - 1.96 * se_diff5
ic_biais_sup5 <- moyenne_diff5 + 1.96 * se_diff5


# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean5, Iohexol$Diff5, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + CKD-EPI 2021)/2",
  ylab = "Différence (CKD-EPI 2021 - DFG mesuré)",
  main = "E",
  ylim = c(-100, 100))


# Ajout de la ligne de biais
abline(h = moyenne_diff5, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures5, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures5, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf5, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup5, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)

#-----------------

#Moyenne 

Iohexol <- Iohexol %>%
  mutate(Mean6 = (EKFCcre + DFGindexé) / 2,
         Diff6 = EKFCcre - DFGindexé)

# Calcul des statistiques
moyenne_diff6 <- mean(Iohexol$Diff6)
ecart_type_diff6 <- sd(Iohexol$Diff6)
n <- nrow(Iohexol)  
se_diff6 <- ecart_type_diff6 / sqrt(n)

limites_inferieures6 <- moyenne_diff6 - 1.96 * ecart_type_diff6
limites_superieures6 <- moyenne_diff6 + 1.96 * ecart_type_diff6

ic_biais_inf6 <- moyenne_diff6 - 1.96 * se_diff6
ic_biais_sup6 <- moyenne_diff6 + 1.96 * se_diff6


# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean6, Iohexol$Diff6, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + EKFC cre)/2",
  ylab = "Différence (EKFC cre - DFG mesuré)",
  main = "F",
  ylim = c(-100, 100))


# Ajout de la ligne de biais
abline(h = moyenne_diff6, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures6, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures6, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf6, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup6, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)


# Reset plotting area
par(mfrow = c(1, 1))  # Back to single plot layout
#------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1, 2))  # 1 row, 2 columns 

#Moyenne 

Iohexol <- Iohexol %>%
  mutate(Mean7 = (EKFCcrePopulation + DFGindexé) / 2,
         Diff7 = EKFCcrePopulation - DFGindexé)

# Calcul des statistiques
moyenne_diff7 <- mean(Iohexol$Diff7)
ecart_type_diff7 <- sd(Iohexol$Diff7)
n <- nrow(Iohexol)  
se_diff7 <- ecart_type_diff7 / sqrt(n)

limites_inferieures7 <- moyenne_diff7 - 1.96 * ecart_type_diff7
limites_superieures7 <- moyenne_diff7 + 1.96 * ecart_type_diff7

ic_biais_inf7 <- moyenne_diff7 - 1.96 * se_diff7
ic_biais_sup7 <- moyenne_diff7 + 1.96 * se_diff7


# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean7, Iohexol$Diff7, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + EKFC cre PS)/2",
  ylab = "Différence (EKFC cre PS - DFG mesuré)",
  main = "G",
  ylim = c(-100, 100))


# Ajout de la ligne de biais
abline(h = moyenne_diff7, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures7, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures7, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf7, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup7, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)
#------------------------
#Moyenne 

Iohexol <- Iohexol %>%
  mutate(Mean8 = (CKDEPIcys + DFGindexé) / 2,
         Diff8 = CKDEPIcys - DFGindexé)

# Calcul des statistiques
moyenne_diff8 <- mean(Iohexol$Diff8)
ecart_type_diff8 <- sd(Iohexol$Diff8)
n <- nrow(Iohexol)  
se_diff8 <- ecart_type_diff8 / sqrt(n)

limites_inferieures8 <- moyenne_diff8 - 1.96 * ecart_type_diff8
limites_superieures8 <- moyenne_diff8 + 1.96 * ecart_type_diff8

ic_biais_inf8 <- moyenne_diff8 - 1.96 * se_diff8
ic_biais_sup8 <- moyenne_diff8 + 1.96 * se_diff8


# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean8, Iohexol$Diff8, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + CKD-EPI cys C)/2",
  ylab = "Différence (CKD-EPI cys C - DFG mesuré)",
  main = "H",
  ylim = c(-100, 100))


# Ajout de la ligne de biais
abline(h = moyenne_diff8, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures8, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures8, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf8, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup8, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)

# Reset plotting area
par(mfrow = c(1, 1))  # Back to single plot layout

#-------------------------------------------------------------------------

par(mfrow = c(1, 2))  # 1 row, 2 columns 

#Moyenne 

Iohexol <- Iohexol %>%
  mutate(Mean11 = (CKDEPImix2021 + DFGindexé) / 2,
         Diff11 = CKDEPImix2021 - DFGindexé)

# Calcul des statistiques
moyenne_diff11 <- mean(Iohexol$Diff11)
ecart_type_diff11 <- sd(Iohexol$Diff11)
n <- nrow(Iohexol)  
se_diff11 <- ecart_type_diff11 / sqrt(n)

limites_inferieures11 <- moyenne_diff11 - 1.96 * ecart_type_diff11
limites_superieures11 <- moyenne_diff11 + 1.96 * ecart_type_diff11

ic_biais_inf11 <- moyenne_diff11 - 1.96 * se_diff11
ic_biais_sup11 <- moyenne_diff11 + 1.96 * se_diff11


# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean11, Iohexol$Diff11, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + CKD-EPI mix 2021)/2",
  ylab = "Différence (CKD-EPI mix 2021 - DFG mesuré)",
  main = "I",
  ylim = c(-100, 100))


# Ajout de la ligne de biais
abline(h = moyenne_diff11, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures11, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures11, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf11, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup11, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)

#--------------------

Iohexol <- Iohexol %>%
  mutate(Mean12 = (EKFCcys + DFGindexé) / 2,
         Diff12 = EKFCcys - DFGindexé)

# Calcul des statistiques
moyenne_diff12<- mean(Iohexol$Diff12)
ecart_type_diff12 <- sd(Iohexol$Diff12)
n <- nrow(Iohexol)  
se_diff12 <- ecart_type_diff12 / sqrt(n)

limites_inferieures12 <- moyenne_diff12 - 1.96 * ecart_type_diff12
limites_superieures12 <- moyenne_diff12 + 1.96 * ecart_type_diff12

ic_biais_inf12 <- moyenne_diff12 - 1.96 * se_diff12
ic_biais_sup12 <- moyenne_diff12 + 1.96 * se_diff12


# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean12, Iohexol$Diff12, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + EKFC cys C)/2",
  ylab = "Différence (EKFC cys C - DFG mesuré)",
  main = "J",
  ylim = c(-100, 100))


# Ajout de la ligne de biais
abline(h = moyenne_diff12, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures12, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures12, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf12, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup12, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)


# Reset plotting area
par(mfrow = c(1, 1))  # Back to single plot layout
#------------------------------------------------------------------------------------------------------

par(mfrow = c(1, 2))  # 1 row, 2 columns 

#Moyenne 

Iohexol <- Iohexol %>%
  mutate(Mean13 = (EKFCmix + DFGindexé) / 2,
         Diff13 = EKFCmix - DFGindexé)

# Calcul des statistiques
moyenne_diff13 <- mean(Iohexol$Diff13)
ecart_type_diff13 <- sd(Iohexol$Diff13)
n <- nrow(Iohexol)  
se_diff13 <- ecart_type_diff13 / sqrt(n)

limites_inferieures13 <- moyenne_diff13 - 1.96 * ecart_type_diff13
limites_superieures13 <- moyenne_diff13 + 1.96 * ecart_type_diff13

ic_biais_inf13 <- moyenne_diff13 - 1.96 * se_diff13
ic_biais_sup13 <- moyenne_diff13 + 1.96 * se_diff13


# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean13, Iohexol$Diff13, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + EKFC mix)/2",
  ylab = "Différence (EKFC mix - DFG mesuré)",
  main = "K",
  ylim = c(-100, 100))


# Ajout de la ligne de biais
abline(h = moyenne_diff13, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures13, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures13, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf13, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup13, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)

#--------------------

Iohexol <- Iohexol %>%
  mutate(Mean14 = (EKFCmixPS + DFGindexé) / 2,
         Diff14 = EKFCmixPS - DFGindexé)

# Calcul des statistiques
moyenne_diff14<- mean(Iohexol$Diff14)
ecart_type_diff14 <- sd(Iohexol$Diff14)
n <- nrow(Iohexol)  
se_diff14 <- ecart_type_diff14 / sqrt(n)

limites_inferieures14 <- moyenne_diff14 - 1.96 * ecart_type_diff14
limites_superieures14 <- moyenne_diff14 + 1.96 * ecart_type_diff14

ic_biais_inf14<- moyenne_diff14 - 1.96 * se_diff14
ic_biais_sup14 <- moyenne_diff14 + 1.96 * se_diff14


# Tracé du graphique Bland-Altman
plot(
  Iohexol$Mean14, Iohexol$Diff14, 
  pch = 16, col = "blue",
  xlab = "Moyenne (DFG mesuré + EKFC mix PS)/2",
  ylab = "Différence (EKFC mix PS - DFG mesuré)",
  main = "L",
  ylim = c(-100, 100))


# Ajout de la ligne de biais
abline(h = moyenne_diff14, col = "blue", lwd = 2)

# Ajout des limites d'accord (Les deux distributions)
abline(h = limites_superieures14, col = "red", lty = 2, lwd = 2)
abline(h = limites_inferieures14, col = "red", lty = 2, lwd = 2)

# Ajout des limites d'accord (Biais)

abline(h = ic_biais_inf14, col = "blue", lty = 2, lwd = 2)
abline(h = ic_biais_sup14, col = "blue", lty = 2, lwd = 2)

# Ajout des légendes
legend(
  "bottom",
  legend = c("Biais", "IC Biais (95%)", "IC (95%)"),
  col = c("blue", "blue", "red"),
  lty = c(1, 2, 2),
  lwd = 2,
  box.lty = 0)


# Reset plotting area
par(mfrow = c(1, 1))  # Back to single plot layout
#========================================================================================================
##bland_altman chez lees volontaires saints
#========================================================================================================= 

#Créé la fonction bland_altman

bland_altman_plot <- function(data, measured, estimated, main_title) {
  # Create mean and difference
  Mean <- (data[[measured]] + data[[estimated]]) / 2
  Diff <- data[[estimated]] - data[[measured]]
  
  # Compute statistics
  moyenne_diff <- mean(Diff, na.rm = TRUE)
  ecart_type_diff <- sd(Diff, na.rm = TRUE)
  n <- sum(!is.na(Diff))
  se_diff <- ecart_type_diff / sqrt(n)
  
  # Limits of agreement
  limites_inferieures <- moyenne_diff - 1.96 * ecart_type_diff
  limites_superieures <- moyenne_diff + 1.96 * ecart_type_diff
  
  # Confidence interval for bias
  ic_biais_inf <- moyenne_diff - 1.96 * se_diff
  ic_biais_sup <- moyenne_diff + 1.96 * se_diff
  
  # Plot
  plot(
    Mean, Diff,
    pch = 16, col = "blue",
    xlab = paste0("Moyenne (", estimated, " + ", measured, ") / 2"),
    ylab = paste0("Difference (", estimated, " - ", measured, ")"),
    main = main_title,
    ylim = c(-100, 100)
  )
  
  abline(h = moyenne_diff, col = "blue", lwd = 2)
  abline(h = limites_superieures, col = "red", lty = 2, lwd = 2)
  abline(h = limites_inferieures, col = "red", lty = 2, lwd = 2)
  abline(h = ic_biais_inf, col = "blue", lty = 2, lwd = 2)
  abline(h = ic_biais_sup, col = "blue", lty = 2, lwd = 2)
  
  legend(
    "bottom",
    legend = c("Bias", "CI Bias (95%)", "Limits of Agreement (95%)"),
    col = c("blue", "blue", "red"),
    lty = c(1, 2, 2),
    lwd = 2,
    box.lty = 0
  )
}


#Appeler les graph

par(mfrow = c(1, 2))  # Two plots side by side
bland_altman_plot(Iohexol_60, measured = "DFGindex", estimated = "CKDEPI2009", main_title = "A")
bland_altman_plot(Iohexol_60, measured = "DFGindex", estimated = "CKDEPI2009avecrace", main_title = "B")

par(mfrow = c(1, 2))
bland_altman_plot(Iohexol_60, "DFGindex", "CKDEPImix2009", "C")
bland_altman_plot(Iohexol_60, "DFGindex", "CKDEPImix2009avecrace", "D")

par(mfrow = c(1, 2))
bland_altman_plot(Iohexol_60, "DFGindex", "CKDEPI2021", "E")
bland_altman_plot(Iohexol_60, "DFGindex", "EKFCcre", "F")

par(mfrow = c(1, 2))
bland_altman_plot(Iohexol_60, "DFGindex", "EKFCcrePopulation", "G")
bland_altman_plot(Iohexol_60, "DFGindex", "CKDEPIcys", "H")

par(mfrow = c(1, 2))
bland_altman_plot(Iohexol_60, "DFGindex", "CKDEPImix2021", "I")
bland_altman_plot(Iohexol_60, "DFGindex", "EKFCcys", "J")

par(mfrow = c(1, 2))
bland_altman_plot(Iohexol_60, "DFGindex", "EKFCmix", "K")
bland_altman_plot(Iohexol_60, "DFGindex", "EKFCmixPS", "L")

par(mfrow = c(1, 1))  # Reset layout

#============================================================
#Graphique du Prof Delanay

library(haven)
library(foreign)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(questionr)

#1. Créer la base longue avec indicateur P30
# Remplacer ici par noms de colonnes réels

#Iohexol_long2 <- Iohexol1 %>%
#pivot_longer(cols = starts_with("eGFR_"), 
# names_to = "Formules", 
#values_to = "eGFR_est") %>%
# mutate(
#P30 = abs(eGFR_est - DFGindexé) / DFGindexé <= 0.3,
#tranche_age = cut(Age, breaks = seq(10, 100, by = 5), right = FALSE),
#age_moyen = ifelse(
#is.na(tranche_age),
#NA,
#as.numeric(gsub("\\[|\\)|\\]", "", str_extract(as.character(tranche_age), "^\\[?\\d+")))
#) + 2.5)

#save(Iohexol_long2, file = "Iohexol_long2.Rda")

#2. Calculer %P30 par Formule et âge

#Iohexol_long2 <- rename(Iohexol_long2, Formules = Méthode) #Dpyr

df_p30 <- Iohexol_long2 %>%
  group_by(Formules, age_moyen) %>%
  summarise(P30_percent = mean(P30, na.rm = TRUE) * 100, .groups = "drop")

#3.1. Tracer les courbes P30: Equations basés sur la créatinine

df_p30A <- df_p30 %>%
  filter(Formules %in% c("CKD-EPI 2009", 
                        "CKD-EPI 2009 avec CE",
                        "CKD-EPI 2021",
                        "EKFC cre",
                        "EKFC cre PS"))

ggplot(df_p30A, aes(x = age_moyen, y = P30_percent, color = Formules, linetype = Formules)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 50, ymax = 100, fill = "grey30", alpha = 0.1) +
  geom_smooth(se = FALSE, size = 1.2) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "P30 : % des estimations de DFG dans ±30% de la valeur mesurée",
    x = "Âge (années)",
    y = "P30 (%)",
    color = "Formules",
    linetype = "Formules"
  ) +
  theme_minimal()

#Seuil P30: 75%

ggplot(df_p30A, aes(x = age_moyen, y = P30_percent, color = Formules, linetype = Formules)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 75, ymax = 100, fill = "grey30", alpha = 0.1) +
  geom_smooth(se = FALSE, size = 1.2) +
  scale_y_continuous(limits = c(50, 100)) +
  labs(
    title = "Seuil de performance (P30 > 75%)",
    x = "Âge (années)",
    y = "P30 (%)",
    color = "Formules",
    linetype = "Formules"
  ) +
  theme_minimal()

==================================
  #P30 selon le GFR mesurée
  
  df_last <- Iohexol_long2 %>%
  mutate(P30 = abs(eGFR_est - DFGindexé) / DFGindexé <= 0.3,
         tranche_mGFR = cut(DFGindexé, breaks = seq(0, 150, by = 10), right = FALSE),
         mGFR_moyen = ifelse(
           is.na(tranche_mGFR),
           NA,
           as.numeric(gsub("\\[|\\)|\\]", "", str_extract(as.character(tranche_mGFR), "^\\[?\\d+"))) #Extrait le début du libellé de la tranche 
         ) + 5
  )


#2. Calcul du %P30 selon mGFR_moyen et Formules

df_p30_mGFR_meth <- df_last %>%
  group_by(Formules, mGFR_moyen) %>%
  summarise(P30_percent = mean(P30, na.rm = TRUE) * 100, .groups = "drop")



#3.  Graphique P30 vs mGFR

df_lastA <- df_p30_mGFR_meth %>%
  filter(Formules %in% c("CKD-EPI 2009", 
                         "CKD-EPI 2009 avec CE",
                         "CKD-EPI 2021",
                         "EKFC cre",
                         "EKFC cre PS"
  ))


ggplot(df_lastA, aes(x = mGFR_moyen, y = P30_percent, color = Formules, linetype = Formules)) +
  geom_smooth(se = FALSE, size = 1.2) +
  annotate("rect", xmin = 0, xmax = 150, ymin = 75, ymax = 100, fill = "gray", alpha = 0.2) +
  labs(
    title = "P30 (%) selon le DFG mesuré",
    x = "DFG mesuré (ml/min/1.73m²)",
    y = "P30 (%)",
    color = "Formules",
    linetype = "Formules"
  ) +
  scale_y_continuous(limits = c(25, 100)) +
  theme_minimal()


#===================================
#3.2. Tracer les courbes P30: Equations basés sur la cystatine C

df_p30B <- df_p30 %>%
  filter(Formules %in% c("CKD-EPI cys C", 
                        "CKD-EPI-mix-2009",
                        "CKD-EPI mix 2009 avec CE",
                        "CKD-EPI mix 2021",
                        "EKFC cys C",
                        "EKFC mix",
                        "EKFC mix PS"))

ggplot(df_p30B, aes(x = age_moyen, y = P30_percent, color = Formules, linetype = Formules)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 50, ymax = 100, fill = "grey30", alpha = 0.1) +
  geom_smooth(se = FALSE, size = 1.2) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "P30 : % des estimations de DFG dans ±30% de la valeur mesurée",
    x = "Âge (années)",
    y = "P30 (%)",
    color = "Formules",
    linetype = "Formules"
  ) +
  scale_y_continuous(limits = c(25, 100)) +
  theme_minimal()

#Seuil P30: 75%

ggplot(df_p30B, aes(x = age_moyen, y = P30_percent, color = Formules, linetype = Formules)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 75, ymax = 100, fill = "grey30", alpha = 0.1) +
  geom_smooth(se = FALSE, size = 1.2) +
  scale_y_continuous(limits = c(50, 100)) +
  labs(
    title = "Seuil de performance (P30 > 75%)",
    x = "Âge (années)",
    y = "P30 (%)",
    color = "Formules",
    linetype = "Formules"
  ) +
  theme_minimal()

#Equations basé sur Cystatine"P30 (%) selon le DFG mesuré"

df_lastB <- df_p30_mGFR_meth %>%
  filter(Formules %in% c("CKD-EPI cys C", 
                         "CKD-EPI-mix-2009",
                         "CKD-EPI mix 2009 avec CE",
                         "CKD-EPI mix 2021",
                         "EKFC cys C",
                         "EKFC mix",
                         "EKFC mix PS"
  ))


ggplot(df_lastB, aes(x = mGFR_moyen, y = P30_percent, color = Formules, linetype = Formules)) +
  geom_smooth(se = FALSE, size = 1.2) +
  annotate("rect", xmin = 0, xmax = 150, ymin = 75, ymax = 100, fill = "gray", alpha = 0.2) +
  labs(
    title = "P30 (%) selon le DFG mesuré",
    x = "DFG mesuré (ml/min/1.73m²)",
    y = "P30 (%)",
    color = "Formules",
    linetype = "Formules"
  ) +
  scale_y_continuous(limits = c(25, 100)) +
  theme_minimal()
-----------------------------------------
 # Biais entre DFG estimé et mesuré selon l'âge
#/Equations basés sur la créatinine
  
  #1. calculer le biais  
  
  Iohexol_long2 <- Iohexol_long2 %>%
  mutate(biais = eGFR_est - DFGindexé)

#2. Moyenne du biais par tranche d’âge et Formules

df_biais <- Iohexol_long2 %>%
  group_by(Formules, age_moyen) %>%
  summarise(
    biais_moyen = mean(biais, na.rm = TRUE),
    .groups = "drop"
  )

#3. Graphique du biais

df_biais_A <- df_biais %>%
  filter(Formules %in% c("CKD-EPI 2009", 
                        "CKD-EPI 2009 avec CE",
                        "CKD-EPI 2021",
                        "EKFC cre",
                        "EKFC cre PS"))

ggplot(df_biais_A, aes(x = age_moyen, y = biais_moyen, color = Formules, linetype = Formules)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 5, fill = "grey30", alpha = 0.1) +
  geom_smooth(se = FALSE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Biais entre DFG estimé et mesuré selon l'âge",
    x = "Âge (années)",
    y = "Biais (DFG estimé - DFG mesuré) [ml/min/1.73m²]",
    color = "Formules",
    linetype = "Formules"
  ) +
  theme_minimal()

#/Equations basés sur la cystatine

#Cystatine

df_biais_B <- df_biais %>%
  filter(Formules %in% c("CKD-EPI cys C", 
                        "CKD-EPI-mix-2009",
                        "CKD-EPI mix 2009 avec CE",
                        "CKD-EPI mix 2021",
                        "EKFC cys C",
                        "EKFC mix",
                        "EKFC mix PS"))

ggplot(df_biais_B, aes(x = age_moyen, y = biais_moyen, color = Formules, linetype = Formules)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -5, ymax = 5, fill = "grey30", alpha = 0.1) +
  geom_smooth(se = FALSE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Biais entre DFG estimé et mesuré selon l'âge",
    x = "Âge (années)",
    y = "Biais (DFG estimé - DFG mesuré) [ml/min/1.73m²]",
    color = "Formules",
    linetype = "Formules"
  ) +
  theme_minimal()
----------------------------------------------------------------
  
  #Biais en fonction de la GFR mesurée (et non plus de l’âge).
  #/Equations basées sur la créatinine
  
  # 2. Grouper les données en tranches de mGFR (e.g., 10 unités)
  df_bmGFR <- Iohexol_long2 %>%
  mutate(gfr_group = cut(DFGindexé, breaks = seq(0, 150, by = 10), right = FALSE),
         gfr_moyen = ifelse(
           is.na(gfr_group),
           NA,
           as.numeric(gsub("\\[|\\)|\\]", "", str_extract(as.character(gfr_group), "^\\[?\\d+"))) #Extrait le début du libellé de la tranche 
         ) + 5
  )



# 3. Calculer le biais moyen par Formules et niveau de GFR mesurée
df_biais2 <- df_bmGFR %>%
  group_by(Formules, gfr_moyen) %>%
  summarise(biais_moyen = mean(biais, na.rm = TRUE), .groups = "drop")


df_biais2_A <- df_biais2 %>%
  filter(Formules %in% c("CKD-EPI 2009", 
                        "CKD-EPI 2009 avec CE",
                        "CKD-EPI 2021",
                        "EKFC cre",
                        "EKFC cre PS"
  ))

ggplot(df_biais2_A, aes(x = gfr_moyen, y = biais_moyen, color = Formules, linetype = Formules)) +
  geom_smooth(se = FALSE, size = 1.2) +
  annotate("rect", xmin = 0, xmax = 150, ymin = -5, ymax = 5, fill = "gray", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Biais par rapport au DFG mesuré",
    x = "DFG mesurée (ml/min/1.73m²)",
    y = "Biais (DFG estimé - DFG mesuré) [ml/min/1.73m²]",
    color = "Formules",
    linetype = "Formules"
  ) +
  #scale_y_continuous(limits = c(-20, 20)) +
  theme_minimal()

#/Equations basées sur la cystatine

df_biais2_B <- df_biais2 %>%
  filter(Formules %in% c("CKD-EPI cys C", 
                        "CKD-EPI-mix-2009",
                        "CKD-EPI mix 2009 avec CE",
                        "CKD-EPI mix 2021",
                        "EKFC cys C",
                        "EKFC mix",
                        "EKFC mix PS"
  ))

ggplot(df_biais2_B, aes(x = gfr_moyen, y = biais_moyen, color = Formules, linetype = Formules)) +
  geom_smooth(se = FALSE, size = 1.2) +
  annotate("rect", xmin = 0, xmax = 150, ymin = -5, ymax = 5, fill = "gray", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Biais par rapport au DFG mesuré",
    x = "DFG mesuré (ml/min/1.73m²)",
    y = "Biais (DFG estimé - DFG mesuré)",
    color = "Formules",
    linetype = "Formules"
  ) +
  #scale_y_continuous(limits = c(-20, 20)) +
  theme_minimal()
  
#=========================================================================================================
#Biais en focntion de l'IMC

# 2. Grouper les données en tranches de IMC (e.g., 5 unités)

df_IMC <- Iohexol_long2 %>%
  mutate(IMC_group = cut(IMC, breaks = seq(15, 50, by = 5), right = FALSE),
         IMC_moyen = ifelse(
           is.na(IMC_group),
           NA,
           as.numeric(gsub("\\[|\\)|\\]", "", str_extract(as.character(IMC_group), "^\\[?\\d+"))) #Extrait le début du libellé de la tranche 
         ) + 5)

# 3. Calculer le biais moyen par méthode et niveau de IMC

df_biais25 <- df_IMC %>%
  group_by(Formules, IMC_moyen) %>%
  summarise(biais_moyenIMC = mean(biais, na.rm = TRUE), .groups = "drop")

#Equations basés sur la créatinine selon l'IMC

df_biais2_I <- df_biais25 %>%
  filter(Formules %in% c("CKD-EPI 2009", 
                        "CKD-EPI 2009 avec CE",
                        "CKD-EPI 2021",
                        "EKFC cre",
                        "EKFC cre PS"
  ))

ggplot(df_biais2_I, aes(x = IMC_moyen, y = biais_moyenIMC, color = Formules, linetype = Formules)) +
  geom_smooth(se = FALSE, size = 1.2) +
  annotate("rect", xmin = 15, xmax = 50, ymin = -5, ymax = 5, fill = "gray", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Biais par rapport à l'IMC",
    x = "IMC (Kg/m²)",
    y = "Biais (DFG estimé - DFG mesuré) [ml/min/1.73m²]",
    color = "Formules",
    linetype = "Formules"
  ) +
  #scale_y_continuous(limits = c(-20, 20)) +
  theme_minimal()

#Equations basés sur la cystatine selon l'IMC

df_biais2_IB <- df_biais25 %>%
  filter(Formules %in% c("CKD-EPI cys C", 
                        "CKD-EPI-mix-2009",
                        "CKD-EPI mix 2009 avec CE",
                        "CKD-EPI mix 2021",
                        "EKFC cys C",
                        "EKFC mix",
                        "EKFC mix PS"
  ))

ggplot(df_biais2_IB, aes(x = IMC_moyen, y = biais_moyenIMC, color = Formules, linetype = Formules)) +
  geom_smooth(se = FALSE, size = 1.2) +
  annotate("rect", xmin = 15, xmax = 50, ymin = -5, ymax = 5, fill = "gray", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Biais par rapport à l'IMC",
    x = "IMC (Kg/m²)",
    y = "Biais (DFG estimé - DFG mesuré)",
    color = "Formules",
    linetype = "Formules"
  ) +
  #scale_y_continuous(limits = c(-20, 20)) +
  theme_minimal()
#=========================================
#Volontaires sains
#===========================================
#Iohexol_60 <- Iohexol_60["Identification"]

# Enlever les tirets dans Iohexol_60$Identification
#Iohexol_60$Identification <- gsub("-", "", Iohexol_60$Identification)

#Supprimer les espaces

#Iohexol1$Identification <- trimws(Iohexol1$Identification)
#Iohexol_60$Identification <- trimws(Iohexol_60$Identification)

#Jointure

#Iohexol_60 <- Iohexol_60 %>%
#left_join(Iohexol1, by = "Identification")

#Pivoter longer

#Iohexol_long3 <- Iohexol_60 %>%
# pivot_longer(
# cols = starts_with("eGFR_"), 
#names_to = "Formules", 
#values_to = "eGFR_est"
#) %>%
#mutate(
# P30 = abs(eGFR_est - DFGindexé) / DFGindexé <= 0.3,
#tranche_age = cut(Age, breaks = seq(10, 100, by = 5), right = FALSE),
#age_moyen = ifelse(
#  is.na(tranche_age),
#NA,
# as.numeric(gsub("\\[|\\)|\\]", "", str_extract(as.character(tranche_age), "^\\[?\\d+")))
#) + 2.5
# )


## Recoding df_p30$Méthode into df_p30$Méthode_rec
#df_p30$Méthode <- df_p30$Méthode
#df_p30$Méthode[df_p30$Méthode == "eGFR_CKDEPI2009"] <- "CKD-EPI 2009"
#df_p30$Méthode[df_p30$Méthode == "eGFR_CKDEPI2009avecrace"] <- "CKD-EPI 2009 avec CE"
#df_p30$Méthode[df_p30$Méthode == "eGFR_CKDEPI2021"] <- "CKD-EPI 2021"
#df_p30$Méthode[df_p30$Méthode == "eGFR_CKDEPIcys"] <- "CKD-EPI cys C"
#df_p30$Méthode[df_p30$Méthode == "eGFR_CKDEPImix2009"] <- "CKD-EPI mix 2009"
#df_p30$Méthode[df_p30$Méthode == "eGFR_CKDEPImix2009avecrace"] <- "CKD-EPI mix 2009 avec CE"
#df_p30$Méthode[df_p30$Méthode == "eGFR_CKDEPImix2021"] <- "CKD-EPI mix 2021"
#df_p30$Méthode[df_p30$Méthode == "eGFR_Créatinine"] <- "Créatinine"
#df_p30$Méthode[df_p30$Méthode == "eGFR_CystatineC"] <- "CystatineC"
#df_p30$Méthode[df_p30$Méthode == "eGFR_EKFCcre"] <- "EKFC cre"
#df_p30$Méthode[df_p30$Méthode == "eGFR_EKFCcrePopulation"] <- "EKFC cre PS"
#df_p30$Méthode[df_p30$Méthode == "eGFR_EKFCcys"] <- "EKFC cys C"
#df_p30$Méthode[df_p30$Méthode == "eGFR_EKFCmix"] <- "EKFC mix"
#df_p30$Méthode[df_p30$Méthode == "eGFR_EKFCmixPS"] <- "EKFC mix PS"


#Iohexol_long3$Formules[Iohexol_long3$Formules == "eGFR_CKDEPI2009"] <- "CKD-EPI 2009"
#Iohexol_long3$Formules[Iohexol_long3$Formules == "eGFR_CKDEPI2009avecrace"] <- "CKD-EPI 2009 avec CE"
#Iohexol_long3$Formules[Iohexol_long3$Formules == "eGFR_CKDEPI2021"] <- "CKD-EPI 2021"
#Iohexol_long3$Formules[Iohexol_long3$Formules == "eGFR_CKDEPIcys"] <- "CKD-EPI cys C"
#Iohexol_long3$Formules[Iohexol_long3$Formules == "eGFR_CKDEPImix2009"] <- "CKD-EPI mix 2009"
#Iohexol_long3$Formules[Iohexol_long3$Formules == "eGFR_CKDEPImix2009avecrace"] <- "CKD-EPI mix 2009 avec CE"
#Iohexol_long3$Formules[Iohexol_long3$Formules == "eGFR_CKDEPImix2021"] <- "CKD-EPI mix 2021"
#Iohexol_long3$Formules[Iohexol_long3$Formules == "eGFR_Créatinine"] <- "Créatinine"
#Iohexol_long3$Formules[Iohexol_long3$Formules == "eGFR_CystatineC"] <- "CystatineC"
#Iohexol_long3$Formules[Iohexol_long3$Formules == "eGFR_EKFCcre"] <- "EKFC cre"
#Iohexol_long3$Formules[Iohexol_long3$Formules == "eGFR_EKFCcrePopulation"] <- "EKFC cre PS"
#Iohexol_long3$Formules[Iohexol_long3$Formules == "eGFR_EKFCcys"] <- "EKFC cys C"
#Iohexol_long3$Formules[Iohexol_long3$Formules == "eGFR_EKFCmix"] <- "EKFC mix"
#Iohexol_long3$Formules[Iohexol_long3$Formules == "eGFR_EKFCmixPS"] <- "EKFC mix PS"



#Iohexol_long3 <- rename(Iohexol_long3, Formules = Méthode) #Dpyr
#save(Iohexol_long3, file = "Iohexol_long3.Rda")
#save(Iohexol_60, file = "Iohexol_60.Rda")

#2. Calculer %P30 par formule et âge


df_p30 <- Iohexol_long3 %>%
  group_by(Formules, age_moyen) %>%
  summarise(P30_percent = mean(P30, na.rm = TRUE) * 100, .groups = "drop")

#3.1. Tracer les courbes P30: Equations basés sur la créatinine

df_p30A <- df_p30 %>%
  filter(Formules %in% c("CKD-EPI 2009", 
                        "CKD-EPI 2009 avec CE",
                        "CKD-EPI 2021",
                        "EKFC cre",
                        "EKFC cre PS"))

ggplot(df_p30A, aes(x = age_moyen, y = P30_percent, color = Formules, linetype = Formules)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 50, ymax = 100, fill = "grey30", alpha = 0.1) +
  geom_smooth(se = FALSE, size = 1.2) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "P30 : % des estimations de DFG dans ±30% de la valeur mesurée",
    x = "Âge (années)",
    y = "P30 (%)",
    color = "Formules",
    linetype = "Formules"
  ) +
  theme_minimal()

#Seuil P30: 75%

ggplot(df_p30A, aes(x = age_moyen, y = P30_percent, color = Formules, linetype = Formules)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 75, ymax = 100, fill = "grey30", alpha = 0.1) +
  geom_smooth(se = FALSE, size = 1.2) +
  scale_y_continuous(limits = c(50, 100)) +
  labs(
    title = "Seuil de performance (P30 > 75%)",
    x = "Âge (années)",
    y = "P30 (%)",
    color = "Formules",
    linetype = "Formules"
  ) +
  theme_minimal()


#3.2. Tracer les courbes P30: Equations basés sur la cystatine C

df_p30B <- df_p30 %>%
  filter(Formules %in% c("CKD-EPI cys C", 
                        "CKD-EPI mix 2009",
                        "CKD-EPI mix 2009 avec CE",
                        "CKD-EPI mix 2021",
                        "EKFC cys C",
                        "EKFC mix",
                        "EKFC mix PS"))

ggplot(df_p30B, aes(x = age_moyen, y = P30_percent, color = Formules, linetype = Formules)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 50, ymax = 100, fill = "grey30", alpha = 0.1) +
  geom_smooth(se = FALSE, size = 1.2) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "P30 : % des estimations de DFG dans ±30% de la valeur mesurée",
    x = "Âge (années)",
    y = "P30 (%)",
    color = "Formules",
    linetype = "Formules"
  ) +
  scale_y_continuous(limits = c(25, 100)) +
  theme_minimal()

#Seuil P30: 75%

ggplot(df_p30B, aes(x = age_moyen, y = P30_percent, color = Formules, linetype = Formules)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 75, ymax = 100, fill = "grey30", alpha = 0.1) +
  geom_smooth(se = FALSE, size = 1.2) +
  scale_y_continuous(limits = c(50, 100)) +
  labs(
    title = "Seuil de performance (P30 > 75%)",
    x = "Âge (années)",
    y = "P30 (%)",
    color = "Formules",
    linetype = "Formules"
  ) +
  theme_minimal()

-----------------------------------------
  #Biais entre DFG estimé et mesuré selon l'âge
  #Creatinine
  
  #1. calculer le biais  
  
  Iohexol_long3 <- Iohexol_long3 %>%
  mutate(biais = eGFR_est - DFGindexé)

#2. Moyenne du biais par tranche d’âge et formule

df_biais <- Iohexol_long3 %>%
  group_by(Formules, age_moyen) %>%
  summarise(
    biais_moyen = mean(biais, na.rm = TRUE),
    .groups = "drop"
  )

#3. Graphique du biais

df_biais_A <- df_biais %>%
  filter(Formules %in% c("CKD-EPI 2009", 
                        "CKD-EPI 2009 avec CE",
                        "CKD-EPI 2021",
                        "EKFC cre",
                        "EKFC cre PS"))

ggplot(df_biais_A, aes(x = age_moyen, y = biais_moyen, color = Formules, linetype = Formules)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 5, fill = "grey30", alpha = 0.1) +
  geom_smooth(se = FALSE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Biais entre DFG estimé et mesuré selon l'âge",
    x = "Âge (années)",
    y = "Biais (DFG estimé - DFG mesuré) [ml/min/1.73m²]",
    color = "Formules",
    linetype = "Formules"
  ) +
  theme_minimal()


#Cystatine

df_biais_B <- df_biais %>%
  filter(Formules %in% c("CKD-EPI cys C", 
                        "CKD-EPI mix 2009",
                        "CKD-EPI mix 2009 avec CE",
                        "CKD-EPI mix 2021",
                        "EKFC cys C",
                        "EKFC mix",
                        "EKFC mix PS"))

ggplot(df_biais_B, aes(x = age_moyen, y = biais_moyen, color = Formules, linetype = Formules)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -5, ymax = 5, fill = "grey30", alpha = 0.1) +
  geom_smooth(se = FALSE, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Biais entre DFG estimé et mesuré selon l'âge",
    x = "Âge (années)",
    y = "Biais (DFG estimé - DFG mesuré) [ml/min/1.73m²]",
    color = "Formules",
    linetype = "Formules"
  ) +
  theme_minimal()
#------------------------------------------------------------------------------------------------------------------------
  
  #Biais en fonction de la GFR mesurée (et non plus de l’âge).
  
  
  # 2. Grouper les données en tranches de mGFR (e.g., 10 unités)
  df_bmGFR <- Iohexol_long3 %>%
  mutate(gfr_group = cut(DFGindexé, breaks = seq(40, 150, by = 10), right = FALSE),
         gfr_moyen = ifelse(
           is.na(gfr_group),
           NA,
           as.numeric(gsub("\\[|\\)|\\]", "", str_extract(as.character(gfr_group), "^\\[?\\d+"))) #Extrait le début du libellé de la tranche 
         ) + 5)



# 3. Calculer le biais moyen par méthode et niveau de GFR mesurée
df_biais2 <- df_bmGFR %>%
  group_by(Formules_b, gfr_moyen) %>%
  summarise(Biais_moyen = mean(Biais, na.rm = TRUE), .groups = "drop")


df_biais2_A <- df_biais2 %>%
  filter(Formules_b%in% c("CKD-EPI 2009", 
                        "CKD-EPI 2009 avec CE",
                        "CKD-EPI 2021",
                        "EKFC cre",
                        "EKFC cre PS"
  ))

ggplot(df_biais2_A, aes(x = gfr_moyen, y = Biais_moyen, color = Formules_b, linetype = Formules_b)) +
  geom_smooth(se = FALSE, size = 1.2) +
  annotate("rect", xmin = 40, xmax = 150, ymin = -5, ymax = 5, fill = "gray", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Biais par rapport au DFG mesuré",
    x = "DFG mesurée (ml/min/1.73m²)",
    y = "Biais (DFG estimé - DFG mesuré) [ml/min/1.73m²]",
    color = "Formules",
    linetype = "Formules"
  ) +
  #scale_y_continuous(limits = c(-20, 20)) +
  theme_minimal()


df_biais2_B <- df_biais2 %>%
  filter(Formules_b %in% c("CKD-EPI cys C", 
                        "CKD-EPI mix 2009",
                        "CKD-EPI mix 2009 avec CE",
                        "CKD-EPI mix 2021",
                        "EKFC cys C",
                        "EKFC mix",
                        "EKFC mix PS"
  ))

ggplot(df_biais2_B, aes(x = gfr_moyen, y = Biais_moyen, color = Formules_b, linetype = Formules_b)) +
  geom_smooth(se = FALSE, size = 1.2) +
  annotate("rect", xmin = 40, xmax = 150, ymin = -5, ymax = 5, fill = "gray", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Biais par rapport au DFG mesuré",
    x = "DFG mesuré (ml/min/1.73m²)",
    y = "Biais (DFG estimé - DFG mesuré)",
    color = "Formules",
    linetype = "Formules"
  ) +
  #scale_y_continuous(limits = c(-20, 20)) +
  theme_minimal()
----------------------------------------------------------------------------
  
  #P30 selon le GFR mesurée
  
  df_last <- Iohexol_long3 %>%
  mutate(P30 = abs(eGFR_est - DFGindexé) / DFGindexé <= 0.3,
         tranche_mGFR = cut(DFGindexé, breaks = seq(50, 130, by = 10), right = FALSE),
         mGFR_moyen = ifelse(
           is.na(tranche_mGFR),
           NA,
           as.numeric(gsub("\\[|\\)|\\]", "", str_extract(as.character(tranche_mGFR), "^\\[?\\d+"))) #Extrait le début du libellé de la tranche 
         ) + 5
  )


#2. Calcul du %P30 selon mGFR_moyen et formule

df_p30_mGFR_meth <- df_last %>%
  group_by(Formules, mGFR_moyen) %>%
  summarise(P30_percent = mean(P30, na.rm = TRUE) * 100, .groups = "drop")



#3.  Graphique P30 vs mGFR: creatinine

df_lastA <- df_p30_mGFR_meth %>%
  filter(Formules%in% c("CKD-EPI 2009", 
                        "CKD-EPI 2009 avec CE",
                        "CKD-EPI 2021",
                        "EKFC cre",
                        "EKFC cre PS"
  ))


ggplot(df_lastA, aes(x = mGFR_moyen, y = P30_percent, color = Formules, linetype = Formules)) +
  geom_smooth(se = FALSE, size = 1.2) +
  annotate("rect", xmin = 50, xmax = 130, ymin = 75, ymax = 100, fill = "gray", alpha = 0.2) +
  labs(
    title = "P30 (%) selon le DFG mesuré",
    x = "DFG mesuré (ml/min/1.73m²)",
    y = "P30 (%)",
    color = "Formules",
    linetype = "Formules"
  ) +
  scale_y_continuous(limits = c(25, 100)) +
  theme_minimal()

#3.  Graphique P30 vs mGFR: cystatine

df_lastB <- df_p30_mGFR_meth %>%
  filter(Formules %in% c("CKD-EPI cys C", 
                        "CKD-EPI mix 2009",
                        "CKD-EPI mix 2009 avec CE",
                        "CKD-EPI mix 2021",
                        "EKFC cys C",
                        "EKFC mix",
                        "EKFC mix PS"
  ))


ggplot(df_lastB, aes(x = mGFR_moyen, y = P30_percent, color = Formules, linetype = Formules)) +
  geom_smooth(se = FALSE, size = 1.2) +
  annotate("rect", xmin = 50, xmax = 130, ymin = 75, ymax = 100, fill = "gray", alpha = 0.2) +
  labs(
    title = "P30 (%) selon le DFG mesuré",
    x = "DFG mesuré (ml/min/1.73m²)",
    y = "P30 (%)",
    color = "Formules",
    linetype = "Formules"
  ) +
  scale_y_continuous(limits = c(25, 100)) +
  theme_minimal()
#=========================================================================================================
#Biais selon l'IMC

# 2. Grouper les données en tranches de IMC (e.g., 5 unités)

df_IMC <- Iohexol_long3 %>%
  mutate(IMC_group = cut(IMC, breaks = seq(15, 50, by = 5), right = FALSE),
         IMC_moyen = ifelse(
           is.na(IMC_group),
           NA,
           as.numeric(gsub("\\[|\\)|\\]", "", str_extract(as.character(IMC_group), "^\\[?\\d+"))) #Extrait le début du libellé de la tranche 
         ) + 5)

# 3. Calculer le biais moyen par méthode et niveau de GFR mesurée

df_biais25 <- df_IMC %>%
  group_by(Formules, IMC_moyen) %>%
  summarise(biais_moyenIMC = mean(Biais, na.rm = TRUE), .groups = "drop")


df_biais2_I <- df_biais25 %>%
  filter(Formules%in% c("CKD-EPI 2009", 
                        "CKD-EPI 2009 avec CE",
                        "CKD-EPI 2021",
                        "EKFC cre",
                        "EKFC cre PS"))


ggplot(df_biais2_I, aes(x = IMC_moyen, y = biais_moyenIMC, color = Formules, linetype = Formules)) +
  geom_smooth(se = FALSE, size = 1.2) +
  annotate("rect", xmin = 15, xmax = 50, ymin = -5, ymax = 5, fill = "gray", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Biais par rapport à l'IMC",
    x = "IMC (Kg/m²)",
    y = "Biais (DFG estimé - DFG mesuré) [ml/min/1.73m²]",
    color = "Formules",
    linetype = "Formules"
  ) +
  #scale_y_continuous(limits = c(-20, 20)) +
  theme_minimal()


df_biais2_IB <- df_biais25 %>%
  filter(Formules %in% c("CKD-EPI cys C", 
                        "CKD-EPI mix 2009",
                        "CKD-EPI mix 2009 avec CE",
                        "CKD-EPI mix 2021",
                        "EKFC cys C",
                        "EKFC mix",
                        "EKFC mix PS"
  ))

ggplot(df_biais2_IB, aes(x = IMC_moyen, y = biais_moyenIMC, color = Formules, linetype = Formules)) +
  geom_smooth(se = FALSE, size = 1.2) +
  annotate("rect", xmin = 15, xmax = 50, ymin = -5, ymax = 5, fill = "gray", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Biais par rapport à l'IMC",
    x = "IMC (Kg/m²)",
    y = "Biais (DFG estimé - DFG mesuré)",
    color = "Formules",
    linetype = "Formules"
  ) +
  #scale_y_continuous(limits = c(-20, 20)) +
  theme_minimal()


#----------------------------

# COmmence ici

packages <- c("haven", "foreign", "dplyr", "tidyr", "stringr", "ggplot2", "questionr")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

packages <- c(
  "readxl", "labelled", "expss", "summarytools", "tidyverse",
  "hrbrthemes", "gtsummary", "ggcorrplot", "e1071", "car", "patchwork"
)

# Install missing packages only
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}
















