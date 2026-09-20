#Dashboard Shiny - JDA 2026
#Par Patrick Ntwali Matabaro

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)
library(tidyr)
library(scales)

dashboard_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#fbfcfc", color = NA),
    panel.background = element_rect(fill = "#fbfcfc", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#e7ecec", linewidth = 0.35),
    axis.title = element_text(color = "#344646"),
    axis.text = element_text(color = "#405252"),
    plot.title = element_text(face = "bold", color = "#1f3434", size = 15),
    plot.subtitle = element_text(color = "#657474"),
    legend.position = "bottom",
    legend.title = element_text(color = "#344646"),
    legend.text = element_text(color = "#405252")
  )

data_path <- "LD_BD.Rda"

env <- new.env(parent = emptyenv())
load(data_path, envir = env)

raw_data <- env$LD_BD %>%
  mutate(
    Date = as.POSIXct(Date),
    SensorID = as.character(SensorID),
    Location = as.character(Location),
    Floor = as.character(Floor),
    site = as.character(site),
    sensor_label = paste(Location, "- étage", Floor, "-", SensorID),
    calendar_date = as.Date(format(Date, "%Y-%m-%d", tz = "Europe/Brussels")),
    hour = hour(Date),
    weekday = factor(
      wday(Date, label = TRUE, abbr = FALSE, week_start = 1),
      levels = wday(seq.Date(as.Date("2026-05-04"), by = "day", length.out = 7),
                    label = TRUE, abbr = FALSE, week_start = 1)
    ),
    week_start = floor_date(calendar_date, "week", week_start = 1),
    heating_degree = pmax(0, 16.5 - TempOutside)
  )

sensor_choices <- raw_data %>%
  distinct(sensor_label, SensorID, Location, Floor) %>%
  arrange(Location) %>%
  transmute(label = sensor_label, value = SensorID) %>%
  { setNames(.$value, .$label) }

x_variables <- c(
  "Température extérieure" = "TempOutside",
  "Degrés-jours de chauffage" = "heating_degree",
  "Humidité relative" = "humidity_relative",
  "Vitesse du vent" = "wind_speed",
  "Pression" = "pressure",
  "Consommation d'énergie (kWh/m²)" = "kWhm2"
)

aggregate_series <- function(data, level) {
  unit <- switch(
    level,
    "Horaire" = "hour",
    "Journalier" = "day",
    "Hebdomadaire" = "week",
    "Mensuel" = "month"
  )

  data %>%
    mutate(period = floor_date(Date, unit, week_start = 1)) %>%
    group_by(period) %>%
    summarise(
      temp_inside = mean(TempInside, na.rm = TRUE),
      temp_outside = mean(TempOutside, na.rm = TRUE),
      heating_degree = mean(heating_degree, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
}

app_css <- "
:root {
  --ht-bg: #f4f7f6;
  --ht-surface: #ffffff;
  --ht-border: #d8e1df;
  --ht-ink: #173333;
  --ht-muted: #637574;
  --ht-primary: #245c5f;
  --ht-accent: #f28f3b;
}

body {
  background: var(--ht-bg);
  color: var(--ht-ink);
}

.navbar {
  background: linear-gradient(90deg, #1f4f52, #2f6f6a) !important;
  box-shadow: 0 2px 12px rgba(23, 51, 51, 0.14);
}

.navbar .navbar-brand {
  font-weight: 700;
  letter-spacing: 0;
}

.bslib-sidebar-layout > .sidebar {
  background: #eef3f2;
  border-right: 1px solid var(--ht-border);
}

.filter-title {
  color: var(--ht-primary);
  font-size: 0.78rem;
  font-weight: 800;
  letter-spacing: 0.04em;
  margin: 1.05rem 0 0.5rem;
  text-transform: uppercase;
}

.form-label, .control-label {
  color: #294747;
  font-weight: 650;
}

.form-control, .form-select, .selectize-input {
  border-color: #cbd8d6 !important;
  border-radius: 8px !important;
  box-shadow: none !important;
}

.app-hero {
  background: var(--ht-surface);
  border: 1px solid var(--ht-border);
  border-radius: 10px;
  margin-bottom: 1rem;
  padding: 1.05rem 1.25rem;
}

.app-hero h1 {
  color: var(--ht-ink);
  font-size: 1.45rem;
  font-weight: 800;
  margin: 0;
}

.app-hero p {
  color: var(--ht-muted);
  margin: 0.2rem 0 0;
}

.context-strip {
  align-items: center;
  background: #eaf2f0;
  border: 1px solid var(--ht-border);
  border-left: 5px solid var(--ht-accent);
  border-radius: 8px;
  color: #294747;
  display: flex;
  flex-wrap: wrap;
  font-size: 0.92rem;
  gap: 0.55rem;
  margin-bottom: 1rem;
  padding: 0.7rem 0.9rem;
}

.context-chip {
  background: #ffffff;
  border: 1px solid #dbe5e3;
  border-radius: 999px;
  padding: 0.22rem 0.7rem;
}

.bslib-value-box {
  background: var(--ht-surface) !important;
  border: 1px solid var(--ht-border) !important;
  border-radius: 10px !important;
  box-shadow: 0 8px 22px rgba(23, 51, 51, 0.06);
}

.bslib-value-box .value-box-title {
  color: var(--ht-muted);
  font-weight: 700;
}

.bslib-value-box .value-box-value {
  color: var(--ht-primary);
  font-weight: 800;
}

.card {
  border: 1px solid var(--ht-border);
  border-radius: 10px;
  box-shadow: 0 10px 26px rgba(23, 51, 51, 0.06);
}

.nav-tabs {
  border-bottom-color: var(--ht-border);
}

.nav-tabs .nav-link {
  color: #35706b;
  font-weight: 650;
  border-radius: 8px 8px 0 0;
}

.nav-tabs .nav-link.active {
  color: var(--ht-primary);
  border-top: 3px solid var(--ht-accent);
  font-weight: 800;
}
"

ui <- page_sidebar(
  title = "Projet LD",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#245c5f",
    base_font = font_google("Inter")
  ),
  sidebar = sidebar(
    width = 330,
    div(class = "filter-title", "Capteur"),
    selectInput("sensor", "Capteur", choices = sensor_choices),
    div(class = "filter-title", "Temps"),
    radioButtons(
      "date_mode",
      "Vue temporelle",
      choices = c("Une journée" = "day", "Une période" = "period"),
      selected = "period",
      inline = TRUE
    ),
    conditionalPanel(
      "input.date_mode == 'day'",
      dateInput(
        "single_date",
        "Journée",
        value = format(min(raw_data$calendar_date, na.rm = TRUE), "%Y-%m-%d"),
        min = format(min(raw_data$calendar_date, na.rm = TRUE), "%Y-%m-%d"),
        max = format(max(raw_data$calendar_date, na.rm = TRUE), "%Y-%m-%d"),
        language = "fr"
      )
    ),
    conditionalPanel(
      "input.date_mode == 'period'",
      dateRangeInput(
        "dates",
        "Période",
        start = format(min(raw_data$calendar_date, na.rm = TRUE), "%Y-%m-%d"),
        end = format(max(raw_data$calendar_date, na.rm = TRUE), "%Y-%m-%d"),
        min = format(min(raw_data$calendar_date, na.rm = TRUE), "%Y-%m-%d"),
        max = format(max(raw_data$calendar_date, na.rm = TRUE), "%Y-%m-%d"),
        language = "fr",
        separator = " à "
      )
    ),
    div(class = "filter-title", "Graphiques"),
    selectInput(
      "aggregation",
      "Agrégation de la série",
      choices = c("Horaire", "Journalier", "Hebdomadaire", "Mensuel"),
      selected = "Horaire"
    ),
    selectInput(
      "xvar",
      "Variable explicative",
      choices = x_variables,
      selected = "heating_degree"
    ),
    checkboxInput("show_points", "Afficher les points", FALSE)
  ),
  tags$head(tags$style(HTML(app_css))),
  div(
    class = "app-hero",
    h1("Site LD"),
    p("Par Patrick Ntwali Matabaro")
  ),
  uiOutput("context_strip"),
  layout_column_wrap(
    width = 1 / 4,
    value_box("Température moyenne", textOutput("avg_temp")),
    value_box("Minimum", textOutput("min_temp")),
    value_box("Maximum", textOutput("max_temp")),
    value_box("Observations", textOutput("n_obs"))
  ),
  navset_card_tab(
    nav_panel("Série chronologique", plotlyOutput("time_plot", height = 520)),
    nav_panel("Résumé hebdomadaire", plotlyOutput("weekly_plot", height = 520)),
    nav_panel("Résumé horaire", plotlyOutput("hourly_plot", height = 520)),
    nav_panel("Relation x-y", plotlyOutput("xy_plot", height = 520)),
    nav_panel("Heatmap", plotlyOutput("heatmap_plot", height = 560)),
    nav_panel("Données", DTOutput("data_table"))
  )
)

server <- function(input, output, session) {
  observeEvent(input$date_mode, {
    if (identical(input$date_mode, "day")) {
      updateSelectInput(
        session,
        "aggregation",
        choices = c("Horaire"),
        selected = "Horaire"
      )
    } else {
      updateSelectInput(
        session,
        "aggregation",
        choices = c("Horaire", "Journalier", "Hebdomadaire", "Mensuel"),
        selected = "Horaire"
      )
    }
  }, ignoreInit = TRUE)

  output$context_strip <- renderUI({
    req(input$sensor, input$date_mode, input$aggregation)

    sensor_label <- names(sensor_choices)[sensor_choices == input$sensor][1]
    date_label <- if (identical(input$date_mode, "day")) {
      req(input$single_date)
      paste("Journée :", format(as.Date(input$single_date), "%d/%m/%Y"))
    } else {
      req(input$dates)
      paste(
        "Période :",
        format(as.Date(input$dates[1]), "%d/%m/%Y"),
        "-",
        format(as.Date(input$dates[2]), "%d/%m/%Y")
      )
    }

    div(
      class = "context-strip",
      span(class = "context-chip", paste("Capteur :", sensor_label)),
      span(class = "context-chip", date_label),
      span(class = "context-chip", paste("Agrégation :", input$aggregation))
    )
  })

  filtered_data <- reactive({
    req(input$sensor, input$date_mode)

    data <- raw_data %>%
      filter(SensorID == input$sensor)

    if (identical(input$date_mode, "day")) {
      req(input$single_date)
      data <- data %>%
        filter(calendar_date == as.Date(input$single_date))
    } else {
      req(input$dates)
      data <- data %>%
        filter(
          calendar_date >= as.Date(input$dates[1]),
          calendar_date <= as.Date(input$dates[2])
        )
    }

    data %>%
      arrange(Date)
  })

  output$avg_temp <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("Aucune donnée")
    sprintf("%.1f °C", mean(data$TempInside, na.rm = TRUE))
  })

  output$min_temp <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("Aucune donnée")
    sprintf("%.1f °C", min(data$TempInside, na.rm = TRUE))
  })

  output$max_temp <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("Aucune donnée")
    sprintf("%.1f °C", max(data$TempInside, na.rm = TRUE))
  })

  output$n_obs <- renderText({
    comma(nrow(filtered_data()), big.mark = " ")
  })

  output$time_plot <- renderPlotly({
    validate(need(nrow(filtered_data()) > 0, "Aucune donnée disponible pour cette sélection."))

    series <- aggregate_series(filtered_data(), input$aggregation)
    x_axis_label <- switch(
      input$aggregation,
      "Horaire" = if (identical(input$date_mode, "day")) "Heure de la journée" else "Date et heure",
      "Journalier" = "Jour",
      "Hebdomadaire" = "Semaine",
      "Mensuel" = "Mois"
    )
    x_date_breaks <- switch(
      input$aggregation,
      "Horaire" = if (identical(input$date_mode, "day")) "2 hours" else "2 weeks",
      "Journalier" = "2 weeks",
      "Hebdomadaire" = "1 month",
      "Mensuel" = "1 month"
    )
    x_date_labels <- switch(
      input$aggregation,
      "Horaire" = if (identical(input$date_mode, "day")) "%H:%M" else "%d/%m\n%H:%M",
      "Journalier" = "%d/%m",
      "Hebdomadaire" = "%d/%m",
      "Mensuel" = "%b %Y"
    )

    smooth_window <- if (identical(input$aggregation, "Horaire")) 3 else 7
    series <- series %>%
      mutate(
        temp_smooth = if (n() >= smooth_window) {
          as.numeric(stats::filter(temp_inside, rep(1 / smooth_window, smooth_window), sides = 2))
        } else {
          temp_inside
        }
      )

    p <- ggplot(series, aes(period, temp_inside)) +
      geom_ribbon(
        aes(ymin = temp_inside - 0.15, ymax = temp_inside + 0.15),
        fill = "#d8ece8",
        alpha = 0.65
      ) +
      geom_line(color = "#6d8f8c", linewidth = 0.6, alpha = 0.65) +
      geom_line(aes(y = temp_smooth), color = "#28666e", linewidth = 1.15, na.rm = TRUE) +
      labs(
        title = "Évolution de la température",
        subtitle = paste("Agrégation", tolower(input$aggregation)),
        x = x_axis_label,
        y = "Température intérieure (°C)"
      ) +
      scale_x_datetime(
        date_breaks = x_date_breaks,
        date_labels = x_date_labels,
        timezone = "Europe/Brussels"
      ) +
      dashboard_theme

    if (isTRUE(input$show_points)) {
      p <- p + geom_point(color = "#f28f3b", size = 1.35, alpha = 0.5)
    }

    ggplotly(p, tooltip = c("x", "y"))
  })

  output$weekly_plot <- renderPlotly({
    weekly <- filtered_data() %>%
      group_by(weekday) %>%
      summarise(
        temp_inside = mean(TempInside, na.rm = TRUE),
        temp_min = min(TempInside, na.rm = TRUE),
        temp_max = max(TempInside, na.rm = TRUE),
        .groups = "drop"
      )

    p <- ggplot(weekly, aes(weekday, temp_inside, group = 1)) +
      geom_linerange(aes(ymin = temp_min, ymax = temp_max), color = "#b8c9c7", linewidth = 5, alpha = 0.55) +
      geom_col(aes(fill = temp_inside), width = 0.62, alpha = 0.95) +
      geom_text(
        aes(label = sprintf("%.1f °C", temp_inside)),
        vjust = -0.65,
        color = "#263838",
        size = 3.7
      ) +
      scale_fill_gradient(low = "#8fc7bd", high = "#f28f3b") +
      expand_limits(y = max(weekly$temp_inside, na.rm = TRUE) + 0.4) +
      labs(
        title = "Température moyenne par jour",
        subtitle = "Les barres montrent la moyenne, le trait clair indique l'amplitude observée",
        x = NULL,
        y = "Température intérieure moyenne (°C)"
      ) +
      guides(fill = "none") +
      dashboard_theme

    ggplotly(p)
  })

  output$hourly_plot <- renderPlotly({
    hourly <- filtered_data() %>%
      group_by(weekday, hour) %>%
      summarise(
        temp_inside = mean(TempInside, na.rm = TRUE),
        .groups = "drop"
      )

    p <- ggplot(hourly, aes(hour, temp_inside, color = weekday)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.4) +
      scale_x_continuous(breaks = seq(0, 23, by = 2)) +
      labs(x = "Heure", y = "Température intérieure moyenne (°C)", color = "Jour") +
      dashboard_theme

    ggplotly(p)
  })

  output$xy_plot <- renderPlotly({
    req(input$xvar)
    data <- filtered_data() %>%
      filter(!is.na(.data[[input$xvar]]), !is.na(TempInside))

    p <- ggplot(data, aes(.data[[input$xvar]], TempInside, color = weekday)) +
      geom_point(alpha = 0.62, size = 1.8) +
      labs(
        x = names(x_variables)[match(input$xvar, x_variables)],
        y = "Température intérieure (°C)",
        color = "Jour"
      ) +
      dashboard_theme +
      theme(
        axis.title.x = element_text(margin = margin(t = 14)),
        legend.position = "right",
        plot.margin = margin(t = 8, r = 24, b = 18, l = 8)
      )

    ggplotly(p) %>%
      layout(
        legend = list(
          orientation = "v",
          x = 1.02,
          xanchor = "left",
          y = 0.5,
          yanchor = "middle"
        ),
        margin = list(r = 150, b = 70)
      )
  })

  output$heatmap_plot <- renderPlotly({
    heat <- filtered_data() %>%
      group_by(weekday, hour) %>%
      summarise(temp_inside = mean(TempInside, na.rm = TRUE), .groups = "drop") %>%
      complete(weekday, hour = 0:23) %>%
      arrange(weekday, hour) %>%
      group_by(weekday) %>%
      mutate(
        temp_display = if (sum(!is.na(temp_inside)) >= 5) {
          as.numeric(stats::filter(temp_inside, rep(1 / 3, 3), sides = 2))
        } else {
          temp_inside
        },
        temp_display = if_else(is.na(temp_display), temp_inside, temp_display)
      ) %>%
      ungroup()

    p <- ggplot(heat, aes(hour, weekday, fill = temp_display)) +
      geom_tile(width = 1.02, height = 0.96, color = NA) +
      scale_x_continuous(breaks = seq(0, 23, by = 2), expand = c(0, 0)) +
      scale_fill_gradientn(
        colours = c("#355c7d", "#7fc8a9", "#f8e08e", "#f28f3b"),
        na.value = "#eef2f2"
      ) +
      labs(
        title = "Chaleur moyenne selon le jour et l'heure",
        x = "Heure",
        y = NULL,
        fill = "°C"
      ) +
      dashboard_theme +
      theme(
        panel.grid = element_blank(),
        axis.ticks = element_blank()
      )

    ggplotly(p)
  })

  output$data_table <- renderDT({
    filtered_data() %>%
      select(
        Date, SensorID, Floor, Location, site, TempInside, TempOutside,
        humidity_relative, wind_speed, pressure, kWhm2, heating_degree
      ) %>%
      datatable(
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 15, scrollX = TRUE)
      )
  })
}

shinyApp(ui, server)
