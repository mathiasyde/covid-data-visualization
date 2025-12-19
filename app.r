library(shiny)
library(ggplot2)
library(ggridges)
library(dplyr)
library(grid)
library(bslib)
library(tidyr)
library(sf)
library(leaflet)
library(plotly)
library(viridis)
library(lubridate)
library(readr)
library(ggstream)
library(scales)
library(shinycssloaders)

ui <- page_navbar(
  title = "Exploratory visualization of the COVID-19 pandemic in the U.S.",
  theme = bs_theme(bootswatch = "flatly"),
  tabPanel(
    "Chicago COVID-19 Exploration",
    sidebarLayout(
      sidebarPanel(
        dateRangeInput(
          "range",
          label = "Date range",
          start = "2021-01-01",
          end = "2023-06-30"
        ),
        uiOutput("AnimationControls"),
        p("Use the animation slider to change the current date of visualization."),
        
        p("The x-axes are not syncroized between plots, so enable the date line to help correlate dates between plots."),
        input_switch("dateLine", "Enable Date Line"),
        p("Please note that the animation rendering may be slow."),
        input_switch("enableAnnotations", "Display annotations")
      ),
      mainPanel(
        h3("Exploring the COVID-19 pandemic in Chicago."),
        layout_columns(
          p(
            "The state of Illinois were one of the first states in the U.S. to be impacted by COVID-19 after an
						international travel event in early 2020. Here, we will explore historical data of the pandemic in the
						Illinois, focusing on Chicago."
          )
        ),
        p("Area above the y=0 line represents vaccinated outcomes; area below represents unvaccinated outcomes."),
        plotOutput("ChicagoOutcomes"),
        plotOutput("ChicagoPopulation")
      )
    )
  ),
  
  tabPanel(
    "Vaccine Effectiveness Analysis",
    sidebarLayout(
      sidebarPanel(
        h4("Filter Options"),
        dateRangeInput(
          "date_range_analysis",
          label = "Date Range",
          start = "2021-01-01",
          end = "2023-06-30"
        ),
        checkboxGroupInput(
          "age_groups_filter",
          label = "Select Age Groups",
          choices = c("0-4", "5-11", "12-17", "18-29", "30-49", "50-64", "65-79", "80+"),
          selected = c("0-4", "5-11", "12-17", "18-29", "30-49", "50-64", "65-79", "80+")
        )
      ),
      mainPanel(
        h3("Vaccine Effectiveness in Chicago"),
        p("Analyzing how vaccines protected Chicago residents and what influenced vaccine outcomes."),
        
        # Graph 1: Vaccine Rollout Timeline
        h4("1. COVID-19 Spread and Vaccine Rollout Timeline"),
        plotlyOutput("vaccineRolloutPlot", height = "400px"),
        br(),
        
        # Graph 2: Risk Reduction Analysis
        h4("2. Vaccine Risk Reduction by Outcome Type"),
        plotOutput("riskReductionPlot", height = "400px")
      )
    )
  ),
  
  tabPanel(
    "Vaccine Effectiveness - Vaccine Waning",
    sidebarLayout(
      sidebarPanel(
        p("3 graphs are displayed on this page. They show the derived value 
        VE (Vaccine Effectiveness) that is used by the CDC and WHO to calculated how 
        effective a vaccine is. This is shown over time, and devided into age groups"),
        p("This is used to visualize potential vaccine waning over time"),
        p("Some of the graphs are very heavy, and load slowly so be patient. 
          Any changes in the date range below, will reload the graphs."),
        p("The grahps are interactable and the legend to the right of the visualization works to filter age groups shown on the graph."), 
        dateRangeInput(
          "ve_date_range",
          "Date Range",
          start = "2021-01-01",
          end = "2023-06-30"
        ),
        br(),
        input_switch("omicron_line", "Add Omicron peak line"),
        p(" *The Omicron peak line overlay is very unstable on the stream graph
          so it isn't shown on that."),
        checkboxGroupInput(
          "ve_age_groups",
          "Age group filter (spaghetti Graph)",
          choices = c("All ages (combined)", "0-4", "5-11", "12-17", "18-29", "30-49", "50-64", "65-79", "80+"),
          selected = c("All ages (combined)", "0-4", "5-11", "12-17", "18-29", "30-49", "50-64", "65-79", "80+")
        )
      ),
      mainPanel(
        conditionalPanel(
          condition = "!output.ve_stream",
          p("When the bars below are moving, the graphes are loading. Please wait...")
        ),
        h3("Exploring vaccination waning over time"),
        br(),
        p("This data shows the calculated VE = (1 - (Vaccinated Rate / Unvaccinated Rate)) * 100. 
        in Chicago durring the covid pandemic."),
        br(),
        h5("1. Spaghetti  graph"),
        plotlyOutput("ve_spaghetti", height = "800px")|> withSpinner(),
        br(),
        h5("2. Streamgraph"),
        plotlyOutput("ve_stream", height = "600px")|> withSpinner(),
        p("*Streamgraph shows relative VE contribution by age group. Vertical scale on the y-axis does not represent percentage."),
        br(),
        h5("3. Area graph"),
        plotlyOutput("ve_area", height = "600px")|> withSpinner()
      )
    )
  ),
  
  tabPanel(
    "ZIP Code Analysis",
    sidebarLayout(
      sidebarPanel(
        h4("Map Controls"),
        
        # NEW: Toggle between vaccination and COVID progression
        radioButtons(
          "map_type",
          label = "Select Map Type",
          choices = c(
            "Vaccination Progress" = "vaccination",
            "COVID-19 Spread" = "covid"
          ),
          selected = "vaccination"
        ),
        
        hr(),
        
        uiOutput("zip_date_selector"),
        
        p("Select a week to view data. Data is reported weekly."),
        
        hr(),
        
        # Dynamic metric selector - REPLACES the static selectInput
        uiOutput("metric_selector"),
        
        # Information box explaining the selected metric
        uiOutput("metric_info_box"),
        
        hr(),
        
        h5("Display Options"),
        selectInput(
          "color_scheme",
          label = "Color Palette",
          choices = c(
            "Yellow-Orange-Red" = "YlOrRd",
            "Blues" = "Blues",
            "Greens" = "Greens",
            "Reds" = "Reds",
            "Purple-Blue" = "PuBu",
            "Viridis" = "viridis"
          ),
          selected = "YlOrRd"
        ),
        
        checkboxInput(
          "show_labels",
          "Show ZIP Code Labels",
          value = TRUE
        ),
        
        hr(),
        
        div(
          style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px;",
          h5("How to Use:"),
          tags$ul(
            tags$li("Toggle between Vaccination and COVID-19 data"),
            tags$li("Use the slider to select different weeks"),
            tags$li("Choose different metrics to visualize"),
            tags$li("Hover over ZIP codes for detailed information"),
            tags$li("Click a ZIP code to see its trend over time")
          )
        )
      ),
      
      mainPanel(
        uiOutput("map_title"),
        p("Interactive map showing vaccination rates across Chicago ZIP codes over time."),
        
        leafletOutput("chicagoZipMap", height = "600px"),
        
        br(),
        
        fluidRow(
          column(4,
                 wellPanel(
                   h5("Date Statistics"),
                   textOutput("map_stats_date")
                 )
          ),
          column(4,
                 wellPanel(
                   h5("City Total"),
                   textOutput("map_stats_total")
                 )
          ),
          column(4,
                 wellPanel(
                   h5("Average per ZIP"),
                   textOutput("map_stats_avg")
                 )
          )
        ),
        
        hr(),
        
        h4("Selected ZIP Code Time Series"),
        p("Click on a ZIP code in the map above to see its progression over time."),
        plotOutput("selectedZipTimeSeries", height = "350px")
      )
    )
  ),
  
  nav_spacer(),
  nav_menu(
    "More",
    tabPanel(
      "About",
      p("lorem ipsum" )
    ),
    tabPanel(
      "References",
      tags$ul(
        tags$li(a("Centers for Disease Control and Prevention: \"COVID-19 Timeline\" (July 8, 2024)",
                  href = "https://www.cdc.gov/museum/timeline/covid19.html"
        ))
      )
    )
  )
)

server <- shinyServer(function(input, output, session) {
  Annotations <- reactive({
    list(
      Omicron = if (input$enableAnnotations) {
        geom_vline(xintercept = as.Date("2022-01-01"), linetype="dashed", color="red", linewidth=0.5)
      } else NULL,
    )
  })

  Chicago <- list()
  
  # === DATA ===
  Chicago$Outcomes <- read.csv("data/chicago_outcomes.csv", stringsAsFactors = FALSE) |>
    mutate(Date = as.Date(Date)) |>
    mutate(Age.Group = factor(Age.Group, levels = c("0-4", "5-11", "12-17", "18-29", "30-49", "50-64", "65-79", "80+"), ordered = TRUE))
  
  Chicago$Population <- read.csv("data/chicago_population.csv", stringsAsFactors = FALSE) |>
    mutate(Date = as.Date(Date))
  
  Chicago$Raw <- readr::read_csv("datasets/chicago.csv", show_col_types = FALSE) %>%
    mutate(week_end = mdy(`Week End`))
  
  
  # Load ZIP code data and boundaries
  Chicago$ZipVaccination <- tryCatch({
    read.csv("data/chicago_zip_vaccination.csv", stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date), ZIP_Code = as.character(ZIP_Code))
  }, error = function(e) {
    warning("ZIP vaccination data not found. Please run prepare_zip_data.r first.")
    NULL
  })
  
  # Load ZIP code COVID PROGRESSION data
  Chicago$ZipProgression <- tryCatch({
    read.csv("data/chicago_zip_progression.csv", stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date), ZIP_Code = as.character(ZIP_Code))
  }, error = function(e) {
    warning("ZIP progression data not found. Please run prepare_zip_data.r first.")
    NULL
  })
  
  Chicago$Boundaries <- tryCatch({
  boundaries <- st_read("geographic/chicago_zip_boundaries.geojson", quiet = TRUE)
  boundaries$ZIP_Code <- as.character(boundaries$ZIP_Code)
  boundaries
}, error = function(e) {
    warning("Geographic boundaries not found. Please run prepare_zip_data.r first.")
    NULL
  })
  
  # Reactive value to store selected ZIP code
  selected_zip <- reactiveVal(NULL)
  
  # Dynamic date selector - slider that snaps to available dates
  output$zip_date_selector <- renderUI({
    # Use the appropriate dataset based on map type
    data_to_use <- if (!is.null(input$map_type) && input$map_type == "covid") {
      Chicago$ZipProgression
    } else {
      Chicago$ZipVaccination
    }
    
    if (is.null(data_to_use)) {
      return(p("Loading data..."))
    }
    
    # Get all available dates from the appropriate dataset
    available_dates <- sort(unique(as.Date(data_to_use$Date)))
    
    # Find a good default date
    default_date <- if (!is.null(input$map_type) && input$map_type == "covid") {
      # For COVID, start with early 2021 when there was activity
      available_dates[which.min(abs(available_dates - as.Date("2021-03-01")))]
    } else {
      # For vaccination, start with mid-2021
      available_dates[which.min(abs(available_dates - as.Date("2021-06-01")))]
    }
    
    sliderInput(
      "zip_date",
      label = "Select Week",
      min = min(available_dates),
      max = max(available_dates),
      value = default_date,
      timeFormat = "%Y-%m-%d",
      animate = animationOptions(interval = 500, loop = TRUE)
    )
  })
  
  # Dynamic map title based on map type
  output$map_title <- renderUI({
    req(input$map_type)
    if (input$map_type == "vaccination") {
      h3("COVID-19 Vaccination Rates by ZIP Code in Chicago")
    } else {
      h3("COVID-19 Spread and Impact by ZIP Code in Chicago")
    }
  })
  
  # Dynamic metric selector based on map type
  output$metric_selector <- renderUI({
    req(input$map_type)
    if (input$map_type == "vaccination") {
      selectInput(
        "map_metric",
        label = "Select Vaccination Metric",
        choices = c(
          "1st Dose Rate (%)" = "Vaccinated_1st_Dose",
          "Fully Vaccinated Rate (%)" = "Fully_Vaccinated",
          "Booster Rate (%)" = "Boosted"
        ),
        selected = "Fully_Vaccinated"
      )
    } else {
      selectInput(
        "map_metric",
        label = "Select COVID-19 Metric",
        choices = c(
          "Weekly Cases" = "Cases_Weekly",
          "Weekly Deaths" = "Deaths_Weekly",
          "Case Rate (per 100k)" = "Case_Rate_Weekly"
        ),
        selected = "Cases_Weekly"
      )
    }
  })
  
  #Information box explaining the metric
  output$metric_info_box <- renderUI({
    req(input$map_type, input$map_metric)
    
    # Define explanations for each metric
    if (input$map_type == "vaccination") {
      explanation <- switch(input$map_metric,
                            "Vaccinated_1st_Dose" = "Percentage of the ZIP code population that received at least one vaccine dose.",
                            "Fully_Vaccinated" = "Percentage of the ZIP code population that completed their vaccination series (typically 2 doses).",
                            "Boosted" = "Percentage of the ZIP code population that received a booster shot.",
                            "Total_Doses" = "Total number of vaccine doses administered in this ZIP code (includes 1st, 2nd, and booster doses).",
                            "Select a metric to see its explanation."
      )
      icon_name <- "syringe"
      box_color <- "#e8f5e9"  # Light green
    } else {
      explanation <- switch(input$map_metric,
                            "Cases_Weekly" = "Number of new COVID-19 cases reported in this ZIP code during the selected week.",
                            "Deaths_Weekly" = "Number of COVID-19 deaths reported in this ZIP code during the selected week.",
                            "Case_Rate_Weekly" = "Weekly COVID-19 cases per 100,000 residents. This population-adjusted rate allows fair comparison between ZIP codes of different sizes.",
                            "Select a metric to see its explanation."
      )
      icon_name <- "info-circle"
      box_color <- "#fff3e0"  # Light orange
    }
    
    # Create the info box
    div(
      style = paste0("background-color: ", box_color, "; padding: 12px; border-radius: 5px; border-left: 4px solid #2196F3; margin-top: 10px;"),
      tags$div(
        style = "display: flex; align-items: start;",
        tags$div(
          style = "margin-right: 10px; margin-top: 2px;",
          icon(icon_name, style = "color: #2196F3;")
        ),
        tags$div(
          style = "flex: 1;",
          tags$strong("What this means:"),
          tags$p(
            style = "margin: 5px 0 0 0; font-size: 13px; line-height: 1.4;",
            explanation
          )
        )
      )
    )
  })
  
  # === UI ===
  output$AnimationControls = renderUI({
    sliderInput(
      inputId = "date",
      label = "Progression",
      min = input$range[1],
      max = input$range[2],
      value = input$range[1],
      timeFormat = "%Y-%m-%d",
      animate = animationOptions(interval = 30)
    )
  })
  
  #Mathias graphs/plots
  # === PLOTS ===
  output$ChicagoPopulation <- renderPlot({
    Date.Start <- as.Date(input$range[1])
    Date.End <- as.Date(input$range[2])
    
    ggplot(Chicago$Population, aes(x=Date)) +
      geom_line(aes(y=Population.Boosted, color="Boosted")) +
      geom_line(aes(y=Population.Vaccinated, color="Vaccinated")) +
      geom_line(aes(y=Population.Unvaccinated, color="Unvaccinated")) +
      labs(y = "Population", x="Date", color="Group") +
      theme_minimal() +
      scale_color_manual(values = c("Boosted" = "blue", "Vaccinated" = "green", "Unvaccinated" = "red")) +
      {if(input$dateLine) geom_vline(xintercept = as.Date(input$date), linetype="dashed", color="red", size=0.5) }+
      {if(input$dateLine) geom_text(aes(x=as.Date(input$date), y=0, label=format(as.Date(input$date), "%Y-%m-%d")), vjust=-1, color="red") }+
      ggtitle("Chicago Population by Vaccination Status Over Time") +

            {if(input$enableAnnotations) geom_vline(xintercept = as.Date("2022-01-01"), linetype="dashed", color="red", size=0.5) }+
      {if(input$enableAnnotations) geom_text(aes(x=as.Date("2022-01-01"), y=0, label="new omicrant variant"), vjust=-1, color="red") }
  })
  
  output$ChicagoOutcomes <- renderPlot({
    Date.Start <- as.Date(input$range[1])
    Date.End <- as.Date(input$range[2])
    
    data <- Chicago$Outcomes |> filter(Date >= Date.Start & Date <= Date.End)
    
    ggplot(data, aes(x=Date, fill=Age.Group)) +
      geom_bar(aes(y=Outcome.Vaccinated), stat="identity", alpha=0.7) +
      geom_bar(aes(y=-Outcome.Unvaccinated), stat="identity", alpha=0.7) +
      geom_hline(yintercept = 0, color = "#222222", linewidth = 0.2) +
      {if(input$dateLine) geom_vline(xintercept = as.Date(input$date), linetype="dashed", color="red", size=0.5) }+
      {if(input$dateLine) geom_text(aes(x=as.Date(input$date), y=0, label=format(as.Date(input$date), "%Y-%m-%d")), vjust=-1, color="red") }+
      theme_minimal() +
      labs(y = "Number of Outcomes (Unvaccinated vs Vaccinated)", x="") +
      facet_wrap(~Outcome, scales = "free_y", ncol = 1)
  })
  
  #Jonas Graph/Plots
  ve_base <- reactive({
    req(Chicago$Raw)
    
    Chicago$Raw %>%
      filter(
        Outcome == "Cases",
        week_end >= as.Date(input$ve_date_range[1]),
        week_end <= as.Date(input$ve_date_range[2]),
        !is.na(`Unvaccinated Rate`),
        `Unvaccinated Rate` > 0,
        !is.na(`Vaccinated Rate`)
      ) %>%
      mutate(
        VE = (1 - (`Vaccinated Rate` / `Unvaccinated Rate`)) * 100,
        VE = pmax(pmin(VE, 100), -100)
      ) %>%
      rename(
        Date = week_end
      )
  })
  
  
  
  #Spaghetti Plot
  output$ve_spaghetti <- renderPlotly({
    d <- ve_base()
    req(nrow(d) > 0)
    
    d <- d %>%
      mutate(`Age Group` = ifelse(
        `Age Group` == "All",
        "All ages (combined)",
        `Age Group`
      ))
    
    ve_all <- d %>% filter(`Age Group` == "All ages (combined)")
    ve_age <- d %>% filter(`Age Group` != "All ages (combined)")
    ve_combined <- bind_rows(ve_age, ve_all)
    
    req(input$ve_age_groups)
    ve_combined <- ve_combined %>% filter(`Age Group` %in% input$ve_age_groups)
    req(nrow(ve_combined) > 0)
    
    desired_order <- c(
      "0-4", "5-11", "12-17", "18-29",
      "30-49", "50-64", "65-79", "80+",
      "All ages (combined)"
    )
    
    
    ve_combined <- ve_combined %>%
      mutate(`Age Group` = factor(`Age Group`, levels = desired_order))
    
    tmp <- ve_combined %>% mutate(`Age Group 2` = `Age Group`)
    
    p <- ggplot(ve_combined, aes(x = Date, y = VE)) +
      labs(x = "Date", y = "Vaccination effectiveness (%)") +
      geom_line(
        data = tmp %>% select(-`Age Group`),
        aes(group = `Age Group 2`),
        color = "grey",
        linewidth = 0.5,
        alpha = 0.5
      ) +
      geom_line(aes(color = `Age Group`), linewidth = 1.2) +
      scale_color_viridis_d() +
      facet_wrap(~ `Age Group`) +
      theme_minimal() +
      theme(legend.position = "none") +
      { if (isTRUE(input$omicron_line))
        geom_vline(
          xintercept = as.Date("2022-01-01"),
          linetype = "dashed",
          color = "red",
          linewidth = 0.5
        )
      } +
      { if (isTRUE(input$omicron_line))
        geom_text(
          aes(
            x = as.Date("2022-01-01"),
            y = 0,
            label = "Omicron variant peak"
          ),
          vjust = -1,
          color = "red"
        )
      }
    
    ggplotly(p)
  })
  
  
  
  #Streamgraph
  output$ve_stream <- renderPlotly({
    d <- ve_base() %>% filter(`Age Group` != "All")
    req(nrow(d) > 0)
    
    ve_stream <- d %>%
      select(Date, `Age Group` = `Age Group`, VE) %>%
      group_by(Date, `Age Group`) %>%
      summarise(VE= mean(VE, na.rm = TRUE), .groups = "drop") %>%
      mutate(age_lower = readr::parse_number(`Age Group`)) %>%
      arrange(age_lower, `Age Group`, Date) %>%
      mutate(`Age Group` = factor(`Age Group`, levels = unique(`Age Group`))) %>%
      group_by(`Age Group`) %>%
      arrange(Date, .by_group = TRUE) %>%
      mutate(idx = row_number()) %>%
      filter(idx %% 2 == 0) %>%
      ungroup()
    
    p <- ggplot(
      ve_stream,
      aes(
        x = Date,
        y = VE,
        fill = `Age Group`,
        text = paste0(
          "Age group: ", `Age Group`,
          "<br>Date: ", Date,
          "<br>VE: ", round(VE, 1), "%"
        )
      )
    ) +
      geom_stream(bw = 0.7) +
      scale_fill_viridis_d(option = "plasma") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  
  
  #Area Plot
  output$ve_area <- renderPlotly({
    d <- ve_base() %>% filter(`Age Group` != "All")
    req(nrow(d) > 0)
    
    ve_area <- d %>%
      mutate(age_lower = readr::parse_number(`Age Group`))
    
    order <- ve_area %>%
      distinct(`Age Group`, age_lower) %>%
      arrange(age_lower) %>%
      pull(`Age Group`)
    
    ve_area$`Age Group` <- factor(ve_area$`Age Group`, levels = order)
    
    omicron_date <- as.Date("2022-01-01")
    
    p <- ggplot(ve_area, aes(Date, VE, fill = `Age Group`)) +
      geom_area() +
      labs(y= "Vaccination effectiveness (%)") +
      scale_fill_viridis_d() +
      theme_minimal() +
      { if (isTRUE(input$omicron_line))
        geom_vline(
          xintercept = omicron_date,
          linetype = "dashed",
          color = "red",
          linewidth = 0.5
        )
      }
    
    g <- ggplotly(p)
    
    if (isTRUE(input$omicron_line)) {
      g <- g %>% layout(
        annotations = list(
          list(
            x = format(omicron_date, "%Y-%m-%d"),
            xref = "x",
            y = 1,
            yref = "paper",
            text = "Omicron variant peak",
            showarrow = FALSE,
            yanchor = "bottom",
            font = list(color = "red")
          )
        )
      )
    }
    
    g
  })
  
  
  
  
  #Michelle Graphs/Plots
  # GRAPH 1: Vaccine Rollout Timeline
  output$vaccineRolloutPlot <- renderPlotly({
    # Prepare the data
    data <- Chicago$Outcomes |>
      filter(Date >= as.Date(input$date_range_analysis[1]) & 
               Date <= as.Date(input$date_range_analysis[2])) |>
      group_by(Date, Outcome) |>
      summarise(
        Total_Vaccinated = sum(Outcome.Vaccinated, na.rm = TRUE),
        Total_Unvaccinated = sum(Outcome.Unvaccinated, na.rm = TRUE),
        Total_Cases = sum(Outcome.Vaccinated, na.rm = TRUE) + sum(Outcome.Unvaccinated, na.rm = TRUE),
        .groups = "drop"
      )
    
    pop_data <- Chicago$Population |>
      filter(Date >= as.Date(input$date_range_analysis[1]) & 
               Date <= as.Date(input$date_range_analysis[2])) |>
      mutate(Vaccination_Rate = (Population.Vaccinated / 
                                   (Population.Vaccinated + Population.Unvaccinated)) * 100)
    
    # Get cases data
    cases_data <- data |> filter(Outcome == "Cases")
    
    # Find the peak
    peak_date <- cases_data$Date[which.max(cases_data$Total_Cases)]
    peak_value <- max(cases_data$Total_Cases, na.rm = TRUE)
    
    # Create native plotly plot
    plot_ly() %>%
      # Add total cases line FIRST (so it's the primary data)
      add_trace(
        data = cases_data,
        x = ~Date,
        y = ~Total_Cases,
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#e74c3c', width = 2),
        name = 'Total Cases',
        text = ~paste0("Date: ", format(Date, "%B %d, %Y"), 
                       "<br>Total Cases: ", format(Total_Cases, big.mark = ",")),
        hovertemplate = '%{text}<extra></extra>'
      ) %>%
      # Add vaccination rate area (subtle background)
      add_trace(
        data = pop_data,
        x = ~Date,
        y = ~Vaccination_Rate,
        type = 'scatter',
        mode = 'none',
        fill = 'tozeroy',
        fillcolor = 'rgba(52, 152, 219, 0.3)',
        name = 'Vaccination Rate (%)',
        text = ~paste0("Date: ", format(Date, "%B %d, %Y"), 
                       "<br>Vaccination Rate: ", round(Vaccination_Rate, 1), "%"),
        hovertemplate = '%{text}<extra></extra>',
        yaxis = 'y2'
      ) %>%
      # Add peak marker line
      add_segments(
        x = peak_date,
        xend = peak_date,
        y = 0,
        yend = peak_value,
        line = list(color = '#c0392b', width = 2, dash = 'dash'),
        showlegend = FALSE,
        hoverinfo = 'skip'
      ) %>%
      # Add peak annotation with better positioning
      add_annotations(
        x = peak_date,
        y = peak_value * 1.05,
        text = paste0("<b>Peak: Omicron Variant Surge</b><br>",
                      format(peak_date, "%B %d, %Y")),
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 2,
        arrowsize = 1,
        arrowwidth = 2,
        arrowcolor = "#c0392b",
        ax = 50,
        ay = -50,
        font = list(color = "#c0392b", size = 11),
        bgcolor = "rgba(255, 255, 255, 0.9)",
        bordercolor = "#c0392b",
        borderwidth = 1.5,
        borderpad = 4
      ) %>%
      # Layout with dual y-axes and better spacing
      layout(
        title = list(
          text = "<b>COVID-19 Cases vs. Vaccination Rate Over Time</b><br><sub>How vaccine rollout impacted case numbers in Chicago</sub>",
          font = list(size = 16)
        ),
        xaxis = list(
          title = "Date",
          tickangle = -45
        ),
        yaxis = list(
          title = "Number of Cases",
          side = "left",
          showgrid = TRUE
        ),
        yaxis2 = list(
          title = "Vaccination Rate (%)",
          overlaying = "y",
          side = "right",
          showgrid = FALSE,
          range = c(0, 100)  # Fixed range 0-100% for vaccination rate
        ),
        hovermode = 'x unified',
        legend = list(
          orientation = "h",
          y = -0.25,  # Moved further down to avoid overlap
          x = 0.5,
          xanchor = "center",
          yanchor = "top"
        ),
        margin = list(b = 120, t = 80, l = 60, r = 60)  # Increased bottom margin
      )
  })
  
  # GRAPH 2: Risk Reduction Analysis
  output$riskReductionPlot <- renderPlot({
    data <- Chicago$Outcomes |>
      filter(Date >= as.Date(input$date_range_analysis[1]) & 
               Date <= as.Date(input$date_range_analysis[2])) |>
      group_by(Outcome) |>
      summarise(
        Total_Vaccinated = sum(Outcome.Vaccinated, na.rm = TRUE),
        Total_Unvaccinated = sum(Outcome.Unvaccinated, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        Rate_Vaccinated = Total_Vaccinated / sum(Total_Vaccinated) * 100000,
        Rate_Unvaccinated = Total_Unvaccinated / sum(Total_Unvaccinated) * 100000,
        Risk_Reduction = ((Rate_Unvaccinated - Rate_Vaccinated) / Rate_Unvaccinated) * 100
      )
    
    plot_data <- data |>
      select(Outcome, Rate_Vaccinated, Rate_Unvaccinated) |>
      pivot_longer(cols = c(Rate_Vaccinated, Rate_Unvaccinated), 
                   names_to = "Status", 
                   values_to = "Rate") |>
      mutate(Status = ifelse(Status == "Rate_Vaccinated", "Vaccinated", "Unvaccinated"))
    
    ggplot(plot_data, aes(x = Outcome, y = Rate, fill = Status)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_text(aes(label = round(Rate, 1)), 
                position = position_dodge(width = 0.7), 
                vjust = -0.5, 
                size = 3.5) +
      scale_fill_manual(values = c("Vaccinated" = "#0072B2", "Unvaccinated" = "#D55E00")) +
      theme_minimal() +
      labs(
        title = "Vaccine Effectiveness: Relative Rates by Outcome",
        subtitle = "Comparing rates between vaccinated and unvaccinated individuals",
        x = "Outcome Type",
        y = "Relative Rate (per 100k)",
        fill = "Vaccination Status"
      ) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(size = 11)
      )
  })
  
  # GRAPH 3: ZIP Code Map (Vaccination Data Only)
  # Main ZIP code map
  output$chicagoZipMap <- renderLeaflet({
    # Check if data is loaded
    if (is.null(Chicago$ZipVaccination) || is.null(Chicago$ZipProgression) || is.null(Chicago$Boundaries)) {
      # Return an empty leaflet map with message
      leaflet() %>%
        addTiles() %>%
        setView(lng = -87.6298, lat = 41.8781, zoom = 10)
    } else if (is.null(input$map_metric) || is.null(input$map_type)) {
      # Map metric not selected yet
      leaflet() %>%
        addTiles() %>%
        setView(lng = -87.6298, lat = 41.8781, zoom = 10)
    } else {
      # Use tryCatch to catch any errors
      tryCatch({
        # Select the appropriate dataset based on map type
        current_data <- if (input$map_type == "vaccination") {
          Chicago$ZipVaccination
        } else {
          Chicago$ZipProgression
        }
        
        # Find the nearest available date
        selected_date <- as.Date(input$zip_date)
        available_dates <- sort(unique(as.Date(current_data$Date)))
        nearest_date <- available_dates[which.min(abs(available_dates - selected_date))]
        
        map_data <- current_data %>%
          mutate(Date = as.Date(Date)) %>%
          filter(Date == nearest_date)
        
        map_sf <- Chicago$Boundaries %>%
          left_join(map_data, by = "ZIP_Code")
        
        # Get metric values and clean them thoroughly
        metric_values <- as.numeric(map_sf[[input$map_metric]])
        
        # Replace any problematic values with NA
        metric_values[is.na(metric_values)] <- NA
        metric_values[is.nan(metric_values)] <- NA
        metric_values[is.infinite(metric_values)] <- NA
        metric_values[metric_values < 0] <- NA  # Remove negative values
        
        # Check if we have any valid data
        valid_values <- metric_values[!is.na(metric_values)]
        
        if (length(valid_values) == 0) {
          # No valid data - return message
          return(
            leaflet() %>%
              addTiles() %>%
              setView(lng = -87.6298, lat = 41.8781, zoom = 10) %>%
              addControl(html = "<div style='background:white; padding:10px; border-radius:5px;'>
                       <strong>No data available for this date/metric</strong><br>
                       Try selecting a different date or metric.</div>",
                         position = "topright")
          )
        }
        
        # Create color palette with the valid range
        if (input$color_scheme == "viridis") {
          pal <- colorNumeric(
            palette = viridis::viridis(256),
            domain = range(valid_values, na.rm = TRUE),
            na.color = "#808080"
          )
        } else {
          pal <- colorNumeric(
            palette = input$color_scheme,
            domain = range(valid_values, na.rm = TRUE),
            na.color = "#808080"
          )
        }
        
        # Dynamic metric label based on map type
        metric_label <- if (input$map_type == "vaccination") {
          switch(input$map_metric,
                 "Vaccinated_1st_Dose" = "1st Dose Rate (%)",
                 "Fully_Vaccinated" = "Fully Vaccinated (%)",
                 "Boosted" = "Booster Rate (%)",
                 "Metric")
        } else {
          switch(input$map_metric,
                 "Cases_Weekly" = "Weekly Cases",
                 "Deaths_Weekly" = "Weekly Deaths",
                 "Case_Rate_Weekly" = "Case Rate (per 100k)",
                 "Metric")
        }
        
        # Create map
        map_output <- leaflet(map_sf) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addPolygons(
            fillColor = ~pal(metric_values),
            weight = 1.5,
            opacity = 1,
            color = "white",
            fillOpacity = 0.7,
            highlightOptions = highlightOptions(
              weight = 3,
              color = "#666",
              fillOpacity = 0.9,
              bringToFront = TRUE
            ),
            label = ~paste0(
              "<strong>ZIP Code: ", ZIP_Code, "</strong><br>",
              metric_label, ": ", 
              ifelse(is.na(metric_values), "No data", 
                     format(round(metric_values, 1), big.mark = ","))
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            ),
            layerId = ~ZIP_Code
          )
        
        # Add ZIP code labels if enabled
        if(input$show_labels) {
          map_output <- map_output %>%
            addLabelOnlyMarkers(
              data = st_centroid(st_geometry(map_sf)),
              label = map_sf$ZIP_Code,
              labelOptions = labelOptions(
                noHide = TRUE,
                direction = "center",
                textOnly = TRUE,
                style = list(
                  "color" = "#000000",
                  "font-size" = "10px",
                  "font-weight" = "bold"
                )
              )
            )
        }
        
        # Add legend
        map_output %>%
          addLegend(
            pal = pal,
            values = valid_values,
            opacity = 0.7,
            title = metric_label,
            position = "bottomright",
            na.label = "No data"
          )
        
      }, error = function(e) {
        # If any error occurs, return a map with error message
        leaflet() %>%
          addTiles() %>%
          setView(lng = -87.6298, lat = 41.8781, zoom = 10) %>%
          addControl(html = paste0("<div style='background:white; padding:10px; border-radius:5px;'>
                   <strong>Error rendering map</strong><br>", e$message, "</div>"),
                     position = "topright")
      })
    }
  })
  
  # Observe map clicks to select ZIP code
  observeEvent(input$chicagoZipMap_shape_click, {
    click <- input$chicagoZipMap_shape_click
    if (!is.null(click)) {
      selected_zip(click$id)
    }
  })
  
  # Summary statistics for the map
  output$map_stats_date <- renderText({
    if (is.null(Chicago$ZipVaccination) || is.null(Chicago$ZipProgression)) return("Data not loaded")
    
    req(input$map_metric, input$zip_date, input$map_type)
    
    # Select appropriate dataset
    current_data <- if (input$map_type == "vaccination") {
      Chicago$ZipVaccination
    } else {
      Chicago$ZipProgression
    }
    
    # Snap to nearest available date
    selected_date <- as.Date(input$zip_date)
    available_dates <- sort(unique(as.Date(current_data$Date)))
    nearest_date <- available_dates[which.min(abs(available_dates - selected_date))]
    
    date_str <- format(nearest_date, "%B %d, %Y")
    paste0("Date: ", date_str)
  })
  
  output$map_stats_total <- renderText({
    if (is.null(Chicago$ZipVaccination) || is.null(Chicago$ZipProgression)) return("Data not loaded")
    
    req(input$map_metric, input$zip_date, input$map_type)
    
    # Select appropriate dataset
    current_data <- if (input$map_type == "vaccination") {
      Chicago$ZipVaccination
    } else {
      Chicago$ZipProgression
    }
    
    # Snap to nearest available date
    selected_date <- as.Date(input$zip_date)
    available_dates <- sort(unique(as.Date(current_data$Date)))
    nearest_date <- available_dates[which.min(abs(available_dates - selected_date))]
    
    data <- current_data %>%
      mutate(Date = as.Date(Date)) %>%
      filter(Date == nearest_date)
    
    if (nrow(data) == 0) return("No data for this date")
    
    total <- sum(data[[input$map_metric]], na.rm = TRUE)
    
    if (grepl("Rate|Percent", input$map_metric)) {
      avg <- mean(data[[input$map_metric]], na.rm = TRUE)
      paste0("City Avg: ", format(round(avg, 1), big.mark = ","), 
             ifelse(grepl("Percent|Dose|Vaccinated|Boosted", input$map_metric), "%", ""))
    } else {
      paste0("Total: ", format(round(total, 0), big.mark = ","))
    }
  })
  
  output$map_stats_avg <- renderText({
    if (is.null(Chicago$ZipVaccination) || is.null(Chicago$ZipProgression)) return("Data not loaded")
    
    req(input$map_metric, input$zip_date, input$map_type)
    
    # Select appropriate dataset
    current_data <- if (input$map_type == "vaccination") {
      Chicago$ZipVaccination
    } else {
      Chicago$ZipProgression
    }
    
    # Snap to nearest available date
    selected_date <- as.Date(input$zip_date)
    available_dates <- sort(unique(as.Date(current_data$Date)))
    nearest_date <- available_dates[which.min(abs(available_dates - selected_date))]
    
    data <- current_data %>%
      mutate(Date = as.Date(Date)) %>%
      filter(Date == nearest_date)
    
    if (nrow(data) == 0) return("No data for this date")
    
    avg <- mean(data[[input$map_metric]], na.rm = TRUE)
    
    paste0("Per ZIP: ", format(round(avg, 1), big.mark = ","),
           ifelse(grepl("Percent|Dose|Vaccinated|Boosted", input$map_metric), "%", ""))
  })
  
  # Time series for selected ZIP code
  output$selectedZipTimeSeries <- renderPlot({
    if (is.null(Chicago$ZipVaccination) || is.null(Chicago$ZipProgression) || is.null(selected_zip())) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Click on a ZIP code in the map to see its time series",
                 size = 6, color = "gray50") +
        theme_void()
    } else {
      req(input$map_metric, input$map_type)
      
      # Select appropriate dataset
      current_data <- if (input$map_type == "vaccination") {
        Chicago$ZipVaccination
      } else {
        Chicago$ZipProgression
      }
      
      zip_data <- current_data %>%
        filter(ZIP_Code == selected_zip())
      
      if (nrow(zip_data) == 0) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste0("No data available for ZIP code ", selected_zip()),
                   size = 6, color = "gray50") +
          theme_void()
      } else {
        metric_label <- if (input$map_type == "vaccination") {
          switch(input$map_metric,
                 "Vaccinated_1st_Dose" = "1st Dose Rate (%)",
                 "Fully_Vaccinated" = "Fully Vaccinated (%)",
                 "Boosted" = "Booster Rate (%)",
                 "Metric")
        } else {
          switch(input$map_metric,
                 "Cases_Weekly" = "Weekly Cases",
                 "Deaths_Weekly" = "Weekly Deaths",
                 "Case_Rate_Weekly" = "Case Rate (per 100k)",
                 "Metric")
        }
        
        # Choose color based on map type
        plot_color <- if (input$map_type == "vaccination") "#2E86AB" else "#e74c3c"
        
        ggplot(zip_data, aes(x = Date, y = .data[[input$map_metric]])) +
          geom_line(color = plot_color, size = 1) +
          geom_area(fill = plot_color, alpha = 0.2) +
          geom_vline(xintercept = as.Date(input$zip_date), 
                     linetype = "dashed", color = "red", size = 0.8) +
          geom_point(data = zip_data %>% filter(Date == as.Date(input$zip_date)),
                     color = "red", size = 3) +
          labs(
            title = paste0("ZIP Code ", selected_zip(), ": ", metric_label, " Over Time"),
            subtitle = paste0("Red line shows current date: ", 
                              format(as.Date(input$zip_date), "%B %d, %Y")),
            x = "Date",
            y = metric_label
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(size = 11, color = "gray40"),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )
      }
    }
  })
})

shinyApp(ui, server)