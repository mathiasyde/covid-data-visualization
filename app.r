library(shiny)
library(ggplot2)
library(ggridges)
library(dplyr)
library(grid)
library(bslib)
library(tidyr)
library(sf)
library(leaflet)

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
        plotOutput("vaccineRolloutPlot", height = "400px"),
        br(),
        
        # Graph 2: Risk Reduction Analysis
        h4("2. Vaccine Risk Reduction by Outcome Type"),
        plotOutput("riskReductionPlot", height = "400px")
      )
    )
  ),
  
  tabPanel(
    "ZIP Code Analysis",
    sidebarLayout(
      sidebarPanel(
        h4("Map Controls"),
        
        uiOutput("zip_date_selector"),
        
        p("Select a week to view data. Data is reported weekly."),
        
        hr(),
        
        selectInput(
          "map_metric",
          label = "Select Vaccination Metric",
          choices = c(
            "1st Dose Rate (%)" = "Vaccinated_1st_Dose",
            "Fully Vaccinated Rate (%)" = "Fully_Vaccinated",
            "Booster Rate (%)" = "Boosted",
            "Total Doses Given" = "Total_Doses"
          ),
          selected = "Fully_Vaccinated"
        ),
        
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
            tags$li("Use the dropdown to select different weeks"),
            tags$li("Choose different vaccination metrics to visualize"),
            tags$li("Hover over ZIP codes for detailed information"),
            tags$li("Click a ZIP code to see its vaccination trend over time")
          )
        )
      ),
      
      mainPanel(
        h3("COVID-19 Vaccination Rates by ZIP Code in Chicago"),
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
  
  # Load ZIP code data and boundaries
  Chicago$ZipData <- tryCatch({
    read.csv("data/chicago_zip_data.csv", stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date))
  }, error = function(e) {
    warning("ZIP code data not found. Please run prepare_zip_data.r first.")
    NULL
  })
  
  Chicago$Boundaries <- tryCatch({
    st_read("geographic/chicago_zip_boundaries.geojson", quiet = TRUE)
  }, error = function(e) {
    warning("Geographic boundaries not found. Please run prepare_zip_data.r first.")
    NULL
  })
  
  # Reactive value to store selected ZIP code
  selected_zip <- reactiveVal(NULL)
  
  # Dynamic date selector - slider that snaps to available dates
  output$zip_date_selector <- renderUI({
    if (is.null(Chicago$ZipData)) {
      return(p("Loading data..."))
    }
    
    # Get all available dates from the data
    available_dates <- sort(unique(as.Date(Chicago$ZipData$Date)))
    
    # Find a good default date (around middle of 2021)
    default_date <- available_dates[which.min(abs(available_dates - as.Date("2021-06-01")))]
    
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
  
  #Michelle Graphs/Plots
  # GRAPH 1: Vaccine Rollout Timeline
  output$vaccineRolloutPlot <- renderPlot({
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
    
    ggplot() +
      geom_line(data = data |> filter(Outcome == "Cases"), 
                aes(x = Date, y = Total_Cases, color = "Total Cases"), 
                size = 1) +
      geom_area(data = pop_data, 
                aes(x = Date, y = Vaccination_Rate * 100, fill = "Vaccination Rate"), 
                alpha = 0.3) +
      scale_y_continuous(
        name = "Number of Cases",
        sec.axis = sec_axis(~./100, name = "Vaccination Rate (%)")
      ) +
      scale_color_manual(values = c("Total Cases" = "#e74c3c")) +
      scale_fill_manual(values = c("Vaccination Rate" = "#3498db")) +
      theme_minimal() +
      labs(
        title = "COVID-19 Cases vs. Vaccination Rate Over Time",
        subtitle = "How vaccine rollout impacted case numbers in Chicago",
        x = "Date",
        color = "",
        fill = ""
      ) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1)
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
    if (is.null(Chicago$ZipData) || is.null(Chicago$Boundaries)) {
      # Return an empty leaflet map with message
      leaflet() %>%
        addTiles() %>%
        setView(lng = -87.6298, lat = 41.8781, zoom = 10)
    } else if (is.null(input$map_metric)) {
      # Map metric not selected yet
      leaflet() %>%
        addTiles() %>%
        setView(lng = -87.6298, lat = 41.8781, zoom = 10)
    } else {
      # Data is loaded and metric is selected, render the map
      
      # Find the nearest available date (since slider might pick in-between dates)
      selected_date <- as.Date(input$zip_date)
      available_dates <- sort(unique(as.Date(Chicago$ZipData$Date)))
      nearest_date <- available_dates[which.min(abs(available_dates - selected_date))]
      
      map_data <- Chicago$ZipData %>%
        mutate(Date = as.Date(Date)) %>%
        filter(Date == nearest_date)
      
      map_sf <- Chicago$Boundaries %>%
        left_join(map_data, by = "ZIP_Code")
      
      metric_values <- map_sf[[input$map_metric]]
      
      if (input$color_scheme == "viridis") {
        pal <- colorNumeric(
          palette = viridis::viridis(256),
          domain = metric_values,
          na.color = "#808080"
        )
      } else {
        pal <- colorNumeric(
          palette = input$color_scheme,
          domain = metric_values,
          na.color = "#808080"
        )
      }
      
      metric_label <- switch(input$map_metric,
                             "Vaccinated_1st_Dose" = "1st Dose Rate (%)",
                             "Fully_Vaccinated" = "Fully Vaccinated (%)",
                             "Boosted" = "Booster Rate (%)",
                             "Total_Doses" = "Total Doses",
                             "1st Dose Rate (%)"  # default
      )
      
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
            ifelse(is.na(metric_values), "No data", round(metric_values, 2))
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
          values = ~metric_values,
          opacity = 0.7,
          title = metric_label,
          position = "bottomright",
          na.label = "No data"
        )
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
    if (is.null(Chicago$ZipData)) return("Data not loaded")
    
    req(input$map_metric, input$zip_date)
    
    # Snap to nearest available date
    selected_date <- as.Date(input$zip_date)
    available_dates <- sort(unique(as.Date(Chicago$ZipData$Date)))
    nearest_date <- available_dates[which.min(abs(available_dates - selected_date))]
    
    date_str <- format(nearest_date, "%B %d, %Y")
    paste0("Date: ", date_str)
  })
  
  output$map_stats_total <- renderText({
    if (is.null(Chicago$ZipData)) return("Data not loaded")
    
    req(input$map_metric, input$zip_date)
    
    # Snap to nearest available date
    selected_date <- as.Date(input$zip_date)
    available_dates <- sort(unique(as.Date(Chicago$ZipData$Date)))
    nearest_date <- available_dates[which.min(abs(available_dates - selected_date))]
    
    data <- Chicago$ZipData %>%
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
    if (is.null(Chicago$ZipData)) return("Data not loaded")
    
    req(input$map_metric, input$zip_date)
    
    # Snap to nearest available date
    selected_date <- as.Date(input$zip_date)
    available_dates <- sort(unique(as.Date(Chicago$ZipData$Date)))
    nearest_date <- available_dates[which.min(abs(available_dates - selected_date))]
    
    data <- Chicago$ZipData %>%
      mutate(Date = as.Date(Date)) %>%
      filter(Date == nearest_date)
    
    if (nrow(data) == 0) return("No data for this date")
    
    avg <- mean(data[[input$map_metric]], na.rm = TRUE)
    
    paste0("Per ZIP: ", format(round(avg, 1), big.mark = ","),
           ifelse(grepl("Percent|Dose|Vaccinated|Boosted", input$map_metric), "%", ""))
  })
  
  # Time series for selected ZIP code
  output$selectedZipTimeSeries <- renderPlot({
    if (is.null(Chicago$ZipData) || is.null(selected_zip())) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Click on a ZIP code in the map to see its time series",
                 size = 6, color = "gray50") +
        theme_void()
    } else {
      req(input$map_metric)
      
      zip_data <- Chicago$ZipData %>%
        filter(ZIP_Code == selected_zip())
      
      if (nrow(zip_data) == 0) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste0("No data available for ZIP code ", selected_zip()),
                   size = 6, color = "gray50") +
          theme_void()
      } else {
        metric_label <- switch(input$map_metric,
                               "Vaccinated_1st_Dose" = "1st Dose Rate (%)",
                               "Fully_Vaccinated" = "Fully Vaccinated (%)",
                               "Boosted" = "Booster Rate (%)",
                               "Total_Doses" = "Total Doses",
                               "1st Dose Rate (%)"  # default
        )
        
        ggplot(zip_data, aes(x = Date, y = .data[[input$map_metric]])) +
          geom_line(color = "#2E86AB", size = 1) +
          geom_area(fill = "#2E86AB", alpha = 0.2) +
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