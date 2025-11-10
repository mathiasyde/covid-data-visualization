library(shiny)
library(ggplot2)
library(ggridges)
library(dplyr)
library(grid)
library(bslib)
library(tidyr)

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
	      plotOutput("riskReductionPlot", height = "400px"),
	      br(),
	      
	      # Graph 3: Age Group Comparison
	      h4("3. Vaccine Benefits by Age Group"),
	      plotOutput("ageGroupBenefitPlot", height = "500px")
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
	Chicago <- list()

	# === DATA ===
	Chicago$Outcomes <- read.csv("data/chicago_outcomes.csv", stringsAsFactors = FALSE) |>
		mutate(Date = as.Date(Date)) |>
		mutate(Age.Group = factor(Age.Group, levels = c("0-4", "5-11", "12-17", "18-29", "30-49", "50-64", "65-79", "80+"), ordered = TRUE))

	Chicago$Population <- read.csv("data/chicago_population.csv", stringsAsFactors = FALSE) |>
		mutate(Date = as.Date(Date))

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
		# Keep the population plot limited to the selected date range so the x-axis
		# aligns with the outcomes plot below.
		Date.Start <- as.Date(input$range[1])
		Date.End <- as.Date(input$range[2])

		ggplot(Chicago$Population, aes(x=Date)) +
			geom_line(aes(y=Population.Boosted, color="Boosted")) +
			geom_line(aes(y=Population.Vaccinated, color="Vaccinated")) +
			geom_line(aes(y=Population.Unvaccinated, color="Unvaccinated")) +
			labs(y = "Population", x="Date", color="Group") +
			theme_minimal() +
			scale_color_manual(values = c("Boosted" = "blue", "Vaccinated" = "green", "Unvaccinated" = "red")) +

			# draw a vertical line to show the current date
			{if(input$dateLine) geom_vline(xintercept = as.Date(input$date), linetype="dashed", color="red", size=0.5) }+
			{if(input$dateLine) geom_text(aes(x=as.Date(input$date), y=0, label=format(as.Date(input$date), "%Y-%m-%d")), vjust=-1, color="red") }+
			
			ggtitle("Chicago Population by Vaccination Status Over Time")
	})

	output$ChicagoOutcomes <- renderPlot({
		Date.Start <- as.Date(input$range[1])
		Date.End <- as.Date(input$range[2])

		data <- Chicago$Outcomes |> filter(Date >= Date.Start & Date <= Date.End)

		ggplot(data, aes(x=Date, fill=Age.Group)) +
			geom_bar(aes(y=Outcome.Vaccinated), stat="identity", alpha=0.7) +
			geom_bar(aes(y=-Outcome.Unvaccinated), stat="identity", alpha=0.7) +
			# draw a horizontal line to better see where y=0
			geom_hline(yintercept = 0, color = "#222222", linewidth = 0.2) +

			# draw a vertical line to show the current date
			{if(input$dateLine) geom_vline(xintercept = as.Date(input$date), linetype="dashed", color="red", size=0.5) }+
			{if(input$dateLine) geom_text(aes(x=as.Date(input$date), y=0, label=format(as.Date(input$date), "%Y-%m-%d")), vjust=-1, color="red") }+

			theme_minimal() +
			labs(y = "Number of Outcomes (Unvaccinated vs Vaccinated)", x="") +
			facet_wrap(~Outcome, scales = "free_y", ncol = 1)
	})
	
	#Michelle Graphs/Plots
	# GRAPH 1: Vaccine Rollout Timeline
	output$vaccineRolloutPlot <- renderPlot({
	  # Filter data based on date range
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
	  
	  # Get population data for vaccination rates
	  pop_data <- Chicago$Population |>
	    filter(Date >= as.Date(input$date_range_analysis[1]) & 
	             Date <= as.Date(input$date_range_analysis[2])) |>
	    mutate(Vaccination_Rate = (Population.Vaccinated / 
	                                 (Population.Vaccinated + Population.Unvaccinated)) * 100)
	  
	  # Create dual-axis plot
	  ggplot() +
	    # Cases over time (line)
	    geom_line(data = data |> filter(Outcome == "Cases"), 
	              aes(x = Date, y = Total_Cases, color = "Total Cases"), 
	              size = 1) +
	    # Vaccination rate (area)
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
	      # Calculate rate per 100k (approximate)
	      Rate_Vaccinated = Total_Vaccinated / sum(Total_Vaccinated) * 100000,
	      Rate_Unvaccinated = Total_Unvaccinated / sum(Total_Unvaccinated) * 100000,
	      # Calculate risk reduction percentage
	      Risk_Reduction = ((Rate_Unvaccinated - Rate_Vaccinated) / Rate_Unvaccinated) * 100
	    )
	  
	  # Reshape data for grouped bar chart
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
	
	# GRAPH 3: Age Group Benefit Analysis
	output$ageGroupBenefitPlot <- renderPlot({
	  # Filter by selected age groups and date range
	  data <- Chicago$Outcomes |>
	    filter(Date >= as.Date(input$date_range_analysis[1]) & 
	             Date <= as.Date(input$date_range_analysis[2])) |>
	    filter(Age.Group %in% input$age_groups_filter) |>
	    group_by(Age.Group, Outcome) |>
	    summarise(
	      Total_Vaccinated = sum(Outcome.Vaccinated, na.rm = TRUE),
	      Total_Unvaccinated = sum(Outcome.Unvaccinated, na.rm = TRUE),
	      .groups = "drop"
	    ) |>
	    mutate(
	      Total = Total_Vaccinated + Total_Unvaccinated,
	      Vaccinated_Percentage = (Total_Vaccinated / Total) * 100,
	      # Calculate protection benefit (lower percentage = better protection)
	      Protection_Benefit = 100 - Vaccinated_Percentage
	    )
	  
	  ggplot(data, aes(x = Age.Group, y = Protection_Benefit, fill = Outcome)) +
	    geom_bar(stat = "identity", position = "dodge") +
	    geom_text(aes(label = paste0(round(Protection_Benefit, 1), "%")), 
	              position = position_dodge(width = 0.9), 
	              vjust = -0.5, 
	              size = 3) +
	    scale_fill_brewer(palette = "Set2") +
	    theme_minimal() +
	    labs(
	      title = "Vaccine Protection Benefit by Age Group",
	      subtitle = "Higher percentage = greater protection from vaccines",
	      x = "Age Group",
	      y = "Protection Benefit (%)",
	      fill = "Outcome Type"
	    ) +
	    theme(
	      legend.position = "bottom",
	      plot.title = element_text(face = "bold", size = 14),
	      axis.text.x = element_text(angle = 45, hjust = 1)
	    ) +
	    facet_wrap(~Outcome, ncol = 1, scales = "free_y")
	})
})

shinyApp(ui, server)