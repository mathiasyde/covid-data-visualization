library(shiny)
library(ggplot2)
library(ggridges)
library(dplyr)
library(bslib)

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
				p("Please note that the animation rendering may be slow.")
			),
			mainPanel(
				h3("Exploring the COVID-19 pandemic in Chicago."),
				layout_columns(
					p(
						"The state of Illinois were one of the first states in the U.S. to be impacted by COVID-19 after an
						international travel event in early 2020. Here, we will explore historical data of the pandemic in the
						Illinois, focusing on Chicago."
					),
					plotOutput("ChicagoPopulation")
				),
				p("Area above the y=0 line represents vaccinated outcomes; area below represents unvaccinated outcomes."),
				plotOutput("ChicagoOutcomes")
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
				tags$li(a("CDC: Centers for Disease Control and Prevention, COVID-19 Timeline (July 8, 2024)",
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

	# === PLOTS ===
	output$ChicagoPopulation <- renderPlot({
		targetDate <- as.Date(input$date)
		pop_df <- Chicago$Population

		# Guard: if no population data, nothing to plot
		if (is.null(pop_df) || nrow(pop_df) == 0) return(NULL)

		# Get the most recent row on/before target date; if none, fallback to earliest available
		most_recent <- pop_df |> dplyr::filter(Date <= targetDate) |> dplyr::slice_tail(n = 1)
		if (nrow(most_recent) == 0) {
			most_recent <- pop_df |> dplyr::arrange(Date) |> dplyr::slice(1)
		}

		boosted <- most_recent$Population.Boosted
		vaccinated <- most_recent$Population.Vaccinated
		unvaccinated <- most_recent$Population.Unvaccinated

		boosted <- ifelse(is.na(boosted), 0, boosted)
		vaccinated <- ifelse(is.na(vaccinated), 0, vaccinated)
		unvaccinated <- ifelse(is.na(unvaccinated), 0, unvaccinated)

		data <- data.frame(
			group = c("Boosted", "Vaccinated", "Unvaccinated"),
			value = c(boosted, vaccinated, unvaccinated)
		)

		ggplot(data, aes(x="", y=value, fill=group)) +
			geom_bar(stat="identity", width=1, color="white") +
			coord_polar("y", start=0) +
			theme_void() + 
			ggtitle(paste("Chicago Population by Vaccination Status on", format(targetDate, "%Y-%m-%d")))
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
			# note: commented out for now since shiny is incredibly slow rendering this plot
			# geom_vline(xintercept = as.Date(input$date), linetype="dashed", color="red", size=0.5) +
			# geom_text(aes(x=as.Date(input$date), y=0, label=format(as.Date(input$date), "%Y-%m-%d")), vjust=-1, color="red") +

			theme_minimal() +
			theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
			labs(y = "Number of Outcomes (Unvaccinated vs Vaccinated)", x="") +
			facet_wrap(~Outcome, scales = "free_y", ncol = 1)
	})
})

shinyApp(ui, server)