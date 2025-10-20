library(shiny)
library(ggplot2)
library(ggridges)
library(dplyr)

ui <- fluidPage(
	titlePanel("Exploratory visualization of the COVID-19 pandemic in the U.S."),
	sidebarLayout(
		sidebarPanel(
			dateRangeInput(
				"range",
				label = "Date range",
				start = "2021-01-01",
				end = "2023-06-30"
			),
			uiOutput("AnimationControls"),
		),
		mainPanel(
			h3("Chicago Historical Outcomes by Vaccination Status"),
			plotOutput("ChicagoOutcomes"),
			plotOutput("ChicagoPopulation")
		)
	)
)

server <- shinyServer(function(input, output, session) {
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
		most_recent <- Chicago$Population |> filter(Date <= as.Date(input$date)) |> tail(1)

		data <- data.frame(
			group=c("Vaccinated", "Unvaccinated", "Boosted"),
			value=c(
				most_recent$Population.Vaccinated,
				most_recent$Population.Unvaccinated,
				most_recent$Population.Boosted
			)
		)

		ggplot(data, aes(x="", y=value, fill=group)) +
			geom_bar(stat="identity", width=1, color="white") +
			coord_polar("y", start=0) +
			theme_void()
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
			geom_vline(xintercept = as.Date(input$date), linetype="dashed", color="red", size=0.5) +
			geom_text(aes(x=as.Date(input$date), y=0, label=format(as.Date(input$date), "%Y-%m-%d")), vjust=-1, color="red") +

			theme_minimal() +
			theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
			labs(y = "Number of Outcomes (Unvaccinated vs Vaccinated)", x="") +
			facet_wrap(~Outcome, scales = "free_y", ncol = 1)
	})
})

shinyApp(ui, server)