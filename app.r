library(shiny)
library(ggplot2)
library(ggridges)
library(dplyr)

Chicago <- read.csv("data/historical_covid_cases_by_vaccination_status_chicago.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
	titlePanel("Exploratory visualization of the COVID-19 pandemic in the U.S."),
	sidebarLayout(
		sidebarPanel(
			helpText("This app visualizes COVID data."),
			dateRangeInput(
				"dateRange",
				label = "Date range",
				start = "2021-01-01",
				end = "2023-06-30"
			)
		),
		mainPanel(
			h3("Chicago Historical Outcomes by Vaccination Status"),
			plotOutput("Chicago")							
		)
	)
)

server <- shinyServer(function(input, output, session) {
	output$Chicago <- renderPlot({
		Chicago$Date <- as.Date(Chicago$Date)
		Date.Start <- if (!is.null(input$dateRange)) as.Date(input$dateRange[1]) else min(Chicago$Date, na.rm = TRUE)
		Date.End <- if (!is.null(input$dateRange)) as.Date(input$dateRange[2]) else max(Chicago$Date, na.rm = TRUE)

		AGE_LEVELS <- c("0-4", "5-11", "12-17", "18-29", "30-49", "50-64", "65-79", "80+")
		Chicago <- Chicago |> mutate(Age.Group = factor(Age.Group, levels = AGE_LEVELS, ordered = TRUE))

		ggplot(filter(Chicago, Date >= Date.Start & Date <= Date.End), aes(x=Date, fill=Age.Group)) +
			# stacked bar chart for vaccinated vs unvaccinated outcomes
			geom_bar(aes(y=Outcome.Vaccinated), stat="identity", alpha=0.7) +
			geom_bar(aes(y=-Outcome.Unvaccinated), stat="identity", alpha=0.7) +
			# draw a horizontal baseline at y = 0 for better readability
			geom_hline(yintercept = 0, color = "#222222", linewidth = 0.2) +
			scale_y_continuous(labels = abs) +
			labs(y = "Number of Outcomes (Unvaccinated vs Vaccinated)", x = paste("Date Range:", Date.Start, "to", Date.End)) +
			theme_minimal() +
			theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
			facet_wrap(~Outcome, scales = "free_y", ncol = 1)
	})
})

shinyApp(ui, server)