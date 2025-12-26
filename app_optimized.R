library(shiny)
library(ggplot2)
library(ggridges)
library(dplyr)
library(grid)
library(bslib)
library(tidyr)
library(leaflet)
library(plotly)
library(viridis)
library(lubridate)
library(readr)
library(ggstream)
library(scales)
library(shinycssloaders)
library(DT)
library(sf)

VACCINATION_STATUS_COLOR_MAP = c("Boosted"      = "#2196F3", 
                                 "Vaccinated"   = "#4CAF50", 
                                 "Unvaccinated" = "#e74c3c")

# Custom CSS for better styling
custom_css <- "
  .hero-section {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    padding: 80px 20px;
    text-align: center;
    border-radius: 10px;
    margin-bottom: 30px;
    box-shadow: 0 10px 30px rgba(0,0,0,0.2);
  }
  
  .hero-title {
    font-size: 48px;
    font-weight: bold;
    margin-bottom: 20px;
    text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
  }
  
  .hero-subtitle {
    font-size: 24px;
    margin-bottom: 30px;
    opacity: 0.95;
  }
  
  .stat-box {
    background: white;
    padding: 30px;
    border-radius: 10px;
    text-align: center;
    box-shadow: 0 4px 15px rgba(0,0,0,0.1);
    transition: transform 0.3s ease;
    margin: 10px;
  }
  
  .stat-box:hover {
    transform: translateY(-5px);
    box-shadow: 0 8px 25px rgba(0,0,0,0.15);
  }
  
  .stat-number {
    font-size: 48px;
    font-weight: bold;
    color: #667eea;
    margin-bottom: 10px;
  }
  
  .stat-label {
    font-size: 18px;
    color: #666;
    font-weight: 500;
  }
  
  .timeline-card {
    background: white;
    padding: 25px;
    border-radius: 10px;
    margin: 20px 0;
    border-left: 5px solid #667eea;
    box-shadow: 0 2px 10px rgba(0,0,0,0.08);
  }
  
  .chapter-header {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    padding: 40px 20px;
    border-radius: 10px;
    margin-bottom: 30px;
    text-align: center;
  }
  
  .info-card {
    background: #f8f9fa;
    padding: 20px;
    border-radius: 8px;
    margin: 15px 0;
    border-left: 4px solid #667eea;
  }
  
  .insight-box {
    background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
    color: white;
    padding: 20px;
    border-radius: 10px;
    margin: 20px 0;
    box-shadow: 0 4px 15px rgba(0,0,0,0.1);
  }
  
  .btn-explore {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    padding: 15px 40px;
    font-size: 18px;
    border: none;
    border-radius: 50px;
    cursor: pointer;
    transition: all 0.3s ease;
    box-shadow: 0 4px 15px rgba(102, 126, 234, 0.4);
  }
  
  .btn-explore:hover {
    transform: translateY(-2px);
    box-shadow: 0 6px 20px rgba(102, 126, 234, 0.6);
  }
  
  .tab-content {
    padding: 20px;
  }
"

# Function to create IFR timeseries plot (AI-generated visualization)
make_ifr_timeseries <- function(d_ifr, show_all = FALSE, use_log = TRUE) {
  dd <- if (show_all) d_ifr else filter(d_ifr, `Age.Group` != "All")
  y_var <- if (use_log) "IFR_pos" else "IFR"
  
  p <- ggplot(dd, aes(
    Date, .data[[y_var]],
    color = `Age.Group`,
    text = paste0(
      "Week End: ", format(Date, "%Y-%m-%d"),
      "<br>Age Group: ", `Age.Group`,
      "<br>Cases: ", format(Cases, big.mark = ","),
      "<br>Deaths: ", format(Deaths, big.mark = ","),
      "<br>IFR: ", ifelse(is.na(IFR), "NA", paste0(round(IFR, 3), "%"))
    )
  )) +
    geom_line(linewidth = 0.7, na.rm = TRUE) +
    geom_point(size = 1.1, alpha = 0.6, na.rm = TRUE) +
    scale_color_viridis_d(end = 0.95) +
    { if (use_log) scale_y_log10(labels = ~paste0(.x, "%"))
      else scale_y_continuous(labels = ~paste0(.x, "%")) } +
    labs(
      title = "Infection Fatality Rate Over Time by Age Group",
      subtitle = "AI-Generated Visualization: Weekly IFR calculated as deaths ÷ reported cases",
      x = "Week End",
      y = "IFR (%)",
      color = "Age Group"
    ) +
    theme_minimal(base_size = 12)
  
  ggplotly(p, tooltip = "text") %>%
    layout(legend = list(itemclick = "toggleothers"))
}

ui <- page_navbar(
  title = "COVID-19 in Chicago: A Data Story",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#667eea",
    secondary = "#764ba2"
  ),
  
  tags$head(
    tags$style(HTML(custom_css))
  ),
  
  # ============================================
  # LANDING PAGE / HOME
  # ============================================
  tabPanel(
    "Home",
    icon = icon("home"),
    
    fluidPage(
      # Hero Section
      div(class = "hero-section",
          div(class = "hero-title", "The COVID-19 Story"),
          div(class = "hero-subtitle", "How Vaccines Changed the Pandemic in Chicago"),
          br(),
          p(style = "font-size: 18px; max-width: 800px; margin: 0 auto;",
            "From devastating early days to vaccine breakthrough, explore how Chicago fought back 
          against COVID-19 through data, science, and community resilience."
          )
      ),
      
      # Key Statistics
      fluidRow(
        column(3,
               div(class = "stat-box",
                   div(class = "stat-number", "3+"),
                   div(class = "stat-label", "Years of Data")
               )
        ),
        column(3,
               div(class = "stat-box",
                   div(class = "stat-number", "59"),
                   div(class = "stat-label", "ZIP Codes Tracked")
               )
        ),
        column(3,
               div(class = "stat-box",
                   div(class = "stat-number", "8"),
                   div(class = "stat-label", "Age Groups Analyzed")
               )
        ),
        column(3,
               div(class = "stat-box",
                   div(class = "stat-number", "85%"),
                   div(class = "stat-label", "Risk Reduction")
               )
        )
      ),
      
      br(), br(),
      
      # The Story Timeline
      fluidRow(
        column(12,
               h2("The Journey", style = "text-align: center; color: #667eea; font-weight: bold; margin-bottom: 40px;"),
               
               fluidRow(
                 column(6,
                        div(class = "timeline-card",
                            h3("📊 Chapter 1: The Beginning", style = "color: #667eea;"),
                            p("A Complete Overview"),
                            p("Before vaccines existed, COVID-19 struck Chicago with devastating force. 
                  Explore how the pandemic affected different age groups and outcome types 
                  during those dark early days.")
                        )
                 ),
                 column(6,
                        div(class = "timeline-card",
                            h3("💉 Chapter 2: The Turning Point", style = "color: #667eea;"),
                            p("The Vaccines Roll Out"),
                            p("Vaccines arrived and everything changed. Watch the dramatic shift as 
                  Chicago's population went from unprotected to vaccinated, and see how 
                  this impacted case numbers even during the massive Omicron surge.")
                        )
                 )
               ),
               
               fluidRow(
                 column(6,
                        div(class = "timeline-card",
                            h3("🔬 Chapter 3: The Evidence", style = "color: #667eea;"),
                            p("Measuring Vaccine Effectiveness"),
                            p("Did vaccines actually work? The data says yes—definitively. Dive into 
                  vaccine effectiveness across age groups, see how protection varied and 
                  waned over time, and understand the real-world impact in concrete numbers.")
                        )
                 ),
                 column(6,
                        div(class = "timeline-card",
                            h3("🗺️ Chapter 4: The Disparities", style = "color: #667eea;"),
                            p("Geographic Patterns Across Chicago"),
                            p("Not all neighborhoods had the same story. Explore dramatic differences 
                  in vaccination rates and COVID outcomes across Chicago's ZIP codes—revealing 
                  important equity challenges we still face today.")
                        )
                 )
               ),
               
               div(class = "timeline-card", style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white;",
                   h3("🔍 Chapter 5: Explore the Data", style = "color: white;"),
                   p("Browse the Raw Datasets"),
                   p("View the actual data tables behind all visualizations. Search, sort, and explore 
              the processed data that powers this entire dashboard.")
               )
        )
      ),
      
      br(), br(),
      
      # Call to Action
      fluidRow(
        column(12, align = "center",
               h2("Ready to Begin?", style = "color: #667eea; font-weight: bold;"),
               p(style = "font-size: 18px; color: #666; margin-bottom: 30px;",
                 "Start your journey through the data by clicking below"
               ),
               actionButton(
                 "start_journey",
                 "Begin the Story →",
                 class = "btn-explore",
                 onclick = "document.querySelector('a[data-value=\"The Beginning\"]').click();"
               )
        )
      ),
      
      br(), br(),
      
      # Quick Stats
      fluidRow(
        column(12,
               h3("Why This Matters", style = "text-align: center; color: #667eea; margin-bottom: 30px;"),
               
               fluidRow(
                 column(4,
                        div(style = "text-align: center; padding: 20px;",
                            icon("shield-virus", style = "font-size: 48px; color: #667eea;"),
                            h4("Protection That Worked"),
                            p("Vaccines reduced hospitalizations by up to 85%, saving countless lives and 
                  preventing overwhelming healthcare systems.")
                        )
                 ),
                 column(4,
                        div(style = "text-align: center; padding: 20px;",
                            icon("people-group", style = "font-size: 48px; color: #667eea;"),
                            h4("Not Everyone Equal"),
                            p("Geographic disparities revealed equity challenges, with some neighborhoods 
                  facing double the case rates due to lower vaccination access.")
                        )
                 ),
                 column(4,
                        div(style = "text-align: center; padding: 20px;",
                            icon("chart-line", style = "font-size: 48px; color: #667eea;"),
                            h4("Data Tells Stories"),
                            p("Behind every number is a human story. This dashboard honors those who 
                  suffered and celebrates the science that helped us fight back.")
                        )
                 )
               )
        )
      )
    )
  ),
  
  # ============================================
  # CHAPTER 1: THE BEGINNING
  # ============================================
  tabPanel(
    "The Beginning",
    icon = icon("virus"),
    
    div(class = "tab-content",
        div(class = "chapter-header",
            h1("Chapter 1: The Beginning"),
            h3("How COVID-19 Hit Chicago"),
            p(style = "font-size: 16px; margin-top: 15px; opacity: 0.9;",
              "2021-2023 | The Complete Story"
            ),
            br(),
            actionButton("goto_turning", "Next: The Turning Point →", 
                         class = "btn-explore",
                         style = "margin-top: 10px;",
                         onclick = "document.querySelector('a[data-value=\"The Turning Point\"]').click();")
        ),
        
        fluidRow(
          column(12,
                 div(class = "info-card",
                     p(style = "font-size: 16px; margin: 0;",
                       "This visualization shows COVID-19 outcomes in Chicago from the beginning of the 
              pandemic through the vaccine era. Watch how the story changes: in the early months, 
              all outcomes were unvaccinated (below the line). As vaccines rolled out, the balance 
              shifted. This is the complete picture of Chicago's COVID-19 journey."
                     )
                 )
          )
        ),
        
        br(),
        
        sidebarLayout(
          sidebarPanel(
            style = "background-color: #f8f9fa; border-radius: 10px; padding: 20px;",
            
            h4("Explore the Early Pandemic", style = "color: #667eea;"),
            
            dateRangeInput(
              "beginning_range",
              label = "Date Range",
              start = "2021-01-01",
              end = "2023-06-30"
            ),
            
            uiOutput("BeginningAnimationControls"),
            p("Use the animation slider to change the current date of visualization."),
            
            hr(),
            
            input_switch("beginning_dateLine", "Enable Date Line"),
            p("The date line helps identify the current animation date on the chart."),
            
            input_switch("beginning_annotations", "Display Annotations"),
            p("Shows key events like the Omicron variant peak."),
            
            hr(),
            
            div(class = "info-card", style = "background: white;",
                h5("📖 Reading This Chart:", style = "color: #667eea;"),
                tags$ul(
                  tags$li("Each bar represents one week of data"),
                  tags$li("Different colors show different age groups"),
                  tags$li("Above y=0: Vaccinated outcomes"),
                  tags$li("Below y=0: Unvaccinated outcomes"),
                  tags$li("Notice how the balance shifts over time")
                )
            )
          ),
          
          mainPanel(
            plotOutput("beginningOutcomes", height = "700px"),
            
            br(),
            
            div(class = "insight-box",
                h4("📊 What This Shows", style = "margin-top: 0;"),
                p("This shows the complete timeline of COVID-19 outcomes in Chicago..."),
                p(style = "margin-bottom: 0;",
                  strong("The shift in this balance tells the story of vaccines' impact."))
            ),
            
            hr(),
            
            h3("Understanding the Impact: Infection Fatality Rate", style = "color: #667eea;"),
            p("This AI-generated visualization shows how deadly COVID-19 was across different age groups over time. 
            The Infection Fatality Rate (IFR) represents the percentage of confirmed cases that resulted in death."),
            
            fluidRow(
              column(3,
                     checkboxInput("ifr_include_all", "Include 'All Ages' Group", value = FALSE)
              ),
              column(3,
                     checkboxInput("ifr_log_scale", "Use Log Scale", value = TRUE)
              ),
              column(6,
                     div(class = "info-card", style = "background: white; padding: 10px;",
                         p(style = "margin: 0; font-size: 12px;",
                           strong("Note:"), "Only weeks with 25+ cases are included in the calculation.")
                     )
              )
            ),
            
            plotlyOutput("ifr_timeseries", height = "500px") |> withSpinner(),
            
            br(),
            
            div(class = "info-card", style = "background: #fff3e0;",
                h5("📊 Reading This Chart:", style = "color: #667eea;"),
                tags$ul(
                  tags$li("Each line represents an age group"),
                  tags$li("Higher percentages mean more deaths per confirmed case"),
                  tags$li("Older age groups show dramatically higher IFR throughout the pandemic"),
                  tags$li("The log scale helps compare groups with very different rates"),
                  tags$li("Click legend items to show/hide specific age groups"),
                  tags$li("This graph was created by providing chat GPT with the main dataset used for this project and
                          then prompting it with: Based on this dataset, I need you to make some A.I. Graphes in R. The idea that I want you to explore is 
                          : 'Infection Fatality Rate by Age Group (IFR curves)'. I need two graphs. I want you to pick the graph types that you think 
                          provides the best way to visualize the data in an informative and pleasing way. I need you to generate all the code for these graphs.
                          It would be a very cool addition, if the graphs were animated or interactable in some way. It is all going to be incorporated in a 
                          dashboard that is going to be published using shinyapp.")
                )
            )
          )
        )
    )
  ),
  
  # ============================================
  # CHAPTER 2: THE TURNING POINT
  # ============================================
  tabPanel(
    "The Turning Point",
    icon = icon("syringe"),
    
    div(class = "tab-content",
        div(class = "chapter-header",
            h1("Chapter 2: The Turning Point"),
            h3("When Vaccines Arrived"),
            p(style = "font-size: 16px; margin-top: 15px; opacity: 0.9;",
              "December 2020 - June 2023 | The Vaccine Era Begins"
            ),
            br(),
            div(style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
                actionButton("goto_beginning", "← Previous: The Beginning", 
                             class = "btn-explore",
                             onclick = "document.querySelector('a[data-value=\"The Beginning\"]').click();"),
                actionButton("goto_evidence", "Next: The Evidence →", 
                             class = "btn-explore",
                             onclick = "document.querySelector('a[data-value=\"The Evidence\"]').click();")
            )
        ),
        
        fluidRow(
          column(12,
                 div(class = "info-card",
                     p(style = "font-size: 16px; margin: 0;",
                       "In December 2020, vaccines arrived in Chicago. Watch how vaccination rates climbed 
              while case patterns began to shift. The Omicron variant in January 2022 tested 
              vaccines' effectiveness during the largest surge—but the story had fundamentally 
              changed from the dark days of 2020."
                     )
                 )
          )
        ),
        
        br(),
        
        sidebarLayout(
          sidebarPanel(
            style = "background-color: #f8f9fa; border-radius: 10px; padding: 20px;",
            
            h4("Timeline Controls", style = "color: #667eea;"),
            
            dateRangeInput(
              "turning_date_range",
              label = "Date Range",
              start = "2020-12-01",
              end = "2023-06-30"
            ),
            
            hr(),
            
            input_switch("turning_annotations", "Display Annotations"),
            p("Shows the Omicron variant peak on both visualizations."),
            
            hr(),
            
            div(class = "info-card", style = "background: white;",
                h5("🔍 What to Look For:", style = "color: #667eea;"),
                tags$ul(
                  tags$li(strong("Blue area:"), "Vaccination coverage growing"),
                  tags$li(strong("Red line:"), "COVID-19 case counts"),
                  tags$li(strong("The intersection:"), "How they relate"),
                  tags$li(strong("Omicron peak:"), "Largest wave, different outcome")
                )
            )
          ),
          
          mainPanel(
            h3("COVID-19 Cases vs. Vaccination Rate Over Time"),
            p("This interactive visualization shows the relationship between vaccination rollout 
            and COVID-19 case numbers. Hover over the chart to see exact values."),
            plotlyOutput("turningRolloutPlot", height = "450px") |> withSpinner(),
            
            br(), br(),
            
            h3("The Population Shift: Unvaccinated → Vaccinated"),
            p("Watch Chicago's population transition from unvaccinated (red) to vaccinated (green) 
            and boosted (blue) over time."),
            plotOutput("turningPopulation", height = "400px"),
            
            br(),
            
            div(class = "insight-box",
                h4("🔄 The Transformation", style = "margin-top: 0;"),
                p("Notice the crossover point: by mid-2021, more Chicagoans were vaccinated than 
              unvaccinated. Even when Omicron caused a massive spike in cases (the highest we'd 
              seen), the outcomes for vaccinated individuals were dramatically different."),
                p(style = "margin-bottom: 0;",
                  strong("The next chapter shows exactly HOW different."))
            )
          )
        )
    )
  ),
  
  # ============================================
  # CHAPTER 3: THE EVIDENCE
  # ============================================
  tabPanel(
    "The Evidence",
    icon = icon("microscope"),
    
    div(class = "tab-content",
        div(class = "chapter-header",
            h1("Chapter 3: The Evidence"),
            h3("Measuring How Well Vaccines Worked"),
            p(style = "font-size: 16px; margin-top: 15px; opacity: 0.9;",
              "Vaccine Effectiveness Analysis Across Age Groups and Time"
            ),
            br(),
            div(style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
                actionButton("goto_turning2", "← Previous: The Turning Point", 
                             class = "btn-explore",
                             onclick = "document.querySelector('a[data-value=\"The Turning Point\"]').click();"),
                actionButton("goto_disparities", "Next: The Disparities →", 
                             class = "btn-explore",
                             onclick = "document.querySelector('a[data-value=\"The Disparities\"]').click();")
            )
        ),
        
        fluidRow(
          column(12,
                 div(class = "info-card",
                     p(style = "font-size: 16px; margin: 0;",
                       "Did vaccines actually work? The answer is definitively yes. Vaccine Effectiveness 
              (VE) measures how much vaccines reduce risk compared to being unvaccinated. These 
              visualizations show VE across age groups and over time, revealing both remarkable 
              protection and important limitations like vaccine waning."
                     )
                 )
          )
        ),
        
        br(),
        
        sidebarLayout(
          sidebarPanel(
            style = "background-color: #f8f9fa; border-radius: 10px; padding: 20px;",
            
            h4("Analysis Controls", style = "color: #667eea;"),
            
            dateRangeInput(
              "ve_date_range",
              "Date Range",
              start = "2021-01-01",
              end = "2023-06-30"
            ),
            
            br(),
            
            input_switch("ve_omicron_line", "Show Omicron Peak Line"),
            
            hr(),
            
            checkboxGroupInput(
              "ve_age_groups",
              "Age Groups (for Spaghetti Plot)",
              choices = c("All ages (combined)", "0-4", "5-11", "12-17", 
                          "18-29", "30-49", "50-64", "65-79", "80+"),
              selected = c("All ages (combined)", "0-4", "5-11", "12-17", 
                           "18-29", "30-49", "50-64", "65-79", "80+")
            ),
            
            hr(),
            
            div(class = "info-card", style = "background: white;",
                h5("📊 Understanding VE:", style = "color: #667eea;"),
                tags$ul(
                  tags$li(strong("100% VE:"), "Perfect protection"),
                  tags$li(strong("80% VE:"), "80% reduction in risk vs. unvaccinated"),
                  tags$li(strong("50% VE:"), "Half the risk of unvaccinated"),
                  tags$li(strong("0% VE:"), "No protection")
                ),
                p(style = "font-size: 12px; margin-top: 10px; margin-bottom: 0;",
                  em("Note: Graphs may take a moment to load. Changes to date range will reload visualizations."))
            )
          ),
          
          mainPanel(
            tabsetPanel(
              id = "ve_viz_tabs",
              type = "tabs",
              
              # Tab 1: Spaghetti Plot
              tabPanel(
                "By Age Group",
                br(),
                h4("Vaccine Effectiveness by Age Group"),
                p("Each line represents an age group. Gray lines show all groups at once, 
                colored lines in each panel highlight that specific group."),
                plotlyOutput("ve_spaghetti", height = "800px") |> withSpinner()
              ),
              
              # Tab 2: Stream Plot
              # tabPanel(
              #   "Flow Over Time",
              #   br(),
              #   h4("Vaccine Effectiveness Flow Over Time"),
              #   p(strong("Note:"), "This streamgraph shows relative VE contribution by age group. 
              #   The y-axis represents relative flow, not absolute percentages."),
              #   plotlyOutput("ve_stream", height = "600px") |> withSpinner()
              # ),
              
              ## For static streamgraph
              tabPanel(
                "Flow Over Time",
                br(),
                h4("Vaccine Effectiveness Flow Over Time"),
                p(strong("Note:"), "This streamgraph shows relative VE contribution by age group. 
                The y-axis represents relative flow, not absolute percentages."),
                plotOutput("ve_stream", height = "600px") |> withSpinner()  # Changed from plotlyOutput
              ),
              
              # Tab 3: Risk Reduction
              tabPanel(
                "Risk Reduction",
                br(),
                h4("The Bottom Line: Risk Reduction"),
                p("These concrete numbers show how much vaccines reduced rates of cases, 
                hospitalizations, and deaths compared to unvaccinated individuals."),
                plotlyOutput("ve_riskReduction", height = "450px") |> withSpinner()
              ),
              
              # Tab 4: Area Chart
              tabPanel(
                "Age Group Explorer",
                br(),
                h4("Interactive Age Group Explorer"),
                p("This area chart lets you focus on specific age groups using the legend."),
                plotlyOutput("ve_area", height = "600px") |> withSpinner()
              )
            ),
            
            br(),
            
            div(class = "insight-box",
                h4("✅ Key Findings", style = "margin-top: 0;"),
                tags$ul(
                  tags$li("Vaccines consistently reduced risk across ALL age groups"),
                  tags$li("Effectiveness varied by age: highest in young adults (90%+), 
                      lower in elderly (60-80%)"),
                  tags$li("VE decreased over time, showing vaccine waning—evidence for boosters"),
                  tags$li("Even with waning, vaccines still provided significant protection"),
                  tags$li("Hospitalizations and deaths were reduced MORE than cases—vaccines 
                      prevented severe outcomes even when breakthrough infections occurred")
                )
            )
          )
        )
    )
  ),
  
  # ============================================
  # CHAPTER 4: THE DISPARITIES
  # ============================================
  tabPanel(
    "The Disparities",
    icon = icon("map-marked-alt"),
    
    div(class = "tab-content",
        div(class = "chapter-header",
            h1("Chapter 4: The Disparities"),
            h3("Different Communities, Different Stories"),
            p(style = "font-size: 16px; margin-top: 15px; opacity: 0.9;",
              "Geographic Patterns Across Chicago's 59 ZIP Codes"
            ),
            br(),
            div(style = "display: flex; gap: 10px; justify-content: center; margin-top: 10px;",
                actionButton("goto_evidence2", "← Previous: The Evidence", 
                             class = "btn-explore",
                             onclick = "document.querySelector('a[data-value=\"The Evidence\"]').click();"),
                actionButton("goto_explore", "Next: Explore Data →", 
                             class = "btn-explore",
                             onclick = "document.querySelector('a[data-value=\"Explore Data\"]').click();")
            )
        ),
        
        fluidRow(
          column(12,
                 div(class = "info-card",
                     p(style = "font-size: 16px; margin: 0;",
                       "Vaccines didn't reach everyone equally. These maps reveal dramatic differences 
              in vaccination rates and COVID-19 outcomes across Chicago neighborhoods. Toggle 
              between views to see relationships. Click any ZIP code to see its unique story 
              over time. These patterns reveal important equity issues we must address."
                     )
                 )
          )
        ),
        
        br(),
        
        sidebarLayout(
          sidebarPanel(
            style = "background-color: #f8f9fa; border-radius: 10px; padding: 20px;",
            
            h4("Map Controls", style = "color: #667eea;"),
            
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
            p("Data is reported weekly. Use the slider to watch patterns change over time."),
            
            hr(),
            
            uiOutput("metric_selector"),
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
            
            div(class = "info-card", style = "background: white;",
                h5("🗺️ How to Use:", style = "color: #667eea;"),
                tags$ul(
                  tags$li("Toggle between vaccination and COVID data"),
                  tags$li("Move the time slider to see changes"),
                  tags$li("Hover over ZIP codes for details"),
                  tags$li("Click a ZIP code to see its timeline"),
                  tags$li("Try the animation to watch patterns unfold")
                )
            )
          ),
          
          mainPanel(
            uiOutput("map_title"),
            
            leafletOutput("chicagoZipMap", height = "600px") |> withSpinner(),
            
            br(),
            
            fluidRow(
              column(4,
                     wellPanel(
                       style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border: none;",
                       h5("📅 Date", style = "color: white;"),
                       textOutput("map_stats_date")
                     )
              ),
              column(4,
                     wellPanel(
                       style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); color: white; border: none;",
                       h5("🏙️ City Total", style = "color: white;"),
                       textOutput("map_stats_total")
                     )
              ),
              column(4,
                     wellPanel(
                       style = "background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); color: white; border: none;",
                       h5("📊 Average per ZIP", style = "color: white;"),
                       textOutput("map_stats_avg")
                     )
              )
            ),
            
            hr(),
            
            h4("Selected ZIP Code Timeline"),
            p("Click on any ZIP code in the map above to see its progression over time."),
            plotOutput("selectedZipTimeSeries", height = "350px"),
            
            br(),
            
            div(class = "insight-box",
                h4("⚖️ Why Geographic Disparities Matter", style = "margin-top: 0;"),
                p("These maps don't just show data—they reveal inequality. Communities with lower 
              vaccination rates often faced higher COVID-19 impacts. South and West Side 
              neighborhoods frequently showed both lower vaccination coverage AND worse outcomes."),
                p("Understanding these patterns is crucial for ensuring ALL communities have equal 
              access to public health protection in future crises."),
                p(style = "margin-bottom: 0;",
                  strong("Equity isn't just about fairness—it's about saving lives."))
            )
          )
        )
    )
  ),
  
  # ============================================
  # CHAPTER 5: EXPLORE RAW DATA
  # ============================================
  tabPanel(
    "Explore Data",
    icon = icon("table"),
    
    div(class = "tab-content",
        div(class = "chapter-header",
            h1("Chapter 5: Explore the Raw Data"),
            h3("Browse the Complete Datasets"),
            p(style = "font-size: 16px; margin-top: 15px; opacity: 0.9;",
              "View the Processed Data Tables Directly"
            ),
            br(),
            actionButton("goto_disparities2", "← Previous: The Disparities", 
                         class = "btn-explore",
                         style = "margin-top: 10px;",
                         onclick = "document.querySelector('a[data-value=\"The Disparities\"]').click();")
        ),
        
        fluidRow(
          column(12,
                 div(class = "info-card",
                     p(style = "font-size: 16px; margin: 0;",
                       "This section allows you to browse the actual data tables that power all the visualizations 
              in this dashboard. These are the processed CSV files after data preparation. Use the tabs 
              below to switch between different datasets, search for specific values, and explore the 
              raw numbers behind the story."),
                     p("All tables are searchable and sortable. Click column headers to sort. Use the search 
          box to find specific values. You can also adjust how many rows are displayed per page."),
                 )
          )
        ),
        
        tabsetPanel(
          id = "data_tables",
          
          # Chicago Outcomes Table
          tabPanel(
            "Outcomes by Age & Vaccination Status",
            br(),
            div(class = "info-card",
                h4("📊 Dataset: Chicago Outcomes", style = "color: #667eea; margin-top: 0;"),
                p("This table shows weekly outcomes (Cases, Hospitalizations, Deaths) broken down by 
              age group and vaccination status. Each row represents one week of data for one age 
              group and one outcome type."),
                tags$ul(
                  tags$li(strong("Outcome.Vaccinated:"), "Count for vaccinated individuals"),
                  tags$li(strong("Outcome.Unvaccinated:"), "Count for unvaccinated individuals"),
                  tags$li(strong("Outcome.Boosted:"), "Count for boosted individuals (if available)")
                )
            ),
            br(),
            DT::dataTableOutput("table_outcomes")
          ),
          
          # Chicago Population Table
          tabPanel(
            "Population by Vaccination Status",
            br(),
            div(class = "info-card",
                h4("👥 Dataset: Chicago Population", style = "color: #667eea; margin-top: 0;"),
                p("This table shows the estimated population of Chicago over time, broken down by 
              vaccination status. Watch how the population shifted from unvaccinated to vaccinated 
              and boosted."),
                tags$ul(
                  tags$li(strong("Population.Vaccinated:"), "Estimated vaccinated population"),
                  tags$li(strong("Population.Unvaccinated:"), "Estimated unvaccinated population"),
                  tags$li(strong("Population.Boosted:"), "Estimated boosted population")
                )
            ),
            br(),
            DT::dataTableOutput("table_population")
          ),
          
          # ZIP Code Vaccination Table
          tabPanel(
            "ZIP Code Vaccination Data",
            br(),
            div(class = "info-card",
                h4("🗺️ Dataset: ZIP Code Vaccinations", style = "color: #667eea; margin-top: 0;"),
                p("Weekly vaccination metrics for each of Chicago's 59 ZIP codes. Shows vaccination 
              rates and total doses administered."),
                tags$ul(
                  tags$li(strong("Vaccinated_1st_Dose:"), "% with at least 1 dose"),
                  tags$li(strong("Fully_Vaccinated:"), "% who completed series"),
                  tags$li(strong("Boosted:"), "% who received booster")
                )
            ),
            br(),
            DT::dataTableOutput("table_zip_vaccination")
          ),
          
          # ZIP Code Progression Table
          tabPanel(
            "ZIP Code COVID Progression",
            br(),
            div(class = "info-card",
                h4("📈 Dataset: ZIP Code COVID Progression", style = "color: #667eea; margin-top: 0;"),
                p("Weekly COVID-19 outcomes for each ZIP code including cases, deaths, tests, and rates."),
                tags$ul(
                  tags$li(strong("Cases_Weekly:"), "New cases that week"),
                  tags$li(strong("Deaths_Weekly:"), "Deaths that week"),
                  tags$li(strong("Case_Rate_Weekly:"), "Cases per 100k residents")
                )
            ),
            br(),
            DT::dataTableOutput("table_zip_progression")
          )
        ),
        
        br(), br(),
        
    )
  ),
  
  # ============================================
  # ABOUT & REFERENCES
  # ============================================
  nav_spacer(),
  nav_menu(
    "More",
    icon = icon("ellipsis"),
    
    tabPanel(
      "About",
      icon = icon("info-circle"),
      
      fluidPage(
        fluidRow(
          column(12,
                 div(class = "chapter-header",
                     h1("About This Project")
                 ),
                 
                 br(),
                 
                 div(class = "info-card",
                     h3("Our Mission", style = "color: #667eea;"),
                     p("This interactive dashboard was created to tell the story of COVID-19 vaccines 
                in Chicago—from the devastating early days of the pandemic through vaccine 
                rollout and its measurable impact on our community. Our goal is to make complex 
                public health data accessible, engaging, and meaningful for everyone.")
                 ),
                 
                 br(),
                 
                 div(class = "info-card",
                     h3("The Team", style = "color: #667eea;"),
                     p("This project was created by [Your Team Names] as part of [Course/Project Name]."),
                     p("Each team member contributed unique visualizations and analysis:"),
                     tags$ul(
                       tags$li(strong("Mathias:"), "Early pandemic outcomes and population shifts"),
                       tags$li(strong("Michelle:"), "Vaccine rollout timeline and ZIP code geographic analysis"),
                       tags$li(strong("Jonas:"), "Vaccine effectiveness analysis across age groups")
                     )
                 ),
                 
                 br(),
                 
                 div(class = "info-card",
                     h3("Data Sources", style = "color: #667eea;"),
                     p("All data comes from official public health sources:"),
                     tags$ul(
                       tags$li(a("CDC COVID-19 Outcomes by Vaccination Status",
                                 href = "https://healthdata.gov/dataset/COVID-19-Outcomes-by-Vaccination-Status-Historical/fmz3-7y63/about_data",
                                 target = "_blank")),
                       tags$li(a("Chicago COVID-19 Daily Cases and Deaths",
                                 href = "https://data.cityofchicago.org/Health-Human-Services/COVID-19-Daily-Cases-Deaths-and-Hospitalizations-H/naz8-j4nc/about_data",
                                 target = "_blank")),
                       tags$li(a("Chicago COVID-19 Vaccinations by ZIP Code",
                                 href = "https://data.cityofchicago.org/Health-Human-Services/COVID-19-Vaccinations-by-ZIP-Code-Historical/553k-3xzc/about_data",
                                 target = "_blank")),
                       tags$li(a("Chicago COVID-19 Progression by ZIP Code",
                                 href = "https://data.cityofchicago.org/Health-Human-Services/COVID-19-Progression-by-ZIP-Code-Historical/vrgd-sgft",
                                 target = "_blank"))
                     )
                 ),
                 
                 br(),
                 
                 div(class = "info-card",
                     h3("Methodology", style = "color: #667eea;"),
                     p(strong("Vaccine Effectiveness (VE) Calculation:")),
                     p("VE = (1 - (Vaccinated Rate / Unvaccinated Rate)) × 100"),
                     p("This standard formula, used by the CDC and WHO, shows the percentage reduction 
                in risk for vaccinated individuals compared to unvaccinated individuals."),
                     br(),
                     p(strong("Data Processing:")),
                     tags$ul(
                       tags$li("Vaccination percentages capped at 100% to correct data quality issues"),
                       tags$li("ZIP code boundaries from Chicago Data Portal (2024)"),
                       tags$li("Weekly reporting for geographic data, daily for citywide data"),
                       tags$li("Age groups standardized across all datasets")
                     )
                 ),
                 
                 br(),
                 
                 div(class = "info-card",
                     h3("Technologies Used", style = "color: #667eea;"),
                     tags$ul(
                       tags$li(strong("R"), "for data processing and analysis"),
                       tags$li(strong("Shiny"), "for interactive dashboard framework"),
                       tags$li(strong("ggplot2 & Plotly"), "for data visualizations"),
                       tags$li(strong("Leaflet"), "for geographic mapping"),
                       tags$li(strong("sf"), "for spatial data processing"),
                       tags$li(strong("ggstream"), "for streamgraph visualizations"),
                       tags$li(strong("DT"), "for interactive data tables")
                     )
                 )
          )
        )
      )
    ),
    
    tabPanel(
      "References",
      icon = icon("book"),
      
      fluidPage(
        fluidRow(
          column(12,
                 div(class = "chapter-header",
                     h1("References & Further Reading")
                 ),
                 
                 br(),
                 
                 div(class = "info-card",
                     h3("Key References", style = "color: #667eea;"),
                     tags$ol(
                       tags$li(a("CDC: COVID-19 Timeline (July 8, 2024)",
                                 href = "https://www.cdc.gov/museum/timeline/covid19.html",
                                 target = "_blank")),
                       tags$li(a("CDC: Understanding How Vaccines Work",
                                 href = "https://www.cdc.gov/vaccines/hcp/conversations/understanding-vacc-work.html",
                                 target = "_blank")),
                       tags$li(a("WHO: Vaccine Efficacy, Effectiveness and Protection",
                                 href = "https://www.who.int/news-room/feature-stories/detail/vaccine-efficacy-effectiveness-and-protection",
                                 target = "_blank")),
                       tags$li(a("Chicago Department of Public Health: COVID-19 Dashboard",
                                 href = "https://www.chicago.gov/city/en/sites/covid-19/home.html",
                                 target = "_blank"))
                     )
                 ),
                 
                 br(),
                 
                 div(class = "info-card",
                     h3("Data Portals", style = "color: #667eea;"),
                     tags$ul(
                       tags$li(a("Chicago Data Portal",
                                 href = "https://data.cityofchicago.org/",
                                 target = "_blank")),
                       tags$li(a("HealthData.gov COVID-19 Resources",
                                 href = "https://healthdata.gov/browse?tags=covid-19",
                                 target = "_blank"))
                     )
                 ),
                 
                 br(),
                 
                 div(class = "info-card",
                     h3("Academic Resources", style = "color: #667eea;"),
                     p("For those interested in deeper technical details about vaccine effectiveness 
                measurement and epidemiological methods, we recommend:"),
                     tags$ul(
                       tags$li("CDC's MMWR (Morbidity and Mortality Weekly Report) publications on COVID-19"),
                       tags$li("The New England Journal of Medicine COVID-19 vaccine studies"),
                       tags$li("The Lancet's COVID-19 Resource Centre")
                     )
                 )
          )
        )
      )
    )
  )
)

# ============================================
# SERVER
# ============================================
server <- shinyServer(function(input, output, session) {
  
  Chicago <- list()
  
  # === LOAD DATA ===
  # Load pre-computed data to improve performance
  Chicago$Outcomes <- read.csv("data/chicago_outcomes.csv", stringsAsFactors = FALSE) |>
    mutate(Date = as.Date(Date)) |>
    mutate(Age.Group = factor(Age.Group, levels = c("0-4", "5-11", "12-17", "18-29", "30-49", "50-64", "65-79", "80+"), ordered = TRUE))
  
  Chicago$Population <- read.csv("data/chicago_population.csv", stringsAsFactors = FALSE) |>
    mutate(Date = as.Date(Date))
  
  # Load pre-computed VE data instead of raw data
  Chicago$VE_Precomputed <- tryCatch({
    read_csv("data/ve_precomputed.csv", show_col_types = FALSE) %>%
      mutate(Date = as.Date(Date))
  }, error = function(e) {
    # Fallback to raw data if precomputed doesn't exist
    readr::read_csv("datasets/chicago.csv", show_col_types = FALSE) %>%
      mutate(week_end = mdy(`Week End`))
  })
  
  Chicago$VE_Stream <- tryCatch({
    read_csv("data/ve_stream_precomputed.csv", show_col_types = FALSE) %>%
      mutate(Date = as.Date(Date))
  }, error = function(e) { NULL })
  
  Chicago$ZipVaccination <- tryCatch({
    read.csv("data/chicago_zip_vaccination.csv", stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date), ZIP_Code = as.character(ZIP_Code))
  }, error = function(e) { NULL })
  
  Chicago$ZipProgression <- tryCatch({
    read.csv("data/chicago_zip_progression.csv", stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date), ZIP_Code = as.character(ZIP_Code))
  }, error = function(e) { NULL })
  
  Chicago$IFR <- tryCatch({
    read.csv("data/chicago_ifr.csv", stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date))
  }, error = function(e) { NULL })
  
  # Load simplified boundaries
  Chicago$Boundaries <- tryCatch({
    boundaries <- st_read("geographic/chicago_zip_boundaries.geojson", quiet = TRUE)
    boundaries$ZIP_Code <- as.character(boundaries$ZIP_Code)
    boundaries
  }, error = function(e) { NULL })
  
  selected_zip <- reactiveVal(NULL)
  
  # === ANIMATION SLIDER FOR THE BEGINNING TAB ===
  output$BeginningAnimationControls <- renderUI({
    sliderInput(
      inputId = "beginning_date",
      label = "Animation Progression",
      min = input$beginning_range[1],
      max = input$beginning_range[2],
      value = input$beginning_range[1],
      timeFormat = "%Y-%m-%d",
      animate = animationOptions(interval = 30)
    )
  })
  
  # === TAB 1: THE BEGINNING ===
  output$beginningOutcomes <- renderPlot({
    Date.Start <- as.Date(input$beginning_range[1])
    Date.End <- as.Date(input$beginning_range[2])
    
    data <- Chicago$Outcomes |> filter(Date >= Date.Start & Date <= Date.End)
    
    ggplot(data, aes(x=Date, fill=Age.Group)) +
      geom_bar(aes(y=Outcome.Vaccinated), stat="identity", alpha=0.7) +
      geom_bar(aes(y=-Outcome.Unvaccinated), stat="identity", alpha=0.7) +
      geom_hline(yintercept = 0, color = "#222222", linewidth = 0.2) +
      {if(input$beginning_dateLine) geom_vline(xintercept = as.Date(input$beginning_date), 
                                               linetype="dashed", color="red", size=0.5) }+
      {if(input$beginning_dateLine) geom_text(aes(x=as.Date(input$beginning_date), y=0, 
                                                  label=format(as.Date(input$beginning_date), "%Y-%m-%d")), vjust=-1, color="red") }+
      {if(input$beginning_annotations) geom_vline(xintercept = as.Date("2022-01-01"), linetype="dashed", color="red", size=0.5) }+
      {if(input$beginning_annotations) geom_text(aes(x=as.Date("2022-01-01"), y=0, label="Omicron peak"), vjust=-1, color="red") }+
      theme_minimal() +
      labs(y = "Number of Outcomes (Unvaccinated vs Vaccinated)", x="") +
      facet_wrap(~Outcome, scales = "free_y", ncol = 1)
  })
  
  # AI-generated IFR visualization
  output$ifr_timeseries <- renderPlotly({
    if (is.null(Chicago$IFR)) {
      # Return empty plot with message if data not available
      plotly::plot_ly() %>%
        plotly::add_annotations(
          text = "IFR data not available. Please run prepare_ifr_data.R",
          showarrow = FALSE,
          font = list(size = 16, color = "gray")
        )
    } else {
      # Filter by date range from the tab
      ifr_data <- Chicago$IFR %>%
        filter(Date >= as.Date(input$beginning_range[1]),
               Date <= as.Date(input$beginning_range[2]))
      
      make_ifr_timeseries(ifr_data, 
                          show_all = input$ifr_include_all,
                          use_log = input$ifr_log_scale)
    }
  })
  
  # === TAB 2: THE TURNING POINT ===
  output$turningRolloutPlot <- renderPlotly({
    data <- Chicago$Outcomes |>
      filter(Date >= as.Date(input$turning_date_range[1]) & 
               Date <= as.Date(input$turning_date_range[2])) |>
      group_by(Date, Outcome) |>
      summarise(Total_Cases = sum(Outcome.Vaccinated, na.rm = TRUE) + 
                  sum(Outcome.Unvaccinated, na.rm = TRUE), .groups = "drop")
    
    pop_data <- Chicago$Population |>
      filter(Date >= as.Date(input$turning_date_range[1]) & 
               Date <= as.Date(input$turning_date_range[2])) |>
      mutate(Vaccination_Rate = (Population.Vaccinated / 
                                   (Population.Vaccinated + Population.Unvaccinated)) * 100)
    
    cases_data <- data |> filter(Outcome == "Cases")
    peak_date <- cases_data$Date[which.max(cases_data$Total_Cases)]
    peak_value <- max(cases_data$Total_Cases, na.rm = TRUE)
    
    p <- plot_ly() %>%
      add_trace(data = cases_data, x = ~Date, y = ~Total_Cases, type = 'scatter',
                mode = 'lines', line = list(color = '#e74c3c', width = 2),
                name = 'Total Cases',
                text = ~paste0("Date: ", format(Date, "%B %d, %Y"), 
                               "<br>Total Cases: ", format(Total_Cases, big.mark = ",")),
                hovertemplate = '%{text}<extra></extra>') %>%
      add_trace(data = pop_data, x = ~Date, y = ~Vaccination_Rate, type = 'scatter',
                mode = 'none', fill = 'tozeroy', fillcolor = 'rgba(52, 152, 219, 0.3)',
                name = 'Vaccination Rate (%)',
                text = ~paste0("Date: ", format(Date, "%B %d, %Y"), 
                               "<br>Vaccination Rate: ", round(Vaccination_Rate, 1), "%"),
                hovertemplate = '%{text}<extra></extra>', yaxis = 'y2')
    
    # Add annotation only if switch is enabled
    if (input$turning_annotations) {
      p <- p %>%
        add_segments(x = peak_date, xend = peak_date, y = 0, yend = peak_value,
                     line = list(color = '#c0392b', width = 2, dash = 'dash'),
                     showlegend = FALSE, hoverinfo = 'skip') %>%
        add_annotations(x = peak_date, y = peak_value * 1.05,
                        text = paste0("<b>Peak: Omicron Variant Surge</b><br>",
                                      format(peak_date, "%B %d, %Y")),
                        xref = "x", yref = "y", showarrow = TRUE,
                        arrowhead = 2, arrowsize = 1, arrowwidth = 2, arrowcolor = "#c0392b",
                        ax = 50, ay = -50, font = list(color = "#c0392b", size = 11),
                        bgcolor = "rgba(255, 255, 255, 0.9)", bordercolor = "#c0392b",
                        borderwidth = 1.5, borderpad = 4)
    }
    
    p %>%
      layout(xaxis = list(title = "Date", tickangle = -45),
             yaxis = list(title = "Number of Cases", side = "left", showgrid = TRUE),
             yaxis2 = list(title = "Vaccination Rate (%)", overlaying = "y", 
                           side = "right", showgrid = FALSE, range = c(0, 100)),
             hovermode = 'x unified',
             legend = list(orientation = "h", y = -0.2, x = 0.5, 
                           xanchor = "center", yanchor = "top"),
             margin = list(b = 100, t = 40, l = 60, r = 60))
  })
  
  output$turningPopulation <- renderPlot({
    ggplot(Chicago$Population, aes(x=Date)) +
      # plot Boosted + Vaccinated first to put behind other lines
      geom_line(aes(y=Population.Boosted + Population.Vaccinated, color="Vaccinated + Boosted"), size=0.8, linetype="dashed") +
      geom_line(aes(y=Population.Boosted, color="Boosted"), size=1.2) +
      geom_line(aes(y=Population.Vaccinated, color="Vaccinated"), size=1.2) +
      geom_line(aes(y=Population.Unvaccinated, color="Unvaccinated"), size=1.2) +
      labs(y = "Population", x="Date", color="Vaccination Status") +
      theme_minimal() +
      scale_color_manual(values = c("Vaccinated + Boosted" = VACCINATION_STATUS_COLOR_MAP[["Boosted"]], VACCINATION_STATUS_COLOR_MAP)) +
      {if(input$turning_annotations) geom_vline(xintercept = as.Date("2022-01-01"), linetype="dashed", color="red", size=0.5) }+
      {if(input$turning_annotations) geom_text(aes(x=as.Date("2022-01-01"), y=0, label="Omicron peak"), vjust=-1, color="red") }+
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
      # chicago population line
      geom_hline(yintercept=2700000, linewidth=0.2, linetype = "dashed", color="red")
  })
  
  # === TAB 3: THE EVIDENCE ===
  # Use pre-computed data for faster rendering
  ve_base <- reactive({
    if (!is.null(Chicago$VE_Precomputed) && "VE" %in% names(Chicago$VE_Precomputed)) {
      # Use precomputed data
      Chicago$VE_Precomputed %>%
        filter(Date >= as.Date(input$ve_date_range[1]),
               Date <= as.Date(input$ve_date_range[2]))
    } else {
      # Fallback to computing on the fly
      req(Chicago$Raw)
      Chicago$Raw %>%
        filter(Outcome == "Cases",
               week_end >= as.Date(input$ve_date_range[1]),
               week_end <= as.Date(input$ve_date_range[2]),
               !is.na(`Unvaccinated Rate`), `Unvaccinated Rate` > 0,
               !is.na(`Vaccinated Rate`)) %>%
        mutate(VE = (1 - (`Vaccinated Rate` / `Unvaccinated Rate`)) * 100,
               VE = pmax(pmin(VE, 100), -100)) %>%
        rename(Date = week_end)
    }
  })
  
  # Only render spaghetti plot when its tab is active
  output$ve_spaghetti <- renderPlotly({
    d <- ve_base()
    req(nrow(d) > 0)
    d <- d %>% mutate(`Age Group` = ifelse(`Age Group` == "All", "All ages (combined)", `Age Group`))
    req(input$ve_age_groups)
    d <- d %>% filter(`Age Group` %in% input$ve_age_groups)
    req(nrow(d) > 0)
    
    desired_order <- c("0-4", "5-11", "12-17", "18-29", "30-49", "50-64", "65-79", "80+", "All ages (combined)")
    d <- d %>% mutate(`Age Group` = factor(`Age Group`, levels = desired_order))
    tmp <- d %>% mutate(`Age Group 2` = `Age Group`)
    
    p <- ggplot(d, aes(x = Date, y = VE)) +
      labs(x = "Date", y = "Vaccine Effectiveness (%)") +
      geom_line(data = tmp %>% select(-`Age Group`), aes(group = `Age Group 2`), 
                color = "grey", linewidth = 0.5, alpha = 0.5) +
      geom_line(aes(color = `Age Group`), linewidth = 1.2) +
      scale_color_viridis_d() +
      facet_wrap(~ `Age Group`) +
      theme_minimal() +
      theme(legend.position = "none") +
      {if (isTRUE(input$ve_omicron_line)) 
        geom_vline(xintercept = as.Date("2022-01-01"), linetype = "dashed", 
                   color = "red", linewidth = 0.5)}
    
    ggplotly(p)
  })
  
  ## Original streamgraph
  # output$ve_stream <- renderPlotly({
  #   # Use pre-computed stream data if available
  #   if (!is.null(Chicago$VE_Stream)) {
  #     ve_stream <- Chicago$VE_Stream %>%
  #       filter(Date >= as.Date(input$ve_date_range[1]),
  #              Date <= as.Date(input$ve_date_range[2])) %>%
  #       arrange(age_lower, `Age Group`, Date) %>%
  #       mutate(`Age Group` = factor(`Age Group`, levels = unique(`Age Group`)))
  #   } else {
  #     d <- ve_base() %>% filter(`Age Group` != "All")
  #     req(nrow(d) > 0)
  # 
  #     ve_stream <- d %>%
  #       select(Date, `Age Group`, VE) %>%
  #       group_by(Date, `Age Group`) %>%
  #       summarise(VE = mean(VE, na.rm = TRUE), .groups = "drop") %>%
  #       mutate(age_lower = readr::parse_number(`Age Group`)) %>%
  #       arrange(age_lower, `Age Group`, Date) %>%
  #       mutate(`Age Group` = factor(`Age Group`, levels = unique(`Age Group`))) %>%
  #       group_by(`Age Group`) %>%
  #       arrange(Date, .by_group = TRUE) %>%
  #       mutate(idx = row_number()) %>%
  #       filter(idx %% 2 == 0) %>%
  #       ungroup()
  #   }
  # 
  #   req(nrow(ve_stream) > 0)
  # 
  #   p <- ggplot(ve_stream, aes(x = Date, y = VE, fill = `Age Group`,
  #                              text = paste0("Age group: ", `Age Group`,
  #                                            "<br>Date: ", Date,
  #                                            "<br>VE: ", round(VE, 1), "%"))) +
  #     geom_stream(bw = 0.7) +
  #     scale_fill_viridis_d(option = "plasma") +
  #     theme_minimal()
  # 
  #   ggplotly(p, tooltip = "text")
  # 
  # 
  # })
  
  
  
  ## A possible change for the streamplot, but makes it static (so no interactivity)
  output$ve_stream <- renderPlot({
    # Use pre-computed stream data if available
    if (!is.null(Chicago$VE_Stream)) {
      ve_stream <- Chicago$VE_Stream %>%
        filter(Date >= as.Date(input$ve_date_range[1]),
               Date <= as.Date(input$ve_date_range[2])) %>%
        arrange(age_lower, `Age Group`, Date) %>%
        mutate(`Age Group` = factor(`Age Group`, levels = unique(`Age Group`)))
    } else {
      d <- ve_base() %>% filter(`Age Group` != "All")
      req(nrow(d) > 0)
      
      ve_stream <- d %>%
        select(Date, `Age Group`, VE) %>%
        group_by(Date, `Age Group`) %>%
        summarise(VE = mean(VE, na.rm = TRUE), .groups = "drop") %>%
        mutate(age_lower = readr::parse_number(`Age Group`)) %>%
        arrange(age_lower, `Age Group`, Date) %>%
        mutate(`Age Group` = factor(`Age Group`, levels = unique(`Age Group`))) %>%
        group_by(`Age Group`) %>%
        arrange(Date, .by_group = TRUE) %>%
        mutate(idx = row_number()) %>%
        filter(idx %% 2 == 0) %>%
        ungroup()
    }
    
    req(nrow(ve_stream) > 0)
    
    # Keep the beautiful streamgraph with consistent y-axis!
    ggplot(ve_stream, aes(x = Date, y = VE, fill = `Age Group`)) +
      geom_stream(bw = 0.7, type = "mirror") +  # type = "mirror" keeps it centered
      scale_fill_viridis_d(option = "plasma") +
      labs(y = "Relative VE Contribution", x = "Date") +
      theme_minimal(base_size = 13) +  # Slightly larger text for readability
      theme(
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()  # Cleaner look
      )
  })
  
  
  output$ve_riskReduction <- renderPlotly({
    data <- Chicago$Outcomes |>
      filter(Date >= as.Date(input$ve_date_range[1]) & 
               Date <= as.Date(input$ve_date_range[2])) |>
      group_by(Outcome) |>
      summarise(Total_Vaccinated = sum(Outcome.Vaccinated, na.rm = TRUE),
                Total_Unvaccinated = sum(Outcome.Unvaccinated, na.rm = TRUE),
                .groups = "drop") |>
      mutate(Rate_Vaccinated = Total_Vaccinated / sum(Total_Vaccinated) * 100000,
             Rate_Unvaccinated = Total_Unvaccinated / sum(Total_Unvaccinated) * 100000)
    
    reductions <- data |>
      mutate(Reduction = round((1 - Rate_Vaccinated/Rate_Unvaccinated) * 100, 0))
    
    # Define colors matching your original
    colors <- c("Cases" = "#E74C3C", 
                "Hospitalizations" = "#F39C12", 
                "Deaths" = "#8E44AD")
    
    # Create animation data - one row per frame per outcome
    n_frames <- 50
    
    animation_data <- lapply(1:n_frames, function(i) {
      progress <- i / n_frames
      
      lapply(c("Cases", "Hospitalizations", "Deaths"), function(outcome_name) {
        outcome_data <- data |> filter(Outcome == outcome_name)
        if(nrow(outcome_data) == 0) return(NULL)
        
        # Calculate interpolated position on log scale
        x_current <- 1 + progress
        y_current <- outcome_data$Rate_Unvaccinated[1] * 
          ((outcome_data$Rate_Vaccinated[1] / outcome_data$Rate_Unvaccinated[1]) ^ progress)
        
        data.frame(
          frame_id = i,
          outcome = outcome_name,
          x_start = 1,
          x_end = x_current,
          y_start = outcome_data$Rate_Unvaccinated[1],
          y_end = y_current,
          stringsAsFactors = FALSE
        )
      }) |> bind_rows()
    }) |> bind_rows()
    
    # Build the plot
    fig <- plot_ly()
    
    # Add animated line segments and markers for each outcome
    for(outcome_name in c("Cases", "Hospitalizations", "Deaths")) {
      anim_subset <- animation_data |> filter(outcome == outcome_name)
      
      if(nrow(anim_subset) == 0) next
      
      fig <- fig |>
        add_segments(
          data = anim_subset,
          x = ~x_start,
          xend = ~x_end,
          y = ~y_start,
          yend = ~y_end,
          frame = ~frame_id,
          line = list(color = colors[[outcome_name]], width = 3),
          name = outcome_name,  
          legendgroup = outcome_name,
          showlegend = FALSE,
          hoverinfo = 'none'
        ) |>
        add_markers(
          data = anim_subset,
          x = ~x_end,
          y = ~y_end,
          frame = ~frame_id,
          marker = list(size = 10, color = colors[[outcome_name]]),
          name = outcome_name,
          legendgroup = outcome_name,
          showlegend = TRUE,
          hoverinfo = 'none'
        )
    }
    
    # Add static elements (labels and annotations)
    for(outcome_name in c("Cases", "Hospitalizations", "Deaths")) {
      outcome_data <- data |> filter(Outcome == outcome_name)
      reduction_data <- reductions |> filter(Outcome == outcome_name)
      
      if(nrow(outcome_data) == 0) next
      
      # Left label (unvaccinated rate)
      fig <- fig |>
        add_annotations(
          x = 0.85,
          y = outcome_data$Rate_Unvaccinated[1],
          text = format(round(outcome_data$Rate_Unvaccinated[1], 0), big.mark = ","),
          showarrow = FALSE,
          xanchor = "right",
          font = list(size = 13, color = colors[[outcome_name]], family = "Arial")
        )
      
      # Right label (vaccinated rate)
      fig <- fig |>
        add_annotations(
          x = 2.15,
          y = outcome_data$Rate_Vaccinated[1],
          text = format(round(outcome_data$Rate_Vaccinated[1], 0), big.mark = ","),
          showarrow = FALSE,
          xanchor = "left",
          font = list(size = 13, color = colors[[outcome_name]], family = "Arial")
        )
      
      # Middle label (reduction percentage)
      y_middle <- sqrt(outcome_data$Rate_Unvaccinated[1] * outcome_data$Rate_Vaccinated[1])
      fig <- fig |>
        add_annotations(
          x = 1.5,
          y = y_middle,
          text = ifelse(outcome_name == "Cases", 
                        paste0("↑ ", abs(reduction_data$Reduction[1]), "%"),
                        paste0("↓ ", reduction_data$Reduction[1], "%")),
          showarrow = FALSE,
          font = list(size = 15, color = "black", family = "Arial Black"),
          bgcolor = "rgba(255, 255, 255, 0.9)",
          bordercolor = colors[[outcome_name]],
          borderwidth = 2,
          borderpad = 4
        )
    }
    
    # Configure layout and animation
    fig <- fig |>
      layout(
        title = list(
          text = "Vaccination Dramatically Reduces Risk Across All Outcomes",
          font = list(size = 16, family = "Arial", color = "#333")
        ),
        xaxis = list(
          tickvals = c(1, 2),
          ticktext = c("Unvaccinated", "Vaccinated"),
          range = c(0.7, 2.3),
          showgrid = FALSE,
          zeroline = FALSE,
          title = ""
        ),
        yaxis = list(
          title = "Rate per 100,000 (log scale)",
          type = "log",
          tickvals = c(100, 500, 1000, 5000, 10000, 50000, 100000),
          ticktext = c("100", "500", "1,000", "5,000", "10,000", "50,000", "100,000"),
          showgrid = TRUE,
          gridcolor = "#e0e0e0",
          zeroline = FALSE
        ),
        hovermode = FALSE,
        plot_bgcolor = "#fafafa",
        paper_bgcolor = "white",
        margin = list(t = 80, b = 80, l = 100, r = 100),
        legend = list(
          orientation = "h",
          y = -0.2,
          x = 0.5,
          xanchor = "center",
          yanchor = "top"
        )
      ) |>
      style(name = "Cases", traces = 2) |>
      style(name = "Hospitalizations", traces = 4) |>
      style(name = "Deaths", traces = 6) |>
      animation_opts(
        frame = 60,
        transition = 0,
        redraw = FALSE,
        mode = "immediate"
      ) |>
      animation_button(visible = FALSE) |>
      animation_slider(visible = FALSE) |>
      config(displayModeBar = FALSE)
    
    # Trigger looping animation with JavaScript
    fig <- htmlwidgets::onRender(fig, "
    function(el, x) {
      function animate() {
        Plotly.animate(el, null, {
          frame: {duration: 60, redraw: false},
          transition: {duration: 0},
          fromcurrent: true,
          mode: 'immediate'
        }).then(function() {
          // Loop: restart animation after it finishes
          setTimeout(animate, 100);
        });
      }
      setTimeout(animate, 500);
    }
  ")
    
    fig
  })
  
  output$ve_area <- renderPlotly({
    d <- ve_base() %>% filter(`Age Group` != "All")
    req(nrow(d) > 0)
    
    ve_area <- d %>% mutate(age_lower = readr::parse_number(`Age Group`))
    order <- ve_area %>% distinct(`Age Group`, age_lower) %>% arrange(age_lower) %>% pull(`Age Group`)
    ve_area$`Age Group` <- factor(ve_area$`Age Group`, levels = order)
    
    p <- ggplot(ve_area, aes(Date, VE, fill = `Age Group`)) +
      geom_area() +
      labs(y = "Vaccine Effectiveness (%)") +
      scale_fill_viridis_d() +
      theme_minimal() +
      {if (isTRUE(input$ve_omicron_line)) 
        geom_vline(xintercept = as.Date("2022-01-01"), linetype = "dashed", 
                   color = "red", linewidth = 0.5)}
    
    ggplotly(p)
  })
  
  # === TAB 4: THE DISPARITIES ===
  output$zip_date_selector <- renderUI({
    data_to_use <- if (!is.null(input$map_type) && input$map_type == "covid") {
      Chicago$ZipProgression
    } else {
      Chicago$ZipVaccination
    }
    
    if (is.null(data_to_use)) return(p("Loading data..."))
    
    available_dates <- sort(unique(as.Date(data_to_use$Date)))
    default_date <- if (!is.null(input$map_type) && input$map_type == "covid") {
      available_dates[which.min(abs(available_dates - as.Date("2021-03-01")))]
    } else {
      available_dates[which.min(abs(available_dates - as.Date("2021-06-01")))]
    }
    
    sliderInput("zip_date", label = "Select Week", min = min(available_dates), 
                max = max(available_dates), value = default_date, timeFormat = "%Y-%m-%d",
                animate = animationOptions(interval = 500, loop = TRUE))
  })
  
  output$map_title <- renderUI({
    req(input$map_type)
    if (input$map_type == "vaccination") {
      h3("Vaccination Rates Across Chicago ZIP Codes", style = "color: #667eea;")
    } else {
      h3("COVID-19 Impact Across Chicago ZIP Codes", style = "color: #667eea;")
    }
  })
  
  output$metric_selector <- renderUI({
    req(input$map_type)
    if (input$map_type == "vaccination") {
      selectInput("map_metric", label = "Select Vaccination Metric",
                  choices = c("1st Dose Rate (%)" = "Vaccinated_1st_Dose",
                              "Fully Vaccinated Rate (%)" = "Fully_Vaccinated",
                              "Booster Rate (%)" = "Boosted"),
                  selected = "Fully_Vaccinated")
    } else {
      selectInput("map_metric", label = "Select COVID-19 Metric",
                  choices = c("Weekly Cases" = "Cases_Weekly",
                              "Weekly Deaths" = "Deaths_Weekly",
                              "Case Rate (per 100k)" = "Case_Rate_Weekly"),
                  selected = "Cases_Weekly")
    }
  })
  
  output$metric_info_box <- renderUI({
    req(input$map_type, input$map_metric)
    
    if (input$map_type == "vaccination") {
      explanation <- switch(input$map_metric,
                            "Vaccinated_1st_Dose" = "Percentage of the ZIP code population that received at least one vaccine dose.",
                            "Fully_Vaccinated" = "Percentage of the ZIP code population that completed their vaccination series (typically 2 doses).",
                            "Boosted" = "Percentage of the ZIP code population that received a booster shot.",
                            "Select a metric to see its explanation.")
      box_color <- "#e8f5e9"
    } else {
      explanation <- switch(input$map_metric,
                            "Cases_Weekly" = "Number of new COVID-19 cases reported in this ZIP code during the selected week.",
                            "Deaths_Weekly" = "Number of COVID-19 deaths reported in this ZIP code during the selected week.",
                            "Case_Rate_Weekly" = "Weekly COVID-19 cases per 100,000 residents. This population-adjusted rate allows fair comparison between ZIP codes of different sizes.",
                            "Select a metric to see its explanation.")
      box_color <- "#fff3e0"
    }
    
    div(
      style = paste0("background-color: ", box_color, "; padding: 12px; border-radius: 5px; border-left: 4px solid #667eea; margin-top: 10px;"),
      tags$p(style = "margin: 0; font-size: 13px; line-height: 1.4;", 
             tags$strong("What this means: "), explanation)
    )
  })
  
  output$chicagoZipMap <- renderLeaflet({
    if (is.null(Chicago$ZipVaccination) || is.null(Chicago$ZipProgression) || 
        is.null(Chicago$Boundaries) || is.null(input$map_metric) || is.null(input$map_type)) {
      return(leaflet() %>% addTiles() %>% setView(lng = -87.6298, lat = 41.8781, zoom = 10))
    }
    
    tryCatch({
      current_data <- if (input$map_type == "vaccination") Chicago$ZipVaccination else Chicago$ZipProgression
      selected_date <- as.Date(input$zip_date)
      available_dates <- sort(unique(as.Date(current_data$Date)))
      nearest_date <- available_dates[which.min(abs(available_dates - selected_date))]
      
      map_data <- current_data %>% mutate(Date = as.Date(Date)) %>% filter(Date == nearest_date)
      map_sf <- Chicago$Boundaries %>% left_join(map_data, by = "ZIP_Code")
      
      metric_values <- as.numeric(map_sf[[input$map_metric]])
      metric_values[is.na(metric_values) | is.nan(metric_values) | 
                      is.infinite(metric_values) | metric_values < 0] <- NA
      valid_values <- metric_values[!is.na(metric_values)]
      
      if (length(valid_values) == 0) {
        return(leaflet() %>% addTiles() %>% setView(lng = -87.6298, lat = 41.8781, zoom = 10))
      }
      
      pal <- if (input$color_scheme == "viridis") {
        colorNumeric(palette = viridis::viridis(256), domain = range(valid_values, na.rm = TRUE), na.color = "#808080")
      } else {
        colorNumeric(palette = input$color_scheme, domain = range(valid_values, na.rm = TRUE), na.color = "#808080")
      }
      
      metric_label <- if (input$map_type == "vaccination") {
        switch(input$map_metric, "Vaccinated_1st_Dose" = "1st Dose Rate (%)",
               "Fully_Vaccinated" = "Fully Vaccinated (%)", "Boosted" = "Booster Rate (%)", "Metric")
      } else {
        switch(input$map_metric, "Cases_Weekly" = "Weekly Cases", "Deaths_Weekly" = "Weekly Deaths",
               "Case_Rate_Weekly" = "Case Rate (per 100k)", "Metric")
      }
      
      map_output <- leaflet(map_sf) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(metric_values), weight = 1.5, opacity = 1, color = "white", fillOpacity = 0.7,
          highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
          label = ~paste0("<strong>ZIP Code: ", ZIP_Code, "</strong><br>", metric_label, ": ", 
                          ifelse(is.na(metric_values), "No data", format(round(metric_values, 1), big.mark = ","))) %>% 
            lapply(htmltools::HTML),
          labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                      textsize = "13px", direction = "auto"),
          layerId = ~ZIP_Code
        )
      
      if(input$show_labels) {
        map_output <- map_output %>%
          addLabelOnlyMarkers(
            data = st_centroid(st_geometry(map_sf)), label = map_sf$ZIP_Code,
            labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
                                        style = list("color" = "#000000", "font-size" = "10px", "font-weight" = "bold"))
          )
      }
      
      map_output %>%
        addLegend(pal = pal, values = valid_values, opacity = 0.7, title = metric_label, 
                  position = "bottomright", na.label = "No data")
      
    }, error = function(e) {
      leaflet() %>% addTiles() %>% setView(lng = -87.6298, lat = 41.8781, zoom = 10)
    })
  })
  
  observeEvent(input$chicagoZipMap_shape_click, {
    click <- input$chicagoZipMap_shape_click
    if (!is.null(click)) selected_zip(click$id)
  })
  
  output$map_stats_date <- renderText({
    if (is.null(Chicago$ZipVaccination) || is.null(Chicago$ZipProgression)) return("Loading...")
    req(input$map_metric, input$zip_date, input$map_type)
    
    current_data <- if (input$map_type == "vaccination") Chicago$ZipVaccination else Chicago$ZipProgression
    selected_date <- as.Date(input$zip_date)
    available_dates <- sort(unique(as.Date(current_data$Date)))
    nearest_date <- available_dates[which.min(abs(available_dates - selected_date))]
    
    format(nearest_date, "%B %d, %Y")
  })
  
  output$map_stats_total <- renderText({
    if (is.null(Chicago$ZipVaccination) || is.null(Chicago$ZipProgression)) return("Loading...")
    req(input$map_metric, input$zip_date, input$map_type)
    
    current_data <- if (input$map_type == "vaccination") Chicago$ZipVaccination else Chicago$ZipProgression
    selected_date <- as.Date(input$zip_date)
    available_dates <- sort(unique(as.Date(current_data$Date)))
    nearest_date <- available_dates[which.min(abs(available_dates - selected_date))]
    
    data <- current_data %>% mutate(Date = as.Date(Date)) %>% filter(Date == nearest_date)
    if (nrow(data) == 0) return("No data")
    
    if (grepl("Rate|Percent", input$map_metric)) {
      avg <- mean(data[[input$map_metric]], na.rm = TRUE)
      paste0(format(round(avg, 1), big.mark = ","), 
             ifelse(grepl("Percent|Dose|Vaccinated|Boosted", input$map_metric), "%", ""))
    } else {
      total <- sum(data[[input$map_metric]], na.rm = TRUE)
      format(round(total, 0), big.mark = ",")
    }
  })
  
  output$map_stats_avg <- renderText({
    if (is.null(Chicago$ZipVaccination) || is.null(Chicago$ZipProgression)) return("Loading...")
    req(input$map_metric, input$zip_date, input$map_type)
    
    current_data <- if (input$map_type == "vaccination") Chicago$ZipVaccination else Chicago$ZipProgression
    selected_date <- as.Date(input$zip_date)
    available_dates <- sort(unique(as.Date(current_data$Date)))
    nearest_date <- available_dates[which.min(abs(available_dates - selected_date))]
    
    data <- current_data %>% mutate(Date = as.Date(Date)) %>% filter(Date == nearest_date)
    if (nrow(data) == 0) return("No data")
    
    avg <- mean(data[[input$map_metric]], na.rm = TRUE)
    paste0(format(round(avg, 1), big.mark = ","),
           ifelse(grepl("Percent|Dose|Vaccinated|Boosted", input$map_metric), "%", ""))
  })
  
  output$selectedZipTimeSeries <- renderPlot({
    if (is.null(Chicago$ZipVaccination) || is.null(Chicago$ZipProgression) || is.null(selected_zip())) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                                 label = "Click on a ZIP code in the map to see its timeline", 
                                 size = 6, color = "gray50") + theme_void())
    }
    
    req(input$map_metric, input$map_type)
    current_data <- if (input$map_type == "vaccination") Chicago$ZipVaccination else Chicago$ZipProgression
    zip_data <- current_data %>% filter(ZIP_Code == selected_zip())
    
    if (nrow(zip_data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                                 label = paste0("No data for ZIP ", selected_zip()), 
                                 size = 6, color = "gray50") + theme_void())
    }
    
    metric_label <- if (input$map_type == "vaccination") {
      switch(input$map_metric, "Vaccinated_1st_Dose" = "1st Dose Rate (%)",
             "Fully_Vaccinated" = "Fully Vaccinated (%)", "Boosted" = "Booster Rate (%)", "Metric")
    } else {
      switch(input$map_metric, "Cases_Weekly" = "Weekly Cases", "Deaths_Weekly" = "Weekly Deaths",
             "Case_Rate_Weekly" = "Case Rate (per 100k)", "Metric")
    }
    
    plot_color <- if (input$map_type == "vaccination") "#667eea" else "#e74c3c"
    
    ggplot(zip_data, aes(x = Date, y = .data[[input$map_metric]])) +
      geom_line(color = plot_color, size = 1) +
      geom_area(fill = plot_color, alpha = 0.2) +
      geom_vline(xintercept = as.Date(input$zip_date), linetype = "dashed", color = "red", size = 0.8) +
      geom_point(data = zip_data %>% filter(Date == as.Date(input$zip_date)), color = "red", size = 3) +
      labs(title = paste0("ZIP Code ", selected_zip(), ": ", metric_label),
           subtitle = paste0("Current date: ", format(as.Date(input$zip_date), "%B %d, %Y")),
           x = "Date", y = metric_label) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 14, color = "#667eea"),
            plot.subtitle = element_text(size = 11, color = "gray40"),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # === TAB 5: EXPLORE RAW DATA ===
  # Use server-side processing for large data tables
  output$table_outcomes <- DT::renderDataTable({
    DT::datatable(
      Chicago$Outcomes,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(0, 'desc')),
        processing = TRUE,
        serverSide = FALSE,  # Keep client-side for smaller data
        dom = 'Bfrtip'
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })
  
  output$table_population <- DT::renderDataTable({
    DT::datatable(
      Chicago$Population,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(0, 'desc')),
        processing = TRUE,
        serverSide = FALSE
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })
  
  output$table_zip_vaccination <- DT::renderDataTable({
    if (is.null(Chicago$ZipVaccination)) {
      return(DT::datatable(data.frame(Message = "Data not available. Please run prepare_zip_data.R first.")))
    }
    
    DT::datatable(
      Chicago$ZipVaccination,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(0, 'desc')),
        processing = TRUE,
        serverSide = FALSE
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })
  
  output$table_zip_progression <- DT::renderDataTable({
    if (is.null(Chicago$ZipProgression)) {
      return(DT::datatable(data.frame(Message = "Data not available. Please run prepare_zip_data.R first.")))
    }
    
    DT::datatable(
      Chicago$ZipProgression,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(0, 'desc')),
        processing = TRUE,
        serverSide = FALSE
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })
})

shinyApp(ui, server)