# Full Shiny App: Country Comparison & Correlation Explorer
# Purpose: Exploratory Data Analysis for Psychometric Data

library(shiny)
library(tidyverse)

# 1. DATA LOADING
# Make sure to select your RDS file (24 facets)
raw_data <- readRDS("HEXACO_facets.rds")

# 2. USER INTERFACE
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"), # Adding a professional theme
  titlePanel("Advanced Psychometric Dashboard"),
  
  # Tabset creates the navigation menu
  tabsetPanel(
    
    # TAB 1: Comparison between countries
    tabPanel("Country Comparison",
             fluidRow(
               column(6, 
                      wellPanel(
                        selectInput("facet_1", "Choose Facet:", choices = colnames(raw_data)[1:24]),
                        selectInput("country_1", "Country 1:", choices = unique(raw_data$country))
                      ),
                      plotOutput("plot_1"),
                      textOutput("stats_1")
               ),
               column(6, 
                      wellPanel(
                        selectInput("facet_2", "Choose Facet:", choices = colnames(raw_data)[1:24]),
                        selectInput("country_2", "Country 2:", choices = unique(raw_data$country), selected = "US")
                      ),
                      plotOutput("plot_2"),
                      textOutput("stats_2")
               )
             )
    ),
    
    # TAB 2: Correlation Analysis (Scatter Plot)
    tabPanel("Correlation Explorer",
             sidebarLayout(
               sidebarPanel(
                 helpText("Analyze the relationship between two traits"),
                 selectInput("scat_x", "Select X Axis:", choices = colnames(raw_data)[1:24], selected = "EAnxi"),
                 selectInput("scat_y", "Select Y Axis:", choices = colnames(raw_data)[1:24], selected = "ESent"),
                 selectInput("scat_country", "Filter by Country:", choices = c("All", unique(raw_data$country)))
               ),
               mainPanel(
                 plotOutput("scatter_plot"),
                 hr(), # Horizontal line
                 verbatimTextOutput("corr_value") # Displaying the correlation coefficient
               )
             )
    )
  )
)

# 3. SERVER LOGIC
server <- function(input, output) {
  
  # --- LOGIC FOR TAB 1 (COMPARISON) ---
  output$plot_1 <- renderPlot({
    req(input$facet_1, input$country_1)
    data_1 <- raw_data %>% filter(country == input$country_1)
    ggplot(data_1, aes(x = .data[[input$facet_1]])) +
      geom_histogram(fill = "firebrick", color = "white", bins = 25) +
      theme_minimal() + labs(title = paste("Results for", input$country_1))
  })
  
  output$stats_1 <- renderText({
    req(input$facet_1, input$country_1)
    df <- raw_data[raw_data$country == input$country_1, ]
    val <- mean(df[[input$facet_1]], na.rm = TRUE)
    sd <- sd(df[[input$facet_1]], na.rm = TRUE)
    paste("Average Score:", round(val, 2),"| SD:", round(sd,2))
  })
  
  output$plot_2 <- renderPlot({
    req(input$facet_2, input$country_2)
    data_2 <- raw_data %>% filter(country == input$country_2)
    ggplot(data_2, aes(x = .data[[input$facet_2]])) +
      geom_histogram(fill = "seagreen", color = "white", bins = 25) +
      theme_minimal() + labs(title = paste("Results for", input$country_2))
  })
  
  output$stats_2 <- renderText({
    req(input$facet_2, input$country_2)
    df <- raw_data[raw_data$country == input$country_2, ]
    val <- mean(df[[input$facet_2]], na.rm = TRUE)
    sd <- sd(df[[input$facet_2]], na.rm = TRUE)
    paste("Average Score:", round(val, 2),"| SD:", round(sd,2))
  })
  
  # --- LOGIC FOR TAB 2 (CORRELATION) ---
  output$scatter_plot <- renderPlot({
    req(input$scat_x, input$scat_y, input$scat_country)
    
    # Filter by country if not "All"
    plot_df <- if(input$scat_country == "All") raw_data else raw_data %>% filter(country == input$scat_country)
    
    ggplot(plot_df, aes(x = .data[[input$scat_x]], y = .data[[input$scat_y]])) +
      geom_point(alpha = 0.2, color = "darkblue") + # alpha makes points transparent for overlap
      geom_smooth(method = "lm", color = "orange") + # Adds a regression line
      theme_minimal() +
      labs(title = paste("Relationship between", input$scat_x, "and", input$scat_y))
  })
  
  output$corr_value <- renderPrint({
    req(input$scat_x, input$scat_y, input$scat_country)
    plot_df <- if(input$scat_country == "All") raw_data else raw_data %>% filter(country == input$scat_country)
    
    # Calculate Pearson Correlation
    r_val <- cor(plot_df[[input$scat_x]], plot_df[[input$scat_y]], use = "complete.obs")
    cat("Pearson Correlation (r):", round(r_val, 3))
  })
}

# 4. RUN APP
shinyApp(ui, server)
