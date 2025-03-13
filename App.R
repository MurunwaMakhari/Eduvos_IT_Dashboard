library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Eduvos IT Graduate Survey"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Tech Tools", tabName = "tools", icon = icon("laptop-code")),
      menuItem("Industries", tabName = "industries", icon = icon("building")),
      menuItem("Job Roles", tabName = "roles", icon = icon("briefcase"))
    )
  ),
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(plotOutput("employmentChart"), width = 6),
                box(dataTableOutput("employmentTable"), width = 6)
              )),
      
      # Tech Tools Tab
      tabItem(tabName = "tools",
              fluidRow(
                selectInput("toolType", "Select Tool Type:", choices = c("ProgLang", "Databases", "WebFramework")),
                plotlyOutput("topToolsChart")
              )),
      
      # Industries Tab
      tabItem(tabName = "industries",
              plotlyOutput("industryChart")),
      
      # Job Roles Tab
      tabItem(tabName = "roles",
              plotlyOutput("jobRoleChart"))
    )
  )
)

server <- function(input, output) {
  
  
  # Load dataset
  graduate_data <- read.csv("graduate_survey.csv", stringsAsFactors = FALSE)
  
  # Employment Rate Visualization
  employment_summary <- graduate_data %>%
    separate_rows(Employment, sep = ";") %>%
    mutate(Employment = str_trim(Employment)) %>%
    group_by(StudyField) %>%
    summarise(
      Employed = sum(str_detect(Employment, "Employed")),
      Total = n(),
      EmploymentRate = round((Employed / Total) * 100, 1)
    )
  
  output$employmentChart <- renderPlot({
    ggplot(employment_summary, aes(x = reorder(StudyField, EmploymentRate), y = EmploymentRate, fill = EmploymentRate)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      coord_flip() +
      labs(title = "Employment Rate by Study Field", x = "Study Field", y = "Employment Rate (%)") +
      theme_minimal() +
      scale_fill_gradient(low = "lightblue", high = "darkblue")
  })
  
  output$employmentTable <- renderDataTable({
    datatable(employment_summary)
  })
  
  # Tech Tools Visualization
  output$topToolsChart <- renderPlotly({
    selected_col <- input$toolType
    tool_counts <- graduate_data %>%
      separate_rows(!!sym(selected_col), sep = ";") %>%
      count(!!sym(selected_col), sort = TRUE) %>%
      top_n(10, n)
    
    ggplot(tool_counts, aes(x = reorder(!!sym(selected_col), n), y = n, fill = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste("Top", selected_col, "Used"), x = selected_col, y = "Count") +
      theme_minimal()
  })
  
  # Industries Visualization
  output$industryChart <- renderPlotly({
    industry_counts <- graduate_data %>%
      separate_rows(Industry, sep = ";") %>%
      count(Industry, sort = TRUE) %>%
      top_n(10, n)
    
    ggplot(industry_counts, aes(x = reorder(Industry, n), y = n, fill = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top Industries Graduates Enter", x = "Industry", y = "Count") +
      theme_minimal()
  })
  
  # Job Roles Visualization
  output$jobRoleChart <- renderPlotly({
    role_counts <- graduate_data %>%
      separate_rows(Role, sep = ";") %>%
      count(Role, sort = TRUE) %>%
      top_n(10, n)
    
    ggplot(role_counts, aes(x = reorder(Role, n), y = n, fill = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top Job Roles Graduates Enter", x = "Job Role", y = "Count") +
      theme_minimal()
  })
}
shinyApp(ui, server)
