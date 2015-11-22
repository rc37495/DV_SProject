#ui.R

require(shiny)
require(shinydashboard)
require(leaflet)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Crosstab", tabName = "crosstab", icon = icon("dashboard")),
      menuItem("Scatterplot", tabName = "scatter", icon = icon("th")),
      menuItem("Bar Chart", tabName = "barchart", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "crosstab",
              sliderInput("KPI1", "KPI_Low_Max_value:", 
                          min = 1, max = 35,  value = 35),
              sliderInput("KPI2", "KPI_Medium_Max_value:", 
                          min = 35, max = 50,  value = 50),
              actionButton(inputId = "clicks1",  label = "Click me"),
              plotOutput("distPlot1")
      ),
      
      # Second tab content
      tabItem(tabName = "scatter",
              actionButton(inputId = "clicks2",  label = "Click me"),
              plotOutput("distPlot2")
      ),
      
      # Third tab content
      tabItem(tabName = "barchart",
              actionButton(inputId = "clicks3",  label = "Click me"),
              plotOutput("distPlot3")
      )
    )
  )
)