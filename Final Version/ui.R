# The link for the Shiny App is https://rfilho50530.shinyapps.io/BookAnalysisProject/

library(data.table)
library(igraph)
library(ggplot2)
library(shiny)
library(shinythemes)
library(hash)
library(shinyWidgets)
library(shinydashboard)


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Book Network Analysis"),
  dashboardSidebar(
    sidebarMenu(id = "sidebarid",
                menuItem("About Me", tabName = "aboutme"),
                menuItem("General", tabName = "general",
                         menuSubItem("Descriptive Statistics", tabName = "statistics"), 
                         menuSubItem("Network Exploration", tabName = "graph"),
                         menuSubItem("Network Analysis", tabName = "homophily")
                ),
                menuItem("Author", tabName = "author"),
                conditionalPanel(
                  'input.sidebarid == "statistics"',
                  awesomeRadio(
                    inputId = "bar.chart",
                    label = h4("Statistics by:"), 
                    choices = c("Year" = "chart.year",
                                "Category" = "chart.category",
                                "Author" = "chart.author",
                                "Book" = "chart.book"),
                    selected = "chart.year"
                  )
                ),
                
                conditionalPanel(
                  'input.sidebarid == "homophily"',
                                   selectInput("category.name",
                              h4("Select category"),
                              NULL),
                  div(style="text-align:center; color: red","This action may take awhile to appear",br(), "due to the complex computation", br(), "being carried out.")
                  
                ),
                
                conditionalPanel(
                  'input.sidebarid == "author"',
                  selectInput("author.name",
                              h4("Select author"),
                              NULL)
                ),
                
                conditionalPanel(
                  "input.sidebarid == 'statistics' || input.sidebarid == 'graph' || input.sidebarid == 'author'",  
                  sliderInput("top.n.values",
                              h4("Top N"),
                              value = 5,
                              step = 5,
                              min = 5,
                              max = 50)
                ),
                
                conditionalPanel(
                  "input.sidebarid == 'statistics' || input.sidebarid == 'graph'",  
                  sliderInput("year.range", 
                              label = h4("Year Range"), 
                              min = 1850, 
                              max = 2020, 
                              value = c(1850, 2020)
                        )
                ),
                
                conditionalPanel(
                  "input.sidebarid == 'graph' || input.sidebarid == 'author'",
                  switchInput(
                    inputId = "switch.value",
                    label = "Slide for degree distribution,...", 
                    labelWidth = "80px"
                  )
                )
                
                )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "aboutme",
              uiOutput("aboutme")
      ),
      tabItem(tabName = "statistics",
              uiOutput("general.statistics")
      ),
      tabItem(tabName = "graph",
              uiOutput("general.graph")
      ),
      tabItem(tabName = "author",
              uiOutput("author")
      ),
      tabItem(tabName = "homophily",
              uiOutput("homophily")
      )
    )
  )
)

ui