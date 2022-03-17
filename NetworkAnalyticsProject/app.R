#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
                         menuSubItem("Statistics", tabName = "statistics"), 
                         menuSubItem("Graph", tabName = "graph")
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
                  'input.sidebarid == "author"',
                  selectInput("author.name",
                              h4("Select author"),
                              NULL)
                ),
                conditionalPanel(
                  "input.sidebarid == 'statistics' || input.sidebarid == 'graphs' || input.sidebarid == 'author'",  
                  sliderInput("top.n.values",
                              h4("Top N"),
                              value = 5,
                              step = 5,
                              min = 5,
                              max = 50),
                  sliderInput("year.range", 
                              label = h4("Year Range"), 
                              min = 1850, 
                              max = 2020, 
                              value = c(1850, 2020)),
                ),
    
                conditionalPanel(
                  "input.sidebarid == 'graph'",
                  switchInput(
                    inputId = "switch.value",
                    label = "Network Exploration", 
                    labelWidth = "80px"
                  )
                )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "aboutme",
              h1("This Shinny App aims at analyzing authors and their respective books over different dimensions such as rankings, categories, and number of books.
              The database used to implement such analyses was extracted from Kaggle and can be found here: https://www.kaggle.com/dylanjcastillo/7k-books-with-metadata.
              On that, we performed the following data cleaning steps:
                1.	Reduced the number of categories to only appear the top 50
              2.	Normalized the names of the authors and books
              3.	Separated the books with co-authors in different rows, one for each
              4.	Omitted the rows with null information
              5.	Dealt with special characters such as Russian names
              We separated the information through two different tabs:
                1.	General -Performs descriptive statistics, network exploration and network analysis on the overall data
              2.	Author - Performs both descriptive statistics and network exploration on a selected author 
              The user can interact with most graphs for a clearer and more concise visualization.
              ")
      ),
      tabItem(tabName = "statistics",
            uiOutput("general.statistics")
      ),
      tabItem(tabName = "graph",
              uiOutput("general.graph")),
      tabItem(tabName = "author",
              uiOutput("author"))
    )
  )
)

# Define server
server <- function(input, output, session) {
  output$general.statistics <- renderUI(
    list(
      if(input$bar.chart == "chart.year"){
        list(
          infoBox(
            "Number of books", get.books.count.year.range(input$year.range), width = 6
          ),
          infoBox(
            "Number of authors", get.authors.count.year.range(input$year.range), width = 6
          ),
          box(title = "Number of books published per year", 
              renderPlot(plot.books.published.by.year(input$year.range))
          ),
          box(title = "Number of authors per year",
              renderPlot(plot.distinct.authors.by.year(input$year.range)))
        )
      },
      if(input$bar.chart == "chart.author"){
        list(
          box(title = "Number of Authors per Book Distribution", 
              renderPlot(plot.books.by.authornumber(input$year.range))
          ),
          box(title = paste0(input$top.n.values, " Authors With Most Books"),
              renderTable(get.authors.most.books(input$top.n.values, input$year.range))
          )
        )
      },
      if(input$bar.chart == "chart.book"){
        list(
          h3(paste0("Average Book Rating: ", get.average.book.rating(input$year.range))),
          renderPlot(plot.books.by.ranking(input$year.range)),
          h3(paste0(input$top.n.values, " Books With Highest Rating")),
          renderTable(get.books.highest.rating(input$top.n.values, input$year.range))
        )
      },
      if(input$bar.chart == "chart.category"){
        list(
          h3(paste0(input$top.n.values,"Categories With Most Books")),
          renderTable(get.categories.most.books(input$top.n.values, input$year.range)),
          h3("Distinct Authors for each Category"),
          renderTable(get.distinct.author.per.category(input$year.range))
        )
      }
    )
  )
  
  output$general.graph <- renderUI(
    list(
      h3(print("Authors connected if books written are of the same category")),
      renderPlot(plot.similar.category.network(input$year.range, input$top.n.values, input$switch.value)),
      if(input$switch.value == TRUE) {
        renderPrint(plot.similar.category.network(input$year.range, input$top.n.values, input$switch.value))
      },
      h3(print("Authors connected if similar rating")),
      renderPlot(plot.similar.rating.network(input$year.range, input$top.n.values, input$switch.value)),
      if(input$switch.value == TRUE) {
        renderPrint(plot.similar.rating.network(input$year.range, input$top.n.values, input$switch.value))
      },
      h3(print("Authors connected if co-written a book")),
      renderPlot(plot.co.authors.network(input$year.range, input$top.n.values, input$switch.value)),
      if(input$switch.value == TRUE) {
        renderPrint(plot.co.authors.network(input$year.range, input$top.n.values, input$switch.value))
      }
    )
  )
  
  output$author <- renderUI(
    list(
      # h3(paste("Average Book Rating for Author Selected:", author.avg.rank(input$author.name, input$year.range))),
      list(
        h3("Graph that connects author with the book"),
        renderPlot(plot.author.to.books.network(input$author.name, input$year.range)),
        h3("Graph that connects author with categories"),
        renderPlot(plot.author.to.categories.network(input$author.name, input$year.range)),
        h3("Graph that connect authors with similar rating"),
        renderPlot(plot.similar.rank.authors(input$author.name, input$year.range, input$top.n.values)),
        h3("Graph that connect authors with similar category"),
        renderPlot(plot.similar.category.authors(input$author.name, input$year.range, input$top.n.values)),
        h3("Authors connected if co-written a book"),
        renderPlot(plot.co.authors(input$author.name, input$year.range))
      )
    )
  )
  
  updateSelectInput(
    session,
    inputId = "author.name",
    choices = get.unique.authors()
  )
}

# Run the application 
shinyApp(ui = ui, server = server)