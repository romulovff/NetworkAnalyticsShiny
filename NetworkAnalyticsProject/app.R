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

# Define UI
ui <- fluidPage(theme = shinytheme("superhero"),
                
                #App title
              titlePanel("Book Network Analysis"),
                
                sidebarLayout(
                    #Sidebar
                    sidebarPanel(
                      conditionalPanel(
                        condition = "input.maintabs == 'General'",
                        awesomeRadio(
                          inputId = "bar.chart",
                          label = h4("Statistics by"), 
                          choices = c("Year" = "chart.year",
                                      "Category" = "chart.category",
                                      "Author" = "chart.author",
                                      "Book" = "chart.book"),
                          selected = "chart.year"
                        )
                      ),
                      conditionalPanel(
                        condition = "input.maintabs == 'Author'",
                        selectInput("author.name",
                                    h4("Select author"),
                                    NULL)
                      ),
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
                      conditionalPanel(
                        condition = "input.maintabs == 'General' & input.generaltab == 'Graph'",
                        switchInput(
                          inputId = "switch.value",
                          label = "Network Exploration", 
                          labelWidth = "80px"
                        )
                      ),
                    ),
                    #Tabs
                    mainPanel(tabsetPanel(type = "tabs",
                                          id = "maintabs",
                                          tabPanel("General", 
                                                   tabsetPanel(type = "tabs",
                                                               id = "generaltab",
                                                               tabPanel("Statistics"),
                                                               tabPanel("Graph"))),
                                          tabPanel("Author", 
                                                   tabsetPanel(type = "tabs",
                                                               id = "authortab"))
                    ),
                              #verbatimTextOutput("test"),
                              htmlOutput("out")
                    )
                )
)

# Define server
server <- function(input, output, session) {
    output$test <- renderText({
        paste(input$top.authors.volume, input$top.authors.ranking)
        paste("Average Book Rating for Author Selected:", author.avg.rank(input$author.name))
    })
    
    output$out <- renderUI(
      if(input$maintabs == "General"){
        if(input$generaltab == "Statistics"){
          list(
            if(input$bar.chart == "chart.year"){
              list(
                h3(paste0("Number of Books: ", get.books.count.year.range(input$year.range))),
                renderPlot(plot.books.published.by.year(input$year.range)),
                h3(paste0("Number of Distinct Authors: ", get.authors.count.year.range(input$year.range))),
                renderPlot(plot.distinct.authors.by.year(input$year.range))
              )
            },
            if(input$bar.chart == "chart.author"){
              list(
                h3("Number of Authors per Book Distribution: "),
                renderPlot(plot.books.by.authornumber(input$year.range)),
                h3(paste0(input$top.n.values, " Authors With Most Books")),
                renderTable(get.authors.most.books(input$top.n.values, input$year.range))
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
        } else {
          if(input$generaltab == "Graph") {
            list(
              h3(print("Authors connected if books written are of the same category")),
              renderPlot(plot.similar.category.network(input$year.range, input$top.n.values, input$switch.value)),
              h3(print("Authors connected if similar rating")),
              renderPlot(plot.similar.rating.network(input$year.range, input$top.n.values, input$switch.value)),
              h3(print("Authors connected if co-written a book")),
              renderPlot(plot.co.authors.network(input$top.n.values, input$switch.value))
            )
          }
        }
      } else {
        if(input$maintabs == "Author") {
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
                h3("Graph that connects authors with similar rating and category"),
                renderPlot(plot.similar.rank.category.authors(input$author.name, input$year.range, input$top.n.values)) # Discuss removal
              )
          )
        }
      }
    )
    
    updateSelectInput(
      session,
      inputId = "author.name",
      choices = get.unique.authors()
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
