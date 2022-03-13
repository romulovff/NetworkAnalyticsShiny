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

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                
                #App title
                titlePanel("Book Network Analysis"),
                
                sidebarLayout(
                    #Sidebar
                    sidebarPanel(
                        radioButtons("bar.chart", h4("Statistics by:"),
                                     c("Year" = "chart.year",
                                       "Category" = "chart.category",
                                       "Author" = "chart.author",
                                       "Book" = "chart.book")),
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
                        selectInput("author.name",
                                    h4("Select author"),
                                    NULL)
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
                                                               id = "authortab",
                                                               tabPanel("Statistics"),
                                                               tabPanel("Graph")))
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
        list(
          if(input$bar.chart == "chart.year" && input$generaltab == "Statistics"){
            list(
              h3(paste0("Number of Books: ", get.books.count.year.range(input$year.range))),
              renderPlot(plot.books.published.by.year(input$year.range)),
              h3(paste0("Number of Distinct Authors: ", get.authors.count.year.range(input$year.range))),
              renderPlot(plot.distinct.authors.by.year(input$year.range))
            )
          },
          if(input$bar.chart == "chart.author" && input$generaltab == "Statistics"){
            list(
              h3("Number of Authors per Book Distribution: "),
              renderPlot(plot.books.by.authornumber(input$year.range)),
              h3(paste0(input$top.n.values, " Authors With Most Books")),
              renderTable(get.authors.most.books(input$top.n.values, input$year.range))
            )
          },
          if(input$bar.chart == "chart.book" && input$generaltab == "Statistics"){
            list(
              h3(paste0("Average Book Rating: ", get.average.book.rating(input$year.range))),
              renderPlot(plot.books.by.ranking(input$year.range)),
              h3(paste0(input$top.n.values, " Books With Highest Rating")),
              renderTable(get.books.highest.rating(input$top.n.values, input$year.range))
            )
          },
          if(input$bar.chart == "chart.category" && input$generaltab == "Statistics"){
            list(
              h3(paste0(input$top.n.values,"Categories With Most Books")),
              renderTable(get.categories.most.books(input$top.n.values, input$year.range)),
              h3("Distinct Authors for each Category"),
              renderTable(get.distinct.author.per.category(input$year.range))
            )
          }
        )
      } else {
        if(input$maintabs == "Author") {
          list(
            h3(paste("Average Book Rating for Author Selected:", author.avg.rank(input$author.name, input$year.range)))
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
