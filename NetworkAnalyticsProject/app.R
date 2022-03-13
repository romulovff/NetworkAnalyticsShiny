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
                        radioButtons("bar.chart", "Statistics by:",
                                     c("Year" = "chart.year",
                                       "Category" = "chart.category",
                                       "Rating" = "chart.rating")),
                        sliderInput("top.n.values",
                                    "Top N",
                                    value = 5,
                                    step = 5,
                                    min = 5,
                                    max = 50),
                        selectInput("author.name",
                                    "Select author",
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
              h2(paste0("Total Books in Dataset: ", nrow(dt.books))),
              renderPlot(plot.books.published.by.year()),
              h2(paste0(input$top.n.values, " Authors With Most Books in the Dataset")),
              renderTable(get.authors.most.books(input$top.n.values))
            )
          },
          if(input$bar.chart == "chart.rating" && input$generaltab == "Statistics"){
            list(
              h2(paste0("Average Book Rating: ", round(mean(dt.books$average_rating, na.rm = TRUE),3))),
              renderPlot(plot.books.by.ranking()),
              h2(paste0(input$top.n.values, " Books With Highest Rating in the Dataset")),
              renderTable(get.books.highest.rating(input$top.n.values))
            )
          },
          if(input$bar.chart == "chart.category" && input$generaltab == "Statistics"){
            list(
              h2(paste0(input$top.n.values," Categories With Most Books in the Dataset")),
              renderTable(get.categories.most.books(input$top.n.values))
            )
          }
        )
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
