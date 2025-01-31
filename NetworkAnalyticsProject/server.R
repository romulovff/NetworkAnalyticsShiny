library(data.table)
library(igraph)
library(ggplot2)
library(shiny)
library(shinythemes)
library(hash)
library(shinyWidgets)
library(shinydashboard)

# Define server
server <- function(input, output, session) {
  
  output$homophily <- renderUI(
    list(
      infoBox(
        "Homophily", print.homophily(input$category.name), width = 4
      )
    )
  )
  
  
  output$aboutme <- renderUI(
    HTML(
      "
      <h4>This Shinny App aims at analyzing authors and their respective books over different dimensions such as rankings, categories, and number of books.</h4>
      <h4>The database used to implement such analyses was extracted from Kaggle and can be found <a href=https://www.kaggle.com/dylanjcastillo/7k-books-with-metadata>here</a>.</h4>
      <h4>On that, we performed the following data cleaning steps:</h4>
      <ol>
        <li>Reduced the number of categories to only appear the top 50</li>
        <li>Normalized the names of the authors and books</li>
        <li>Separated the books with co-authors in different rows, one for each</li>
        <li>Omitted the rows with null information</li>
        <li>Dealt with special characters such as Russian names</li>
      </ol><br>
      <h4>We separated the information through two different tabs:</h4>
      <ol>
        <li><b>General -</b> Performs descriptive statistics, network exploration and network analysis on the overall data</li>
        <li><b>Author -</b> Performs both descriptive statistics and network exploration on a selected author</li>
      </ol>
      <h4>The user can interact with most graphs for a clearer and more concise visualization.</h4><br>
      <h4>This project was developed by:</h4>
      <ol>
        <li>Francisco Perestrello - 39001</li>
        <li>Maria Ferreira - 50465</li>
        <li>Monica Pinto - 39349</li>
        <li>Romulo Filho - 50530</li>
        <li>Vasco Grincho - 39357</li>
      </ol>
      "
    )
  )
  
  
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
          infoBox(
            "Average Book Rating", get.average.book.rating(input$year.range), width = 12
          ),
          box(title = "Number of Books per Rating", 
              renderPlot(plot.books.by.ranking(input$year.range))
          ),
          box(title = paste0(input$top.n.values, " Books with Highest Rating"), 
              renderTable(get.books.highest.rating(input$top.n.values, input$year.range))
          )
        )
      },
      if(input$bar.chart == "chart.category"){
        list(
          box(title = paste0("Number of Authors for each top ", input$top.n.values, " category"),
              renderTable(get.distinct.author.per.category(input$year.range, input$top.n.values))
          ),
          box(title = paste0(input$top.n.values, " Categories with Most Books"), 
              renderTable(get.categories.most.books(input$top.n.values, input$year.range))
          )
        )
      }
    )
  )
  
  output$general.graph <- renderUI(
    list(
      box(title = paste0("Top ",input$top.n.values, " Authors connected if books written are on the same category"),
          renderPlot(plot.similar.category.network(input$year.range, input$top.n.values, input$switch.value)),
          if(input$switch.value == TRUE) {
            renderPrint(plot.similar.category.network(input$year.range, input$top.n.values, input$switch.value))
          },
          "The Top N Authors are choosen by the number of books they have written."
      ),
      box(title = paste0("Top ",input$top.n.values, " Authors connected if authors have a similar rating"),
          renderPlot(plot.similar.rating.network(input$year.range, input$top.n.values, input$switch.value)),
          if(input$switch.value == TRUE) {
            renderPrint(plot.similar.rating.network(input$year.range, input$top.n.values, input$switch.value))
          },
          "The Top N Authors are choosen by the number of books they have written.",
          "The similar rating was done through getting the average rating of each author by averaging all the ratings of their books. Then, these ratings were put into classes for a more efficient analysis."
      ),
      box(title = paste0("Top ",input$top.n.values, " Authors connected if co-written a book"),
          renderPlot(plot.co.authors.network(input$year.range, input$top.n.values, input$switch.value)),
          if(input$switch.value == TRUE) {
            renderPrint(plot.co.authors.network(input$year.range, input$top.n.values, input$switch.value))
          },
          "The Top N Authors are choosen by their rating."
      )
    )
  )
  
  output$author <- renderUI(
    list(
      list(
        infoBox(
          "Averate rating", author.avg.rank(input$author.name, input$year.range), width = 4
        ),
        infoBox(
          "Number of books", author.n.books(input$author.name, input$year.range), width = 4
        ),
        infoBox(
          "Top category", author.top.category(input$author.name, input$year.range), width = 4
        ),
        box(title = paste0(input$author.name, "'s books"),
            renderPlot(plot.author.to.books.network(input$author.name, input$year.range))
        ),
        box(title = paste0(input$author.name, "'s categories"),
            renderPlot(plot.author.to.categories.network(input$author.name, input$year.range))
        ),
        box(title = paste0("Connects ", input$author.name, " with others that have the same top category"),
            renderPlot(plot.similar.category.authors(input$author.name, input$year.range, input$top.n.values)),
            "The top category was obtained by counting the number of books on each one and choosing the one with the highest value.",
        ),
        box(title = paste0("Connects ", input$author.name, " with others that have a similar rating"),
            renderPlot(plot.similar.rank.authors(input$author.name, input$year.range, input$top.n.values)),
            "The similar rating was done through getting the average rating of each author by averaging all the ratings of their books. Then, these ratings were put into classes for a more efficient analysis.",
            "The classes are range from 0 until 5 with a width of 0.5."
        ),
        box(title = paste0("Connects ", input$author.name, " with others with whom he/she has co-written a book"),
            renderPlot(plot.co.authors(input$author.name, input$year.range, input$switch.value)),
            if(input$switch.value == TRUE) {
              renderPrint(plot.co.authors(input$author.name, input$year.range, input$switch.value))
            }
        )
      )
    )
  )
  observe({
    updateSelectInput(
      session,
      inputId = "author.name",
      choices = get.unique.authors()
    )
    updateSelectInput(
      session,
      inputId = "category.name",
      choices = get.unique.categories()
    )
  })
  
}

server