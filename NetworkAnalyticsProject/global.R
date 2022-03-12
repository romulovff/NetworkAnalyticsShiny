library(data.table)
library(igraph)
library(ggplot2)
library(shiny)
library(dplyr)

author.avg.rank <- function(author.name) {
  dt.author <- dt.books[dt.books$authors == author.name,]
  average_rating_author <- mean(dt.author$average_rating)
  round(average_rating_author,3)
}

get.unique.authors <- function() {
  unique(dt.books$authors)
}

get.authors.most.books <- function(number.authors) {
  dt.authors.books <- dt.books %>% count(authors, sort = TRUE)
  dt.authors.books[1:number.authors,]
}

get.categories.most.books <- function(number.categories) {
  dt.categories.books <- dt.books %>% count(categories, sort = TRUE)
  dt.categories.books[1:number.categories,]
}

get.books.highest.rating <- function(number.books) {
  dt.books %>% slice_max(average_rating, n = number.books, with_ties = FALSE)
}

plot.books.published.by.year <- function() {
  ggplot(dt.books,aes(x=published_year)) + geom_bar() + ggtitle('Books per Year')
}

plot.books.by.ranking <- function() {
  ggplot(dt.books,aes(x=average_rating)) + geom_histogram() +  ggtitle('Books per Ranking')
}

load("books.RData")


