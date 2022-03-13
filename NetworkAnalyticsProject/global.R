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

get.authors.most.books <- function(number.authors, year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.authors.books <- dt.books.range %>% count(authors, sort = TRUE)
  dt.authors.books[1:number.authors,]
}

get.average.book.rating <- function(year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  round(mean(dt.books.range$average_rating, na.rm = TRUE),3)
}

get.categories.most.books <- function(number.categories, year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.categories.books <- dt.books.range %>% count(categories, sort = TRUE)
  dt.categories.books[1:number.categories,]
}

get.books.highest.rating <- function(number.books, year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.books.range %>% slice_max(average_rating, n = number.books, with_ties = FALSE)
}

get.books.count.year.range <- function(year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  length(unique(dt.books.range$title))
}

plot.books.published.by.year <- function(year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  ggplot(dt.books.range, aes(x=published_year)) + geom_bar() + ggtitle('Books per Year')
}

plot.books.by.ranking <- function(year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  ggplot(dt.books.range,aes(x=average_rating)) + geom_histogram() +  ggtitle('Books per Ranking')
}

load("books.RData")


