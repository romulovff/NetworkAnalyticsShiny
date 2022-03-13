library(data.table)
library(igraph)
library(ggplot2)
library(shiny)
library(dplyr)

author.avg.rank <- function(author.name, year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.author <- dt.books.range[dt.books.range$authors == author.name,]
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

get.authors.count.year.range <- function(year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  length(unique(dt.books.range$authors))
}

get.distinct.author.per.category <- function(year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.category.number <- aggregate(data = dt.books.range,
                            authors ~ categories,
                            function(authors) length(unique(authors)))
  dt.category.number[order(-dt.category.number$authors),]
}

plot.books.published.by.year <- function(year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  ggplot(dt.books.range, aes(x=published_year)) + geom_bar() + ggtitle('Books per Year')
}

plot.distinct.authors.by.year <- function(year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.author.number <- aggregate(data = dt.books.range,
                                  authors ~ published_year,
                                  function(authors) length(unique(authors)))
  ggplot(dt.author.number, aes(x=published_year, y=authors)) + geom_bar(stat="identity") + ggtitle('Distinct Authors per Year')
}

plot.books.by.ranking <- function(year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  ggplot(dt.books.range,aes(x=average_rating)) + geom_histogram() +  ggtitle('Books per Ranking')
}

plot.books.by.authornumber <- function(year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.authors.number <- dt.books.range %>% count(title, sort = TRUE)
  ggplot(dt.authors.number,aes(x=n)) + geom_histogram() +  ggtitle('Books per Number of Authors')
}

load("books.RData")


