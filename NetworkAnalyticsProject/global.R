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

## INDIVIDUAL NETWORKS

plot.author.to.books.network <- function(author.name) {
  dt.author <- dt.books[dt.books$authors == author.name, ]
  dt.author.books <- dt.author[, c("title", "authors")][2:1]
  dt.author.books <- dt.author.books[!duplicated(dt.author.books), ]
  g.author.to.books.network <- graph.data.frame(dt.author.books, directed = TRUE)
  plot(g.author.to.books.network)
}

# Aqui talvez fizesse sentido a introdução de pesos
plot.author.to.categories.network <- function(author.name) {
  dt.author <- dt.books[dt.books$authors == author.name, ]
  dt.author.categories <- dt.author[, c("categories", "authors")][2:1]
  # No caso de se introduzir pesos, aqui não se remove duplicados
  dt.author.categories <- dt.author.categories[!duplicated(dt.author.categories), ]
  g.author.to.categories.network <- graph.data.frame(dt.author.categories, directed = TRUE)
  plot(g.author.to.categories.network)
}

plot.similar.rank.authors <- function(author.name) {
  dt.author <- dt.books[dt.books$authors == author.name, ]
  author.class <- dt.author$avg_rating_individual_class[1]
  authors.similar.class <- unique(dt.books[dt.books$avg_rating_individual_class == author.class, ]$authors)
  authors.similar.class <- authors.similar.class[authors.similar.class != author.name]
  #só aparecem 30 autores
  dt.author.similar.ranking <- cbind(authors = authors.similar.class, author = author.name)[1:30,]
  g.books.ranking <- graph.data.frame(dt.author.similar.ranking, directed = FALSE)
  plot(g.books.ranking)
}

plot.similar.category.authors <- function(author.name) {
  dt.author <- dt.books[dt.books$authors == author.name, ]
  author.main.category <- dt.author$main_category[1]
  authors.similar.category <- unique(dt.books[dt.books$main_category == author.main.category, ]$authors)
  authors.similar.category <- authors.similar.category[authors.similar.category != author.name]
  #só aparecem 30 autores
  dt.author.similar.category <- cbind(authors = authors.similar.category, author = author.name)[1:30,]
  g.books.category<- graph.data.frame(dt.author.similar.category, directed = FALSE)
  plot(g.books.category)
}

plot.similar.rank.category.authors <- function(author.name) {
  dt.author <- dt.books[dt.books$authors == author.name, ]
  author.class <- dt.author$avg_rating_individual_class[1]
  author.main.category <- dt.author$main_category[1]
  authors.similar.class <- unique(dt.books[dt.books$avg_rating_individual_class == author.class, ]$authors)
  authors.similar.class <- authors.similar.class[authors.similar.class != author.name]
  authors.similar.category <- unique(dt.books[dt.books$main_category == author.main.category, ]$authors)
  authors.similar.category <- authors.similar.category[authors.similar.category != author.name]
  authors.similar.class.category <- c(authors.similar.class, authors.similar.category)
  #só aparecem 30 autores
  dt.authors.similar.class.category <- cbind(authors = authors.similar.class.category, author = author.name)[1:30,]
  g.books.class.category<- graph.data.frame(dt.authors.similar.class.category, directed = FALSE)
  plot(g.books.class.category)
}

# OVERALL NETWORKS

plot.similar.category.network <- function() {
  dt.top.50.authors.published <- as.data.table(dt.books)
  dt.top.50.authors.published <- dt.top.50.authors.published[, n_books := .N, by="authors"]
  top.50.authors.published <- unique(dt.top.50.authors.published[order(-n_books)]$authors)[1:30]
  dt.50.authors.published <- dt.books[dt.top.50.authors.published$authors %in% top.50.authors.published, ]
  dt.authors <- data.table(authors = unique(dt.50.authors.published$authors), type= TRUE)
  dt.categories <- data.table(authors = unique(dt.50.authors.published$categories), type= FALSE)
  dt.vertices <- rbind(dt.authors, dt.categories)
  g <- graph.data.frame(dt.50.authors.published[c("authors","categories")], directed = FALSE, vertices = dt.vertices)
  g.categories <- bipartite.projection(g)$proj2
  plot(g.categories)
}

load("books.RData")


