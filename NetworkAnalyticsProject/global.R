library(data.table)
library(igraph)
library(ggplot2)
library(shiny)
library(dplyr)
library(hash)
library(randomcoloR)

author.avg.rank <- function(author.name, year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.author <- dt.books.range[dt.books.range$authors == author.name,]
  average_rating_author <- mean(dt.author$average_rating)
  round(average_rating_author,3)
}

get.unique.authors <- function() {
  dt.authors.books <- dt.books %>% count(authors, sort = TRUE)
  unique(dt.authors.books$authors)
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

get.clusteringcoef <- function(g) {
  transitivity(g, type = "average")
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

plot.author.to.books.network <- function(author.name, year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.author <- dt.books.range[dt.books.range$authors == author.name, ]
  dt.author.books <- dt.author[, c("title", "authors")][2:1]
  dt.author.books <- dt.author.books[!duplicated(dt.author.books), ]
  g.author.to.books.network <- graph.data.frame(dt.author.books, directed = TRUE)
  plot(g.author.to.books.network)
}

# Aqui talvez fizesse sentido a introdu??o de pesos
plot.author.to.categories.network <- function(author.name, year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.author <- dt.books.range[dt.books.range$authors == author.name, ]
  dt.author.categories <- dt.author[, c("categories", "authors")][2:1]
  # No caso de se introduzir pesos, aqui n?o se remove duplicados
  dt.author.categories <- dt.author.categories[!duplicated(dt.author.categories), ]
  g.author.to.categories.network <- graph.data.frame(dt.author.categories, directed = TRUE)
  plot(g.author.to.categories.network)
}

plot.similar.rank.authors <- function(author.name, year.range, top.n.values) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.author <- dt.books.range[dt.books.range$authors == author.name, ]
  author.class <- dt.author$avg_rating_individual_class[1]
  authors.similar.class <- unique(dt.books.range[dt.books.range$avg_rating_individual_class == author.class, ]$authors)
  authors.similar.class <- authors.similar.class[authors.similar.class != author.name]
  dt.author.similar.ranking <- cbind(authors = authors.similar.class, author = author.name)[1:top.n.values,]
  g.books.ranking <- graph.data.frame(dt.author.similar.ranking, directed = FALSE)
  plot(g.books.ranking)
}

plot.similar.category.authors <- function(author.name, year.range, top.n.values) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.author <- dt.books.range[dt.books.range$authors == author.name, ]
  author.main.category <- dt.author$main_category[1]
  authors.similar.category <- unique(dt.books.range[dt.books.range$main_category == author.main.category, ]$authors)
  authors.similar.category <- authors.similar.category[authors.similar.category != author.name]
  dt.author.similar.category <- cbind(authors = authors.similar.category, author = author.name)[1:top.n.values,]
  g.books.category<- graph.data.frame(dt.author.similar.category, directed = FALSE)
  plot(g.books.category)
}

plot.similar.rank.category.authors <- function(author.name, year.range, top.n.values) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.author <- dt.books.range[dt.books.range$authors == author.name, ]
  author.class <- dt.author$avg_rating_individual_class[1]
  author.main.category <- dt.author$main_category[1]
  authors.similar.class <- unique(dt.books.range[dt.books.range$avg_rating_individual_class == author.class, ]$authors)
  authors.similar.class <- authors.similar.class[authors.similar.class != author.name]
  authors.similar.category <- unique(dt.books.range[dt.books.range$main_category == author.main.category, ]$authors)
  authors.similar.category <- authors.similar.category[authors.similar.category != author.name]
  authors.similar.class.category <- c(authors.similar.class, authors.similar.category)
  #s? aparecem 30 autores
  dt.authors.similar.class.category <- cbind(authors = authors.similar.class.category, author = author.name)[1:top.n.values,]
  g.books.class.category<- graph.data.frame(dt.authors.similar.class.category, directed = FALSE)
  plot(g.books.class.category)
}

# OVERALL NETWORKS

plot.similar.category.network <- function(year.range, top.n.values, switch.value) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.books.range$colors <- unname(setNames(dt.colors[,2], dt.colors[,1])[as.character(dt.books.range$categories)])
  dt.top.50.authors.published <- as.data.table(dt.books.range)
  dt.top.50.authors.published <- dt.top.50.authors.published[, n_books := .N, by="authors"]
  top.50.authors.published <- unique(dt.top.50.authors.published[order(-n_books)]$authors)[1:top.n.values]
  dt.50.authors.published <- dt.books.range[dt.top.50.authors.published$authors %in% top.50.authors.published, ]
  dt.authors <- data.table(authors = unique(dt.50.authors.published$authors), type= TRUE)
  dt.categories <- data.table(authors = unique(dt.50.authors.published$categories), type= FALSE)
  dt.vertices <- rbind(dt.authors, dt.categories)
  g <- graph.data.frame(dt.50.authors.published[c("authors","categories")], directed = FALSE, vertices = dt.vertices)
  g.categories <<- bipartite.projection(g)$proj2
  print(switch.value)
  plot(g.categories, edge.color=dt.books.range$colors)
  save(g.categories, file="books.RData") 
  if(switch.value == TRUE){
    text(-1.5, 1.5, paste("Average clustering coefficient: ",get.clusteringcoef(g.categories)), cex = 0.65, col = "black")
  }
  legend(x = "bottomleft",
         inset = c(-0.15, 0),
         legend = c(keys(dict.colors.categories)) , 
         lty = c(1, 2),
         col = c(values(dict.colors.categories)),
         lwd = 2,
         pch=20,
         cex = 0.7,
         bty = "n",
         xpd = TRUE)
  #legend("bottomleft", legend=c(keys(dict.colors.categories))  , col = c(values(dict.colors.categories)) , bty = "n", pch=20 , pt.cex = 1, cex = 0.7, text.col="black", inset=c(0,0), xpd=TRUE)
}

plot.similar.rating.network <- function(year.range, top.n.values, switch.value) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.books.range$colors <- unname(setNames(dt.colors.avg.rating[,2], dt.colors.avg.rating[,1])[as.character(dt.books.range$avg_rating_class)])
  dt.top.50.authors.published <- as.data.table(dt.books.range)
  dt.top.50.authors.published <- dt.top.50.authors.published[, n_books := .N, by="authors"]
  top.50.authors.published <- unique(dt.top.50.authors.published[order(-n_books)]$authors)[1:top.n.values]
  dt.50.authors.published <- dt.books.range[dt.top.50.authors.published$authors %in% top.50.authors.published, ]
  dt.authors <- data.table(authors = unique(dt.50.authors.published$authors), type= TRUE)
  dt.rating <- data.table(authors = unique(dt.50.authors.published$avg_rating_class), type= FALSE)
  dt.vertices <- rbind(dt.authors, dt.rating)
  g <- graph.data.frame(dt.50.authors.published[c("authors","avg_rating_class")], directed = FALSE, vertices = dt.vertices)
  g.rating <<- bipartite.projection(g)$proj2
  plot(g.rating, edge.color=dt.books.range$colors)
  if(switch.value == TRUE){
    text(-1.5, 1.5, paste("Average clustering coefficient: ",get.clusteringcoef(g.rating)), cex = 0.65, col = "black")
  }
  legend(x = "bottomleft",
         inset = c(-0.15, 0),
         legend = c(keys(dict.colors.rating)) , 
         lty = c(1, 2),
         col = c(values(dict.colors.rating)),
         lwd = 2,
         pch=20,
         cex = 0.7,
         bty = "n",
         xpd = TRUE)
  g.rating
  #legend("bottom", legend=c(keys(dict.colors.rating))  , col = c(values(dict.colors.rating)) , bty = "n", pch=20 , pt.cex = 1, cex = 0.7, text.col="black", horiz=T , inset=c(0, -.15), xpd=TRUE)
}

plot.co.authors.network <- function(top.n.values, switch.value) {
  dt.co.authors <- as.data.table(distinct(dt.books[,c("title","authors","categories","published_year","average_rating")]))
  dt.co.authors[, n_coauthors := .N-1, by = list(title, published_year, average_rating)]
  dt.co.authors <- dt.co.authors[dt.co.authors$n_coauthors != 0 & dt.co.authors$title %in% unique(unique(dt.co.authors, by=c("title","categories","published_year"))[order(-average_rating)][1:top.n.values]$title)]
  dt.cowritten.books <- dt.co.authors[, list(title = unique(title), type = FALSE)]
  dt.cowritters <- dt.co.authors[, list(title = unique(authors), type = TRUE)]
  dt.vertices <- rbind(dt.cowritten.books, dt.cowritters)
  g <- graph.data.frame(dt.co.authors, directed = FALSE, vertices = dt.vertices)
  g.coauthors <<- bipartite.projection(g)$proj2
  save(g.coauthors, file="books.RData") 
  plot(g.coauthors)
  if(switch.value == TRUE){
    text(-1.5, 1.5, paste("Average clustering coefficient: ",get.clusteringcoef(g.coauthors)), cex = 0.65, col = "black")
  }
}
  
  load("books.RData")


