# The link for the Shiny App is https://rfilho50530.shinyapps.io/BookAnalysisProject/

library(data.table)
library(igraph)
library(ggplot2)
library(shiny)
library(dplyr)
library(hash)
library(randomcoloR)

author.avg.rank <- function(author.name) {
  dt.author <- dt.books[dt.books$authors == author.name, ]
  average_rating_author <- mean(dt.author$average_rating)
  round(average_rating_author, 3)
}

author.n.books <- function(author.name) {
  dt.author <- dt.books[dt.books$authors == author.name, ]
  n_books <- nrow(dt.author)
}

author.top.category <- function(author.name) {
  dt.author <- dt.books[dt.books$authors == author.name, ]
  n_books <- dt.author$main_category[1]
}

get.unique.authors <- function() {
  dt.authors.books <- dt.books %>% count(authors, sort = TRUE)
  unique(dt.authors.books$authors)
}

get.unique.categories <- function() {
  dt.categories.books <- dt.books %>% count(categories, sort = TRUE)
  unique(dt.categories.books$categories)
}

get.authors.most.books <- function(number.authors, year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.authors.books <- dt.books.range %>% count(authors, sort = TRUE)
  dt.authors.books[1 : number.authors, ]
}

get.average.book.rating <- function(year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  round(mean(dt.books.range$average_rating, na.rm = TRUE), 3)
}

get.categories.most.books <- function(number.categories, year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.categories.books <- dt.books.range %>% count(categories, sort = TRUE)
  dt.categories.books[1 : number.categories, ]
}

get.books.highest.rating <- function(number.books, year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.books.range <- dt.books.range[,c("title", "authors", "categories", "published_year", "average_rating")]
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

get.distinct.author.per.category <- function(year.range, top.n.values) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.category.number <- aggregate(data = dt.books.range,
                            authors ~ categories,
                            function(authors) length(unique(authors)))
  dt.category.number[order(-dt.category.number$authors), ][1:top.n.values, ]
}

get.clusteringcoef <- function(g) {
  transitivity(g, type = "average")
}

get.degree.centrality <- function(g) {
  degrees <- degree(g)
  final.str <- ""
  for (deg in degrees) {
    final.str <- paste(final.str, deg, " ")
  }
  final.str
}

plot.books.published.by.year <- function(year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  ggplot(dt.books.range, aes(x = published_year)) + geom_bar() + labs(y = "Number of books", x = "Year")
}

plot.distinct.authors.by.year <- function(year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.author.number <- aggregate(data = dt.books.range,
                                  authors ~ published_year,
                                  function(authors) length(unique(authors)))
  ggplot(dt.author.number, aes(x = published_year, y = authors)) + geom_bar(stat = "identity") + labs(y = "Number of authors", x = "Year")
}

plot.books.by.ranking <- function(year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  ggplot(dt.books.range,aes(x = average_rating)) + geom_histogram()  + labs(y = "Number of authors", x = "Average Rating")
}

plot.books.by.authornumber <- function(year.range) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.authors.number <- dt.books.range %>% count(title, sort = TRUE)
  ggplot(dt.authors.number,aes(x = n)) + geom_histogram()  + labs(y = "Number of books", x = "Number of authors")
}

## INDIVIDUAL NETWORKS

plot.author.to.books.network <- function(author.name) {
  dt.author <- dt.books[dt.books$authors == author.name, ]
  dt.author.books <- dt.author[, c("title", "authors")][2:1]
  dt.author.books <- dt.author.books[!duplicated(dt.author.books), ]
  g.author.to.books.network <- graph.data.frame(dt.author.books, directed = FALSE)
  plot(g.author.to.books.network)
}

plot.author.to.categories.network <- function(author.name) {
  dt.author <- dt.books[dt.books$authors == author.name, ]
  dt.author.categories <- dt.author[, c("categories", "authors")][2:1]
  dt.author.categories <- dt.author.categories[!duplicated(dt.author.categories), ]
  g.author.to.categories.network <- graph.data.frame(dt.author.categories, directed = FALSE)
  plot(g.author.to.categories.network)
}

plot.similar.rank.authors <- function(author.name, top.n.values) {
  dt.author <- dt.books[dt.books$authors == author.name, ]
  author.class <- dt.author$avg_rating_individual_class[1]
  authors.similar.class <- unique(dt.books[dt.books$avg_rating_individual_class == author.class, ]$authors)
  authors.similar.class <- authors.similar.class[authors.similar.class != author.name]
  dt.author.similar.ranking <- cbind(authors = authors.similar.class, author = author.name)[1:top.n.values, ]
  g.books.ranking <- graph.data.frame(dt.author.similar.ranking, directed = FALSE)
  plot(g.books.ranking)
}

plot.similar.category.authors <- function(author.name, top.n.values) {
  dt.author <- dt.books[dt.books$authors == author.name, ]
  author.main.category <- dt.author$main_category[1]
  authors.similar.category <- unique(dt.books[dt.books$main_category == author.main.category, ]$authors)
  authors.similar.category <- authors.similar.category[authors.similar.category != author.name]
  dt.author.similar.category <- cbind(authors = authors.similar.category, author = author.name)[1:top.n.values, ]
  g.books.category <- graph.data.frame(dt.author.similar.category, directed = FALSE)
  plot(g.books.category)
}

plot.similar.rank.category.authors <- function(author.name, top.n.values) {
  dt.author <- dt.books[dt.books$authors == author.name, ]
  author.class <- dt.author$avg_rating_individual_class[1]
  author.main.category <- dt.author$main_category[1]
  authors.similar.class <- unique(dt.books[dt.books$avg_rating_individual_class == author.class, ]$authors)
  authors.similar.class <- authors.similar.class[authors.similar.class != author.name]
  authors.similar.category <- unique(dt.books[dt.books$main_category == author.main.category, ]$authors)
  authors.similar.category <- authors.similar.category[authors.similar.category != author.name]
  authors.similar.class.category <- c(authors.similar.class, authors.similar.category)
  dt.authors.similar.class.category <- cbind(authors = authors.similar.class.category, author = author.name)[1:top.n.values, ]
  g.books.class.category<- graph.data.frame(dt.authors.similar.class.category, directed = FALSE)
  plot(g.books.class.category)

}

# OVERALL NETWORKS

plot.similar.category.network <- function(year.range, top.n.values, switch.value) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.top.n.authors.published <- as.data.table(dt.books.range)
  dt.top.n.authors.published <- dt.top.n.authors.published[, n_books := .N, by = "authors"]
  top.n.authors.published <- unique(dt.top.n.authors.published[order(-n_books)]$authors)[1:top.n.values]
  dt.n.authors.published <- dt.books.range[dt.top.n.authors.published$authors %in% top.n.authors.published, ]
  dt.authors <- data.table(authors = unique(dt.n.authors.published$authors), type = TRUE)
  dt.categories <- data.table(authors = unique(dt.n.authors.published$categories), type = FALSE)
  dt.vertices <- rbind(dt.authors, dt.categories)
  g <- graph.data.frame(dt.n.authors.published[c("authors", "categories")], directed = FALSE, vertices = dt.vertices)
  g.categories <- bipartite.projection(g)$proj2
  node.degrees <- degree(g.categories)
  max.degree <- max(node.degrees)
  if(switch.value == TRUE) {
    interval = seq(from = 0, to = max.degree, by = 1)
    hist(node.degrees, breaks = interval)
  } else {
    plot(g.categories)
  } 
  print(paste("Average path length: ", round(mean_distance(g.categories), digits = 2)))
  print(paste("Diameter: ", diameter(g.categories)))
  print(paste("Average degree centrality: ", round(mean(node.degrees), digits = 2)))
  print(paste("Average eigenvector centrality: ", centr_eigen(g.categories)$value))
  print(paste("Average clustering coefficient: ", round(get.clusteringcoef(g.categories), digits = 2)))
  
  if (get.clusteringcoef(g.categories) > 0.5) {
    cat("\nThe high value shows that, on average, the graph neighbors are connected. This shows that, since all authors are connected if they have the same top category, the majority of authors have indeed the same top category. This may be because of the popularity of the categories in place.")
  } else {
    cate("\nThe low value shows that, on average, the graph neighbors are not connected. This shows that, since all authors are connected if they have the same top category, the majority of authors have different categories. This may infer that the authors prefer to have their own style of writing rather than following the trends.")
  }
  
}

plot.similar.rating.network <- function(year.range, top.n.values, switch.value) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.top.n.authors.published <- as.data.table(dt.books.range)
  dt.top.n.authors.published <- dt.top.n.authors.published[, n_books := .N, by = "authors"]
  top.n.authors.published <- unique(dt.top.n.authors.published[order(-n_books)]$authors)[1:top.n.values]
  dt.n.authors.published <- dt.books.range[dt.top.n.authors.published$authors %in% top.n.authors.published, ]
  dt.authors <- data.table(authors = unique(dt.n.authors.published$authors), type = TRUE)
  dt.rating <- data.table(authors = unique(dt.n.authors.published$avg_rating_class), type = FALSE)
  dt.vertices <- rbind(dt.authors, dt.rating)
  g <- graph.data.frame(dt.n.authors.published[c("authors", "avg_rating_class")], directed = FALSE, vertices = dt.vertices)
  g.rating <- bipartite.projection(g)$proj2
  node.degrees <- degree(g.rating)
  max.degree <- max(node.degrees)
  if(switch.value == TRUE) {
    interval = seq(from = 0, to = max.degree, by = 1)
    hist(node.degrees, breaks = interval)
  } else {
    plot(g.rating)
  }
  print(paste("Average path length: ", round(mean_distance(g.rating), digits = 2)))
  print(paste("Diameter: ", diameter(g.rating)))
  print(paste("Average degree centrality: ", round(mean(node.degrees), digits = 2)))
  print(paste("Average eigenvector centrality: ", centr_eigen(g.rating)$value))
  print(paste("Average clustering coefficient: ", round(get.clusteringcoef(g.rating), digits = 2)))
  
  if (get.clusteringcoef(g.rating) > 0.5) {
    cat("\nThe high value on the clustering coefficient shows that, on average, the graph neighbors are connected. This shows that, since all authors are connected if they are in the same rating class, the majority of authors belong to the same class. This may be due the way the books are rated.")
  } else {
    cate("\nThe low value on the clustering coefficient shows that, on average, the graph neighbors are not connected. This shows that, since all authors are connected if they are in the same rating class, the majority of authors belong to different classes. This may infer that the rating method is somewhat random.")
  }
  
}

plot.co.authors.network <- function(year.range, top.n.values, switch.value) {
  dt.books.range <- filter(dt.books, published_year >= min(year.range) & published_year <= max(year.range))
  dt.co.authors <- as.data.table(distinct(dt.books.range[, c("title", "authors", "categories", "published_year", "average_rating")]))
  dt.co.authors[, n_coauthors := .N-1, by = list(title, published_year, average_rating)]
  dt.co.authors <- dt.co.authors[dt.co.authors$n_coauthors != 0 & dt.co.authors$title %in% unique(unique(dt.co.authors, by = c("title", "categories", "published_year"))[order(-average_rating)][1:top.n.values]$title)]
  dt.cowritten.books <- dt.co.authors[, list(title = unique(title), type = FALSE)]
  dt.cowritters <- dt.co.authors[, list(title = unique(authors), type = TRUE)]
  dt.vertices <- rbind(dt.cowritten.books, dt.cowritters)
  g <- graph.data.frame(dt.co.authors, directed = FALSE, vertices = dt.vertices)
  g.coauthors <- bipartite.projection(g)$proj2
  node.degrees <- degree(g.coauthors)
  max.degree <- max(node.degrees)
  if(switch.value == TRUE) {
    interval = seq(from = 0, to = max.degree, by = 1)
    hist(node.degrees, breaks = interval)
  } else {
    plot(g.coauthors)
  }

  print(paste("Average path length: ", round(mean_distance(g.coauthors), digits = 2)))
  print(paste("Diameter: ", diameter(g.coauthors)))
  print(paste("Average degree centrality: ", round(mean(node.degrees), digits = 2)))
  print(paste("Average eigenvector centrality: ", centr_eigen(g.coauthors)$value))
  
  
  
}

plot.co.authors <- function(author.name, switch.value) {
  dt.co.authors <- as.data.table(distinct(dt.books[, c("title", "authors", "categories", "published_year", "average_rating")]))
  dt.co.authors[, n_coauthors := .N - 1, by = list(title, published_year, average_rating)]
  dt.co.authors <- dt.co.authors[dt.co.authors$n_coauthors != 0 & dt.co.authors$title %in% unique(unique(dt.co.authors, by = c("title", "categories", "published_year"))$title)]
  dt.co.authors$title <- tolower(dt.co.authors$title)
  dt.cowritten.books <- dt.co.authors[, list(title = unique(title), type = FALSE)]
  dt.cowritters <- dt.co.authors[, list(title = unique(authors), type = TRUE)]
  dt.vertices <- rbind(dt.cowritten.books, dt.cowritters)
  g <- graph.data.frame(dt.co.authors, directed = FALSE, vertices = dt.vertices)
  g.coauthors <- bipartite.projection(g)$proj2
  g.coauthors.author <- induced_subgraph(g.coauthors, ego(g.coauthors, 1, author.name)[[1]])
  node.degrees <- degree(g.coauthors.author)
  max.degree <- max(node.degrees)
  if(switch.value == TRUE) {
    interval = seq(from = 0, to = max.degree, by = 1)
    hist(node.degrees, breaks = interval)
  } else {
    plot(g.coauthors.author)
  }
  print(paste("Average path length: ", round(mean_distance(g.coauthors.author), digits = 2)))
  print(paste("Diameter: ", diameter(g.coauthors.author)))
  print(paste("Average degree centrality: ", round(mean(node.degrees), digits = 2)))
  print(paste("Average eigenvector centrality: ", centr_eigen(g.coauthors.author)$value))
  print(paste("Average clustering coefficient: ", round(get.clusteringcoef(g.coauthors.author), digits = 2)))
  
  if (get.clusteringcoef(g.coauthors.author) > 0.5) {
    cat(paste("\nThe high value shows that, on average, the graph neighbors are connected, meaning that, for example, an author B who wrote with ", author.name, " also wrote with other author C, that also wrote with ", author.name, ". Knowing this, in this case, the co-authors tend to write books with multiple other authors, all in similar circle. This may imply that the choice of an author to co-write a book may be based on, for example, recognition, types of past books in same genre, or recommendation by another author."))
  } else {
    cat(paste("\nThe low value shows that, on average, the graph neighbors are poorly connected, meaning that, for example, an author B who wrote with ", author.name, ", probably has not written with other author C, that also wrote with ", author.name, ". Knowing this, the co-authors tend to write books with one author, so the network may be randomly formed."))
  }
}

# HOMOPHILY
print.homophily <- function(category_name) {
  #All authors
  dt.books <- na.omit(dt.books)
  dt.books <- data.table(dt.books)[sample(.N, 500)]
  dt.books.cat <- dt.books[dt.books$categories == category_name,]
  
  dt.books.unique.restricted <-data.table(dt.books.cat[,list(authors,n_books_class,avg_rating_individual_class)])
  dt.books.cat.unique <- dt.books.unique.restricted[!duplicated(dt.books.unique.restricted$authors), ]
  
  all.authors <- dt.books.cat.unique[,list(name=unique(authors),type=TRUE)]
  all.books <- dt.books.cat.unique[,list(name=unique(n_books_class),type=FALSE)]
  all.vertices <- rbind(all.authors, all.books)
  
  g <- graph.data.frame(dt.books.cat.unique[,list(authors,n_books_class)],directed=FALSE, vertices=all.vertices)
  g.authors.books <- bipartite.projection(g)$proj2
  
  #Obtain list of connections
  edgelist <- get.data.frame(g.authors.books)
  colnames(edgelist) <- c('author1','author2')
  edgelist <- edgelist[,1:2]
  edgelist1 <- edgelist[,1:2]
  author1.rating <- dt.books.cat.unique[authors %in% edgelist[, "author1"]][,list(authors,avg_rating_individual_class)]
  
  rating1 <- vector()
  for (author in edgelist$author1) {
    the_author <- author1.rating[authors %in% author, ]
    rating <- the_author$avg_rating_individual_class[1]
    rating1 <- c(rating1, rating)
  }
  edgelist1$author1_rating <- rating1
  
  #Obtain authors rating
  rating1 <- vector()
  for (author in edgelist$author1) {
    the_author <- dt.books.cat[dt.books.cat$authors %in% author, ]
    rating <- the_author$avg_rating_individual_class[1]
    rating1 <- c(rating1, rating)
  }
  edgelist$author1_rating <- rating1
  
  rating2 <- vector()
  for (author in edgelist$author2) {
    the_author <- dt.books.cat[dt.books.cat$authors %in% author, ]
    rating <- the_author$avg_rating_individual_class[1]
    rating2 <- c(rating2, rating)
  }
  edgelist$author2_rating <- rating2
  
  #See when rating columns are equal
  validation <- edgelist[,3] == edgelist[,4]
  edgelist$validation <- validation
  
  #All invites & Similar gender invites
  all.rows <- length(edgelist$validation)
  rows.true <- edgelist[edgelist$validation == TRUE,]
  equal.rows <- length(rows.true$validation)
  
  #Homophily
  result <- equal.rows/all.rows
  result <- round(result*100, 2)
  print(paste(result, "%"))
}

load("books.RData")


