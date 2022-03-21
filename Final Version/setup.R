# The link for the Shiny App is https://rfilho50530.shinyapps.io/BookAnalysisProject/

library(data.table)   
library(ggplot2)
library(igraph)
library(shiny)
library(openxlsx)
library(hash)
library(randomcoloR)
library(dplyr)
library(stringr)
library(shinydashboard)

dt.books <- fread("books.csv")
dt.books <- data.frame(dt.books[, c("title", "authors", "categories", "published_year", "average_rating")])

#DATA CLEANING

# Reduce number of categories -> assume top10 categories in terms of amount of books published
dt.books2 <- data.table(dt.books)
dt.books.categories <- dt.books2[, list(n_cat = .N), by = categories][order(-n_cat)][1:50]
dt.books.clean <- dt.books[dt.books$categories %in% dt.books.categories[, categories], ]

# Normalize title and authors names
dt.books.clean[, "title"] = str_to_title(dt.books.clean[, "title"])
dt.books.clean[, "authors"] = str_to_title(dt.books.clean[, "authors"])

# Separate authors 1 by 1 -> at maximum there is a book with 13 authors
dt.books.clean['authors1'] = as.character(lapply(strsplit(as.character(dt.books.clean[, "authors"]),
                                                          split = ";"), "[", 1))
dt.books.clean['authors2'] = as.character(lapply(strsplit(as.character(dt.books.clean[, "authors"]), 
                                                          split = ";"), "[", 2))
dt.books.clean['authors3'] = as.character(lapply(strsplit(as.character(dt.books.clean[, "authors"]), 
                                                          split = ";"), "[", 3))
dt.books.clean['authors4'] = as.character(lapply(strsplit(as.character(dt.books.clean[, "authors"]), 
                                                          split = ";"), "[", 4))
dt.books.clean['authors5'] = as.character(lapply(strsplit(as.character(dt.books.clean[, "authors"]), 
                                                          split = ";"), "[", 5))
dt.books.clean['authors6'] = as.character(lapply(strsplit(as.character(dt.books.clean[, "authors"]), 
                                                          split = ";"), "[", 6))
dt.books.clean['authors7'] = as.character(lapply(strsplit(as.character(dt.books.clean[, "authors"]), 
                                                          split = ";"), "[", 7))
dt.books.clean['authors8'] = as.character(lapply(strsplit(as.character(dt.books.clean[, "authors"]), 
                                                          split = ";"), "[", 8))
dt.books.clean['authors9'] = as.character(lapply(strsplit(as.character(dt.books.clean[, "authors"]), 
                                                          split = ";"), "[", 9))
dt.books.clean['authors10'] = as.character(lapply(strsplit(as.character(dt.books.clean[, "authors"]), 
                                                           split = ";"), "[", 10))
dt.books.clean['authors11'] = as.character(lapply(strsplit(as.character(dt.books.clean[, "authors"]), 
                                                           split = ";"), "[", 11))
dt.books.clean['authors12'] = as.character(lapply(strsplit(as.character(dt.books.clean[,"authors"]), 
                                                           split = ";"), "[", 12))
dt.books.clean['authors13'] = as.character(lapply(strsplit(as.character(dt.books.clean[, "authors"]), 
                                                           split = ";"), "[", 13))

#Remove original authors column
dt.books.clean <- subset(dt.books.clean, select = -authors)

#Create data sets with different authors to joined them in only one column
authors1 <- data.frame(dt.books.clean
                       [, c("title", "categories", "published_year", "average_rating", "authors1")])
colnames(authors1) <- c('title', 'categories', 'published_year', 'average_rating', 'authors')

authors2 <- data.frame(dt.books.clean
                       [, c("title", "categories", "published_year", "average_rating", "authors2")])
colnames(authors2) <- c('title', 'categories', 'published_year', 'average_rating', 'authors')

authors3 <- data.frame(dt.books.clean
                       [, c("title", "categories", "published_year", "average_rating", "authors3")])
colnames(authors3) <- c('title', 'categories', 'published_year', 'average_rating', 'authors')

authors4 <- data.frame(dt.books.clean
                       [, c("title", "categories", "published_year", "average_rating", "authors4")])
colnames(authors4) <- c('title', 'categories', 'published_year', 'average_rating', 'authors')

authors5 <- data.frame(dt.books.clean
                       [, c("title", "categories", "published_year", "average_rating", "authors5")])
colnames(authors5) <- c('title', 'categories', 'published_year', 'average_rating', 'authors')

authors6 <- data.frame(dt.books.clean
                       [, c("title", "categories", "published_year", "average_rating", "authors6")])
colnames(authors6) <- c('title', 'categories', 'published_year', 'average_rating', 'authors')

authors7 <- data.frame(dt.books.clean
                       [, c("title", "categories", "published_year", "average_rating", "authors7")])
colnames(authors7) <- c('title', 'categories', 'published_year', 'average_rating', 'authors')

authors8 <- data.frame(dt.books.clean
                       [, c("title", "categories", "published_year", "average_rating", "authors8")])
colnames(authors8) <- c('title', 'categories', 'published_year', 'average_rating', 'authors')

authors9 <- data.frame(dt.books.clean
                       [, c("title", "categories", "published_year", "average_rating", "authors9")])
colnames(authors9) <- c('title', 'categories', 'published_year', 'average_rating', 'authors')

authors10 <- data.frame(dt.books.clean
                        [, c("title", "categories", "published_year", "average_rating", "authors10")])
colnames(authors10) <- c('title', 'categories', 'published_year', 'average_rating', 'authors')

authors11 <- data.frame(dt.books.clean
                        [, c("title", "categories", "published_year", "average_rating", "authors11")])
colnames(authors11) <- c('title', 'categories', 'published_year', 'average_rating', 'authors')

authors12 <- data.frame(dt.books.clean
                        [, c("title", "categories", "published_year", "average_rating", "authors12")])
colnames(authors12) <- c('title', 'categories', 'published_year', 'average_rating', 'authors')

authors13 <- data.frame(dt.books.clean
                        [, c("title", "categories", "published_year", "average_rating", "authors13")])
colnames(authors13) <- c('title', 'categories', 'published_year', 'average_rating', 'authors')

authors <- rbind(authors1, authors2, authors3, authors4,authors5,
                 authors6, authors7, authors8, authors9, authors10,
                 authors11, authors12, authors13, fill = TRUE)

# Remove the fill row
n <- nrow(authors)
authors <- authors[1:(n - 1), ]

# Omit NA values e Distinct
dt.books.clean <- na.omit(authors)
dt.books.clean <- distinct(dt.books.clean[, c(colnames(dt.books.clean))])

# Deal with Special characters
writexl::write_xlsx(dt.books.clean, "dt.books.clean.xlsx")

dt.books <- read.xlsx("dt.books.clean.xlsx") 
dt.books <- na.omit(dt.books)

# Creating the column avg_rating_class that tells for each BOOK the rating that they are in
dt.books$avg_rating_class <- with(dt.books, ifelse(average_rating >= 0 & average_rating < 0.5, "[0-0.5[", ifelse(average_rating >= 0.5 & average_rating < 1, "[0.5 -1[", ifelse(average_rating >= 1 & average_rating < 1.5, "[1-1.5[", ifelse(average_rating >= 1.5 & average_rating < 2, "[1.5-2[", ifelse(average_rating >= 2 & average_rating <= 2.5, "[2-2.5]", ifelse(average_rating >= 2.5 & average_rating <= 3, "[2.5-3[", ifelse(average_rating >= 3 & average_rating <= 3.5, "[3-3.5[", ifelse(average_rating >= 3.5 & average_rating <= 4,  "[3.5-4[", ifelse(average_rating >= 4 & average_rating <= 4.5,  "[4-4.5[", ifelse(average_rating >= 4.5 & average_rating <= 5,  "[4.5-5]", "NA")))))))))))

# Creating the column avg_rating_individual that tells for each AUTHOR the rating that they are in (by doing the average of all their books)
average.ratings <- vector()
for (author in dt.books$authors) {
  the_author <- dt.books[dt.books$authors %in% author, ]
  average_rating_author <- mean(the_author$average_rating)
  average.ratings <- c(average.ratings, average_rating_author)
}
dt.books$avg_rating_individual <- average.ratings

# Creating the column avg_rating_individual_class that tells for each AUTHOR the rating that they are in (same as above but with new column)
dt.books$avg_rating_individual_class <- with(dt.books, ifelse(avg_rating_individual >= 0 & avg_rating_individual < 0.5, "[0-0.5[", ifelse(avg_rating_individual >= 0.5 & avg_rating_individual < 1, "[0.5 -1[", ifelse(avg_rating_individual >= 1 & avg_rating_individual < 1.5, "[1-1.5[", ifelse(avg_rating_individual >= 1.5 & avg_rating_individual < 2, "[1.5-2[", ifelse(avg_rating_individual >= 2 & avg_rating_individual <= 2.5, "[2-2.5]", ifelse(avg_rating_individual >= 2.5 & avg_rating_individual <= 3, "[2.5-3[", ifelse(avg_rating_individual >= 3 & avg_rating_individual <= 3.5, "[3-3.5[", ifelse(avg_rating_individual >= 3.5 & avg_rating_individual <= 4,  "[3.5-4[", ifelse(avg_rating_individual >= 4 & avg_rating_individual <= 4.5,  "[4-4.5[", ifelse(avg_rating_individual >= 4.5 & avg_rating_individual <= 5,  "[4.5-5]", "NA")))))))))))

main.categories <- vector()
for (author in dt.books$author) {
  the_author <- dt.books[dt.books$authors %in% author, ]
  main.category <- names(sort(table(the_author$categories), decreasing = TRUE)[1])
  main.categories <- c(main.categories, main.category)
}
dt.books$main_category <- main.categories

n.books <- vector()

for (author in dt.books$authors) {
  the_author <- dt.books[dt.books$authors %in% author, ]
  #the.author.unique <- the_author[!duplicated(the_author$title), ]
  n.books.author <- nrow(the_author)
  n.books <- c(n.books, n.books.author)
}
dt.books$n_books <- n.books

# Creating the column n_books_class that tells for each AUTHOR the class of number of books they are in
dt.books$n_books_class <- with(dt.books, ifelse(n.books >= 0 & n.books < 2, "[0-2[", ifelse(n.books >= 2 & n.books < 5, "[2-5[", ifelse(n.books >= 5 & n.books < 10, "[5 -10[", ifelse(n.books >= 10 & n.books < 15, "[10-15[", ifelse(n.books >= 15 & n.books < 20, "[15-20[", ifelse(n.books >= 20 & n.books <= 25, "[20-25[", ifelse(n.books >= 25 & n.books <= 30, "[25-30[", ifelse(n.books >= 30 & n.books <= 35, "[30-35[", ifelse(n.books >= 35 & n.books <= 40, "[35-40[", ifelse(n.books >= 40 & n.books <= 45, "[40-45]", "NA")))))))))))

save(dt.books, file = "books.RData")
