library(data.table)   
library(ggplot2)
library(igraph)
library(shiny)

dt.books <- read.csv("books.csv") 
dt.books <- dt.books[,c("title", "authors", "categories", "published_year", "average_rating")]

dt.books$avg_rating_class <- with(dt.books, ifelse(average_rating >= 0 & average_rating < 0.5, "[0-0.5[", ifelse(average_rating >= 0.5 & average_rating < 1, "[0.5 -1[", ifelse(average_rating >= 1 & average_rating < 1.5, "[1-1.5[", ifelse(average_rating >= 1.5 & average_rating < 2, "[1.5-2[", ifelse(average_rating >= 2 & average_rating <= 2.5, "[2-2.5]", ifelse(average_rating >= 2.5 & average_rating <= 3, "[2.5-3[", ifelse(average_rating >= 3 & average_rating <= 3.5, "[3-3.5[", ifelse(average_rating >= 3.5 & average_rating <= 4,  "[3.5-4[", ifelse(average_rating >= 4 & average_rating <= 4.5,  "[4-4.5[", ifelse(average_rating >= 4.5 & average_rating <= 5,  "[4.5-5]", "NA")))))))))))

save(dt.books, file="books.RData") 

