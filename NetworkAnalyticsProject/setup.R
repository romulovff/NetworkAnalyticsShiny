library(data.table)   
library(ggplot2)
library(igraph)
library(shiny)
# library(readxl)
library(openxlsx)

dt.books <- read.xlsx("dt.books.clean.xlsx") 
dt.books <- dt.books[,c("title", "authors", "categories", "published_year", "average_rating")]

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
  main.category <- names(sort(table(the_author$categories), decreasing=TRUE)[1])
  main.categories <- c(main.categories, main.category)
}
dt.books$main_category <- main.categories

set.seed(150)
dict.colors.categories <- hash()
for (category in unique(dt.books$categories)) {
  dict.colors.categories[[category]] <- as.character(randomColor(luminosity="bright"))
}
dt.colors <- data.frame(Col1=c(keys(dict.colors.categories)), Col2=c(values(dict.colors.categories)),
                 stringsAsFactors=FALSE)

dict.colors.rating <- hash()
for (rating_class in unique(dt.books$avg_rating_class)) {
  dict.colors.rating[[rating_class]] <- as.character(randomColor(luminosity="bright"))
}
dt.colors.avg.rating <- data.frame(Col1=c(keys(dict.colors.rating)), Col2=c(values(dict.colors.rating)),
                        stringsAsFactors=FALSE)

save(dt.colors, file="books.RData") 
save(dt.books, file="books.RData") 

