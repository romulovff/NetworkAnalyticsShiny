library(data.table)   
library(ggplot2)
library(igraph)

dt.books <- read.csv("books.clean.csv") 

# Creating the column avg_rating_class that tells for each BOOK the rating that they are in
dt.books$avg_rating_class <- with(dt.books, ifelse(average_rating >= 0 & average_rating < 0.5, "[0-0.5[", ifelse(average_rating >= 0.5 & average_rating < 1, "[0.5 -1[", ifelse(average_rating >= 1 & average_rating < 1.5, "[1-1.5[", ifelse(average_rating >= 1.5 & average_rating < 2, "[1.5-2[", ifelse(average_rating >= 2 & average_rating <= 2.5, "[2-2.5]", ifelse(average_rating >= 2.5 & average_rating <= 3, "[2.5-3[", ifelse(average_rating >= 3 & average_rating <= 3.5, "[3-3.5[", ifelse(average_rating >= 3.5 & average_rating <= 4,  "[3.5-4[", ifelse(average_rating >= 4 & average_rating <= 4.5,  "[4-4.5[", ifelse(average_rating >= 4.5 & average_rating <= 5,  "[4.5-5]", "NA")))))))))))

# Creating the column avg_rating_individual that tells for each AUTHOR the rating that they are in (by doing the average of all their books)
average.ratings <- vector()

for (author in dt.books$authors) {
  the_author <- dt.books[dt.books$authors == author,]
  average_rating_author <- mean(the_author$average_rating)
  average.ratings <- c(average.ratings, average_rating_author)
}

dt.books$avg_rating_individual <- average.ratings

# Creating the column avg_rating_individual_class that tells for each AUTHOR the rating that they are in (same as above but with new column)
dt.books$avg_rating_individual_class <- with(dt.books, ifelse(avg_rating_individual >= 0 & avg_rating_individual < 0.5, "[0-0.5[", ifelse(avg_rating_individual >= 0.5 & avg_rating_individual < 1, "[0.5 -1[", ifelse(avg_rating_individual >= 1 & avg_rating_individual < 1.5, "[1-1.5[", ifelse(avg_rating_individual >= 1.5 & avg_rating_individual < 2, "[1.5-2[", ifelse(avg_rating_individual >= 2 & avg_rating_individual <= 2.5, "[2-2.5]", ifelse(avg_rating_individual >= 2.5 & avg_rating_individual <= 3, "[2.5-3[", ifelse(avg_rating_individual >= 3 & avg_rating_individual <= 3.5, "[3-3.5[", ifelse(avg_rating_individual >= 3.5 & avg_rating_individual <= 4,  "[3.5-4[", ifelse(avg_rating_individual >= 4 & avg_rating_individual <= 4.5,  "[4-4.5[", ifelse(avg_rating_individual >= 4.5 & avg_rating_individual <= 5,  "[4.5-5]", "NA")))))))))))
