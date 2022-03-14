dt.authors <- dt.books[, list(unique(authors), type = FALSE)]
dt.categories <- dt.books[, list(name = unique(categories), type = TRUE)]
dt.vertices <- rbind(dt.authors, dt.categories)
g <- graph.data.frame(dt.books[, list(authors, categories)], directed = FALSE, vertices = dt.vertices)
g.categories <- bipartite.projection(g)$proj2