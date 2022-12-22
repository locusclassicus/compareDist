# min for distances
get.pred.min <- function(data) { 
  predicted <- c()
  for (i in 1:nrow(data)) {
    nearest <- names(which.min(data[i, -i]))
    predicted <- c(predicted, nearest)
  }
  predicted <- factor(sub("_.*", "", predicted))
  return(predicted)
}

# max for similarities
get.pred.max <- function(data) { 
  predicted <- c()
  for (i in 1:nrow(data)) {
    nearest <- names(which.max(data[i, -i]))
    predicted <- c(predicted, nearest)
  }
  predicted <- factor(sub("_.*", "", predicted))
  return(predicted)
} 