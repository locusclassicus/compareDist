# min for distances
get.pred.min <- function(data) { 
  predicted <- c()
  for (i in 1:nrow(data)) {
    nearest <- names(which.min(data[i, -i]))
    predicted <- c(predicted, nearest)
  }
  predicted <- sub("_.*", "", predicted) # здесь убираем фактор
  return(predicted)
}

# max for similarities
get.pred.max <- function(data) { 
  predicted <- c()
  for (i in 1:nrow(data)) {
    nearest <- names(which.max(data[i, -i]))
    predicted <- c(predicted, nearest)
  }
  predicted <- sub("_.*", "", predicted) # здесь убираем фактор
  return(predicted)
} 