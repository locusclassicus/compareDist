# tweak the similarity function 
tfidf.dist <- function(data){
  result <- data.frame()
  # subset text
  for(j in 1:nrow(data)){
    #subset words
    idx <- which(data[j,] != 0)
    docs <- data[-j,idx]
    
    # similarity
    sim <- c()
    for (i in 1:nrow(docs)) {
      s <- sum(docs[i,] * (1 / (1 + colSums(docs[-i,]))))
      sim <- c(sim, s)
    }
    output <- data.frame(doc = rep(rownames(data)[j], nrow(docs)),
                         docs = rownames(data)[-j],
                         sim = sim)
    result <- rbind(result, output)
  }
  # transform long data to distance matrix
  result <- reshape(result, idvar = "doc", timevar = "docs", direction = "wide")
  rownames(result) <- result$doc 
  result <- result[,-1]
  colnames(result) <- rownames(result)
  return(result)
}