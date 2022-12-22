dist.resample <- function(sample.size=3000, 
                          method="euclidean", 
                          nr.features = 100, 
                          direction = "min",
                          relative = T, 
                          use.scaled.freq = T) {
  
  
  # empty tibble to store results
  tbl.all <- tibble()
  
  # source local functions
  if(method == "tfidf") {
    source("~/R_Workflow/compareDist/Tfidf.R")
  }
  source("~/R_Workflow/compareDist/GetPred.R")
  
  # philentropy distances
  phil.distances <- philentropy::getDistMethods()
  
  # make samples
  current.samples = lapply(input.texts, sample, size = sample.size, 
                           replace=T)
  
  # create dtm
  wordlist = make.frequency.list(current.samples) #
  wordlist_cut <- if(!is.null(nr.features)){
    wordlist[1:nr.features]} else {
      wordlist_cut = wordlist}
  dtm <-  make.table.of.frequencies(corpus = current.samples, 
                                    features = wordlist_cut, 
                                    absent.sensitive = F, 
                                    relative = relative) %>%
    suppressMessages()
  dtm <- as.data.frame.matrix(as.table(dtm))
  
  # scale
  if(use.scaled.freq == T){dtm <- scale(dtm)}
  
  # delete columns with all NAs
  dtm <- dtm[,colSums(is.na(dtm)) < nrow(dtm)]
  # if other NAs left
  dtm[is.na(dtm)] <- 0
  
  # expected classes 
  expected <- sub("_.*", "", rownames(dtm))
  
  # calculate distance
  if(method %in% phil.distances) {
    sample.dist <- philentropy::distance(dtm, method = method,
                                         use.row.names=T) %>%  
      suppressMessages()
  } 
  if(method == "labbe"){
    fn <- function(x, y) sum(abs(x - y)) / (2*sum(x))
    sample.dist <- as.matrix(proxy::dist(dtm, method = fn))
  } 
  if(method == "cng") {
    fn <- function(x, y) {
      idx <- which(x != 0 & y != 0)
      sum(((2*(x[idx]-y[idx]))/(x[idx]+y[idx]))^2)
    }
    sample.dist <- as.matrix(proxy::dist(dtm, method = fn))
  }
  if(method == "tfidf") {
    sample.dist <- tfidf.dist(dtm)
  }
  
  # get predictions for resampled data
  if(direction == "min") { # 1
    predicted <- get.pred.min(sample.dist) 
  } else { predicted <- get.pred.max(sample.dist)}
  
  predicted <- sub("_.*", "", predicted)
  
  # save result 
  tbl <- tibble(predicted = as.factor(predicted), 
                expected = as.factor(expected), 
                size = sample.size, 
                mfw = nr.features, 
                method = method, 
                scale = use.scaled.freq)
  levels(tbl$predicted) <- levels(tbl$expected) # fix factor levels bug
  tbl.all <- bind_rows(tbl.all, tbl)
  
  return(tbl.all)
} 
