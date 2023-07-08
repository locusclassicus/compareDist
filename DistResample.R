dist.resample <- function(sample.size=3000, 
                          method="euclidean", 
                          nr.features = 100, 
                          direction = "min",
                          relative = T, 
                          use.scaled.freq = T) {
  
  
  # empty tibble to store results
  tbl.all <- tibble()
  
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
  
  # get predictions for resampled data
  if(direction == "min") { # 1
    predicted <- get.pred.min(sample.dist) 
  } else { predicted <- get.pred.max(sample.dist)}
  
  # save result 
  tbl <- tibble(predicted = predicted, 
                expected = expected, 
                size = sample.size, 
                mfw = nr.features, 
                method = method, 
                scale = use.scaled.freq) # здесь тоже все факторы убираем
  tbl.all <- bind_rows(tbl.all, tbl)
  
  return(tbl.all)
} 