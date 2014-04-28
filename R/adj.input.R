adj.input <-
function(x,threshold=.5) {
  
  if(measurement(x) %in% c("nominal","ordinal")){
    
    nlabeled <- sum(is.vlabeled(x),na.rm=TRUE)
    if(nlabeled < threshold*length(x))
      measurement(x) <- "interval"
  }
  x
}
