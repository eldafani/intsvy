pisa.mean <- 
function(variable, by, data, export=FALSE, name= "output", folder=getwd(), weight="W_FSTUWT") {
  mean.input <- function(variable, data, weight) {
    # Replicate weight means (sampling error)
    meanrp <-     achmrp <- sapply(1:80, function(i) weighted.mean(as.numeric(data[[variable]]), 
                            data[[paste("W_FSTR", i , sep="")]], na.rm = TRUE))
    
    # Total weighted mean                                                                      
    meantot <- weighted.mean(as.numeric(data[[variable]]), data[[weight]], na.rm = TRUE)
    # Standard error (sampling eror) 
    meanse <- (0.05*sum((meanrp-meantot)^2))^(1/2)
    result <- data.frame("Freq"=sum(!is.na(data[[variable]])), "Mean"= meantot, "s.e."= meanse)
    return(round(result, 2))
  }
  # If by no supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- mean.input(variable=variable, weight=weight, data=data)
  } else {
    for (i in by) {
      data[[c(i)]] <- as.character(data[[c(i)]])
    }
    output <- ddply(data, by, function(x) mean.input(data=x, weight=weight, variable=variable))
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  class(output) <- c("intsvy.mean", "data.frame")
  
  return(output)
}

