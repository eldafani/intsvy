pirls.mean <-
function(variable, by, weight="TOTWGT", data, export=FALSE, name= "output", folder=getwd()) {
  mean.input <- function(variable, weight, data) {
    # Replicate weight means (sampling error)
    meanrp <- sapply(1:max(data[["JKZONE"]]), function(x) weighted.mean(as.numeric(data[[variable]]), ifelse(data[["JKZONE"]] == x, 
              2*data[[weight]]*data[["JKREP"]], data[[weight]]), na.rm = TRUE))
    # Total weighted mean                                                                      
    meantot <- weighted.mean(as.numeric(data[[variable]]), data[[weight]], na.rm = TRUE)
    # Standard error (sampling eror) 
    meanse <- (sum((meanrp-meantot)^2))^(1/2)
    result <- data.frame("n"=sum(!is.na(data[[variable]])), "Mean"= meantot, "Std.err."= meanse)
    return(round(result, 2))
  }
  # If by no supplied, calculate for the complete sample    
  if (missing(by)) { 
    output<-mean.input(variable=variable, weight=weight, data=data)
  } else {
    output<-ddply(data, by, function(x) mean.input(data=x, weight=weight, variable=variable))
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
