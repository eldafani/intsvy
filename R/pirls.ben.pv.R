pirls.ben.pv <-
function(pvlabel, cutoff = c(400, 475, 550, 625), by, weight= "TOTWGT", data, 
                         export=FALSE, name= "output", folder=getwd()) {
  # PV variable names
  pvname <- paste(pvlabel, "0", 1:5, sep='')
  
  pv.ben.input <- function(pvlabel, cutoff, weight, data) {
    
    # Replicate weighted percentages PV1 (sampling error)
    tabpv1 <- sapply(1:length(cutoff), function(z) sapply(1:max(data[["JKZONE"]]), function(x) 
      100*weighted.mean(data[[pvname[1]]]>=cutoff[z], w= ifelse(data[["JKZONE"]] == x, 
      2*data[[weight]]*data[["JKREP"]], data[[weight]]), na.rm = TRUE)))
    
    # Total weighted %s PV1-5 
    tabpv <- sapply(1:length(cutoff), function(z) sapply(pvname, function(x) 
      100*weighted.mean(data[[x]]>=cutoff[z], w=data[[weight]], na.rm=TRUE)))
    
    # Sampling error within (PV1), between PV error, and total (se)
    tabpvw <- sapply(1:length(cutoff), function(y) sum(sapply(1:max(data[["JKZONE"]]), function(x) 
      (tabpv1[x,y]-tabpv[1,y])^2)))
    tabpvb <- (1+1/5)*apply(tabpv, 2, var)
    tabse <- round((tabpvw+tabpvb)^(1/2), 2)
    
    # Total %
    tabtot <- round(apply(tabpv, 2, mean), 2)
    
    # Result
    result <- data.frame("Benchmark"=paste(rep("At or above", length(cutoff)), cutoff), 
                         "Percentage"=tabtot, "Std. err."= tabse, check.names=F)
    return(result)
  }
  
  # If by not supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- pv.ben.input(pvlabel=pvlabel, cutoff=cutoff, weight=weight, data=data)
  } else {
    output <- ddply(data, by, function(x) pv.ben.input(pvlabel=pvlabel, cutoff=cutoff, weight=weight, data=x))
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
