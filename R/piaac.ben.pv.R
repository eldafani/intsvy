piaac.ben.pv <- 
function(pvlabel, cutoff = c(175.99, 225.99, 275.99, 325.99, 375.99), by, weight="SPFWT0", brr_weight="SPFWT", data, 
                         export=FALSE, name= "output", folder=getwd()) {
  # PV variable names
  pvnames <- paste("PV", pvlabel, 1:10, sep="")
   
  pv.ben.input <- function(pvlabel, cutoff, weight, brr_weight, data) {
  
    # Level indicators (1/0) for 10 PVs
    
    # First level indicator (1/0) for 10 PVs
    level1 <- lapply(pvnames, function(x) ifelse(data[[x]] <= cutoff[1], 1, 0))
    
    # Levels in between indicators (1/0) for 10 PVs
    level.int <- lapply(pvnames, function(x) sapply(2:length(cutoff), function(z) 
      ifelse(data[[x]] > cutoff[z-1] & data[[x]] <= cutoff[z], 1, 0)))
    
    # Last level indicator (1/0) for 10 PVs
    levell <- lapply(pvnames, function(x) ifelse(data[[x]] > cutoff[length(cutoff)], 1, 0))
    
    # Recoded data for standard analysis
    level.data <- lapply(1:10, function(x) cbind(level1[[x]], level.int[[x]], levell[[x]]))

    # Percentages for replicates and pvs
    tabpvrp <- lapply(1:10, function(x) sapply(1:80, function(i) 100*apply(level.data[[x]], 2, weighted.mean, 
              w= data[[paste(brr_weight, i , sep="")]], na.rm= TRUE)))
    
    # Total percentages for pvs
    tabpvt <- sapply(1:10, function(x) 100*apply(level.data[[x]], 2, weighted.mean, 
              w= data[[weight]], na.rm= TRUE))
    
    # Mean of means (the one is reported)
    tabtot <- apply(tabpvt, 1, mean)
    
    # Sampling error, between PV error, and total (se)
    cntName <- as.character(unique(data$CNTRYID))[1]
    cc <- piaacReplicationScheme[cntName,"c"]
    if (is.na(cc)) cc <- 1
    
    varw <- apply(sapply(1:10, function(x) cc*apply(sapply(1:80, function(i) (tabpvrp[[x]][, i] - tabpvt[ , x])^2), 1, sum)), 1, mean)
    varb <- (1/(10-1))*apply(sapply(1:10, function(x) (tabpvt[, x]-tabtot)^2), 1, sum)
    tabse <-(varw+(1+1/10)*varb)^(1/2)
    
    # Result
    result <- data.frame("Benchmarks"= c(paste("Below/equal to", cutoff[1]), paste(rep("greater than", length(cutoff) -1), cutoff[1:length(cutoff)-1], 
              "to less/equal than", cutoff[2:length(cutoff)]), paste("Above", cutoff[length(cutoff)])),
                   "Percentage"=round(tabtot, 2), "Std. err."= round(tabse,2), check.names=F)
    
    return(result)
  }
  
  # If by not supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- pv.ben.input(pvlabel=pvlabel, cutoff=cutoff, weight=weight, brr_weight=brr_weight, data=data)
  } else {
    output <- ddply(data, by, function(x) pv.ben.input(pvlabel=pvlabel, cutoff=cutoff, weight=weight, brr_weight=brr_weight, data=x))
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
