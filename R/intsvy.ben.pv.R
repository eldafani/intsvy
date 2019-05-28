intsvy.ben.pv <- function(pvnames, by, cutoff, data, export=FALSE, name= "output", folder=getwd(), config) {

    if (missing(cutoff)) {
    cutoff = config$parameters$cutoffs
    }
  pv.ben.input <- function(pvnames, data, cutoff, config) {
    
    #  JK
    if (config$parameters$weights == "JK") {
      # jack knife
      # in PIRLS / TIMSS

      # data is empty
      if (sum(is.na((data[[pvnames[1]]])))==length(data[[pvnames[1]]])) {
        result <- data.frame(NA, "Freq"=0, "Percentage"=NA, "Std.err."= NA)  
        names(result)[1] <- pvnames[1] 
        return(result)
      }
      
      # Replicate weighted percentages PV1 (sampling error)
      
      R.wt <- sapply(1:max(data[[config$variables$jackknifeZone]]), function(x) 
        ifelse(data[[config$variables$jackknifeZone]] == x, 
               2*data[[config$variables$weight]]*data[[config$variables$jackknifeRep]], data[[config$variables$weight]]))
      
      if (isTRUE(config$parameters$varpv1)) {
      tabpv1 <- sapply(1:length(cutoff), function(z) sapply(1:ncol(R.wt), function(x) 
        100*weighted.mean(data[[pvnames[1]]]>=cutoff[z], w = R.wt[,x])))
            
         # Total weighted %s PV1-5 
      tabpv <- sapply(1:length(cutoff), function(z) sapply(pvnames, function(x) 
        100*weighted.mean(data[[x]]>=cutoff[z], w=data[[config$variables$weight]], na.rm=TRUE)))
      
      # Sampling error within (PV1), between PV error, and total (se)
      tabpvw <- sapply(1:length(cutoff), function(y) sum(sapply(1:max(data[[config$variables$jackknifeZone]]), function(x) 
        (tabpv1[x,y]-tabpv[1,y])^2)))
      tabpvb <- (1+1/5)*apply(tabpv, 2, var)
      tabse <- round((tabpvw+tabpvb)^(1/2), 2)
      
      # Total %
      tabtot <- round(apply(tabpv, 2, mean), 2)
      
      # Result
      result <- data.frame("Benchmark"=paste(rep("At or above", length(cutoff)), cutoff), 
                           "Percentage"=tabtot, "Std. err."= tabse, check.names=F)
      return(result)
      
      } else {
      
        R.wt2 <- sapply(1:max(data[[config$variables$jackknifeZone]]), function(x) 
          ifelse(data[[config$variables$jackknifeZone]] == x, 
                 2*data[[config$variables$weight]]*ifelse(data[[config$variables$jackknifeRep]]==1,0,1), data[[config$variables$weight]]))
        
        R.wt <- cbind(R.wt, R.wt2)  
        
        tabpv1 <-  lapply(1:config$parameters$PVreps, function(m) sapply(1:length(cutoff), function(z) 
          sapply(1:ncol(R.wt), function(x)
          100*weighted.mean(data[[pvnames[m]]]>=cutoff[z], w = R.wt[,x]))))

        # Total weighted %s PV1-5 
        tabpv <- sapply(1:length(cutoff), function(z) sapply(pvnames, function(x) 
          100*weighted.mean(data[[x]]>=cutoff[z], w=data[[config$variables$weight]], na.rm=TRUE)))
        
        # Sampling error within (PV1), between PV error, and total (se)
          tabpvw <- apply(do.call(rbind, lapply(1:config$parameters$PVreps, function(m) 
          sapply(1:length(cutoff), function(y) 
          sum(sapply(1:ncol(R.wt), function(x) (tabpv1[[m]][x,y]-tabpv[m,y])^2))/2))), 2, mean)
        
        tabpvb <- (1+1/5)*apply(tabpv, 2, var)
        tabse <- round((tabpvw+tabpvb)^(1/2), 2)
        
        # Total %
        tabtot <- round(apply(tabpv, 2, mean), 2)
        
        # Result
        result <- data.frame("Benchmark"=paste(rep("At or above", length(cutoff)), cutoff), 
                             "Percentage"=tabtot, "Std. err."= tabse, check.names=F)
        return(result)
        
      }
    }
    # BRR 
    if (config$parameters$weights == "BRR") {
      # balanced repeated replication
      # Replicate weighted %s (sampling error)
      # in PISA / PIAAC
     
      # data is empty
      if (sum(is.na((data[[pvnames[1]]])))==length(data[[pvnames[1]]])) {
        result <- data.frame(NA, "Freq"=0, "Percentage"=NA, "Std.err."= NA)  
        names(result)[1] <- pvnames[1] 
        return(result)
      }

      # First level indicator (1/0) for 5 PVs
      level1 <- lapply(pvnames, function(x) ifelse(data[[x]] <= cutoff[1], 1, 0))
      
      # Levels in between indicators (1/0) for 5 PVs
      level.int <- lapply(pvnames, function(x) sapply(2:length(cutoff), function(z) 
        ifelse(data[[x]] > cutoff[z-1] & data[[x]] <= cutoff[z], 1, 0)))
      
      # Last level indicator (1/0) for 5 PVs
      levell <- lapply(pvnames, function(x) ifelse(data[[x]] > cutoff[length(cutoff)], 1, 0))
      
      # Recoded data for standard analysis
      level.data <- lapply(1:length(pvnames), function(x) cbind(level1[[x]], level.int[[x]], levell[[x]]))
      
      # Percentages for replicates and pvs
      tabpvrp <- lapply(1:length(pvnames), function(x) 
                    sapply(1:config$parameters$BRRreps, function(i) 
                        100*apply(level.data[[x]], 2, weighted.mean, 
                            w = data[[paste(config$variables$weightBRR, i , sep="")]], na.rm= TRUE)))
      
      # Total percentages for pvs
      tabpvt <- sapply(1:length(pvnames), function(x) 
                    100*apply(level.data[[x]], 2, weighted.mean, 
                            w= data[[config$variables$weightFinal]], na.rm= TRUE))
      
      # Mean of means (the one is reported)
      tabtot <- apply(tabpvt, 1, mean)
      
      # Sampling error, between PV error, and total (se)
      varw <- apply(sapply(1:length(pvnames), function(x) 0.05*apply(sapply(1:config$parameters$BRRreps, function(i) (tabpvrp[[x]][, i] - tabpvt[ , x])^2), 1, sum)), 1, mean)
      varb <- (1/(length(pvnames)-1))*apply(sapply(1:length(pvnames), function(x) (tabpvt[, x]-tabtot)^2), 1, sum)
      tabse <-(varw+(1+1/length(pvnames))*varb)^(1/2)
      
      # Result
      result <- data.frame("Benchmarks"= c(paste0("<= ", cutoff[1]),
               paste0(rep("(", length(cutoff) -1), cutoff[1:length(cutoff)-1], 
               ", ", cutoff[2:length(cutoff)], "]"), paste0("> ", cutoff[length(cutoff)])),
               "Percentage"=round(tabtot, 2), "Std. err."= round(tabse,2), check.names=F)
      return(result)
    } 
    if (config$parameters$weights == "mixed_piaac") {
      # mixed design, different for different countries
      # PIAAC

      # data is empty
      if (sum(is.na((data[[pvnames[1]]])))==length(data[[pvnames[1]]])) {
        result <- data.frame(NA, "Freq"=0, "Percentage"=NA, "Std.err."= NA)  
        names(result)[1] <- pvnames[1] 
        return(result)
      }

      # First level indicator (1/0) for 10 PVs
      level1 <- lapply(pvnames, function(x) ifelse(data[[x]] <= cutoff[1], 1, 0))
      
      # Levels in between indicators (1/0) for 10 PVs
      level.int <- lapply(pvnames, function(x) sapply(2:length(cutoff), function(z) 
        ifelse(data[[x]] > cutoff[z-1] & data[[x]] <= cutoff[z], 1, 0)))
      # special case for one row only - lost direction
      if (nrow(data)==1) {
        level.int <- lapply(level.int, matrix, nrow=1)
      }
      
      # Last level indicator (1/0) for 10 PVs
      levell <- lapply(pvnames, function(x) ifelse(data[[x]] > cutoff[length(cutoff)], 1, 0))
      
      # Recoded data for standard analysis
      level.data <- lapply(1:config$parameters$PVreps, function(x) 
                        cbind(level1[[x]], level.int[[x]], levell[[x]]))
      
      # Percentages for replicates and pvs
      tabpvrp <- lapply(1:config$parameters$PVreps, function(x) 
                        sapply(1:config$parameters$BRRreps, function(i) 
                                100*apply(level.data[[x]], 2, weighted.mean, 
                                       w= data[[paste(config$variables$weightBRR, i , sep="")]], na.rm= TRUE)))
      
      # Total percentages for pvs
      tabpvt <- sapply(1:config$parameters$PVreps, function(x) 
                      100*apply(level.data[[x]], 2, weighted.mean, 
                             w= data[[config$variables$weightFinal]], na.rm= TRUE))
      
      # Mean of means (the one is reported)
      tabtot <- apply(tabpvt, 1, mean)
      
      # Sampling error, between PV error, and total (se)
      cntName <- as.character(unique(data$CNTRYID))[1]
      cc <- piaacReplicationScheme[cntName,"c"]
      if (is.na(cc)) cc <- 1
      
      varw <- apply(sapply(1:config$parameters$PVreps, function(x) 
                  cc*apply(sapply(1:config$parameters$BRRreps, function(i) 
                        (tabpvrp[[x]][, i] - tabpvt[ , x])^2), 1, sum)), 1, mean)
      varb <- (1/(config$parameters$PVreps-1))*apply(sapply(1:config$parameters$PVreps, 
                                          function(x) (tabpvt[, x]-tabtot)^2), 1, sum)
      tabse <-(varw+(1+1/config$parameters$PVreps)*varb)^(1/2)
      
      # Result
      result <- data.frame("Benchmarks"= c(paste("Below/equal to", cutoff[1]), 
                          paste(rep("greater than", length(cutoff) -1), cutoff[1:length(cutoff)-1], 
                               "to less/equal than", cutoff[2:length(cutoff)]), paste("Above", cutoff[length(cutoff)])),
                           "Percentage"=round(tabtot, 2), "Std. err."= round(tabse,2), check.names=F)
      
      return(result)
    }
  }
  
  # If by not supplied, calculate for complete sample    
  if (missing(by)) { 
    output <- pv.ben.input(pvnames=pvnames, cutoff=cutoff, data=data, config=config)
  } else {
    for (i in by) {
      data[[c(i)]] <- as.factor(data[[c(i)]])
    }
    output <- ddply(data, by, function(x) pv.ben.input(pvnames=pvnames, cutoff=cutoff, data=x, config=config))
  }
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  return(output)
}
