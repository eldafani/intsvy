intsvy.per.pv <- function(pvnames, by, per, data, export=FALSE, name= "output", folder=getwd(), config) {  
  if (missing(per)) {
    per = config$parameters$percentiles
  }
  
  pv.per.input <- function(pvnames, per, data, config) {
    # Not enough data
    if (nrow(data)<=1)  {
      return(data.frame("Percentiles"=per, "Score"=rep(NA,length(per)), "Std. err."= rep(NA,length(per))))
    }

    #  JK
    if (config$parameters$weights == "JK") {
      # jack knife
      # in PIRLS / TIMSS
      # Replicate weights
      R.wt <- sapply(1:max(data[[config$variables$jackknifeZone]]), function(x) 
                  ifelse(data[[config$variables$jackknifeZone]] == x, 
                           2*data[[config$variables$weight]]*data[[config$variables$jackknifeRep]], data[[config$variables$weight]]))
      
      if (isTRUE(config$parameters$varpv1)) {
      
      # Percentile estimates of PV1 (for sampling error)
      R.per1 <- sapply(1:ncol(R.wt), function(i) 
                  wtd.quantile(data[[pvnames[[1]]]], 
                            probs=per/100, weights=R.wt[,i], na.rm = TRUE))

      # Grand mean of 5 PVs (imputation variance)
      PV.per <- sapply(pvnames, function(x) 
        wtd.quantile(data[[x]], probs=per/100, weights=data[[config$variables$weight]], na.rm = TRUE))
      
      MEAN.per <- apply(PV.per, 1, mean, na.rm=TRUE)
      
      # Sampling variance; imputation variance; and SEs
      var.per.w = apply(sapply(1:ncol(R.wt), function(r) 
        (R.per1[,r]-PV.per[, pvnames[1]])^2), 1, sum, na.rm=TRUE)
      
      var.per.b <- (1+1/length(pvnames))*apply(PV.per, 1, var, na.rm=TRUE)
      per.se <-(var.per.w+ var.per.b)^(1/2)
      
      # Result
      result <- data.frame("Percentiles"= per, "Score"=round(MEAN.per, 2), "Std. err."= round(per.se,2), check.names=F)
      row.names(result) <- NULL
      return(result)
      
      } else {
      
        R.wt2 <- sapply(1:max(data[[config$variables$jackknifeZone]]), function(x) 
          ifelse(data[[config$variables$jackknifeZone]] == x, 
                 2*data[[config$variables$weight]]*ifelse(data[[config$variables$jackknifeRep]]==1,0,1), data[[config$variables$weight]]))
        
        R.wt <- cbind(R.wt, R.wt2)
        
        # Percentile estimates of PV1 (for sampling error)
        R.per <- lapply(1:config$parameters$PVreps, function(m) sapply(1:ncol(R.wt), function(i) 
          wtd.quantile(data[[pvnames[[m]]]], probs=per/100, weights=R.wt[,i], na.rm = TRUE)))
          
               
        # Grand mean of 5 PVs (imputation variance)
        PV.per <- sapply(pvnames, function(x) 
          wtd.quantile(data[[x]], probs=per/100, weights=data[[config$variables$weight]], na.rm = TRUE))
        
        MEAN.per <- apply(PV.per, 1, mean, na.rm=TRUE)
        
        # Sampling variance; imputation variance; and SEs
        
        var.per.w = apply(sapply(1:config$parameters$PVreps, function(m)  
          apply((R.per[[m]]-PV.per[, pvnames[m]])^2, 1, sum, na.rm=TRUE)/2), 1, mean)
        
        var.per.b <- (1+1/length(pvnames))*apply(PV.per, 1, var, na.rm=TRUE)
        per.se <-(var.per.w+ var.per.b)^(1/2)
        
        # Result
        result <- data.frame("Percentiles"= per, "Score"=round(MEAN.per, 2), "Std. err."= round(per.se,2), check.names=F)
        row.names(result) <- NULL
        return(result)  
        
      }
        
        
    }
    # BRR 
    if (config$parameters$weights == "BRR") {
      # balanced repeated replication
      # Replicate weighted %s (sampling error)
      # in PISA 
 
      # Replicate percentiles
      R.per <- lapply(pvnames, function(k) sapply(1:config$parameters$BRRreps, function(i) 
                   wtd.quantile(data[[k]], probs=per/100, weights=data[[paste0(config$variables$weightBRR, i)]], na.rm = TRUE)))
      names(R.per) <- pvnames
      
      # Grand mean of 5 PVs (imputation variance)
      PV.per <- sapply(pvnames, function(x) 
        wtd.quantile(data[[x]], probs=per/100, weights=data[[config$variables$weightFinal]], na.rm = TRUE))
      
      MEAN.per <- apply(PV.per, 1, mean, na.rm=TRUE)
      
      # Sampling variance; imputation variance; and SEs
      var.per.w = apply(sapply(pvnames, function(pv) 
                      0.05*apply(sapply(1:config$parameters$BRRreps, function(r) 
                            (R.per[[pv]][,r]-PV.per[, pv])^2), 1, sum, na.rm=TRUE)), 1, mean, na.rm=TRUE)
      
      var.per.b <- (1/(length(pvnames)-1))*apply(sapply(pvnames, function(pv) 
                      (PV.per[, pv]-MEAN.per)^2), 1, sum, na.rm=TRUE)
      
      per.se <-(var.per.w+(1+1/length(pvnames))*var.per.b)^(1/2)
      
      # Result
      result <- data.frame("Percentiles"= per, "Score"=round(MEAN.per, 2), "Std. err."= round(per.se,2), check.names=F)
      row.names(result) <- NULL
      return(result)
      
    } 
    if (config$parameters$weights == "mixed_piaac") {
      # mixed design, different for different countries
      # PIAAC
      
      stop("Not implemented yet")
    }
  }
  
  # If by not supplied, calculate for complete sample    
  if (missing(by)) { 
    output <- pv.per.input(pvnames=pvnames, per=per, data=data, config=config)
  } else {
    for (i in by) {
      data[[c(i)]] <- as.factor(data[[c(i)]])
    }
    output <- ddply(data, by, function(x) pv.per.input(pvnames=pvnames, per=per, data=x, config=config))
  }
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  return(output)
}

