pirls.per.pv <- 
  function(pvlabel="ASRREA", per = c(5, 10, 25, 75, 90, 95), by, weight="TOTWGT",  
           data, export=FALSE, name= "output", folder=getwd()) {
    
    # PV variable names
    pvnames <- paste(pvlabel, "0", 1:5, sep="")
    
    pv.per.input <- function(pvlabel, per, weight, data) {
      
      # If there is only one observation print NA
      if (nrow(data)==1)  {
        return(data.frame("Percentiles"=per, "Score"=rep(NA,length(per)), "Std. err."= rep(NA,length(per))))
      }
      
      # Replicate weights
      R.wt <- sapply(1:max(data[["JKZONE"]]), function(x) ifelse(data[["JKZONE"]] == x, 
              2*data[[weight]]*data[["JKREP"]], data[[weight]]))
      
      # Percentile estimates of PV1 (for sampling error)
      R.per1 <- sapply(1:ncol(R.wt), function(i) wtd.quantile(data[[pvnames[[1]]]], 
                probs=per/100, weights=R.wt[,i], na.rm = TRUE))
      
      
      # Grand mean of 5 PVs (imputation variance)
      PV.per <- sapply(pvnames, function(x) 
        wtd.quantile(data[[x]], probs=per/100, weights=data[[weight]], na.rm = TRUE))
      
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
    }
    
    # If by not supplied, calculate for the complete sample    
    if (missing(by)) { 
      output <- pv.per.input(pvlabel=pvlabel, per=per, weight=weight, data=data) 
    } else {
      
      for (i in by) {
        data[[c(i)]] <- as.factor(data[[c(i)]])
      }
      
      output <- ddply(data, by, function(x) pv.per.input(pvlabel=pvlabel, per=per, weight=weight, data=x))
    }
    
    if (export)  {
      write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
    }
    
    return(output)
  }
