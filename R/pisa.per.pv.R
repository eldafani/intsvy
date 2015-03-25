pisa.per.pv <- 
  function(pvlabel="READ", per = c(5, 10, 25, 75, 90, 95), by, weight="W_FSTUWT", brr_weight="W_FSTR", 
           data, export=FALSE, name= "output", folder=getwd()) {
    
    # PV variable names
    pvnames <- paste("PV", 1:5, pvlabel, sep="")
    
    pv.per.input <- function(pvlabel, per, weight, data) {
    
    # If there is only one observation print NA
    if (nrow(data)==1)  {
    return(data.frame("Percentiles"=per, "Score"=rep(NA,length(per)), "Std. err."= rep(NA,length(per))))
    }
    
    # Replicate percentiles
    R.per <- lapply(pvnames, function(k) sapply(1:80, function(i) 
    wtd.quantile(data[[k]], probs=per/100, weights=data[[paste0(brr_weight, i)]], na.rm = TRUE)))
    names(R.per) <- pvnames
    
    # Grand mean of 5 PVs (imputation variance)
    PV.per <- sapply(pvnames, function(x) 
      wtd.quantile(data[[x]], probs=per/100, weights=data[[weight]], na.rm = TRUE))
    
    MEAN.per <- apply(PV.per, 1, mean, na.rm=TRUE)
    
    # Sampling variance; imputation variance; and SEs
    
    var.per.w = apply(sapply(pvnames, function(pv) 0.05*apply(sapply(1:80, function(r) (R.per[[pv]][,r]-PV.per[, pv])^2), 1, sum, na.rm=TRUE)), 1, mean, na.rm=TRUE)
    
    var.per.b <- (1/(length(pvnames)-1))*apply(sapply(pvnames, function(pv) (PV.per[, pv]-MEAN.per)^2), 1, sum, na.rm=TRUE)
    
    per.se <-(var.per.w+(1+1/length(pvnames))*var.per.b)^(1/2)
    
      
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
