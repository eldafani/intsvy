pisa.mean.pv <- function(pvlabel, by,  weight="W_FSTUWT", data, export=FALSE, name= "output", folder=getwd()) {
  x  = 1
  pv.input <- function(pvlabel, weight, data) {
    # PV variable names
    pvnames <- paste("PV", 1:5, pvlabel, sep="")
    # Replicate weight means and sds of 5 PVs (sampling error)
    achmrp <- sapply(pvnames, function(k) sapply(1:80, function(i) weighted.mean(data[[k]], 
    data[[paste("W_FSTR", i , sep="")]], na.rm = TRUE)))
    
    R.mean <- sapply(pvnames, function(k) sapply(1:80, function(i) weighted.mean(data[[k]], 
    data[[paste("W_FSTR", i , sep="")]], na.rm = TRUE)))
    
    R.sd <- sapply(1:5, function(x) sapply(1:80, function(i)
      (sum(data[[paste("W_FSTR", i , sep="")]]*(data[[pvnames[[x]]]]-R.mean[i, x])^2, na.rm = TRUE)/sum(data[[paste("W_FSTR", i , sep="")]], na.rm = TRUE))^(1/2)))
    
    
    # Grand mean of 5 PVs (imputation variance)
    PV.mean <- sapply(pvnames, function(x) weighted.mean(data[[x]], data[[weight]], na.rm = TRUE))
    
    PV.sd <- sapply(1:5, function(x)
      (sum(data[[weight]]*(data[[pvnames[x]]]-PV.mean[x])^2, na.rm=TRUE)/sum(data[[weight]], na.rm = TRUE))^(1/2))
    
    # Mean of means (the one is reported)
    MEAN.m <- mean(PV.mean)
    SD.m <- mean(PV.sd)
    
    # Sampling variance; imputation variance; and SEs
    var.mean.w <- mean(sapply(1:5, function(i) 0.05*sum((R.mean[,i]-PV.mean[i])^2)))
    var.mean.b <- (1/(5-1))*sum(sapply(1:5, function(i) (PV.mean[i]-MEAN.m)^2))
    mean.se <-(var.mean.w+(1+1/5)*var.mean.b)^(1/2)
    
    var.sd.w <- mean(sapply(1:5, function(i) 0.05*sum((R.sd[,i]-PV.sd[i])^2)))
    var.sd.b <- (1/(5-1))*sum(sapply(1:5, function(i) (PV.sd[i]-SD.m)^2))
    sd.se <-(var.sd.w+(1+1/5)*var.sd.b)^(1/2)
    
    result <- data.frame("Freq"= length(data[[weight]]), "Mean"= mean(MEAN.m), "s.e."= mean.se, 
                         "SD"=mean(SD.m), "s.e"=sd.se)
    return(round(result, 2))
  }
  # If by no supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- pv.input(pvlabel=pvlabel, weight=weight, data=data)
  } else {
    for (i in by) {
      data[[c(i)]] <- as.character(data[[c(i)]])
    }
    output <- ddply(data, by, function(x) pv.input(data=x, weight=weight, pvlabel=pvlabel))
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
