timss.mean.pv <-
function(pvlabel="BSMMAT", by, weight="TOTWGT", data, export=FALSE, name= "output", folder=getwd()) {
  pv.input <- function(pvlabel, weight, data) {
    # PV variable names
    pvnames <- paste(pvlabel, "0", 1:5, sep="")
    
    # If there is only one observation print NA
    if (nrow(data)==1) { 
      return(data.frame("Freq"= length(data[[weight]]), "Mean"= NA, "s.e."= NA, "SD"=NA, "s.e"=NA))
    } else {

    # Replicate weights
    R.wt <- sapply(1:max(data[["JKZONE"]]), function(x) ifelse(data[["JKZONE"]] == x, 
           2*data[[weight]]*data[["JKREP"]], data[[weight]]))

    # Estimates of PV1 (for sampling error)
    R.mean1 <- sapply(1:ncol(R.wt), function(x) weighted.mean(data[[pvnames[[1]]]], R.wt[, x], na.rm = TRUE))                                                                  
    
    R.sd1 <- sapply(1:ncol(R.wt), function (x) 
      (sum(R.wt[, x]*(data[[pvnames[[1]]]]-R.mean1[x])^2, na.rm = TRUE)/sum(R.wt[, x], na.rm = TRUE))^(1/2))

    # Grand mean of 5 PVs (for imputation variance)
    R.mean <- sapply(pvnames, function(x) weighted.mean(data[[x]], data[[weight]], na.rm = TRUE))
    
    R.sd <- sapply(1:5, function(x) 
      (sum(data[[weight]]*(data[[pvnames[x]]]-R.mean[x])^2, na.rm=TRUE)/sum(data[[weight]], na.rm = TRUE))^(1/2))
          
    # Sampling variance (1st PV); imputation variance; SEs
    v.meanw <- sum((R.mean1-R.mean[1])^2);    v.meanb <- (1+1/5)*var(R.mean)
    v.sdw <- sum((R.sd1 - R.sd[1])^2);  v.sdb <- (1+1/5)*var(R.sd)
    mean.se <-  (v.meanw+v.meanb)^(1/2); sd.se <- (v.sdw+v.sdb)^(1/2)
    


    result <- data.frame("Freq"= length(data[[weight]]), "Mean"= mean(R.mean), "s.e."= mean.se, 
                         "SD"=mean(R.sd), "s.e"=sd.se)
    return(round(result, 2))
    }
  }
  
  # If by no supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- pv.input(pvlabel=pvlabel, weight=weight, data=data)
  }  else {
  output <- ddply(data, by, function(x) pv.input(data=x, weight=weight, pvlabel=pvlabel))
  }
  
  if (export)  {
  write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
