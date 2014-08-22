intsvy.mean.pv.input <- function(pvnames = paste("PV", 1:5, "READ", sep=""), 
  data, final_weight="W_FSTUWT", brr_weight="W_FSTR", replication = 'pisa') {
  # Replicate weighted sds and means of 5 PVs (sampling error)
  
  # If there is only one observation print NA
  if (nrow(data)==1)  
    return(data.frame("Freq"= length(data[[final_weight]]), "Mean"= NA, "s.e."= NA, "SD"=NA, "s.e"=NA))
    
  R.mean <- sapply(pvnames, function(k) 
              sapply(1:80, function(i) 
                weighted.mean(data[[k]], 
                              data[[paste0(brr_weight, i)]], na.rm = TRUE)))
  
  R.sd <- sapply(pvnames, function(x) 
              sapply(1:80, function(i)
                (sum(data[[paste0(brr_weight, i)]]*
                       (data[[x]]-R.mean[i, x])^2, na.rm = TRUE)/
                   sum(data[[paste0(brr_weight, i)]], na.rm = TRUE))^(1/2)))
  
  # Grand mean of 5 PVs (imputation variance)
  PV.mean <- sapply(pvnames, function(x) 
                weighted.mean(data[[x]], data[[final_weight]], na.rm = TRUE))
  
  PV.sd <- sapply(pvnames, function(x)
              (sum(data[[final_weight]]*(data[[x]]-PV.mean[x])^2, na.rm=TRUE)/
                  sum(data[[final_weight]], na.rm = TRUE))^(1/2))
  
  # Mean of means (the one is reported)
  MEAN.m <- mean(PV.mean)
  SD.m <- mean(PV.sd)
  
  cc = 1/20
  if (replication == 'piaac') {
    cntName <- as.character(unique(data$CNTRYID))[1]
    cc <- piaacReplicationScheme[cntName,"c"]
    if (is.na(cc)) cc <- 1
  }
  
  # Sampling variance; imputation variance; and SEs
  var.mean.w <- mean(sapply(seq_along(pvnames), function(i) cc*sum((R.mean[,i]-PV.mean[i])^2)))
  var.mean.b <- (1/(length(pvnames)-1))*sum(sapply(seq_along(pvnames), function(i) (PV.mean[i]-MEAN.m)^2))
  mean.se <-(var.mean.w+(1+1/length(pvnames))*var.mean.b)^(1/2)
  
  var.sd.w <- mean(sapply(seq_along(pvnames), function(i) cc*sum((R.sd[,i]-PV.sd[i])^2)))
  var.sd.b <- (1/(length(pvnames)-1))*sum(sapply(seq_along(pvnames), function(i) (PV.sd[i]-SD.m)^2))
  sd.se <-(var.sd.w+(1+1/length(pvnames))*var.sd.b)^(1/2)
  
  result <- data.frame("Freq"= length(data[[final_weight]]), "Mean"= mean(MEAN.m), "s.e."= mean.se, 
                       "SD"=mean(SD.m), "s.e"=sd.se)
  return(round(result, 2))
}

intsvy.mean.pv <- function(pvnames = paste("PV", 1:5, "READ", sep=""), by, 
                           data, final_weight="W_FSTUWT", brr_weight="W_FSTR", replication='pisa') {
  # If by no supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- intsvy.mean.pv.input(pvnames=pvnames, data=data, final_weight, brr_weight, replication=replication)
  } else {
    for (i in by) 
      data[[c(i)]] <- as.character(data[[c(i)]])

    output <- ddply(data, by, function(x) intsvy.mean.pv.input(pvnames=pvnames, data=x, 
                                                               final_weight, brr_weight,
                                                               replication=replication))
  }
  
  class(output) <- c("intsvy.mean", "data.frame")
  
  output
}
  
  
pisa.mean.pv <-  
function(pvlabel, by, data, export=FALSE, name= "output", folder=getwd(),
  weight="W_FSTUWT") {
  output <- intsvy.mean.pv(paste("PV", 1:5, pvlabel, sep=""), by, data, final_weight=weight, brr_weight="W_FSTR")
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
