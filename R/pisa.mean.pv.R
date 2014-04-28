pisa.mean.pv <- 
function(pvlabel, by, data, export=FALSE, name= "output", folder=getwd()) {
  pv.input <- function(pvlabel="READ", data) {
    # PV variable names
    pvnames <- paste("PV", 1:5, pvlabel, sep="")
    # Replicate weight means of 5 PVs (sampling error)
    achmrp <- sapply(pvnames, function(k) sapply(1:80, function(i) weighted.mean(data[[k]], 
              data[[paste("W_FSTR", i , sep="")]], na.rm = TRUE)))
    # Grand mean of 5 PVs (imputation variance)
    achmtot <- sapply(pvnames, function(x) weighted.mean(data[[x]], data[["W_FSTUWT"]], na.rm = TRUE))
    # Mean of means (the one is reported)
    achtot <- mean(achmtot)
    # Sampling variance; imputation variance; SEs
    varw <- mean(sapply(1:5, function(i) 0.05*sum((achmrp[,i]-achmtot[i])^2)))
    varb <- (1/(5-1))*sum(sapply(1:5, function(i) (achmtot[i]-achtot)^2))
    achse <-(varw+(1+1/5)*varb)^(1/2)
    result <- data.frame("Freq"= sum(!is.na(data[[pvnames[1]]])), "Mean"= achtot, "Std.err."= achse)
    return(round(result, 2))
  }
  # If by no supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- pv.input(pvlabel=pvlabel, data=data)
  } else {
    for (i in by) {
      data[[c(i)]] <- as.character(data[[c(i)]])
    }
    output <- ddply(data, by, function(x) pv.input(data=x, pvlabel=pvlabel))
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}

