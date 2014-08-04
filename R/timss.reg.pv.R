timss.reg.pv <-
function(x, pvlabel="BSMMAT", weight="TOTWGT", by, data, export=FALSE, name= "output", folder=getwd()) {
  
  # PV labels
  pvnames <- paste(pvlabel, "0", 1:5, sep="")
  # List of formulas for each PV
  regform <- lapply(pvnames, function(i) paste(i, "~", paste(x, collapse="+")))
  
  reg.pv.input <- function(x, pvlabel, weight, data) {
    
    # Print NA if no variability or missing
    if (sum(sapply(data[x], function(i) c(sd(i, na.rm=T), sum(!is.na(i)))) == 0, na.rm=T) > 0) {
      return(data.frame("Estimate"=NA, "Std. Error"=NA, "t value"=NA, check.names=F))
    }
    
    # Replicate weighted coefficients for sampling error (PV1 only)
    Regpv1rp <- lapply(1:max(data[["JKZONE"]]), function(i) summary(lm(formula=as.formula(regform[[1]]), data=data, 
                 weights=ifelse(data[["JKZONE"]] == i, 2*data[[weight]]*data[["JKREP"]], data[[weight]]))))
    
    # Combining coefficients and R-squared replicates
    Statrp1 <- sapply(1:max(data[["JKZONE"]]), function(i) c(Regpv1rp[[i]]$coefficients[,1], 100*Regpv1rp[[i]]$r.squared))
    
    
    # Total weighted coefficient for each PV for imputation (between) error
    Regpv <- lapply(regform, function(i) summary(lm(formula=as.formula(i), data=data, weights=data[[weight]])))
    
    Stattot <- sapply(1:5, function(pv) c(Regpv[[pv]]$coefficients[, 1], 100*Regpv[[pv]]$r.squared))
    rownames(Stattot)[nrow(Stattot)] <- "R-squared"
    
    # Mean total coefficients (across PVs)
    Stattotm <- apply(Stattot, 1, mean)
    
    # Sampling error for PV1 (variance within)
    Varw <- apply((Statrp1-Stattot[,1])^2, 1, sum)
    
    # Imputation error (variance between)
    Varb <- (1+1/5)*apply(Stattot, 1, var)
    StatSE <- (Varw+Varb)^(1/2)
    StatT <- Stattotm/StatSE
    # Reg Table
    RegTab <- round(data.frame("Estimate"=Stattotm, "Std. Error"=StatSE, "t value"=StatT, check.names=F),2)
    return(RegTab)
  }
  
  # If by not supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- reg.pv.input(x=x, pvlabel=pvlabel, weight=weight, data=data)
  } else {
    output <- lapply(split(data, factor(data[[by]])), function(i) reg.pv.input(x=x, pvlabel=pvlabel, weight=weight, data=i))
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  return(output)
}
