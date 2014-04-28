pisa.reg.pv <- 
function(x, pvlabel="READ", by, data, export=FALSE, name= "output", folder=getwd()) {

  # PV labels
  pvnames <- paste("PV", 1:5, pvlabel, sep="")
  
  reg.pv.input <- function(x, pvlabel=pvlabel, data) {
    
    # Print NA if no variability or missing
    if (sum(sapply(data[x], function(i) c(sd(i, na.rm=T), sum(!is.na(i)))) == 0, na.rm=T) > 0) {
      return(data.frame("Estimate"=NA, "Std. Error"=NA, "t value"=NA, check.names=F))
    }
    
    # List of formulas for each PV
    regform <- lapply(pvnames, function(i) paste(i, "~", paste(x, collapse="+")))
    # Replicate weighted coefficients for sampling error (5 PVs)
    Coefrpv <- lapply(regform, function(k) lapply(1:80, function(i) 
    summary(lm(formula=as.formula(k), data=data, 
    weights=data[[paste("W_FSTR", i , sep="")]]))))
    
    # Combining coefficients and R-squared replicates
    Statrp <- lapply(1:5, function(pv) sapply(1:80, function(i) 
              c(Coefrpv[[pv]][[i]]$coefficients[,1], 100*Coefrpv[[pv]][[i]]$r.squared)))
        
    # Total weighted coefficient for each PV for imputation (between) error
    Regpv <- lapply(regform, function(i) summary(lm(formula=i, data=data, weights=data[["W_FSTUWT"]])))
    
    Stattot <- sapply(1:5, function(pv) c(Regpv[[pv]]$coefficients[, 1], 100*Regpv[[pv]]$r.squared))
    rownames(Stattot)[nrow(Stattot)] <- "R-squared"
    
    # Mean total coefficients (across PVs)
    Stattotm <- apply(Stattot, 1, mean)
    
    # Sampling error (variance within)
    Varw <- apply(0.05*sapply(lapply(1:5, function(pv) (Statrp[[pv]]-Stattot[,pv])^2), function(e) apply(e, 1, sum)), 1, mean)
    
    # Imputation error (variance between)
    Varb <- (1/4)*apply(sapply(1:5, function(i) (Stattot[, i] - Stattotm)^2), 1, sum)
    
    StatSE <- (Varw+(1+1/5)*Varb)^(1/2)
    StatT <- Stattotm/StatSE
    
    # Reg Table
    RegTab <- round(data.frame("Estimate"=Stattotm, "Std. Error"=StatSE, "t value"=StatT, check.names=F),2)
    return(RegTab)
  }
  
  # If by not supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- reg.pv.input(x=x, pvlabel=pvlabel, data=data) 
  } else {
    output <- lapply(split(data, droplevels(data[by])), function(i) reg.pv.input(x=x, pvlabel=pvlabel, data=i))
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
