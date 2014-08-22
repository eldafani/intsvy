piaac.reg.pv <- 
function(x, pvlabel="LIT", by, data, export=FALSE, name= "output", folder=getwd(), weight="SPFWT0") {

  # PV labels
  pvnames <- paste("PV", pvlabel, 1:10, sep="")
  
  # List of formulas for each PV
  regform <- lapply(pvnames, function(i) paste(i, "~", paste(x, collapse="+")))

  reg.pv.input <- function(x, pvlabel=pvlabel, data, weight = "SPFWT0") {
    
    # Print NA if no variability or missing
    if (sum(sapply(data[x], function(i) c(sd(i, na.rm=T), sum(!is.na(i)))) == 0, na.rm=T) > 0) {
      return(data.frame("Estimate"=NA, "Std. Error"=NA, "t value"=NA, check.names=F))
    }
    
    # Replicate weighted coefficients for sampling error (5 PVs)
    Coefrpv <- lapply(regform, function(k) lapply(1:80, function(i) 
                    summary(lm(formula=as.formula(k), data=data, 
                            weights=data[[paste("SPFWT", i , sep="")]]))))
    
    # Combining coefficients and R-squared replicates
    Statrp <- lapply(1:10, function(pv) sapply(1:80, function(i) 
                    c(Coefrpv[[pv]][[i]]$coefficients[,1], 100*Coefrpv[[pv]][[i]]$r.squared)))
        
    # Total weighted coefficient for each PV for imputation (between) error
    Regpv <- lapply(regform, function(i) lm(formula=as.formula(i), data=data, weights=data[[weight]]))
    
    Stattot <- sapply(1:10, function(pv) c(summary(Regpv[[pv]])$coefficients[, 1], 100*summary(Regpv[[pv]])$r.squared))
    rownames(Stattot)[nrow(Stattot)] <- "R-squared"
    
    # Mean total coefficients (across PVs)
    Stattotm <- apply(Stattot, 1, mean)
    
    cntName <- as.character(unique(data$CNTRYID))[1]
    cc <- piaacReplicationScheme[cntName,"c"]
    if (is.na(cc)) cc <- 1
    # Sampling error (variance within)
    Varw <- apply(cc*sapply(lapply(1:10, function(pv) (Statrp[[pv]]-Stattot[,pv])^2), function(e) apply(e, 1, sum)), 1, mean)
    
    # Imputation error (variance between)
    Varb <- (1/(10-1))*apply(sapply(1:10, function(i) (Stattot[, i] - Stattotm)^2), 1, sum)
    
    StatSE <- (Varw+(1+1/10)*Varb)^(1/2)
    StatT <- Stattotm/StatSE
    
    # Reg Table
    RegTab <- round(data.frame("Estimate"=Stattotm, "Std. Error"=StatSE, "t value"=StatT, check.names=F),2)
    return(RegTab)
  }
  
  # If by not supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- reg.pv.input(x=x, pvlabel=pvlabel, data=data, weight=weight) 
  } else {
    output <- lapply(split(data, droplevels(data[by])), function(i) reg.pv.input(x=x, pvlabel=pvlabel, data=i, weight=weight))
    class(output) <- "intsvy.reg"
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
