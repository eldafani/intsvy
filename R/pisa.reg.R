pisa.reg <-
function(y, x, by, data, export=FALSE, name= "output", folder=getwd()) { 
  
  regform <- paste(y, "~", paste(x, collapse="+"))
  
  reg.input <- function(y, x, data) {
    
    # If no variability in y or x, or if all missing print NA
    if (sum(sapply(data[c(y, x)], function(i) c(sd(i, na.rm=T), sum(!is.na(i)))) == 0, na.rm=T) > 0) {
    return(data.frame("Estimate"=NA, "Std. Error"=NA, "t value"=NA, check.names=F))
    }
    
    # Replicate weights coefficients for sampling error
    Coefrp <- lapply(1:80, function(i) summary(lm(formula=as.formula(regform), data=data, 
              weights=data[[paste("W_FSTR", i , sep="")]])))
    # Combining coefficients and R-squared replicates
    Statrp <- sapply(1:80, function(i) c(Coefrp[[i]]$coefficients[,1], 100*Coefrp[[i]]$r.squared))
  
    # Total weighted coefficients and R-squared
    Reg <- summary(lm(formula=as.formula(regform), data=data, weights=data[["W_FSTUWT"]]))
    Stattot <- c(Reg$coefficients[,1], 100*Reg$r.squared)
    names(Stattot)[length(Stattot)] <- "R-squared"
    
    # Sampling error
    StatSE <- (0.05*apply((Statrp-Stattot)^2, 1, sum))^(1/2)
    # T-value
    StatT <- Stattot/StatSE
    # Reg Table
    RegTab <- round(data.frame("Estimate"=Stattot, "Std. Error"=StatSE, "t value"=StatT, check.names=F),2)
    return(RegTab)
  }
  # If by not supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- reg.input(y=y, x=x, data=data)
  } else {
    output <- lapply(split(data, droplevels(data[by])), function(i) reg.input(y=y, x=x, data=i))
    
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
