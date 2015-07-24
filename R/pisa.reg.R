pisa.reg <-
function(y, x, by, data, export=FALSE, name= "output", folder=getwd(), weight="W_FSTUWT", brr_weight="W_FSTR") { 
  
  regform <- paste(y, "~", paste(x, collapse="+"))
  
  reg.input <- function(y, x, data) {
    
    # If no variability in y or x, or if all missing print NA
    if (sum(sapply(data[c(y, x)], function(i) c(sd(i, na.rm=T), sum(!is.na(i)))) == 0, na.rm=T) > 0) {
      results <- list("replicates"=NA, "residuals"= NA, "reg"=NA)
      return(results)
    }
    
    # Replicate weights coefficients for sampling error
    reg.rp <- lapply(1:80, function(i) summary(lm(formula=as.formula(regform), data=data, 
              weights=data[[paste(brr_weight, i , sep="")]])))
    # Combining coefficients and R-squared replicates
    coef.rp <- sapply(1:80, function(i) c(reg.rp[[i]]$coefficients[,1], "R-squared" = reg.rp[[i]]$r.squared))
  
    resid <- sapply(1:80, function(rep) reg.rp[[rep]]$residuals)
    
    # Total weighted coefficients and R-squared
    reg.tot <- summary(lm(formula=as.formula(regform), data=data, weights=data[[weight]]))
    coef.tot <- c(reg.tot$coefficients[,1], "R-squared" = reg.tot$r.squared)
      
    # Sampling error
    coef.se <- (0.05*apply((coef.rp-coef.tot)^2, 1, sum))^(1/2)
    # T-value
    coef.t <- coef.tot/coef.se
    # Reg Table
    reg.tab <- data.frame("Estimate"=coef.tot, "Std. Error"=coef.se, "t value"=coef.t, check.names=F)
    
    results <- list("replicates"=t(coef.rp), "residuals"= resid, "reg"=reg.tab)
    return(results)
      
  }
  # If by not supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- reg.input(y=y, x=x, data=data)
  } else {
    output <- lapply(split(data, droplevels(data[by])), function(i) reg.input(y=y, x=x, data=i))
  }
  
  class(output) <- "intsvy.reg"
  
  if (export)  {
    write.csv(summary(output), file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}