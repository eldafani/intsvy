timss.reg <-
function(y, x, by, weight="TOTWGT", data, export=FALSE, name= "output", folder=getwd()) { 

  regform <- paste(y, "~", paste(x, collapse="+"))
  
  reg.input <- function(y, x, weight, data) {
    
  # If no variability in y or x, or if all missing print NA
  if (sum(sapply(data[c(y, x)], function(i) c(sd(i, na.rm=T), sum(!is.na(i)))) == 0, na.rm=T) > 0) {
    results <- list("replicates"=NA, "residuals"= NA, "reg"=NA)
    return(results)
  }
    
  # Replicate weights coefficients for sampling error
  reg.rp <- lapply(1:max(data[["JKZONE"]]), function(i) summary(lm(formula= as.formula(regform), data=data, weights= 
           ifelse(data[["JKZONE"]] == i, 2*data[[weight]]*data[["JKREP"]], data[[weight]]))))
  
  # Combine coefficients and R-squared
  coef.rp <- sapply(1:max(data[["JKZONE"]]), function(i) c(reg.rp[[i]]$coefficients[,1], "R-squared" = reg.rp[[i]]$r.squared))
  
  resid <- sapply(1:length(reg.rp), function(rep) reg.rp[[rep]]$residuals)
  
  
  # Total weighted coefficients
  reg.tot <- summary(lm(formula=as.formula(regform), data=data, weights=data[[weight]]))
  coef.tot <- c(reg.tot$coefficients[ ,1] , "R-squared" = reg.tot$r.squared)
  
  # Sampling error
  coef.se <- apply((coef.rp-coef.tot)^2, 1, sum)^(1/2)
  # T-value
  coef.t <- coef.tot/coef.se
  # Reg Table
  reg.tab <- data.frame("Estimate"=coef.tot, "Std. Error"=coef.se, "t value"=coef.t, check.names=F)
  
  results <- list("replicates"=t(coef.rp), "residuals"= resid, "reg"=reg.tab)
  return(results)
  }
  
  # If by not supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- reg.input(y=y, x=x, weight=weight, data=data)
  } else {
    output <- lapply(split(data, droplevels(data[by])), function(i) reg.input(y=y, x=x, weight=weight, data=i))
  }
  
  class(output) <- "intsvy.reg"
  
  if (export)  {
    write.csv(summary(output), file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}