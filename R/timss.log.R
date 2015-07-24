timss.log <-
  function(y, x, by, weight="TOTWGT", data, export=FALSE, name= "output", folder=getwd()) { 
    
    regform <- paste(y, "~", paste(x, collapse="+"))
    
    reg.input <- function(y, x, weight, data) {
      
      # If no variability in y or x, or if all missing print NA
      if (sum(sapply(data[c(y, x)], function(i) c(sd(i, na.rm=T), sum(!is.na(i)))) == 0, na.rm=T) > 0) {
        results <- list("replicates" = NA, "reg"= NA)
        return(results)
      }
      
      # Replicate weights
      rp.wt <- sapply(1:max(data[["JKZONE"]]), function(rp) ifelse(data[["JKZONE"]] == rp, 
               2*data[[weight]]*data[["JKREP"]], data[[weight]]))
      
      rp.wt.n <- nrow(data)*rp.wt/apply(rp.wt, 2, sum)
      
      # Replicate weights coefficients for sampling error
      
      reg.rp <- lapply(1:max(data[["JKZONE"]]), function(rp) summary(glm(formula=as.formula(regform), 
                  family=quasibinomial("logit"), weights=rp.wt.n[, rp], data=data)))
      
      
      
      # Combine coefficients 
      coef.rp <- do.call("cbind", lapply(1:max(data[["JKZONE"]]), function(rp) reg.rp[[rp]]$coefficients[,1]))
      
      # Total weighted coefficient for each PV for imputation (between) error
      reg.tot <- summary(glm(formula=as.formula(regform), family=quasibinomial("logit"), 
                             weights=nrow(data)*data[[weight]]/sum(data[[weight]]), data=data))
      
      
      # Total weighted coefficients
      coef.tot <- reg.tot$coefficients[, 1]
      
      # Sampling error 
      coef.se <- (0.05*apply((coef.rp-coef.tot)^2, 1, sum))^(1/2)
      
      t.stat <- coef.tot/coef.se
      
      # Odds ratios and confidence intervals
      OR <- exp(coef.tot)
      
      # OR confidence intervals 
      CI95low <- exp(coef.tot - 1.96*coef.se)
      CI95up <- exp(coef.tot + 1.96*coef.se)
      
      # Table with estimates
      log.tab <- data.frame("Coef."=coef.tot, "Std. Error"=coef.se, "t value"=t.stat, 
                            as.data.frame(cbind(OR, CI95low, CI95up)), check.names=F)
      
      results <- list("replicates"=coef.rp, "reg"=log.tab)
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