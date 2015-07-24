pisa.log <- 
  function(y, x, by, data, weight="W_FSTUWT", brr_weight="W_FSTR", export=FALSE, name= "output", folder=getwd()) {
    
    regform <- paste(y, "~", paste(x, collapse="+"))
    
    log.input <- function(y, x, data, weight="W_FSTUWT", brr_weight="W_FSTR") {

    # If no variability in y or x, or if all missing print NA
    if (sum(sapply(data[c(y, x)], function(i) c(sd(i, na.rm=T), sum(!is.na(i)))) == 0, na.rm=T) > 0) {
    results <- list("replicates" = NA, "reg"= NA)
    return(results)
    }
      
     
      # Replicate weighted coefficients, normalised weights
      
      coef.rp <- lapply(1:80, function(i) summary(glm(formula=as.formula(regform), family=quasibinomial("logit"), 
                    weights=nrow(data)*data[[paste0(brr_weight, i)]]/sum(data[[paste0(brr_weight, i)]]),
                    data=data)))
      
      
      # Retrieving coefficients
      rp.coef <- sapply(1:80, function(i) coef.rp[[i]]$coefficients[,1])
      
      # Total weighted regressions 
      reg.pv <- summary(glm(formula=as.formula(regform), family=quasibinomial("logit"), 
                    weights=nrow(data)*data[[weight]]/sum(data[[weight]]), data=data))
      
      
      # Total weighted coefficients
      tot.coef <- reg.pv$coefficients[, 1]
      
      # Sampling error 
      coef.se <- (0.05*apply((rp.coef-tot.coef)^2, 1, sum))^(1/2)

      t.stat <- tot.coef/coef.se
      
      # Odds ratios and confidence intervals
      OR <- exp(tot.coef)
      
      # OR confidence intervals 
      CI95low <- exp(tot.coef - 1.96*coef.se)
      CI95up <- exp(tot.coef + 1.96*coef.se)
      
      # Table with estimates
      log.tab <- data.frame("Coef."=tot.coef, "Std. Error"=coef.se, "t value"=t.stat, 
                            as.data.frame(cbind(OR, CI95low, CI95up)), check.names=F)
      
      results <- list("replicates"=t(rp.coef), "reg"=log.tab)
      return(results)
    }
    
    # If by not supplied, calculate for the complete sample    
    if (missing(by)) { 
      output <- log.input(y=y, x=x, data=data, weight=weight, brr_weight=brr_weight)
      
    } else {
      output <- lapply(split(data, droplevels(data[by])), function(i) 
        log.input(y=y, x=x, weight=weight, brr_weight=brr_weight, data=i))
    }
    
    class(output) <- "intsvy.reg"
    
    if (export)  {
      write.csv(summary(output), file=file.path(folder, paste(name, ".csv", sep="")))
    }
    
    return(output)
  }