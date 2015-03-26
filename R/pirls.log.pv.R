pirls.log.pv <- 
  function(pvlabel="ASRREA", cutoff= 550, x, by, weight="TOTWGT", 
           data, export=FALSE, name= "output", folder=getwd()) {
    
    # PV labels
    pvnames <- paste(pvlabel, "0", 1:5, sep="")
    
    log.pv.input <- function(pvlabel, cutoff, x, weight, data) {
      
      # Print NA if no variability or missing
      if (sum(sapply(data[x], function(i) c(sd(i, na.rm=T), sum(!is.na(i)))) == 0, na.rm=T) > 0) {
        return(data.frame("Coef."=NA, "Std. Error"=NA, "t value"=NA, "OR"=NA, "CI95low"=NA, 
                          "CI95up"=NA, check.names=F))
      }
      
      # Dependent binary variable
      di <- as.data.frame(sapply(pvnames, function(pv) ifelse(data[[pv]] > cutoff, 1, 0)))
      names(di) <- paste("DI", 1:5, sep="")
      data <- cbind(data, di)
      
      # List of formulas for each PV
      regform <- lapply(names(di), function(i) paste(i, "~", paste(x, collapse="+")))
      
      # Replicate weights
      rp.wt <- sapply(1:max(data[["JKZONE"]]), function(rp) ifelse(data[["JKZONE"]] == rp, 
               2*data[[weight]]*data[["JKREP"]], data[[weight]]))
      
      rp.wt.n <- nrow(data)*rp.wt/apply(rp.wt, 2, sum)
      
      # Replicate weighted coefficients for sampling error (PV1 only), normalised weights
      
      coef.rp1 <- lapply(1:max(data[["JKZONE"]]), function(rp) summary(glm(formula=as.formula(regform[[1]]), 
                  family=quasibinomial("logit"), weights=rp.wt.n[, rp], data=data)))
      
      
      # Retrieving coefficients
      rp.coef <- do.call(cbind, lapply(1:max(data[["JKZONE"]]), function(rp) coef.rp1[[rp]]$coefficients[,1]))
      
      # Total weighted coefficient for each PV for imputation (between) error
      reg.pv <- lapply(regform, function(pv) 
        summary(glm(formula=as.formula(pv), family=quasibinomial("logit"), 
                    weights=nrow(data)*data[[weight]]/sum(data[[weight]]), data=data)))
      
      
      pv.coef <- sapply(1:length(pvnames), function(pv) reg.pv[[pv]]$coefficients[, 1])
      
      # Mean total coefficients (across PVs)
      mean.coef <- apply(pv.coef, 1, mean)
      
      
      # Sampling error (variance within)
      var.w <- apply((rp.coef - pv.coef[,1])^2, 1, sum)
      
      # Imputation error (variance between)
      var.b <- (1+1/length(pvnames))*apply(pv.coef, 1, var, na.rm=TRUE)
      
      coef.se <- (var.w+(1+1/length(pvnames))*var.b)^(1/2)
      t.stat <- mean.coef/coef.se
      
      # Odds ratios and confidence intervals
      OR<- exp(mean.coef)
      
      # OR confidence intervals 
      CI95low <- exp(mean.coef - 1.96*coef.se)
      CI95up <- exp(mean.coef + 1.96*coef.se)
      
      # Table with estimates
      log.tab <- round(data.frame("Coef."=mean.coef, "Std. Error"=coef.se, "t value"=t.stat, 
                                  as.data.frame(cbind(OR, CI95low, CI95up)), check.names=F),2)
      
      return(log.tab)
    }
    
    # If by not supplied, calculate for the complete sample    
    if (missing(by)) { 
      output <- log.pv.input(pvlabel=pvlabel, cutoff=cutoff, x=x, weight=weight, data=data) 
    } else {
      output <- lapply(split(data, droplevels(data[by])), function(i) 
        log.pv.input(pvlabel=pvlabel, cutoff=cutoff, x=x, weight=weight, data=i))
    }
    
    if (export)  {
      write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
    }
    
    return(output)
  }