timss.reg.pv <-
  function(x, pvlabel="BSMMAT", weight="TOTWGT", by, data, std=FALSE, export=FALSE, name= "output", folder=getwd()) {
    
    # PV labels
    pvnames <- paste(pvlabel, "0", 1:5, sep="")
    # List of formulas for each PV
    regform <- lapply(pvnames, function(i) paste(i, "~", paste(x, collapse="+")))
    
    reg.pv.input <- function(x, pvlabel, weight, data) {
      
      # Print NA if no variability or missing
        if (sum(sapply(data[x], function(i) c(sd(i, na.rm=T), sum(!is.na(i)))) == 0, na.rm=T) > 0) {
          results <- list("replicates"=NA, "residuals"= NA, "var.w"=NA, "var.b"=NA, "reg"=NA)
          return(results)
          }
      
      # Standardise IV and DV variables
      if(std) {
        data <-  cbind(scale(data[c(pvnames, x)]), data[!names(data) %in% c(pvnames, x)])
      }
      
      # Replicate weighted coefficients for sampling error (PV1 only)
      reg.rep <- lapply(1:max(data[["JKZONE"]]), function(i) summary(lm(formula=as.formula(regform[[1]]), data=data, 
                  weights=ifelse(data[["JKZONE"]] == i, 2*data[[weight]]*data[["JKREP"]], data[[weight]]))))
      
      # Combining coefficients and R-squared replicates
      coe.rep <- sapply(1:max(data[["JKZONE"]]), function(i) c(reg.rep[[i]]$coefficients[,1], "R-squared"= reg.rep[[i]]$r.squared))
      
      resid <- sapply(1:length(reg.rep), function(rep) reg.rep[[rep]]$residuals)
      
      
      # Total weighted coefficient for each PV for imputation (between) error
      reg.pv <- lapply(regform, function(i) summary(lm(formula=as.formula(i), data=data, weights=data[[weight]])))
      coe.tot <- sapply(1:5, function(pv) c(reg.pv[[pv]]$coefficients[, 1], "R-squared" = reg.pv[[pv]]$r.squared))
      
      # Mean total coefficients (across PVs)
      stat.tot <- apply(coe.tot, 1, mean)
      
      # Sampling error for PV1 (variance within)
      var.w <- apply((coe.rep-coe.tot[,1])^2, 1, sum)
      
      # Imputation error (variance between)
      var.b <- (1+1/5)*apply(coe.tot, 1, var)
      stat.se <- (var.w + var.b)^(1/2)
      stat.t <- stat.tot/stat.se
      
      # Reg Table
      reg.tab <- data.frame("Estimate"=stat.tot, "Std. Error"=stat.se, "t value"=stat.t, check.names=F)
      
      results <- list("replicates"=coe.rep, "residuals"= resid, "var.w"=var.w, "var.b"=var.b, "reg"=reg.tab)
      return(results)
    }
    
    # If by not supplied, calculate for the complete sample    
    if (missing(by)) { 
      output <- reg.pv.input(x=x, pvlabel=pvlabel, weight=weight, data=data)
    } else {
      output <- lapply(split(data, factor(data[[by]])), function(i) reg.pv.input(x=x, pvlabel=pvlabel, weight=weight, data=i))
    }
    
    class(output) <- "intsvy.reg"
    
    if (export)  {
      write.csv(summary(output), file=file.path(folder, paste(name, ".csv", sep="")))
    }
    return(output)
  }
