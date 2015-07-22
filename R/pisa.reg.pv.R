pisa.reg.pv <- 
  function(x, pvlabel="READ", by, data, export=FALSE, name= "output", folder=getwd(), weight="W_FSTUWT", std=FALSE) {
    
    # PV labels
    pvnames <- paste("PV", 1:5, pvlabel, sep="")
    
    # List of formulas for each PV
    regform <- lapply(pvnames, function(i) paste(i, "~", paste(x, collapse="+")))
    
    reg.pv.input <- function(x, pvlabel, weight, data) {
      
      # Print NA if no variability or missing
      if (sum(sapply(data[x], function(i) c(sd(i, na.rm=T), sum(!is.na(i)))) == 0, na.rm=T) > 0) {
        return(data.frame("Estimate"=NA, "Std. Error"=NA, "t value"=NA, check.names=F))
      }
      
      # Standardise IV and DV variables
      if(std) {
        data <-  cbind(scale(data[c(pvnames, x)]), data[!names(data) %in% c(pvnames, x)])
      }
      
      # Replicate weighted coefficients for sampling error (5 PVs)
      reg.rep <- lapply(regform, function(pv) lapply(1:80, function(rep) 
        summary(lm(formula=as.formula(pv), data=data, weights=data[[paste0("W_FSTR", rep)]]))))
      
      
      # Combining coefficients and R-squared replicates
      coe.rep <- lapply(1:5, function(pv) sapply(1:80, function(rep) 
        c(reg.rep[[pv]][[rep]]$coefficients[,1], "R-squared"= 100*reg.rep[[pv]][[rep]]$r.squared)))
      
      resid <- lapply(1:5, function(pv) sapply(1:80, function(rep) reg.rep[[pv]][[rep]]$residuals))
      
      # Total weighted coefficient for each PV for imputation (between) error
      reg.pv <- lapply(regform, function(pv) summary(lm(formula=as.formula(pv), data=data, weights=data[[weight]])))
      
      coe.tot <- sapply(1:5, function(pv) c(reg.pv[[pv]]$coefficients[, 1], "R-squared" = 100*reg.pv[[pv]]$r.squared))
      
      
      # Mean total coefficients (across PVs)
      stat.tot <- apply(coe.tot, 1, mean)
      
      # Sampling error (variance within)
      var.w <- apply(0.05*sapply(lapply(1:5, function(pv) (coe.rep[[pv]]-coe.tot[,pv])^2), function(e) apply(e, 1, sum)), 1, mean)
      
      # Imputation error (variance between)
      var.b <- (1/4)*apply(sapply(1:5, function(pv) (coe.tot[, pv] - stat.tot)^2), 1, sum)
      
      stat.se <- (var.w +(1+1/5)*var.b)^(1/2)
      stat.t <- stat.tot/stat.se
      
      # Reg Table
      reg.tab <- data.frame("Estimate"=stat.tot, "Std. Error"=stat.se, "t value"=stat.t, check.names=F)
      results <- list("replicates"=lapply(coe.rep, t), "residuals"= resid, "var.w"=var.w, "var.b"=var.b, "reg"=reg.tab)
      class(results) <- "intsvy.reg"
      return(results)
    }
    
    # If by not supplied, calculate for the complete sample    
    if (missing(by)) { 
      output <- reg.pv.input(x=x, pvlabel=pvlabel, weight=weight, data=data)
      reg <- output$reg
      
    } else {
      output <- lapply(split(data, droplevels(data[by])), function(i) 
        reg.pv.input(x=x, pvlabel=pvlabel, weight=weight, data=i))
      reg <- do.call('rbind', lapply(output, function(by) by$reg))
    }
    
    if (export)  {
      write.csv(reg, file=file.path(folder, paste(name, ".csv", sep="")))
    }
    
    return(output)
  }

print.intsvy.reg <- function(x) {
  print(round(x$reg, 2))
}