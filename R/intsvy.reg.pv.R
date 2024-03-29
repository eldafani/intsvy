intsvy.reg.pv <-
  function(x, pvnames, by, data, std=FALSE, export=FALSE, name= "output", folder=getwd(), config) {

  # Remove missing data in IVs
  data <- data[complete.cases(data[, x]), ]
    reg.pv.input <- function(x, pvnames, data, std, config) {
    if (any(sapply(data[x], function(i) all(duplicated(i))))) {
    results <- list("replicates"=NA, "residuals"= NA, "var.w"=NA, "var.b"=NA, "reg"=NA)
    return(results)
    }

      #  JK with weight variables
      if (config$parameters$weights == "JK with weights") {
       
        #pvnames <- paste0("^", config$variables$pvlabelpref, "*[0-9].*", pvnames)
        #pvnames <- grep(pvnames, names(data), value = TRUE)
        weights <- grep(paste0("^", config$variables$weightJK , ".*[0-9]+$"), 
                        names(data), value = TRUE)
        
        # remove missings in pvalues and weights
        data <- data[complete.cases(data[, c(pvnames[1], weights[1], config$variables$weight)]), ]    
        
        
        # data is empty
        if (sum(is.na((data[[pvnames[1]]])))==length(data[[pvnames[1]]])) {
          result <- data.frame(NA, "Freq"=0, "Percentage"=NA, "Std.err."= NA)  
          names(result)[1] <- pvnames[1] 
          return(result)
        }
        
        # List of formulas for each PV
        regform <- lapply(pvnames, function(i) paste(i, "~", paste(x, collapse="+")))
        
         # Replicate weighted coefficients for sampling error
        reg.rep <- lapply(1:length(pvnames), function(m) lapply(1:length(weights), function(i) 
          summary(lm(formula=as.formula(regform[[m]]), data=data, weights=data[[weights[i]]]))))
        
        # Combining coefficients and R-squared replicates
        coe.rep <- lapply(1:length(pvnames), function(m)
          sapply(1:length(weights), function(i)
            c(reg.rep[[m]][[i]]$coefficients[,1], "R-squared"= reg.rep[[m]][[i]]$r.squared)))
        
        resid <- lapply(1:length(pvnames), function(m) 
          sapply(1:length(weights), function(rep) reg.rep[[m]][[rep]]$residuals))
        
        # Total weighted coefficient for each PV for imputation (between) error
        reg.pv <- lapply(regform, function(i)
          summary(lm(formula=as.formula(i), data=data, weights=data[[config$variables$weight]])))
        coe.tot <- sapply(1:length(pvnames), function(pv)
          c(reg.pv[[pv]]$coefficients[, 1], "R-squared" = reg.pv[[pv]]$r.squared))
        
        # Mean total coefficients (across PVs)
        stat.tot <- apply(coe.tot, 1, mean)
        
        # Sampling error (variance within)
        var.w <- mean(sapply(1:length(pvnames), function(m) apply((coe.rep[[m]]-coe.tot[,m])^2, 1, sum)))
        
        # Imputation error (variance between)
        var.b <- (1+1/length(pvnames))*apply(coe.tot, 1, var)
        stat.se <- (var.w + var.b)^(1/2)
        stat.t <- stat.tot/stat.se
      
      # Reg Table
      reg.tab <- data.frame("Estimate"=stat.tot, "Std. Error"=stat.se, "t value"=stat.t, check.names=F)
      
      results <- list("replicates"=coe.rep, "residuals"= resid, "var.w"=var.w, "var.b"=var.b, "reg"=reg.tab)
      return(results)
        
      }  
  
    # BRR / JK
    if (config$parameters$weights == "BRR") {
      # balanced repeated replication
      # Replicate weighted %s (sampling error)
      # in PISA

      #pvnames <- paste0(pvnames, ".*[0-9]|[0-9].*", pvnames)
      #pvnames <- grep(pvnames, names(data), value = TRUE)
      weights <- grep(paste0("^", config$variables$weightBRR , ".*[0-9]+$"), 
                      names(data), value = TRUE)
      
      # remove missings in pvalues and weights
      data <- data[complete.cases(data[, c(pvnames[1], weights[1], config$variables$weightFinal)]), ]    
      
     # List of formulas for each PV
      regform <- lapply(pvnames, function(i) paste(i, "~", paste(x, collapse="+")))

      # Standardise IV and DV variables
      if(std) {
        data <-  cbind(scale(data[c(pvnames, x)]), data[!names(data) %in% c(pvnames, x)])
      }

      # Replicate weighted coefficients for sampling error (PVs)
      reg.rep <- lapply(regform, function(pv) lapply(1:length(weights), function(rep)
        summary(lm(formula=as.formula(pv), data=data, weights=data[[weights[rep]]]))))

      # Combining coefficients and R-squared replicates
      coe.rep <- lapply(1:length(pvnames), function(pv) sapply(1:length(weights), function(rep)
        c(reg.rep[[pv]][[rep]]$coefficients[,1], "R-squared"= reg.rep[[pv]][[rep]]$r.squared)))

      resid <- lapply(1:length(pvnames), function(pv)
                    sapply(1:length(weights),
                          function(rep) reg.rep[[pv]][[rep]]$residuals))

      # Total weighted coefficient for each PV for imputation (between) error
      reg.pv <- lapply(regform, function(pv)
                    summary(lm(formula=as.formula(pv), data=data, weights=data[[config$variables$weightFinal]])))

      coe.tot <- sapply(1:length(pvnames), function(pv)
                    c(reg.pv[[pv]]$coefficients[, 1], "R-squared" = reg.pv[[pv]]$r.squared))


      # Mean total coefficients (across PVs)
      stat.tot <- apply(coe.tot, 1, mean)

      # Sampling error (variance within)
      cc =  1/(length(weights)*(1-0.5)^2)

      var.w <- apply(cc*sapply(lapply(1:length(pvnames), function(pv)
                    (coe.rep[[pv]]-coe.tot[,pv])^2), function(e) apply(e, 1, sum)), 1, mean)

      # Imputation error (variance between)
      var.b <- (1/(length(pvnames)-1))*apply(sapply(1:length(pvnames), function(pv)
                    (coe.tot[, pv] - stat.tot)^2), 1, sum)

      stat.se <- (var.w +(1+1/length(pvnames))*var.b)^(1/2)
      stat.t <- stat.tot/stat.se

      # Reg Table
      reg.tab <- data.frame("Estimate"=stat.tot, "Std. Error"=stat.se, "t value"=stat.t, check.names=F)
      results <- list("replicates"=lapply(coe.rep, t), "residuals"= resid, "var.w"=var.w, "var.b"=var.b, "reg"=reg.tab)
      return(results)

    }
    if (config$parameters$weights == "JK") {
      # jack knife
      # in PIRLS / TIMSS
      
      #pvnames <- grep(pvnames, names(data), value = TRUE)

      # List of formulas for each PV
      regform <- lapply(pvnames, function(i) paste(i, "~", paste(x, collapse="+")))

      # Standardise IV and DV variables
      if(std) {
        data <-  cbind(scale(data[c(pvnames, x)]), data[!names(data) %in% c(pvnames, x)])
      }
      
      R.wt <- sapply(1:max(data[[config$variables$jackknifeZone]]), function(x) 
        ifelse(data[[config$variables$jackknifeZone]] == x, 
               2*data[[config$variables$weight]]*data[[config$variables$jackknifeRep]], data[[config$variables$weight]]))
      

      if (isTRUE(config$parameters$varpv1)) {
        
      # Replicate weighted coefficients for sampling error (PV1 only)
      reg.rep <- lapply(1:ncol(R.wt), function(i) 
      summary(lm(formula=as.formula(regform[[1]]), data=data, weights=R.wt[, i])))
        
      # Combining coefficients and R-squared replicates
      coe.rep <- sapply(1:max(data[[config$variables$jackknifeZone]]), function(i)
                    c(reg.rep[[i]]$coefficients[,1], "R-squared"= reg.rep[[i]]$r.squared))

      resid <- sapply(1:length(reg.rep), function(rep) reg.rep[[rep]]$residuals)

      # Total weighted coefficient for each PV for imputation (between) error
      reg.pv <- lapply(regform, function(i)
              summary(lm(formula=as.formula(i), data=data, weights=data[[config$variables$weight]])))
      coe.tot <- sapply(1:length(pvnames), function(pv)
              c(reg.pv[[pv]]$coefficients[, 1], "R-squared" = reg.pv[[pv]]$r.squared))

      # Mean total coefficients (across PVs)
      stat.tot <- apply(coe.tot, 1, mean)

      # Sampling error for PV1 (variance within)
      var.w <- apply((coe.rep-coe.tot[,1])^2, 1, sum)

      # Imputation error (variance between)
      var.b <- (1+1/length(pvnames))*apply(coe.tot, 1, var)
      stat.se <- (var.w + var.b)^(1/2)
      stat.t <- stat.tot/stat.se
      
      } else {
        
        R.wt2 <- sapply(1:max(data[[config$variables$jackknifeZone]]), function(x) 
          ifelse(data[[config$variables$jackknifeZone]] == x, 
                 2*data[[config$variables$weight]]*ifelse(data[[config$variables$jackknifeRep]]==1,0,1), data[[config$variables$weight]]))
        
        R.wt <- cbind(R.wt, R.wt2)
        
      
        # Replicate weighted coefficients for sampling error
        reg.rep <- lapply(1:length(pvnames), function(m) lapply(1:ncol(R.wt), function(i) 
          summary(lm(formula=as.formula(regform[[m]]), data=data, weights=R.wt[, i]))))
        
       # Combining coefficients and R-squared replicates
        coe.rep <- lapply(1:length(pvnames), function(m)
          sapply(1:ncol(R.wt), function(i)
          c(reg.rep[[m]][[i]]$coefficients[,1], "R-squared"= reg.rep[[m]][[i]]$r.squared)))
        
        resid <- lapply(1:length(pvnames), function(m) 
          sapply(1:ncol(R.wt), function(rep) reg.rep[[m]][[rep]]$residuals))
        
        # Total weighted coefficient for each PV for imputation (between) error
        reg.pv <- lapply(regform, function(i)
          summary(lm(formula=as.formula(i), data=data, weights=data[[config$variables$weight]])))
        coe.tot <- sapply(1:length(pvnames), function(pv)
          c(reg.pv[[pv]]$coefficients[, 1], "R-squared" = reg.pv[[pv]]$r.squared))
        
        # Mean total coefficients (across PVs)
        stat.tot <- apply(coe.tot, 1, mean)
        
        # Sampling error (variance within)
        var.w <- apply(sapply(1:length(pvnames), function(m) apply((coe.rep[[m]]-coe.tot[,m])^2, 1, sum)/2), 1, mean)
        
        # Imputation error (variance between)
        var.b <- (1+1/length(pvnames))*apply(coe.tot, 1, var)
        stat.se <- (var.w + var.b)^(1/2)
        stat.t <- stat.tot/stat.se
    }

      # Reg Table
      reg.tab <- data.frame("Estimate"=stat.tot, "Std. Error"=stat.se, "t value"=stat.t, check.names=F)

      results <- list("replicates"=coe.rep, "residuals"= resid, "var.w"=var.w, "var.b"=var.b, "reg"=reg.tab)
      return(results)

    }
    if (config$parameters$weights == "mixed_piaac") {
      # mixed design, different for different coutnries
      # PIAAC

      # PV labels
     
      # List of formulas for each PV
      regform <- lapply(pvnames, function(i) paste(i, "~", paste(x, collapse="+")))

      # Replicate weighted coefficients for sampling error (PVs)
      Coefrpv <- lapply(regform, function(k) lapply(1:config$parameters$BRRreps, function(i)
        summary(lm(formula=as.formula(k), data=data,
                   weights=data[[paste(config$variables$weightBRR, i , sep="")]]))))

      # Combining coefficients and R-squared replicates
      Statrp <- lapply(1:length(pvnames), function(pv) sapply(1:config$parameters$BRRreps, function(i)
        c(Coefrpv[[pv]][[i]]$coefficients[,1], Coefrpv[[pv]][[i]]$r.squared)))

      # Total weighted coefficient for each PV for imputation (between) error
      Regpv <- lapply(regform, function(i)
              lm(formula=as.formula(i), data=data, weights=data[[config$variables$weightFinal]]))

      Stattot <- sapply(1:length(pvnames), function(pv)
              c(summary(Regpv[[pv]])$coefficients[, 1], summary(Regpv[[pv]])$r.squared))
      rownames(Stattot)[nrow(Stattot)] <- "R-squared"

      # Mean total coefficients (across PVs)
      Stattotm <- apply(Stattot, 1, mean)

      cntName <- as.character(unique(data$CNTRYID))[1]
      cc <- piaacReplicationScheme[cntName,"c"]
      if (is.na(cc)) cc <- 1
      if (length(unique(piaacReplicationScheme[as.character(unique(data$CNTRYID)),"c"])) > 1) {
        warning(paste("In PIAAC study different replications schemes were applied in different countries. \n In the selected set of countries more than one scheme was used. \n Further estimation is performed with coefficient c =", cc))
      }
      # Sampling error (variance within)
      Varw <- apply(cc*sapply(lapply(1:length(pvnames), function(pv)
              (Statrp[[pv]]-Stattot[,pv])^2), function(e) apply(e, 1, sum)), 1, mean)

      # Imputation error (variance between)
      Varb <- (1/(length(pvnames)-1))*apply(sapply(1:length(pvnames), function(i)
              (Stattot[, i] - Stattotm)^2), 1, sum)

      StatSE <- (Varw+(1+1/length(pvnames))*Varb)^(1/2)
      StatT <- Stattotm/StatSE

      # Reg Table
      RegTab <- round(data.frame("Estimate"=Stattotm, "Std. Error"=StatSE, "t value"=StatT, check.names=FALSE),2)

      results <- list("replicates"=t(Statrp), "reg"=RegTab)
      return(results)
    }
  }

  # If by no supplied, calculate for the complete sample
  if (missing(by)) {
    output <- reg.pv.input(x=x, pvnames=pvnames, data=data, std=std, config=config)
  } else {
    output <- lapply(split(data, droplevels(data[by])), function(i)
      reg.pv.input(x=x, pvnames=pvnames, data=i, std=std, config=config))
  }

  if (export)  {
    write.csv(do.call(rbind, lapply(output, function(x) x$reg)), file=file.path(folder, paste(name, ".csv", sep="")))
  }
  class(output) <- "intsvy.reg"
  return(output)

}

