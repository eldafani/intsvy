intsvy.mean <- 
function(variable, by, data, export=FALSE, name= "output", folder=getwd(), config) {
  mean.input <- function(variable, data, config) {
    
    #  JK with weight variables
    if (config$parameters$weights == "JK with weights") {
      
      weights <- grep(paste0("^", config$variables$weightJK , ".*[0-9]+$"), 
                      names(data), value = TRUE)
      
      # remove missings in pvalues and weights
      data <- data[complete.cases(data[, c(variable, weights[1], config$variables$weight)]), ]    
     
      
      meanrp <- sapply(1:length(weights), function(x) 
        weighted.mean(data[[variable]], data[[weights[x]]], na.rm = TRUE))                                                                  
      
      
      # Total weighted mean                                                                      
      meantot <- weighted.mean(as.numeric(data[[variable]]), data[[config$variables$weight]], na.rm = TRUE)
      # Standard error (sampling eror) 
      meanse <- (sum((meanrp-meantot)^2))^(1/2)
      result <- data.frame("Freq"=sum(!is.na(data[[variable]])), "Mean"= meantot, "s.e."= meanse)
      return(result)
    }
    
    # BRR / JK
    if (config$parameters$weights == "BRR") {
      # balanced repeated replication
      # Replicate weighted %s (sampling error)
      # in PISA / PIAAC
      
      weights <- grep("^W_.*[0-9]+$", names(data), value = TRUE)
      
      meanrp <- sapply(1:config$parameters$BRRreps, function(i) 
                      weighted.mean(as.numeric(data[[variable]]), 
                               data[[weights[i]]], na.rm = TRUE))
      
      # Replicate weights for SDs (sampling error)
      sdrp <- sapply(1:config$parameters$BRRreps, function(i)  
                      (sum(data[[weights[i]]]*(data[[variable]]-meanrp[i])^2, na.rm = TRUE)/
                               sum(data[[weights[i]]], na.rm = TRUE))^(1/2))
      
      # Total weighted mean                                                                      
      meantot <- weighted.mean(as.numeric(data[[variable]]), data[[config$variables$weightFinal]], na.rm = TRUE)
      
      # Total weighted SD
      sdtot <-  (sum(data[[config$variables$weightFinal]]*(data[[variable]]-meantot)^2, na.rm=TRUE)/sum(data[[config$variables$weightFinal]], na.rm = TRUE))^(1/2)
      
      # Standard error (sampling eror) 
      cc = 1/(length(weights)*(1-0.5)^2)
      
      meanse <- (cc*sum((meanrp-meantot)^2))^(1/2)
      sdse <- (cc*sum((sdrp-sdtot)^2))^(1/2)
      
      result <- data.frame("Freq"=sum(!is.na(data[[variable]])), "Mean"= meantot, "s.e."= meanse, "SD" = sdtot, "s.e" = sdse)
      return(result)
      
    } 
    if (config$parameters$weights == "JK") {
      # jack knife
      # in PIRLS / TIMSS
      
      R.wt <- sapply(1:max(data[[config$variables$jackknifeZone]]), function(x) 
        ifelse(data[[config$variables$jackknifeZone]] == x, 
               2*data[[config$variables$weight]]*data[[config$variables$jackknifeRep]], data[[config$variables$weight]]))
      
      if (isTRUE(config$parameters$varpv1)) {
      # Replicate weight means (sampling error)
      
      meanrp <- sapply(1:ncol(R.wt), function(x) 
        weighted.mean(data[[variable]], R.wt[, x], na.rm = TRUE))                                                                  
      
      
      # Total weighted mean                                                                      
      meantot <- weighted.mean(as.numeric(data[[variable]]), data[[config$variables$weight]], na.rm = TRUE)
      # Standard error (sampling eror) 
      meanse <- (sum((meanrp-meantot)^2))^(1/2)
      result <- data.frame("Freq"=sum(!is.na(data[[variable]])), "Mean"= meantot, "s.e."= meanse)
      return(round(result, 2))
      
    } else {
      
      R.wt2 <- sapply(1:max(data[[config$variables$jackknifeZone]]), function(x) 
        ifelse(data[[config$variables$jackknifeZone]] == x, 
               2*data[[config$variables$weight]]*ifelse(data[[config$variables$jackknifeRep]]==1,0,1), data[[config$variables$weight]]))
      
      R.wt <- cbind(R.wt, R.wt2)

        meanrp <- sapply(1:ncol(R.wt), function(x) 
          weighted.mean(data[[variable]], R.wt[, x], na.rm = TRUE))                                                                  
        
        
        # Total weighted mean                                                                      
        meantot <- weighted.mean(as.numeric(data[[variable]]), data[[config$variables$weight]], na.rm = TRUE)
        # Standard error (sampling eror) 
        meanse <- (sum((meanrp-meantot)^2)/2)^(1/2)
        result <- data.frame("Freq"=sum(!is.na(data[[variable]])), "Mean"= meantot, "s.e."= meanse)
        return(result)
        
    }
    }
      
    if (config$parameters$weights == "mixed_piaac") {
      # mixed design, different for different coutnries
      # PIAAC
      
      meanrp <- sapply(1:config$parameters$BRRreps, function(i) 
              weighted.mean(as.numeric(data[[variable]]), 
                           data[[paste(config$variables$weightBRR, i , sep="")]], na.rm = TRUE))
      
      # Total weighted mean                                                                      
      meantot <- weighted.mean(as.numeric(data[[variable]]), data[[config$variables$weightFinal]], na.rm = TRUE)
      # Standard error (sampling eror) 
      cntName <- as.character(unique(data$CNTRYID))[1]
      cc <- piaacReplicationScheme[cntName,"c"]
      if (is.na(cc)) cc <- 1
      
      meanse <- (cc*sum((meanrp-meantot)^2))^(1/2)
      result <- data.frame("Freq"=sum(!is.na(data[[variable]])), "Mean"= meantot, "s.e."= meanse)
      return(result)

    } 
  }
  
  # If by no supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- mean.input(variable=variable, data=data, config=config)
  } else {
    for (i in by) {
      data[[c(i)]] <- as.factor(data[[c(i)]])
    }
    output <- ddply(data, by, function(x) mean.input(data=x, variable=variable, config=config))
  }
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  class(output) <- c("intsvy.mean", "data.frame")
  return(output)
  
}

