
intsvy.mean.pv <- 
function(pvnames, by, data, export=FALSE, name= "output", folder=getwd(), config) {
  pv.input <- function(pvnames, data, config) {
    # If there is only one observation print NA
    if (nrow(data) <= 1)  
      return(data.frame("Freq"= length(data[[config$variables$weightFinal]]), "Mean"= NA, "s.e."= NA, "SD"=NA, "s.e"=NA))

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
      
      # Estimates of PV1s (for sampling error)
      R.mean1 <- sapply(pvnames, function(k) sapply(1:length(weights), function(x) 
        weighted.mean(data[[k]], data[[weights[x]]], na.rm = TRUE)))                                                                  
      
      R.sd1 <- sapply(pvnames, function(k) sapply(1:length(weights), function (x) 
        (sum(data[[weights[x]]]*(data[[k]]-R.mean1[x])^2, na.rm = TRUE)/sum(data[[weights[x]]], na.rm = TRUE))^(1/2)))
      
      # Grand mean of PVs (for imputation variance)
      R.mean <- sapply(pvnames, function(x) 
        weighted.mean(data[[x]], data[[config$variables$weight]], na.rm = TRUE))
      
      R.sd <- sapply(1:length(pvnames), function(x) 
        (sum(data[[config$variables$weight]]*(data[[pvnames[x]]]-R.mean[x])^2, na.rm=TRUE)/sum(data[[config$variables$weight]], na.rm = TRUE))^(1/2))
      
      # Sampling variance; imputation variance; SEs
      v.meanw <- mean(sapply(1:length(pvnames), function(m) sum((R.mean1[,m]-R.mean[m])^2)))
      v.meanb <- (1+1/length(pvnames))*var(R.mean)
      v.sdw <- mean(sapply(1:length(pvnames), function(m) sum((R.sd1[,m] - R.sd[m])^2)))
      v.sdb <- (1+1/length(pvnames))*var(R.sd)
      mean.se <-  (v.meanw+v.meanb)^(1/2); sd.se <- (v.sdw+v.sdb)^(1/2)
  
    result <- data.frame("Freq"= length(data[[config$variables$weight]]), 
                         "Mean"= mean(R.mean), "s.e."= mean.se, 
                         "SD"=mean(R.sd), "s.e"=sd.se)
    
    return(round(result, 2))
    }
    
    # BRR / JK
    if (config$parameters$weights %in% c("BRR","mixed_piaac")) {
      # balanced repeated replication
      # Replicate weighted %s (sampling error)
      # in PISA / PIAAC
      #pvnames <- paste0(pvnames, ".*[0-9]|[0-9].*", pvnames)
      #pvnames <- grep(pvnames, names(data), value = TRUE)
      
      weights <- grep(paste0("^", config$variables$weightBRR , ".*[0-9]+$"), 
                      names(data), value = TRUE)
      
      # remove missings in pvalues and weights
      data <- data[complete.cases(data[, c(pvnames[1], weights[1], config$variables$weightFinal)]), ]    
      
      
      # Replicate weighted sds and means of PVs (sampling error)
      R.mean <- sapply(pvnames, function(k) 
        sapply(1:length(weights), function(i) 
          weighted.mean(data[[k]], 
                        data[[weights[i]]], na.rm = TRUE)))
      
      R.sd <- sapply(pvnames, function(x) 
        sapply(1:length(weights), function(i)
          (sum(data[[weights[i]]]*
                 (data[[x]]-R.mean[i, x])^2, na.rm = TRUE)/
             sum(data[[weights[i]]], na.rm = TRUE))^(1/2)))
      
      # Grand mean of PVs (imputation variance)
      PV.mean <- sapply(pvnames, function(x) 
        weighted.mean(data[[x]], data[[config$variables$weightFinal]], na.rm = TRUE))
      
      PV.sd <- sapply(pvnames, function(x)
        (sum(data[[config$variables$weightFinal]]*(data[[x]]-PV.mean[x])^2, na.rm=TRUE)/
           sum(data[[config$variables$weightFinal]], na.rm = TRUE))^(1/2))
      
      # Mean of means (the one is reported)
      MEAN.m <- mean(PV.mean)
      SD.m <- mean(PV.sd)
    
      cc = 1/(length(weights)*(1-0.5)^2)
      
      if (config$parameters$weights == "mixed_piaac") {
        cntName <- as.character(unique(data[,config$variables$countryID]))[1]
        cc <- piaacReplicationScheme[cntName,"c"]
        if (is.na(cc)) cc <- 1
        if (length(unique(piaacReplicationScheme[as.character(unique(data[,config$variables$countryID])),"c"])) > 1) {
          warning(paste("In PIAAC study different replications schemes were applied in different countries. \n In the selected set of countries more than one scheme was used. \n Further estimation is performed with coefficient c =", cc))
        }
      }
    
      
      # Sampling variance; imputation variance; and SEs
      var.mean.w <- mean(sapply(seq_along(pvnames), function(i) cc*sum((R.mean[,i]-PV.mean[i])^2)))
      var.mean.b <- (1/(length(pvnames)-1))*sum(sapply(seq_along(pvnames), function(i) (PV.mean[i]-MEAN.m)^2))
      mean.se <-(var.mean.w+(1+1/length(pvnames))*var.mean.b)^(1/2)
      
      var.sd.w <- mean(sapply(seq_along(pvnames), function(i) cc*sum((R.sd[,i]-PV.sd[i])^2)))
      var.sd.b <- (1/(length(pvnames)-1))*sum(sapply(seq_along(pvnames), function(i) (PV.sd[i]-SD.m)^2))
      sd.se <-(var.sd.w+(1+1/length(pvnames))*var.sd.b)^(1/2)
      
      result <- data.frame("Freq"= length(data[[config$variables$weightFinal]]), "Mean"= mean(MEAN.m), "s.e."= mean.se, 
                           "SD"=mean(SD.m), "s.e"=sd.se)
      return(round(result, 2))

    }
    
    if (config$parameters$weights == "JK") {
      # jack knife
      # in PIRLS / TIMSS
      
      # pvnames <- grep(pvnames, names(data), value = TRUE)

      # Replicate weights
      R.wt <- sapply(1:max(data[[config$variables$jackknifeZone]]), function(x) 
              ifelse(data[[config$variables$jackknifeZone]] == x, 
              2*data[[config$variables$weight]]*data[[config$variables$jackknifeRep]], data[[config$variables$weight]]))

   
      # IEA < 2015, PV1 for sampling error

      if (isTRUE(config$parameters$varpv1)) {
        
      # Estimates of PV1 (for sampling error)
      R.mean1 <- sapply(1:ncol(R.wt), function(x) 
        weighted.mean(data[[pvnames[[1]]]], R.wt[, x], na.rm = TRUE))                                                                  
      
      R.sd1 <- sapply(1:ncol(R.wt), function (x) 
        (sum(R.wt[, x]*(data[[pvnames[[1]]]]-R.mean1[x])^2, na.rm = TRUE)/sum(R.wt[, x], na.rm = TRUE))^(1/2))
      
      # Grand mean of PVs (for imputation variance)
      R.mean <- sapply(pvnames, function(x) 
        weighted.mean(data[[x]], data[[config$variables$weight]], na.rm = TRUE))
      
      R.sd <- sapply(1:length(pvnames), function(x) 
        (sum(data[[config$variables$weight]]*(data[[pvnames[x]]]-R.mean[x])^2, na.rm=TRUE)/sum(data[[config$variables$weight]], na.rm = TRUE))^(1/2))
      
      # Sampling variance (1st PV); imputation variance; SEs
      v.meanw <- sum((R.mean1-R.mean[1])^2);    v.meanb <- (1+1/length(pvnames))*var(R.mean)
      v.sdw <- sum((R.sd1 - R.sd[1])^2);  v.sdb <- (1+1/length(pvnames))*var(R.sd)
      mean.se <-  (v.meanw+v.meanb)^(1/2); sd.se <- (v.sdw+v.sdb)^(1/2)
      
      } 
      
      if (isFALSE(config$parameters$varpv1)) {
      
      R.wt2 <- sapply(1:max(data[[config$variables$jackknifeZone]]), function(x) 
          ifelse(data[[config$variables$jackknifeZone]] == x, 
                 2*data[[config$variables$weight]]*ifelse(data[[config$variables$jackknifeRep]]==1,0,1), data[[config$variables$weight]]))
        
      R.wt <- cbind(R.wt, R.wt2)
        
        
      # Estimates of PVs (for sampling error)
      R.mean1 <- sapply(pvnames, function(k) sapply(1:ncol(R.wt), function(x) 
        weighted.mean(data[[k]], R.wt[, x], na.rm = TRUE)))                                                                  
      
      R.sd1 <- sapply(pvnames, function(k) sapply(1:ncol(R.wt), function (x) 
        (sum(R.wt[, x]*(data[[k]]-R.mean1[x])^2, na.rm = TRUE)/sum(R.wt[, x], na.rm = TRUE))^(1/2)))
      
      # Grand mean of PVs (for imputation variance)
      R.mean <- sapply(pvnames, function(x) 
        weighted.mean(data[[x]], data[[config$variables$weight]], na.rm = TRUE))
      
      R.sd <- sapply(1:length(pvnames), function(x) 
        (sum(data[[config$variables$weight]]*(data[[pvnames[x]]]-R.mean[x])^2, na.rm=TRUE)/sum(data[[config$variables$weight]], na.rm = TRUE))^(1/2))
      
      # Sampling variance; imputation variance; SEs
      v.meanw <- mean(sapply(1:length(pvnames), function(m) sum((R.mean1[,m]-R.mean[m])^2)/2))
      v.meanb <- (1+1/length(pvnames))*var(R.mean)
      v.sdw <- mean(sapply(1:length(pvnames), function(m) sum((R.sd1[,m] - R.sd[m])^2)/2))
      v.sdb <- (1+1/length(pvnames))*var(R.sd)
      mean.se <-  (v.meanw+v.meanb)^(1/2); sd.se <- (v.sdw+v.sdb)^(1/2)
      }
      
      result <- data.frame("Freq"= length(data[[config$variables$weight]]), "Mean"= mean(R.mean), "s.e."= mean.se, 
                           "SD"=mean(R.sd), "s.e"=sd.se)
      
      return(round(result, 2))
    }
  }
  
  # If by no supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- pv.input(pvnames=pvnames, data=data, config=config)
  }  else {
    for (i in by) {
      data[[c(i)]] <- as.factor(data[[c(i)]])
    }
    output <- ddply(data, by, function(x) pv.input(data=x, pvnames=pvnames, config=config))
  }

  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  class(output) <- c("intsvy.mean", "data.frame")
  return(output)
  
}

