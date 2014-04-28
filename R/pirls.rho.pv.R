pirls.rho.pv <-
function(variable, pvlabels, by, weight="TOTWGT", data, export=FALSE, name= "output", folder=getwd()) {
  rho.pv.input <- function(variable, pvlabels, weight, data) {
    
    # Correlation of 2 PVs (variable should be missing)
    
    if (length(pvlabels)==2 & missing(variable)) {
        
    # PV names
    pvnames <- lapply(pvlabels, function(x) paste(x, "0", 1:5, sep=""))
    # Complete dataset (listwise deletion)
    data <- na.omit(data[c(unlist(pvnames), weight, "JKREP", "JKZONE")])
    # Replicate weighted correlations for PV1 (sampling error)
    rhopvrp <- lapply(1:max(data[["JKZONE"]]), function(i) cov.wt(x=data[c(pvnames[[1]][1], pvnames[[2]][1])], cor=T, 
               wt=ifelse(data[["JKZONE"]] == i, 2*data[[weight]]*data[["JKREP"]], data[[weight]]))[[5]])
    # Total weighted correlation for imputation variance
    rhopvtot <- lapply(1:5, function(i) cov.wt(x=data[c(pvnames[[1]][i],pvnames[[2]][i])], cor=T, 
                 wt= data[[weight]])[[5]])
    # Sampling variance, imputation variance, and SEs
    varw <- Reduce("+", lapply(rhopvrp, function(x) (x - rhopvtot[[1]])^2))
    varb <- (1+1/5)* apply(simplify2array(rhopvtot), c(1, 2), var) # slower, but Reduce(var) fails
    rhose <- (varw+varb)^(1/2)
    # Mean total weighted correlation
    rhotot <- Reduce("+", rhopvtot)/length(rhopvtot)
    # Combined rhos and SEs
    rhomat <- round(do.call(cbind, lapply(1:ncol(rhotot), function(x) t(rbind(rhotot[,x], rhose[, x])))), 5)
    # Format and print
    colnames(rhomat) <- unlist(lapply(1:2, function(i) 
    c(paste(pvlabels, "Rho", sep=" ")[i], paste(pvlabels, "s.e.", sep=" ")[i])))
    return(round(rhomat, 3))
    } else {
    
    # Correlation of no PV with PV
    # PV names
    pvnames <- paste(pvlabels, "0", 1:5, sep="")
    # Complete dataset (listwise deletion)
    data <- na.omit(data[c(variable, pvnames, weight, "JKREP", "JKZONE")])
    # Replicate weighted correlations for PV1 (sampling error)
    rhopvrp <- lapply(1:max(data[["JKZONE"]]), function(i) cov.wt(x=data[c(variable, pvnames[1])], cor=T, 
                 wt=ifelse(data[["JKZONE"]] == i, 2*data[[weight]]*data[["JKREP"]], data[[weight]]))[[5]])
    # Total weighted correlation for imputation variance
    rhopvtot <- lapply(pvnames, function(i) cov.wt(x=data[c(variable,i)], cor=TRUE, wt= data[[weight]])[[5]])
    # Sampling variance, imputation variance, and SEs
    varw <- Reduce("+", lapply(rhopvrp, function(x) (x - rhopvtot[[1]])^2))
    varb <- (1+1/5)* apply(simplify2array(rhopvtot), c(1, 2), var) # slower, but Reduce(var) fails
    rhose <- (varw+varb)^(1/2)
    # Mean total weighted correlation
    rhotot <- Reduce("+", rhopvtot)/length(rhopvtot)
    # Combined rhos and SEs
    rhomat <- round(do.call(cbind, lapply(1:ncol(rhotot), function(x) t(rbind(rhotot[,x], rhose[, x])))), 5)
    # Format and print
    colnames(rhomat) <- unlist(lapply(1:2, function(i) 
    c(paste(c(variable, pvlabels), "Rho", sep=" ")[i], paste(c(variable, pvlabels), "SE", sep=" ")[i])))
    return(round(rhomat, 3))
    }
  }
# If by not supplied, calculate for complete sample    
if (missing(by)) { 
output <- rho.pv.input(variable=variable, pvlabels=pvlabels, weight=weight, data=data)
  } else {
    if (length(pvlabels)==2 & missing(variable)) {
    output <- lapply(split(data, factor(data[[by]])), function(x) rho.pv.input(pvlabels=pvlabels, weight=weight, data=x))
    } else {
      output <- lapply(split(data, factor(data[[by]])), function(x) rho.pv.input(variable=variable, pvlabels=pvlabels, weight=weight, data=x))
    }
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
