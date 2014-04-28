timss.rho <-
function(variables, by, weight="TOTWGT", data, export=FALSE, name= "output", folder=getwd()) {
  rho.input <- function(variables, weight, data) {
  # Remove cases listwise
  data <- na.omit(data[c(variables, weight, "JKREP", "JKZONE")]) 
  # Fifth element is correlation matrix
  rhorp <- lapply(1:max(data[["JKZONE"]]), function(i) cov.wt(data[variables],
  wt=ifelse(data[["JKZONE"]] == i, 2*data[[weight]]*data[["JKREP"]], data[[weight]]), cor=TRUE)[[5]])
  rhotot <- cov.wt(data[variables], wt=data[[weight]], cor=TRUE)[[5]]
  # SE formula
  rhose <- Reduce("+", lapply(rhorp, function(x) (x-rhotot)^2))^(1/2) 
  # Combined rhos and SEs
  rhomat <- round(do.call(cbind, lapply(1:ncol(rhotot), function(x) t(rbind(rhotot[,x], rhose[, x])))), 3)
  colnames(rhomat) <- unlist(lapply(1:length(variables), function(x) 
  c(paste(variables, "Rho", sep=" ")[x], paste(variables, "s.e.", sep=" ")[x])))
  return(round(rhomat, 6))
  }
  # If by not supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- rho.input(variables=variables, weight=weight, data=data) 
  }
  else {
    output <- lapply(split(data, factor(data[[by]])), function(x) rho.input(variables=variables, weight=weight, data=x))
  }

  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
