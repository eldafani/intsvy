pisa.log.pv <- 
  function(pvlabel="READ", cutoff= 606.99, x, by, weight="W_FSTUWT", 
           brr_weight="W_FSTR", data, export=FALSE, name= "output", folder=getwd()) {
    
# PV labels
pvnames <- paste("PV", 1:5, pvlabel, sep="")

log.pv.input <- function(pvlabel, cutoff, x, weight, brr_weight, data) {
  
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

# Replicate weighted coefficients for sampling error (5 PVs), normalised weights

coef.rp <- lapply(regform, function(k) lapply(1:80, function(i) 
  summary(glm(formula=as.formula(k), family=quasibinomial("logit"), 
              weights=nrow(data)*data[[paste0(brr_weight, i)]]/sum(data[[paste0(brr_weight, i)]]),
              data=data))))


# Retrieving coefficients
rp.coef <- lapply(1:length(pvnames), function(pv) sapply(1:80, function(i) coef.rp[[pv]][[i]]$coefficients[,1]))

# Total weighted coefficient for each PV for imputation (between) error
reg.pv <- lapply(regform, function(i) 
  summary(glm(formula=as.formula(i), family=quasibinomial("logit"), 
              weights=nrow(data)*data[[weight]]/sum(data[[weight]]), data=data)))


pv.coef <- sapply(1:length(pvnames), function(pv) reg.pv[[pv]]$coefficients[, 1])

# Mean total coefficients (across PVs)
mean.coef <- apply(pv.coef, 1, mean)


# Sampling error (variance within)

var.w <- apply(0.05*sapply(lapply(1:length(pvnames), function(pv) (rp.coef[[pv]]-pv.coef[,pv])^2), 
                           function(e) apply(e, 1, sum)), 1, mean)

# Imputation error (variance between)
var.b <- (1/(length(pvnames)- 1))*apply(sapply(1:length(pvnames), function(pv) 
  (pv.coef[, pv] - mean.coef)^2), 1, sum)

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
  output <- log.pv.input(pvlabel=pvlabel, cutoff=cutoff, x=x, weight=weight, 
            brr_weight=brr_weight, data=data) 
} else {
  output <- lapply(split(data, droplevels(data[by])), function(i) 
    log.pv.input(pvlabel=pvlabel, cutoff=cutoff, x=x, weight=weight, 
                 brr_weight=brr_weight, data=i))
}

if (export)  {
  write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
}

return(output)
}