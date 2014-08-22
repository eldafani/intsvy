plot.intsvy.reg <- function(iregs, se=TRUE, sort=FALSE) {
  # it is assumed that last three columns are:  "Freq"       "Mean" "Std.err."  
  coefE <- lapply(iregs, function(x) x$Estimate)
  coefS <- lapply(iregs, function(x) x$`Std. Error`)
  inds <- which(!is.na(coefE))
  
  estimates <- t(simplify2array(coefE[inds]))
  colnames(estimates) <- rownames(iregs[[inds[1]]])
  
  sErrors <- t(simplify2array(coefS[inds]))
  colnames(sErrors) <- rownames(iregs[[inds[1]]])
  
  if (isTRUE(sort)) {
    ord <- order(estimates[,ncol(estimates)])
    estimates <- estimates[ord,]
    sErrors <- sErrors[ord,]
  }
  
  require(reshape)
  ndf <- data.frame(melt(estimates), melt(sErrors)[,3])
  colnames(ndf) <- c("group", "coefficient", "value", "se")
  
  pl <- ggplot(data=ndf, aes(x = value, y=group, shape= coefficient, color=coefficient)) + 
    geom_point(size=5) + 
    theme_bw() + 
    facet_wrap(~coefficient,  scales="free_x") + 
    theme(legend.position="top") 
    
  if (se) {
    pl <- pl + geom_errorbarh(aes(xmin=value-se, xmax=value+se), width=.5) 
  }
  
  pl
}

