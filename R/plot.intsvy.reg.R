plot.intsvy.reg <- function(x, se=TRUE, sort=FALSE, ...) {
  # it is assumed that last three columns are:  "Freq"       "Mean" "Std.err."  
  coefE <- lapply(x, function(x) x$Estimate)
  coefS <- lapply(x, function(x) x$`Std. Error`)
  inds <- which(!is.na(coefE))
  
  estimates <- t(simplify2array(coefE[inds]))
  colnames(estimates) <- rownames(x[[inds[1]]])
  
  sErrors <- t(simplify2array(coefS[inds]))
  colnames(sErrors) <- rownames(x[[inds[1]]])
  
  if (isTRUE(sort)) {
    ord <- order(estimates[,ncol(estimates)])
    estimates <- estimates[ord,]
    sErrors <- sErrors[ord,]
  }
  
  ndf <- data.frame(melt(estimates), melt(sErrors)[,3])
  colnames(ndf) <- c("group", "coefficient", "value", "se")
  ndf$valueL <- ndf$value - ndf$se
  ndf$valueH <- ndf$value + ndf$se
  
  pl <- ggplot(data=ndf, aes_string(x = "value", y="group", shape= "coefficient", color="coefficient")) + 
    geom_point(size=5) + 
    theme_bw() + 
    facet_wrap(~coefficient,  scales="free_x") + 
    theme(legend.position="top") 
    
  if (se) {
    pl <- pl + geom_errorbarh(aes_string(xmin="valueL", xmax="valueH"), width=.5) 
  }
  
  pl
}

