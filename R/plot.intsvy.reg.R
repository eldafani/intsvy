plot.intsvy.reg <- function(x, vars, se=TRUE, sort=FALSE) {
  if (missing(vars)) {
    vars = row.names(x[[1]])
  }
  # it is assumed that last three columns are: "Freq" "Mean" "Std.err."
  coefE <- lapply(x, function(x) x[row.names(x) %in% vars, "Estimate"])
  coefS <- lapply(x, function(x) x[row.names(x) %in% vars, "Std. Error"])
  inds <- which(!is.na(coefE))
  estimates <- t(simplify2array(coefE[inds]))
  colnames(estimates) <- vars
  sErrors <- t(simplify2array(coefS[inds]))
  colnames(sErrors) <- vars
  if (isTRUE(sort)) {
    ord <- order(estimates[,ncol(estimates)])
    estimates <- estimates[ord,]
    sErrors <- sErrors[ord,]
  }
  ndf <- data.frame(melt(estimates), melt(sErrors)[,3])
  colnames(ndf) <- c("Group", "Coefficient", "Value", "se")
  ndf$valueL <- ndf$Value - ndf$se
  ndf$valueH <- ndf$Value + ndf$se
  # To change order of variable labels in facet_wrap (R-squared appears always in the end)
  ndf$Coefficient <- factor(ndf$Coefficient, levels = vars)
  pl <- ggplot(data=ndf, aes_string(x = "Value", y="Group", shape= "Coefficient", color="Coefficient")) +
    geom_point(size=5) +
    theme_bw() +
    facet_wrap(~Coefficient, scales="free_x") +
    theme(legend.position="top")
  if (se) {
    pl <- pl + geom_errorbarh(aes_string(xmin="valueL", xmax="valueH"), width=.5)
  }
  pl
}
