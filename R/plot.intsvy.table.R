plot.intsvy.table <- function(x, se=FALSE, stacked = FALSE, centered = FALSE, midpoint = NA, ...) {
  # it is assumed that last three columns are:  "Freq"       "Percentage" "Std.err."  
  vars <- setdiff(colnames(x), c("Freq", "Percentage", "Std.err."))
  colnames(x)[ncol(x)] = "se"
  nvar <- length(vars)
  x$PercentageL <- x$Percentage - 1.96*x$se
  x$PercentageH <- x$Percentage + 1.96*x$se
  pl <- NA
  # likert style plots
  if (centered) {
    CumSumPercentage <- CumSumPercentageLow <- NULL
    # make sure that the data.frame x is sorted
    tmp <- apply(x[,vars[-nvar], drop = FALSE], 1, paste, collapse = "")
    if (nvar > 1) {
      x <- x[order(tmp),]
      tmp <- tmp[order(tmp)]
    }
    # calculate midpoint if not specified
    if (is.na(midpoint)) {
      midpoint <- (length(unique(x[,vars[nvar]]))+1) / 2
    }
    # calculate centered cumsums
    if (midpoint %% 1 == 0) {
      # even number of levels
      x$CumSumPercentage <- unlist(tapply(x$Percentage, tmp, function(y) {
        cumsum(y) - sum(y[(1:midpoint)-1]) - y[midpoint]/2
      }))
    } else {
      # odd number of levels
      x$CumSumPercentage <- unlist(tapply(x$Percentage, tmp, function(y) {
        cumsum(y) - sum(y[(0:midpoint)[-1]])
      }))
    }
    x$CumSumPercentageLow <- x$CumSumPercentage - x$Percentage
    
    if (nvar == 1) {
      pl <- ggplot(data=x, aes(y = CumSumPercentage, ymax = CumSumPercentage, ymin = CumSumPercentageLow, x="variable")) + 
        geom_crossbar(aes_string(fill = vars[1]), size=0) + ylab("Percentage")
    } else {
      if (nvar == 2) {
        pl <- ggplot(data=x, aes_string(y = "CumSumPercentage", ymax = "CumSumPercentage", ymin = "CumSumPercentageLow", x=vars[1], fill=vars[2])) + 
          geom_crossbar(size=0)  + ylab("Percentage")
      } else {
        pl <- ggplot(data=x, aes_string(y = "CumSumPercentage", ymax = "CumSumPercentage", ymin = "CumSumPercentageLow", x=vars[1], fill=vars[nvar])) + 
          geom_crossbar(size=0)  +  
          facet_wrap(as.formula(paste0("~", vars[2]))) + ylab("Percentage")
      }
    } 
  }
  if (stacked & !centered) {
    if (nvar == 1) {
      pl <- ggplot(data=x, aes_string(y = "Percentage", x=vars[1])) + 
        geom_bar(stat="identity", position="fill") 
    } else {
      if (nvar == 2) {
        pl <- ggplot(data=x, aes_string(y = "Percentage", x=vars[1], fill=vars[2])) + 
          geom_bar(stat="identity", position="fill")       
      } else {
        pl <- ggplot(data=x, aes_string(y = "Percentage", x=vars[1], fill=vars[3])) + 
          geom_bar(stat="identity", position="stack") +  
          facet_wrap(as.formula(paste0("~", vars[2])))
      }
    }
  } 
  if (!stacked & !centered) {
    if (nvar == 1) {
      pl <- ggplot(data=x, aes_string(y = "Percentage", x=vars[1])) + 
        geom_bar(stat="identity", position=position_dodge(width = 0.9)) 
    } else {
      if (nvar == 2) {
        pl <- ggplot(data=x, aes_string(y = "Percentage", x=vars[1], fill=vars[2])) + 
          geom_bar(stat="identity", position=position_dodge(width = 0.9)) + 
          facet_wrap(as.formula(paste0("~", vars[2])))
      } else {
        pl <- ggplot(data=x, aes_string(y = "Percentage", x=vars[1], fill=vars[2], col= vars[2])) + 
          geom_bar(stat="identity", position=position_dodge(width = 0.9)) + 
          facet_wrap(as.formula(paste0("~", vars[3])))
      }
    }
    
    if (se) {
      pl <- pl + geom_errorbar(aes_string(ymin="PercentageL", ymax="PercentageH"), color="black", 
                               position=position_dodge(width = 0.9), width=.35) 
    } 
  }
  pl <- pl + theme_bw() + coord_flip() + theme(legend.position="top")
  pl
}
  