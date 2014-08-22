plot.intsvy.table <- function(itable, se=FALSE, stacked=FALSE) {
  # it is assumed that last three columns are:  "Freq"       "Percentage" "Std.err."  
  vars <- setdiff(colnames(itable), c("Freq", "Percentage", "Std.err."))
  colnames(itable)[ncol(itable)] = "se"
  nvar <- length(vars)
  pl <- NA
  if (stacked) {
    if (nvar == 1) {
      pl <- ggplot(data=itable, aes_string(y = "Percentage", x=vars[1])) + 
        geom_bar(stat="identity", position="fill") + theme_bw() + coord_flip() + 
        theme(legend.position="top")
    } else {
      if (nvar == 2) {
        pl <- ggplot(data=itable, aes_string(y = "Percentage", x=vars[1], fill=vars[2])) + 
          geom_bar(stat="identity", position="fill") + theme_bw() + coord_flip() + 
          theme(legend.position="top")
      } else {
        pl <- ggplot(data=itable, aes_string(y = "Percentage", x=vars[1], fill=vars[3])) + 
          geom_bar(stat="identity", position="stack") + theme_bw() + coord_flip() + 
          facet_wrap(as.formula(paste0("~", vars[2])))+ 
          theme(legend.position="top")
      }
    }
  } else {
    if (nvar == 1) {
      pl <- ggplot(data=itable, aes_string(y = "Percentage", x=vars[1])) + 
        geom_bar(stat="identity", position=position_dodge(width = 0.9)) + 
        theme_bw() + coord_flip() + 
        theme(legend.position="top")
    } else {
      if (nvar == 2) {
        pl <- ggplot(data=itable, aes_string(y = "Percentage", x=vars[1], fill=vars[2])) + 
          geom_bar(stat="identity", position=position_dodge(width = 0.9)) + 
          theme_bw() + coord_flip() + 
          facet_wrap(as.formula(paste0("~", vars[2])))+ 
          theme(legend.position="top")
      } else {
        pl <- ggplot(data=itable, aes_string(y = "Percentage", x=vars[1], fill=vars[2], col= vars[2])) + 
          geom_bar(stat="identity", position=position_dodge(width = 0.9)) + 
          theme_bw() + coord_flip() + 
          facet_wrap(as.formula(paste0("~", vars[3])))+ 
          theme(legend.position="top")
      }
    }
    
    if (se) {
      pl <- pl + geom_errorbar(aes(ymin=Percentage-se, ymax=Percentage+se), color="black", 
                               position=position_dodge(width = 0.9), width=.35) 
    } 
  }
  pl
}

