intsvy.table <- function(variable, by, data, final_weight="W_FSTUWT", brr_weight="W_FSTR") {
  table.input <- function(variable, data, final_weight="W_FSTUWT", brr_weight="W_FSTR") {
    
    if (sum(is.na((data[[variable]])))==length(data[[variable]])) {
      result <- data.frame(NA, "Freq"=0, "Percentage"=NA, "Std.err."= NA)  
      names(result)[1] <- variable # var label for table, otherwise prints "Var1"
      return(result)
    }
    
    # Replicate weighted %s (sampling error)
    tabrp <- as.matrix(sapply(1:80, function(i) percent(as.factor(as.numeric(data[[variable]])), total=FALSE, 
                                                        weights=  data[[paste(brr_weight, i , sep="")]], na.rm=TRUE)))     
    
    # Total weighted %                                                                      
    tabtot <- percent(as.factor(as.numeric(data[[variable]])), weights= data[[final_weight]], na.rm = TRUE, total=FALSE)
    # Standard error
    if (length(tabtot)!=1) {
      tabse <- sqrt(rowSums((tabrp-tabtot)^2) / 20)
    }
    else {
      tabse <- 0
    }
    result <- data.frame(table(data[[variable]][drop=T]), "Percentage"=round(as.numeric(tabtot), 2), "Std.err."= round(tabse, 2))
    names(result)[1] <- variable # var label for table, otherwise prints "Var1"
    return(result)
  }
  # If by not supplied, calculate for complete sample    
  if (missing(by)) { 
    output <- table.input(variable=variable, data=data, final_weight=final_weight, brr_weight=brr_weight)
  } else {
    # Convert by variables to characters for ddply application
    for (i in by) {
      data[[c(i)]] <- as.character(data[[c(i)]])
    }
    output <- ddply(data, by, function(x) table.input(data=x, variable=variable, final_weight=final_weight, brr_weight=brr_weight))
  }
  class(output) <- c("intsvy.table", "data.frame")
  output
}


pisa.table <- 
function(variable, by, data, export=FALSE, name= "output", folder=getwd()) {
  output <- intsvy.table(variable, by, data, final_weight="W_FSTUWT", brr_weight="W_FSTR")
    
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
#    if (require(xlsx)) {
#      write.xlsx(output, file=file.path(folder, paste(name, ".xlsx", sep="")))
#    }
  }
  
  return(output)
}
