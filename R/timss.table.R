timss.table <-
function(variable, by, data, weight="TOTWGT", export=FALSE, name= "output", folder=getwd()) {
  table.input <- function(variable, data, weight) {
    
    # Return NAs if missing
    if (sum(is.na((data[[variable]])))==length(data[[variable]])) {
      result <- data.frame(NA, "Freq"=0, "Percentage"=NA, "Std.err."= NA)  
      names(result)[1] <- variable 
      return(result)
    }
    
    # Replicate weighted %s (sampling error)
    tabrp <- as.matrix(sapply(1:max(data[["JKZONE"]]), function(x) percent(as.factor(as.numeric(data[[variable]])), total=FALSE, weights= ifelse(data[["JKZONE"]] == x, 
              2*data[[weight]]*data[["JKREP"]], data[[weight]]), na.rm = TRUE)))
    
    # Total weighted %                                                                      
    tabtot <- percent(as.factor(as.numeric(data[[variable]])), weights= data[[weight]], na.rm = TRUE, total=F)
    # Standard error
    if (length(tabtot)!=1) {
      tabse <- apply((tabrp-tabtot)^2, 1, sum)^(1/2)
    }
    else {
      tabse <-0
    }
    
    result <- data.frame(table(data[[variable]][drop=T]), "Percentage"=round(as.numeric(tabtot), 2), 
              "Std.err."= round(tabse, 2))
    names(result)[1] <- variable # var label for table, otherwise prints "Var1"
    return(result)
  }
  # If by not supplied, calculate for complete sample    
  if (missing(by)) { 
    output <-table.input(variable=variable, weight=weight, data=data)
  } else {
    output <- ddply(data, by, function(x) table.input(data=x, weight=weight, variable=variable))
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
