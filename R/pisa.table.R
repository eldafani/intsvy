pisa.table <- 
function(variable, by, data, export=FALSE, name= "output", folder=getwd()) {
  table.input <- function(variable, data) {
    
    if (sum(is.na((data[[variable]])))==length(data[[variable]])) {
      result <- data.frame(NA, "Freq"=0, "Percentage"=NA, "Std.err."= NA)  
      names(result)[1] <- variable # var label for table, otherwise prints "Var1"
      return(result)
     }
          
    # Replicate weighted %s (sampling error)
    tabrp <- as.matrix(sapply(1:80, function(i) percent(as.factor(as.numeric(data[[variable]])), total=FALSE, 
             weights=  data[[paste("W_FSTR", i , sep="")]], na.rm=TRUE)))     

    # Total weighted %                                                                      
    tabtot <- percent(as.factor(as.numeric(data[[variable]])), weights= data[["W_FSTUWT"]], na.rm = TRUE, total=F)
    # Standard error
    if (length(tabtot)!=1) {
      tabse <- (0.05*apply((tabrp-tabtot)^2, 1, sum))^(1/2)
    }
    else {
      tabse <-0
    }
    result <- data.frame(table(data[[variable]][drop=T]), "Percentage"=round(as.numeric(tabtot), 2), "Std.err."= round(tabse, 2))
    names(result)[1] <- variable # var label for table, otherwise prints "Var1"
    return(result)
  }
  # If by not supplied, calculate for complete sample    
  if (missing(by)) { 
    output <- table.input(variable=variable, data=data)
  } else {
  # Convert by variables to characters for ddply application
    for (i in by) {
      data[[c(i)]] <- as.character(data[[c(i)]])
    }
    output <- ddply(data, by, function(x) table.input(data=x, variable=variable))
  }
  
  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  
  return(output)
}
