piaac.table <- 
  function(variable, by, data, export=FALSE, name= "output", folder=getwd()) {
    output <- intsvy.table(variable, by, data, final_weight="SPFWT0", brr_weight="SPFWT")
      
      if (export)  {
        write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
      }
    
    return(output)
  }
