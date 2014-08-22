piaac.mean.pv <- 
  function(pvlabel, by, data, export=FALSE, name= "output", folder=getwd()) {
    output <- intsvy.mean.pv(paste("PV", pvlabel, 1:10, sep=""), by, data, final_weight="SPFWT0", brr_weight="SPFWT", replication = 'piaac')
    
    if (export)  
      write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
    
    return(output)
  }

