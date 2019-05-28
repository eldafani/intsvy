pirls.log.pv <- 
  function(pvlabel="ASRREA", x, cutoff, by, data, 
           export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.log.pv(x=x, pvnames = paste(pvlabel, "0", 1:5, sep=""), cutoff=cutoff, by=by, data=data, export=export, 
                  name= name, folder=folder, config=pirls_conf)
        
  }

pirls2016.log.pv <- 
  function(pvlabel="ASRREA", x, cutoff, by, data, 
           export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.log.pv(x=x, pvnames = paste(pvlabel, "0", 1:5, sep=""), cutoff=cutoff, by=by, data=data, export=export, 
                  name= name, folder=folder, config=pirls2016_conf)
    
  }
