timss.log.pv <- 
  function(pvlabel="BSMMAT", x, by, cutoff, data, 
           export=FALSE, name= "output", folder=getwd()) {
    
  intsvy.log.pv(x=x, pvnames = paste(pvlabel, "0", 1:5, sep=""), cutoff=cutoff, by=by, data=data, export=export, 
                name= name, folder=folder, config=timss8_conf)
  
  }


timss2015.log.pv <- 
  function(pvlabel="BSMMAT", x, by, cutoff, data, 
           export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.log.pv(x=x, pvnames = paste(pvlabel, "0", 1:5, sep=""), cutoff=cutoff, by=by, data=data, export=export, 
                  name= name, folder=folder, config=timss2015_conf)
    
  }
