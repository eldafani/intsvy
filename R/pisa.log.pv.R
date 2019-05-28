pisa.log.pv <- 
  function(pvlabel="READ", x, by, cutoff, data, export=FALSE, 
           name= "output", folder=getwd()) {
    
  intsvy.log.pv(x=x, pvnames = paste("PV", 1:5, pvlabel, sep=""), cutoff=cutoff, by=by, data=data, export=export, 
                name= name, folder=folder, config=pisa_conf)
  
}

pisa2015.log.pv <- 
  function(pvlabel="READ", x, by, cutoff, data, export=FALSE, 
           name= "output", folder=getwd()) {
    
    intsvy.log.pv(x=x, pvnames = paste("PV", 1:10, pvlabel, sep=""), cutoff=cutoff, by=by, data=data, export=export, 
                  name= name, folder=folder, config=pisa2015_conf)
    
}

