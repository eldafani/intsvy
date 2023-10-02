pisa.log.pv <- 
  function(pvlabel, x, by, cutoff, data, export=FALSE, 
           name= "output", folder=getwd()) {
    
  intsvy.log.pv(x=x, pvnames = pvlabel, cutoff=cutoff, by=by, data=data, export=export, 
                name= name, folder=folder, config=pisa_conf)
  
}


