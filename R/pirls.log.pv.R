pirls.log.pv <- 
  function(pvlabel, x, cutoff, by, data, 
           export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.log.pv(x=x, pvnames = pvlabel, cutoff=cutoff, by=by, data=data, export=export, 
                  name= name, folder=folder, config=pirls_conf)
        
}