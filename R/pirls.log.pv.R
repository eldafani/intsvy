pirls.log.pv <- 
  function(pvlabel="ASRREA", x, by,  
           data, export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.log.pv(x=x, pvlabel=pvlabel, by=by, data=data, export=export, 
                  name= name, folder=folder, config=pirls_conf)
        
  }