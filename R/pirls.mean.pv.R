pirls.mean.pv <-
  function(pvlabel="ASRREA", by, data, export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.mean.pv(pvnames = pvlabel, 
                   by=by, data=data, export=export,
                   name=name, folder=folder, config=pirls_conf)
    
}

