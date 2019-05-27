pirls.per.pv <- 
  function(pvlabel="ASRREA", by, per, data, export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.per.pv(pvnames = paste(pvlabel, "0", 1:5, sep=""), per=per, by=by, data=data, export=export, 
                  name= name, folder=folder, config=pirls_conf)
    
}

pirls2016.per.pv <- 
  function(pvlabel="ASRREA", by, per, data, export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.per.pv(pvnames = paste(pvlabel, "0", 1:5, sep=""), per=per, by=by, data=data, export=export, 
                  name= name, folder=folder, config=pirls2016_conf)
    
  }
