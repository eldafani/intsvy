pirls.reg.pv <-
  function(x, pvlabel="ASRREA", by, data, std=FALSE, export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.reg.pv(x=x, pvnames = paste(pvlabel, "0", 1:5, sep=""), by=by, data=data, std=std, export=export, 
                  name= name, folder=folder, config=pirls_conf) 

}

pirls2016.reg.pv <-
  function(x, pvlabel="ASRREA", by, data, std=FALSE, export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.reg.pv(x=x, pvnames = paste(pvlabel, "0", 1:5, sep=""), by=by, data=data, std=std, export=export, 
                  name= name, folder=folder, config=pirls2016_conf) 
    
  }
