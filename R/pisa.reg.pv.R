pisa.reg.pv <- 
  function(x, pvlabel="READ", by, data, export=FALSE, name= "output", folder=getwd(), std=FALSE) {
    
    intsvy.reg.pv(x=x, pvnames = paste("PV", 1:5, pvlabel, sep=""), by=by, data=data, std=std, export=export, 
                  name= name, folder=folder, config=pisa_conf) 
}

pisa2015.reg.pv <- 
  function(x, pvlabel="READ", by, data, export=FALSE, name= "output", folder=getwd(), std=FALSE) {
    
    intsvy.reg.pv(x=x, pvnames = paste("PV", 1:10, pvlabel, sep=""), by=by, data=data, std=std, export=export, 
                  name= name, folder=folder, config=pisa2015_conf) 
  }
