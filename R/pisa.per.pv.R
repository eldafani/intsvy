pisa.per.pv <-  function(pvlabel="READ", by, per, data, export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.per.pv(pvnames = paste("PV", 1:5, pvlabel, sep=""), by=by, per=per, data=data, export=export, 
                  name= name, folder=folder, config=pisa_conf)
    
}

pisa2015.per.pv <-  function(pvlabel="READ", by, per, data, export=FALSE, name= "output", folder=getwd()) {
  
  intsvy.per.pv(pvnames = paste("PV", 1:10, pvlabel, sep=""), by=by, per=per, data=data, export=export, 
                name= name, folder=folder, config=pisa2015_conf)
  
}
