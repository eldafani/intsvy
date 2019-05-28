timss.reg.pv <-
  function(x, pvlabel="BSMMAT",  by, data, std=FALSE, export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.reg.pv(x=x, pvnames = paste(pvlabel, "0", 1:5, sep=""), by=by, data=data, std=std, export=export, 
                  name= name, folder=folder, config=timss8_conf) 
    
}

timss2015.reg.pv <-
  function(x, pvlabel="BSMMAT",  by, data, std=FALSE, export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.reg.pv(x=x, pvnames = paste(pvlabel, "0", 1:5, sep=""), by=by, data=data, std=std, export=export, 
                  name= name, folder=folder, config=timss2015_conf) 
}
