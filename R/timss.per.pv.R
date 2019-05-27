timss.per.pv <- 
  function(pvlabel="BSMMAT", by, per, data, export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.per.pv(pvnames = paste(pvlabel, "0", 1:5, sep=""), by=by, per=per, data=data, export=export, 
                  name= name, folder=folder, config=timss8_conf)

}


timss2015.per.pv <- 
  function(pvlabel="BSMMAT", by, per, data, export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.per.pv(pvnames = paste(pvlabel, "0", 1:5, sep=""), by=by, per=per, data=data, export=export, 
                  name= name, folder=folder, config=timss2015_conf)
    
  }
