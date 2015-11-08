timss.per.pv <- 
  function(pvlabel="BSMMAT", by, 
           data, export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.per.pv(pvlabel=pvlabel, by=by, data=data, export=export, 
                  name= name, folder=folder, config=timss8_conf)

}
