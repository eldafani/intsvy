pisa.mean.pv <-  
function(pvnames, by, data, export=FALSE, name= "output", folder=getwd()) {
  
  intsvy.mean.pv(pvnames = pvnames, by=by, data=data, export=export,
                 name=name, folder=folder, config=pisa_conf)
  
}

