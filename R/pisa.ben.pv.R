pisa.ben.pv <- 
function(pvlabel, by, cutoff, data, export=FALSE, name= "output", folder=getwd()) {
  
  intsvy.ben.pv(pvnames = paste("PV", 1:5, pvlabel, sep=""), by=by, cutoff=cutoff, data=data, export=export, name= name, folder=folder,
                config=pisa_conf)

}

pisa2015.ben.pv <- 
  function(pvlabel, by, cutoff, data, export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.ben.pv(pvnames = paste("PV", 1:10, pvlabel, sep=""), by=by, cutoff=cutoff, data=data, export=export, name= name, folder=folder,
                  config=pisa2015_conf)
    
  }
