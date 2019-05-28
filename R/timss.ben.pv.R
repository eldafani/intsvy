timss.ben.pv <- function(pvlabel, by, cutoff=cutoff, data, export=FALSE, name= "output", folder=getwd()) {
  
  intsvy.ben.pv(pvnames = paste(pvlabel, "0", 1:5, sep=""), by=by, cutoff=cutoff, data=data, export=export, name= name, folder=folder,
                config=timss8_conf)
    
}

timss2015.ben.pv <- function(pvlabel, by, cutoff=cutoff, data, export=FALSE, name= "output", folder=getwd()) {
  
  intsvy.ben.pv(pvnames = paste(pvlabel, "0", 1:5, sep=""), by=by, cutoff=cutoff, data=data, export=export, name= name, folder=folder,
                config=timss2015_conf)
  
}
