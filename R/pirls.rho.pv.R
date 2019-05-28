pirls.rho.pv <-
function(variable, pvlabels, by, data, export=FALSE, name= "output", folder=getwd()) {
  
  intsvy.rho.pv(variable=variable, pvnames = paste(pvlabel, "0", 1:5, sep=""), 
                by=by, data=data, export=export, 
                name= name, folder=folder, config = pirls_conf)
}
