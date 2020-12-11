pirls.rho.pv <-
function(variable, pvlabel, by, data, export=FALSE, name= "output", folder=getwd()) {
  
  intsvy.rho.pv(variable=variable, pvnames = pvlabel, 
                by=by, data=data, export=export, 
                name= name, folder=folder, config = pirls_conf)
}
