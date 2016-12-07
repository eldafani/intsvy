pisa.rho <-
  function(variables, by, data, export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.rho(variables=variables, by=by, data=data, export=export, name=name,
               folder=folder, config = pisa_conf)

}

pisa2015.rho <-
function(variables, by, data, export=FALSE, name= "output", folder=getwd()) {
  
  intsvy.rho(variables=variables, by=by, data=data, export=export, name=name,
             folder=folder, config = pisa2015_conf)
  
}