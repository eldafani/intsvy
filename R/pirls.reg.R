pirls.reg <-
function(y, x, by, data, export=FALSE, name= "output", folder=getwd()) { 
  
  intsvy.reg(x=x, y=y, by=by, data=data, export=export,
             name=name, folder=folder, config=pirls_conf)
  
}

pirls2016.reg <-
  function(y, x, by, data, export=FALSE, name= "output", folder=getwd()) { 
    
    intsvy.reg(x=x, y=y, by=by, data=data, export=export,
               name=name, folder=folder, config=pirls2016_conf)
    
  }