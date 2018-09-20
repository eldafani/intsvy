pirls.log <-
  function(y, x, by, data, export=FALSE, name= "output", folder=getwd()) { 
    
  intsvy.log(x=x, y=y, by=by, data=data, export=export,
            name=name, folder=folder, config=pirls_conf)
  }

pirls2016.log <-
  function(y, x, by, data, export=FALSE, name= "output", folder=getwd()) { 
    
    intsvy.log(x=x, y=y, by=by, data=data, export=export,
               name=name, folder=folder, config=pirls2016_conf)
  }