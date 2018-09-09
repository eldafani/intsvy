timss.reg <-
function(y, x, by, data, export=FALSE, name= "output", folder=getwd()) { 

  intsvy.reg(x=x, y=y, by=by, data=data, export=export,
             name=name, folder=folder, config=timss4_conf)
  
}

timss2015.reg <-
  function(y, x, by, data, export=FALSE, name= "output", folder=getwd()) { 
    
    intsvy.reg(x=x, y=y, by=by, data=data, export=export,
               name=name, folder=folder, config=timss2015_conf)
    
  }
