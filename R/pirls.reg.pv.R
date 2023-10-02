pirls.reg.pv <-
  function(x, pvlabel, by, data, std=FALSE, export=FALSE, name= "output", folder=getwd()) {
    
    intsvy.reg.pv(x=x, pvnames = pvlabel, by=by, data=data, std=std, export=export, 
                  name= name, folder=folder, config=pirls_conf) 

}