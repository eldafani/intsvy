pisa.reg.pv <- 
  function(x, pvlabel, by, data, export=FALSE, name= "output", folder=getwd(), std=FALSE) {
    
    intsvy.reg.pv(x=x, pvnames = pvlabel, by=by, data=data, std=std, export=export, 
                  name= name, folder=folder, config=pisa_conf) 
}

