piaac.reg.pv <- 
function(x, pvlabel="LIT", by, data, export=FALSE, name= "output", std=FALSE, folder=getwd()) {

  intsvy.reg.pv(x=x, pvnames = paste("PV", pvlabel, 1:10, sep=""), by=by, data=data, std=std, export=export, 
                name= name, folder=folder, config=piaac_conf) 
  
}
