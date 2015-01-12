model.path<-"C:\\Users\\Christine Stawitz\\Documents\\GitHub\\estgrowth\\models\\"

OM.ctl<-paste0(model.path,"coa-om\\coaOM.ctl")
EM.ctl<-paste0(model.path,"coa-em\\codEM.ctl")

om.pars<-SS_parlines(ctlfile=OM.ctl)
em.pars<-SS_parlines(ctlfile=EM.ctl)
em.pars[,"Label"]

lo.percent<-rep(.5,11)
hi.percent<-c(500,1000,1000,rep(500,8))
percent.df<-data.frame(label=as.character(em.pars[c(1:6,17,27:30),"Label"]),lo=lo.percent,hi=hi.percent)
standardize_bounds(percent.df,EM.ctl.file=EM.ctl)

SS_changepars(dir=substr(EM.ctl,1,nchar(EM.ctl)-9),ctlfile=substr(EM.ctl,nchar(EM.ctl)-8,nchar(EM.ctl)),
              newctlfile="ControlModified.ctl",strings=c("NatM_p_1_Fem_GP_1"),newval=.3)
