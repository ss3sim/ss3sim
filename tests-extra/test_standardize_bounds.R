require("r4ss")
#Set to the models folder
model.path<-"C:\\Users\\Christine\\Documents\\GitHub\\growth_models\\"

#Path and filename of OM and EM control file
OM.ctl<-"fla-om\\flaOM.ctl"
EM.ctl<-"fla-em\\flaEM.ctl"

#Use SS_parlines to get the proper names for parameters for the data frame
om.pars<-SS_parlines(ctlfile=paste0(model.path,OM.ctl))
em.pars<-SS_parlines(ctlfile=paste0(model.path,EM.ctl))

#From the Johnson et al paper
lo.percent<-c(rep(.5,7),rep(-20,3))
hi.percent<-c(500,1000,1000,rep(500,4),rep(20,3))

#Populate data frame using EM parameter names and percentages from the Johnson et al paper
#Indices are the parameters you want to modify
percent.df<-data.frame(Label=as.character(em.pars[c(1:6,17,24:26),"Label"]),lo=lo.percent,hi=hi.percent)


standardize_bounds(percent_df=percent.df,dir=model.path,em_ctl_file=EM.ctl,om_ctl_file=OM.ctl)

