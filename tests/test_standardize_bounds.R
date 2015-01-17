require("r4ss")
#Set to the models folder
model.path<-"C:\\Users\\Christine\\Documents\\GitHub\\growth_models\\"

#Path and filename of OM and EM control file
OM.ctl<-paste0(model.path,"fla-om\\flaOM.ctl")
EM.ctl<-paste0(model.path,"fla-em\\flaEM.ctl")

#Use SS_parlines to get the proper names for parameters for the data frame
om.pars<-SS_parlines(ctlfile=OM.ctl)
em.pars<-SS_parlines(ctlfile=EM.ctl)

#From the Johnson et al paper
lo.percent<-rep(.5,11)
hi.percent<-c(500,1000,1000,rep(500,8))

#Populate data frame using EM parameter names and percentages from the Johnson et al paper
percent.df<-data.frame(Label=as.character(em.pars[c(1:6,17,27:30),"Label"]),lo=lo.percent,hi=hi.percent)

#Run function
standardize_bounds(percent_df=percent.df,EM_ctl_file=EM.ctl,OM_ctl_file=OM.ctl)

