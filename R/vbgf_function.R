
library(ss3sim)
library(r4ss)

## Todo: update to run ss3sim to get length and age comps
update.packages(c('knitr', 'devtools', 'roxygen2'))
## Build the ss3sim developement package
remove.packages("ss3sim")
devtools::install_github("ss3sim/ss3sim")
library(ss3sim)
library(r4ss)

## I made a copy of the hake-om folder on 7/2/14 so we could test stuff and
## not break the original. Run it to get the initial output files.
setwd("C:/Users/Christine/Documents/GitHub/ss3sim/inst/extdata/models/cod-om")
file.copy("codOM.dat", "codOM_original.dat")
system("ss3 -maxfn 1 -nohess")
replist <- SS_output(dir=getwd(), covar=FALSE)
TSCplot(replist)


##Using hake data as placeholder
# commHake<-get(load(file="C:/Users/Christine Stawitz/Documents/Data/NWFSC-PacFIN-BDS/PacFIN.PWHT.bds.09.Jun.2014.dmp"))
# survHake<-read.csv("C:/Users/Christine Stawitz/Documents/Data/NWFSC-PacFIN-BDS/Survey/HaulLengthAge_Tri1977To2004_Stawitz_20140526.csv")
# filteredCommHake<-commHake[,c(3,11,17,18,50)]
# filteredCommHake<-filteredCommHake[filteredCommHake$SEX!="U",]
# survHake<-survHake[survHake$Sex.Determination!="u",]
# survHake<-survHake[,c(10,14:16)]

#Start values for nls
l.inf.guess<-800
k.guess<-0.3
tzero.guess<-0

sample_fit_VBGF<-function(n.samples, length.data,start.linf,start.k,start.Tzero,scenario){
  #function takes data, number of samples, start values, scenario as input
  #Then fits VBGF to subsampled data
  lines.to.sample<-sample(1:nrow(length.data),size=n.samples,replace=FALSE)
  age.comp<-length.data[lines.to.sample,5]
  length.comp<-length.data[lines.to.sample,2]
  vbgf<-nls(formula=length.comp~Linf*(1-exp(-1*k*(age.comp-Tzero))),
            start=c(Linf=start.linf,k=start.k,Tzero=start.Tzero))
  return(vbgf)
}

vbgfmod<-sample_fit_VBGF(1000,filteredCommHake,l.inf.guess,k.guess,tzero.guess)
