
library(ss3sim)
library(r4ss)

## Todo: update to run ss3sim to get length and age comps
update.packages(c('knitr', 'devtools', 'roxygen2'))
## Build the ss3sim developement package
remove.packages("ss3sim")
devtools::install_github("ss3sim/ss3sim")
library(ss3sim)
library(r4ss)
library(nlme)

## I made a copy of the hake-om folder on 7/2/14 so we could test stuff and
## not break the original. Run it to get the initial output files.
setwd("C:/Users/Christine/Documents/GitHub/ss3sim/inst/extdata/models/cod-om")
file.copy("codOM.dat", "codOM_original.dat")
system("ss3 -maxfn 1 -nohess")
replist <- SS_output(dir=getwd(), covar=FALSE)
TSCplot(replist)


##Using hake data as placeholder
commHake<-get(load(file="C:/Users/Christine Stawitz/Documents/Data/NWFSC-PacFIN-BDS/PacFIN.PWHT.bds.09.Jun.2014.dmp"))
survHake<-read.csv("C:/Users/Christine Stawitz/Documents/Data/NWFSC-PacFIN-BDS/Survey/HaulLengthAge_Tri1977To2004_Stawitz_20140526.csv")
filteredCommHake<-commHake[,c(3,11,17,18,50)]
filteredCommHake<-filteredCommHake[filteredCommHake$SEX!="U",]
filteredCommHake<-filteredCommHake[!is.na(filteredCommHake$age1),]
filteredCommHake<-filteredCommHake[!is.na(filteredCommHake$FISH_LENGTH),]
survHake<-survHake[survHake$Sex.Determination!="u",]
survHake<-survHake[,c(10,14:16)]

vbgf_func<-function(L1,L2,k,ages,a3){
  #Function to predice length given VBGF parameters
  predLength<-L2+(L1-L2)*exp(-k*(ages-a3))
  return(predLength)
}

sample_fit_VBGF<-function(n.samples,length.data,start.L1,start.L2,start.k,start.logsigma,a3,A){
  #function takes data, number of samples, start values, start age
  #Data must have colums ordered: Year, Length, Weight, Sex, age
  #Then fits VBGF to subsampled data
  #Remove fish younger than a3 and older than A
  length.data<-length.data[length.data[,1]>a3,]
  length.data<-length.data[length.data[,1]<A,]
  lines.to.sample<-sample(1:nrow(length.data),size=n.samples,replace=FALSE)

  #Subsample data
  ages<-length.data[lines.to.sample,1]
  length.comp<-length.data[lines.to.sample,2]
  length.df<-data.frame(cbind(ages,length.comp))
  
  get_log_likelihood<-function(logL1,logL2,logk,logsigma){
    #Returns the negative log likelihood function
    
    L1<-exp(logL1)
    L2<-exp(logL2)
    k<-exp(logk)
    sigma<-exp(logsigma)*length.df[,1]
    predLength<-vbgf_func(L1,L2,k,length.df[,1],a3)
    logLik<-sum(-log(sigma)-((log(predLength)-log(length.df[,2]))^2)/(2*sigma^2))
    return(-logLik)
  }
  #Fit using MLE
  mod<-mle2(get_log_likelihood,start=list(logL1=start.L1,logL2=start.L2,logk=start.k,logsigma=start.logsigma))
  return(mod)
}

sim_test<-function(ages,L1,L2,k,sigma,a3,startL1,startL2,startK,startSig,A,numSamplesPerAge,numSamplesToFit){
  #Function to run a simulation test on the function
  #ages = age values to generate lengths for
  #L1,L2,k, sigma = values to generate data with
  #startL1,startL2...=initial values for mle
  #numSamplesPerAge=how many individuals of each Age to generate
  #numSamplesToFit=how many samples to take of the full data for estimation
  #Calculate means of each age
  fakeMeans<-vbgf_func(L1=L1,L2=L2,k=k,sort(unique(ages)),a3)
  fakeData<-matrix(nrow=length(unique(ages))*numSamplesPerAge,ncol=2)
  fakeData[,1]<-sort(rep(unique(ages),numSamplesPerAge))
  #Add variation that increases with age
  vect<-NULL
  for(i in 1:length(unique(ages))){
    vect<-c(vect,rnorm(numSamplesPerAge,fakeMeans[i],sigma*i))
  }
  fakeData[,2]<-vect
  
  #Plot data to check it
  plot(fakeData[,1],fakeData[,2])
  
  #Fit model
  vbgfmod<-sample_fit_VBGF(numSamplesToFit,fakeData,startL1,startL2,startK,startSig,a3,A)
  return(vbgfmod)
}


#Start values for optim
l2.guess<-log(800)
l1.guess<-log(100)
k.guess<-log(0.3)
a3guess<-2
sigma.guess<-log(1)


#Test using fake data
#Generate the data 
lines.to.sample<-sample(1:nrow(filteredCommHake),size=10000,replace=FALSE)
ages<-filteredCommHake[lines.to.sample,5]
meanLengths<-aggregate(FISH_LENGTH~age1,mean,data=filteredCommHake)
L1<-meanLengths[meanLengths$age1==2,2]
L2<-meanLengths[meanLengths$age1==10,2]
vbgfmod<-sim_test(ages,L1,L2,k=0.3,sigma=2,a3=2,startL1=l1.guess,startL2=l2.guess,startK=k.guess,startSig=sigma.guess,
                  A=12,numSamplesPerAge=1000,numSamplesToFit=5000)

#Check parameters
logEst<-vbgfmod@coef
exp(logEst)
#Sigma is still being underestimated - not sure why



#Fit the model to hake data
vbgfmod<-sample_fit_VBGF(1000,filteredCommHake,l1.guess,l2.guess,k.guess,sigma.guess,2,12)