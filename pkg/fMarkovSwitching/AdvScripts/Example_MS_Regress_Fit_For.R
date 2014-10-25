library(fMarkovSwitching)   # Assuming library is installed

data(indep)         # data from package
data(dep)           # data from package

idx=201     # idx of the observation to be forecasted

dep<-dep[1:idx]
indep<-indep[1:idx,]

S=c(1,0,0)
distrib<-"Normal"
k<-2

# new dep and indep (without last observation, which will be forecasted)

dep=as.matrix(dep)
indep=as.matrix(indep)

newDep=dep[-nrow(dep)]        
newIndep=indep[-nrow(indep),]

# Fit the model with ex ante data

myModel<-MS_Regress_Fit(newDep,newIndep,S,k,distrib) 
print(myModel)
plot(myModel)

# New indep matrix (maybe lagged variables ???)

newIndep=as.matrix(t(indep[idx,])) 
nPeriods=1

# Forecasting function

myFor<-MS_Regress_For(myModel,newIndep)

cat("\nForecast for conditional Mean in t+1= ",myFor$condMean,"\n")
cat("Forecast for conditional Standard deviation (sigma) in t+1= ",myFor$condStd,"\n")
