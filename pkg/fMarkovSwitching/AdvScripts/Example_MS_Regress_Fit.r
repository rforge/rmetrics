
library(fMarkovSwitching)   # Assuming library is installed

data(indep)         # data from package
data(dep)           # data from package

S=c(1,0,0)          # swiching in first indep variable (the intercept)
distrib<-"Normal"   # Assumed distribution for residue
k<-2

dep=dep[1:100]
indep=indep[1:100,]

myModel<-MS_Regress_Fit(dep,indep,S,k,distrib)

print(myModel)
plot(myModel)
