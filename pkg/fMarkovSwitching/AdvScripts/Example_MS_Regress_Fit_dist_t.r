library(fMarkovSwitching)   # Assuming library is installed

data(indep)         # data from package
data(dep)           # data from package

S=c(1,0,0)
distrib<-"t"
k<-2

myModel<-MS_Regress_Fit(dep,indep,S,k,distrib)
print(myModel)
plot(myModel)
