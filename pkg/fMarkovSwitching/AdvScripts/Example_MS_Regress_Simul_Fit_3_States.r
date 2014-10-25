# Example script for MS_Regress_Simul and MS_Regress_Fit (3 states, normal distribution)
# The script will first simulate a markov switching process given the input and then fit it using MS_Regress_Fit

library(fMarkovSwitching)   # Assuming library is installed

nr=500              # Number of observations in simulated time series
distrib<-"Normal"   # Distribution assumption
k<-3                # Number of States  

PVec<-rbind(c(.7 ,.05,.15),     # Building transition matrix
            c(.15,.9 ,.05),
            c(.15,.05,.8 ))
        
P<-matrix(PVec,k,k)

S<-c(1,0,0)         # S argument (controls which variables have switching effect)

nS_param<-matrix(c(.5,-.2),2,1)    # Setting up the coefficients at non switching parameters

S_param<-matrix(0,sum(S),k)
S_param[,1]= .2             # Setting up the coefficients at switching parameters
S_param[,2]=-.3
S_param[,3]= .3

sigma<-matrix(0,1,k)
sigma[1,1]=.05               # Setting up the standard deviavion of the model at State 1
sigma[1,2]=.01               # Setting up the standard deviavion of the model at State 2
sigma[1,3]=.02               # Setting up the standard deviavion of the model at State 3

Coeff<-list(P=P               , # saving all coefficients in Coeff list
            S=S               ,
            nS_param=nS_param ,
            S_param=S_param   ,
            sigma=sigma       )

mySimul<-MS_Regress_Simul(nr,Coeff,k,distrib)   # simulation function (returns S4 object)

print(mySimul)
plot(mySimul)

dep<-mySimul@dep
indep<-mySimul@indep

myModel<-MS_Regress_Fit(dep,indep,S,k,distrib)
print(myModel)
plot(myModel)
