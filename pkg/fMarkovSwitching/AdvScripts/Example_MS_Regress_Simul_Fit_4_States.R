# Example script for MS_Regress_Simul and MS_Regress_Fit (4 states, normal distribution)
# The script will first simulate a markov switching process given the input and then fit it using MS_Regress_Fit

library(fMarkovSwitching)   # Assuming library is installed

nr=500
distrib<-"Normal"
k<-4

PVec<-rbind(c(.7,.1,.1,.1),
            c(.1,.7,.1,.1),
            c(.1,.1,.7,.1),
            c(.1,.1,.1,.7))

        
P<-matrix(PVec,k,k)
S<-c(1,0,0)
nS_param<-matrix(c(.5,-.2),2,1)    # Setting up the coefficients at non switching parameters
S_param<-matrix(0,sum(S),k)
S_param[,1]= .2             # Setting up the coefficients at switching parameters
S_param[,2]=-.3
S_param[,3]= .3
S_param[,4]= 1

sigma<-matrix(0,1,k)
sigma[1,1]=.05               # Setting up the standard deviavion of the model at State 1
sigma[1,2]=.01               # Setting up the standard deviavion of the model at State 2
sigma[1,3]=.02               # Setting up the standard deviavion of the model at State 2
sigma[1,4]=.005              # Setting up the standard deviavion of the model at State 2

Coeff<-list(P=P               ,
            S=S               ,
            nS_param=nS_param ,
            S_param=S_param   ,
            sigma=sigma       )

mySimul<-MS_Regress_Simul(nr,Coeff,k,distrib)

print(mySimul)
plot(mySimul)

dep<-mySimul@dep
indep<-mySimul@indep

myModel<-MS_Regress_Fit(dep,indep,S,k,distrib)
print(myModel)
plot(myModel)
