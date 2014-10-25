# Example script for MS_Regress_Simul and MS_Regress_Fit (2 states, normal distribution)
# The script will first simulate a markov switching process given the input and then fit it using MS_Regress_Fit

library(fMarkovSwitching)   # Assuming library is installed

nr=500                      # Number of observations
distrib<-"Normal"           # distribution assumption
k<-2                        # number of states

PVec<-rbind(c(.8 ,.2),      # creating transition matrix 
            c(.2 ,.8))

P<-matrix(PVec,k,k)         # building it as a matrix

S<-c(1,0,0)                 # S argument (controls for where to switch)

nS_param<-matrix(c(.5,-.2),2,1)     # Setting up the coefficients at non switching parameters
S_param<-matrix(0,sum(S),k)         
S_param[,1]= .2                    # Setting up the coefficients at switching parameters (state 1)
S_param[,2]= -.1

sigma<-matrix(0,1,k)
sigma[1,1]=.02               # Setting up the standard deviavion of the model at State 1
sigma[1,2]=.01               # Setting up the standard deviavion of the model at State 2

Coeff<-list(P=P               ,
            S=S               ,
            nS_param=nS_param ,
            S_param=S_param   ,
            sigma=sigma       )

mySimul<-MS_Regress_Simul(nr,Coeff,k,distrib)   # calling simulation funciton

print(mySimul)
plot(mySimul)

dep<-mySimul@dep
indep<-mySimul@indep

myModel<-MS_Regress_Fit(dep,indep,S,k,distrib)  # calling fitting function
print(myModel)
plot(myModel)
