# Example script for MS_Regress_Simul (2 states, t distribution)

library(fMarkovSwitching)   # Assuming library is installed

nr=500
distrib<-"t"
k<-2

PVec<-c(.8 ,.2,
        .1 ,.9)
P<-matrix(PVec,k,k)
S<-c(1,0,0)
nS_param<-matrix(c(.5,-.2),2,1)    # Setting up the coefficients at non switching parameters
S_param<-matrix(0,sum(S),k)
S_param[,1]= .2             # Setting up the coefficients at switching parameters
S_param[,2]=-.3

sigma<-matrix(0,1,k)
sigma[1,1]=.05               # Setting up the standard deviavion of the model at State 1
sigma[1,2]=.01               # Setting up the standard deviavion of the model at State 2

v<-matrix(0,1,k)
v[1,1]<-5
v[1,2]<-10

Coeff<-list(P=P               ,
            S=S               ,
            nS_param=nS_param ,
            S_param=S_param   ,
            sigma=sigma       ,
            v=v               )

mySimul<-MS_Regress_Simul(nr,Coeff,k,distrib)

print(mySimul)
plot(mySimul)

dep<-mySimul@dep
indep<-mySimul@indep

write(indep,file="indep.txt",ncolumns=3)
write(dep,file="dep.txt",ncolumns=1)

#myModel<-MS_Regress_Fit(dep,indep,S,distrib)
#print(myModel)
#plot(myModel)
