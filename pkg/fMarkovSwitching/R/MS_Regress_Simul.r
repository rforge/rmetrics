# Function for Simulation of a MS process

MS_Regress_Simul<-function(nr,Coeff,k,distrib="Normal")
{
# Error checking for arguments

if (!any(distrib=='Normal',distrib=='t'))
    stop('The distrib input should be Normal or t')

if (nr<0)
    stop('Wow, a negative number of observations (nr shoulbe be a positive integer)')

if (ncol(Coeff$P)!=nrow(Coeff$P))
    stop('The Coeff$P matrix should should be a square matrix (ncol=nrow)')

if (any(ncol(Coeff$P)!=k,nrow(Coeff$P)!=k))
    stop('The Coeff$P matrix should should have the same number of row and columns as k')

if ( (any(apply(Coeff$P,2,sum)>1.0001))|(any(apply(Coeff$P,2,sum)<.999)) )
    stop('The sum of each collum in Coeff$p should be equal to 1 (they are probabilities..)')

if (any((Coeff$P<0)+(Coeff$P>1)))
    stop('The Coeff$P stores probabilities and they should be lower than 1 and higher than 0')

if (any(Coeff$sigma<0))
    stop('All values at Coeff$sigma should be positive (they are standard deviations)');

if (any( ( (Coeff$S!=0)+(Coeff$S!=1))==2 ) )
    stop('The Coeff.S should only have values 1 and 0')

if (nrow(Coeff$nS_param)!=sum(Coeff$S==0))
    stop('The Coeff.nS_param should have the same number of rows as the number of zero elements at Coeff.S ');

if (nrow(Coeff$S_param)!=sum(Coeff$S==1))
    stop('The Coeff.S_param should have the same number of rows as number of 1 elements at Coeff.S ');

if (ncol(Coeff$S_param)!=k)
    stop('The Coeff.S_param should have the same number of collums as k ');

Rnd<-runif(nr)      # Seed for state transition process

States<-matrix(0,nr,k)
States[1,1]=1

for (i in 2:nr)  # Loop creation of states 
{

    state_past<-which(States[i-1,]==1)

    if (Rnd[i]<Coeff$P[state_past,state_past])
         States[i,state_past]<-1  # when staying at last state
    else   # when changing to other states
     {
        idx_other<-which(States[i-1,]==0)
        Prob2<-Coeff$P[,state_past]

        a=c(Coeff$P[state_past,state_past] , Prob2[idx_other])

        cum_sum<-cumsum(a)
        sorted<-sort(c(cum_sum , Rnd[i])) # throw the prob at cumsum of other states to get
                                          # where it stands (where to switch)

        idx<-which(Rnd[i]==sorted)-1      # find index

        States[i,idx_other[idx]]<-1       # and change state
      }
}

nIndep<-length(Coeff$S)
n_S<-sum(S)
n_nS<-nIndep-n_S

indep<-matrix(0,nr,nIndep)

for (i in 1:nIndep)
    indep[,i]<-rnorm(nr)

count<-0
countS<-0
indep_S<-matrix(0,nr,n_S)
indep_nS<-matrix(0,nr,n_nS)

for (i in 1:nIndep)
{

    if (Coeff$S[i]==1)
    {
        countS<-countS+1
        indep_S[,countS]<-indep[,i]
    }
    else
    {
        count<-count+1
        indep_nS[,count]<-indep[,i]
    }
}

rndError<-matrix(0,nr,k)

for (i in 1:k)
{
    if (distrib=='Normal')
        rndError[,i]<-Coeff$sigma[1,i]*rnorm(nr)
    if (distrib=='t')
        rndError[,i]<-Coeff$sigma[1,i]*rt(nr,Coeff$v[1,i])
}

Sim_x<-matrix(0,nr,1)
onesVec<-matrix(1,k,1)

Sim_x<-indep_nS%*%Coeff$nS_param+(States*(indep_S%*%Coeff$S_param))%*%onesVec + (States*rndError)%*%onesVec

# Passing up information to output structure
# Creating new MS_Simul S4 object

MS_Model_Out<-new("MS_Simul"          ,
                   nr=nr              ,
                   dep=Sim_x          ,
                   Coeff=Coeff        ,
                   trueStates=States  ,
                   indep=indep        ,
                   k=k                ,
                   S=Coeff$S          ,
                   distrib=distrib    )

return(MS_Model_Out)
}
