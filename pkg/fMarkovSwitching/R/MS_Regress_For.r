# Function for calculation forecast in t+1 for markov switching model

MS_Regress_For<-function(myModel,newIndep)
{

nPeriods=1  # only for 1 period ahead forecast (maybe implement later for n periods ahead)

newIndep<-as.matrix(newIndep)

if (class(myModel)!="MS_Model")
    stop("The input myModel should be a S4 object of class MS_Model")
    
if (myModel@sizeModel$nIndep!=ncol(newIndep))
{
    stop("the number of columns in newIndep should match the number of independent
     variables used to fit the model (myModel@sizeModel$nIndep)")
}

if (nPeriods<1)
    stop("The input nPeriods should be  a positive integer")

# prealocating large matrices and figuring out which variables are switching

n_S   =myModel@sizeModel$n_S
n_nS  =myModel@sizeModel$n_nS
nIndep=myModel@sizeModel$nIndep
S     =myModel@sizeModel$S
k     =myModel@k
Coeff =myModel@Coeff

newIndep_S<-matrix(data = 0 , nrow = 1, ncol = n_S )
newIndep_nS<-matrix(data = 0, nrow = 1, ncol = n_nS)

count_nS=0
count_S=0
for (i in 1:nIndep)
{
  if (S[i]==1)
  {
      count_S<-count_S+1
      newIndep_S[,count_S]<-newIndep[,i]
  }
  else
  {
      count_nS<-count_nS+1
      newIndep_nS[,count_nS]<-newIndep[,i]
  }
}    
    
newFiltProb=myModel@Coeff$P%*%(myModel@filtProb[myModel@sizeModel$nr,]) # this is the filtered probabilities 
                                                                        # of t+1 conditional on the info in t

condMean<-matrix(0,nPeriods,k)  # conditional mean in all states

for (i in 1:nPeriods)
{
    for (j in 1:k)
    {
        condMean[i,j]<-newIndep_nS%*%Coeff$indep_nS + newIndep_S%*%(Coeff$indep_S[,j])
    }
  
    
}    

newCondMean=condMean%*%newFiltProb      # the new conditional mean is the weighted average of the cond means in each state
newCondStd =Coeff$sigma%*%newFiltProb   # same as cond mean.
    
forOut<-list(condMean=newCondMean,
             condStd =newCondStd )
             
return(forOut)                              
    
}
    
    
