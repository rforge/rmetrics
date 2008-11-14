# Function for estimating a MS model from Data

MS_Regress_Fit<-function(dep,indep,S,k=2,distIn="Normal")
{
# Error Checking on arguments

dep<-as.matrix(dep)
indep<-as.matrix(indep)

if (!any(distIn=='Normal',distIn=='t'))
    stop('The distrib input should be Normal or t')

if (ncol(dep)>1)
    stop('The dep variable should be a vector, not a matrix')

if (nrow(dep)!=nrow(indep))
    stop('The number of rows at dep should be equal to the number of rows at indep')

if (k<2||sum(S)==0)
    stop('k should be an integer higher than 1 and S shoud have at least a index with value 1. If you trying to do a simple regression just use lm at stat toolbox')

if (ncol(indep)!=length(S))
    stop('The number of collums at indep should match the number of collums at S')

if (sum((S==0)+(S==1))!=length(S))
    stop('The S input should have only 1 and 0 values (those tell the function where to place markov switching effects)') 

# Some precalculations

# Figuring out size of problem

nr=length(dep)
nIndep=ncol(indep)
n_S=sum(S)
n_nS=nIndep-n_S

count_nS=0
count_S=0
S_S<-0
S_nS<-0

# prealocating large matrices and figuring out which variables are switching

indep_S<-matrix(data = 0, nrow = nr, ncol = n_S)
indep_nS<-matrix(data = 0, nrow = nr, ncol = n_nS)

for (i in 1:length(S))
{
  if (S[i]==1)
  {
      count_S<-count_S+1
      S_S[count_S]<-i
      indep_S[,count_S]<-indep[,i]
  }
  else
  {
      count_nS<-count_nS+1
      S_nS[count_nS]<-i
      indep_nS[,count_nS]<-indep[,i]
  }
}

if (n_nS!=0)
  OLS_indep_nS<-lm(dep~indep_nS-1)$coefficients
  only_S<-FALSE
if(n_nS==0)
  only_S<-TRUE

OLS_indep_S<- lm(dep~indep_S-1)$coefficients

param0_indep_S<-c(OLS_indep_S)
for (i in 2:k)
    param0_indep_S<-c(param0_indep_S , OLS_indep_S/i) # building param0 of switching variables (each decreasing value)

if (distIn=="t")
    param0_v<-c(rep(10,k))  # degree of freedom for t distribution

# Organizing the P matrix (the transition probabilities)

initial_pii<-.9     # initial p_ii for all states

param0_P=matrix((1-initial_pii)/(k-1),1,k*(k-1))
mySeq<-seq(1,(k*(k-1)),k)

for (i in 1:length(param0_P))
{
        if (any(i==mySeq))
            param0_P[i]<-initial_pii
}

# Building param0 vector

if (distIn=="Normal")
{
    if (!only_S)
    {
       param0=c(rep(sd(dep),k)  ,OLS_indep_nS    , param0_indep_S  , param0_P)
       lower =c(rep(0.00001,k)  ,rep(-Inf,n_nS) , rep(-Inf,n_S*k) , rep(0.000001,k*(k-1)))
       upper =c(rep(Inf,k)      ,rep( Inf,n_nS) , rep( Inf,n_S*k) , rep(0.999999,k*(k-1)))
    }
    if (only_S)
    {
       param0=c(rep(sd(dep),k) , param0_indep_S , param0_P)
       lower =c(rep(0.00001,k) ,rep(-Inf,n_nS)  , rep(-Inf,n_S*k) , rep(0.000001,k*(k-1)))
       upper =c(rep(Inf,k)     ,rep( Inf,n_nS)  , rep( Inf,n_S*k) , rep(0.999999,k*(k-1)))
    }
}
if (distIn=="t")       
{
      if (!only_S)
      {
       param0=c(rep(sd(dep),k) , OLS_indep_nS , param0_indep_S ,param0_v  , param0_P)
       lower =c(rep(0.00001,k) ,rep(-Inf,n_nS), rep(-Inf,n_S*k),rep( 1 ,k)  ,rep(0.000001,k*(k-1)))
       upper =c(rep(Inf,k)     ,rep( Inf,n_nS), rep( Inf,n_S*k),rep(Inf,k),  rep(0.999999,k*(k-1)))
      }
if (only_S)
      {
       param0=c(rep(sd(dep),k) ,param0_indep_S  ,param0_v  , param0_P)
       lower= c(rep(0.00001,k) ,rep(-Inf,n_S*k) ,rep( 1,k)  ,  rep(0.000001,k*(k-1)))
       upper= c(rep(Inf,2)     ,rep( Inf,n_S*k) ,rep( Inf,k) , rep(0.999999,k*(k-1)))
      }
}

# linear inequality constraints (see donlp2 documentation for details)

A<-matrix(0,nrow=k,ncol=length(param0)) # see donlp2 documentation for details

if (distIn=="Normal")
    idx_P=k+n_nS+n_S*k+1    # fist index for P matrix in param0 vector (normal dist)
if (distIn=="t")
    idx_P=k+n_nS+n_S*k+k+1  # fist index for P matrix in param0 vector (t dist)

for (i in 1:(k))
    A[i,(idx_P+(i-1)*(k-1)):(idx_P+(k-2)+(i-1)*(k-1))]=1    # see doc of donlp2

lin.lower<-rep(0,k) # the sum of transition probs in each state is higher than 0
lin.upper<-rep(1,k) # the sum of transition probs in each state is lower  than 1

# Fit model with donlp2

typeCall<-"optim"   # this controls the output of the likelihood function. If typeCall== "optim", the 
                    # output is the sum of log likelihood, if typeCall=="filtering", it returns the model coefficients
                    # along with filtered time series (smoothed/filtered probabilibites, conditional mean, etc)

# donlp2 optimization function only takes one argument so Im passing the rest of it as global variables
# (I dont really like doing it, but I have no other choice)

assign("dep"        , dep       , envir = .GlobalEnv)
assign("indep_S"    , indep_S   , envir = .GlobalEnv)
assign("indep_nS"   , indep_nS  , envir = .GlobalEnv)
assign("k"          , k         , envir = .GlobalEnv)
assign("S"          , S         , envir = .GlobalEnv)
assign("distIn"     , distIn    , envir = .GlobalEnv)
assign("typeCall"   , typeCall  , envir = .GlobalEnv)

# Controls for donlp2 Algorithm 

myControl<-donlp2.control()     # create structure

myControl$iterma=2000           # maximum number of iterations
myControl$fnscale=-1            # maximization of function (instead of minimization)
myControl$tau0=001             # precision of convergence
myControl$hessian=TRUE          # bring out hessian matrix for standard error calculation
myControl$silent=TRUE           # no ugly prints in screen
myControl$epsx=1e-3             # Kuhn-Tucker criteria (??) - Changing it doesnt seem to make any difference in optimization process..
myControl$difftype=2            # numerical differentiation (1 - forward difference,2-central diff, 3-six order aprox)
myControl$epsfcn=1e-8          # precision of objective function
myControl$nreset.multiplier=2   # ?? I have no idea...
myControl$nstep=10              # number of backtracking allowed (??)
myControl$delmin=0.1          # constraints are considered as sufficiently satisfied if absolute values of their violation are less than the value. 
myControl$taubnd=0.1          # The positive amount by which bounds may be violated if numerical differention is used. 

options(warn=-1)    # no warnings at the end of optimization

# Fitting with donlp2 

fittedParam<-donlp2(param0,
                    MS_Regress_Lik,
                    par.upper=upper,
                    par.lower=lower,
                    A=A,
                    lin.upper=lin.upper,
                    lin.lower=lin.lower,
                    control=myControl)

# OLD LINES USING OPTIM() (ONLY WORKS FOR K=2, KEEP IT FOR FUTURE REFERENCE)

#fittedParam<-optim(param0,
#                   MS_Regress_Lik,
#                   gr = NULL,
#                   method ="L-BFGS-B",
#                   lower =lower,
#                   upper=upper,
#                   control = list(maxit=5000,fnscale=-1), 
#                   hessian = TRUE)

# Filtering the model to the data in order to recover latent states (filtered probabilities, conditional mean and conditional std)

typeCall<-"filtering"       # changing typeCall in order for MS_Regrees_lik to return the filtered series
assign("typeCall"   , typeCall  , envir = .GlobalEnv)   # Assgining it to the global enviroment

specOut<-MS_Regress_Lik(fittedParam$par)          

# Removing Global Variables

rm("dep"        ,envir = .GlobalEnv,mode="numeric")
rm("indep_S"    ,envir = .GlobalEnv,mode="numeric")
rm("indep_nS"   ,envir = .GlobalEnv,mode="numeric")
rm("k"          ,envir = .GlobalEnv,mode="numeric")
rm("S"          ,envir = .GlobalEnv,mode="numeric")
rm("distIn"     ,envir = .GlobalEnv,mode="character")
rm("typeCall"   ,envir = .GlobalEnv,mode="character")

# Calculation of standard errors (using second partial derivatives matrix (the hessian))

stdCoeff<-tryCatch(expr=sqrt(diag(solve(-fittedParam$hessian))),finally=rep(NaN,length(param0)))

result<-try({stdCoeff<-sqrt(diag(solve(-fittedParam$hessian)))})    # try for the case of non invertable hessian
        
if (class(result)=="try-error")
{   
    print("Attention: cannot invert Hessian matrix. Standard errors not computed.")
    stdCoeff=rep(NaN,length(param0))
}
    

sigma_Std<-matrix(0,1,k)
sigma_Std[1,]<-stdCoeff[1:k]

indep_nS_Std<-matrix(0,n_nS,1)
indep_nS_Std[,1]<-stdCoeff[(k+1):(k+n_nS)]

if (distIn=='t')
{   
    v_Std<-matrix(0,1,k)
    v_Std<-stdCoeff[(length(stdCoeff)-k-k+1):(length(stdCoeff)-k)]
}    

indep_S_Std<-matrix(0,n_S,k)
for (i in 0:k-1)
  indep_S_Std[,i+1]<-stdCoeff[(1+k+n_nS+i*n_S):(k+n_nS+n_S+n_S*i)] 

if (distIn=='Normal')
{   
    Coeff_Std<-list(sigma=sigma_Std,
                    indep_nS=indep_nS_Std,
                    indep_S=indep_S_Std)

}
if (distIn=='t')    
{
    Coeff_Std<-list(sigma=sigma_Std,
                    indep_nS=indep_nS_Std,
                    indep_S=indep_S_Std,
                    v=v_Std)
}

# Expected duration of each regime (1/(1-pii))                

stateDur<-1/(1-diag(specOut$Coeff$P))

# Calculation of smooth Probabilities

smooth_value<-matrix(0,1,k)
smoothProb<-matrix(0,nr,k)
smoothProb[nr,]<-specOut$filtProb[nr,]

Prob_t_1<-matrix(0,nr,k)    # This is the matrix with probability of s(t)=j conditional on the information in t-1
Prob_t_1[1,1:k]<-1/k        # Starting probabilities as 1/k

for (i in 2:nr)
    Prob_t_1[i,1:k]<-specOut$filtProb[i-1,1:k]%*%t(specOut$Coeff$P)     # Filtering with the data

# Smoothing algorithm (there is a nice matrix version of it in Hamilton (1994)) 

for (i in seq(nr-1,1,-1))   # work backwards in time
{
    for (j1 in 1:k)
    {
        for (j2 in 1:k)
            smooth_value[1,j2]=smoothProb[i+1,j2]*specOut$filtProb[i,j1]*specOut$Coeff$P[j2,j1]/Prob_t_1[i+1,j2]
            
        smoothProb[i,j1]=sum(smooth_value)
    }
    
}

sizeModel<-list(n_nS=n_nS,
                n_S=n_S,
                nr=nr,
                nIndep=nIndep,
                S_S=S_S,
                S_nS=S_nS,
                S=S)

# Building MS_Model class for output

MS_Model_Out<-new("MS_Model",
               filtProb=specOut$filtProb  ,
               smoothProb=smoothProb      ,
               Coeff=specOut$Coeff        ,
               condMean=specOut$condMean  ,
               condStd=specOut$condStd    ,
               Coeff_Std=Coeff_Std        ,
               LL=fittedParam$fx          ,
               k=k                        ,
               paramVec=fittedParam$par   ,
               stateDur=stateDur          ,
               nParameter=length(fittedParam$par) ,
               sizeModel=sizeModel        ,
               distrib=distIn )
  
return(MS_Model_Out)
}
