  # Function of likelihood calculation of MS model
    
  MS_Regress_Lik<-function(param)
  {         
  
  # Some precalculations 
  
  # Getting global variables 
  
  dep       <-mget("dep"        ,envir = .GlobalEnv,mode="numeric")$dep
  indep_S   <-mget("indep_S"    ,envir = .GlobalEnv,mode="numeric")$indep_S
  indep_nS  <-mget("indep_nS"   ,envir = .GlobalEnv,mode="numeric")$indep_nS
  k         <-mget("k"          ,envir = .GlobalEnv,mode="numeric")$k
  S         <-mget("S"          ,envir = .GlobalEnv,mode="numeric")$S
  distIn    <-mget("distIn"     ,envir = .GlobalEnv,mode="character")$distIn
  typeCall  <-mget("typeCall"   ,envir = .GlobalEnv,mode="character")$typeCall
  
  nr<-length(dep)
  
  n_indep<-ncol(indep_S)+ncol(indep_nS)
  n_S<-sum(S)
  n_nS<-n_indep-n_S

  # Figuring out size of problem 
  
  count_nS<-0
  count_S<-0
  S_S<-0
  S_nS<-0

  # retrieving coefficients from param vector 
  
  # Retrieving the P matrix
        
  P<-matrix(0,nrow=k,ncol=k)
  

  P[1:(k-1),]<-(param[(length(param)-k*(k-1)+1):length(param)])
  
  temp=0
  for (i in 1:k)
        temp[i]=sum(P[,i])
  
  P[nrow(P),]<-1-temp
    
  if (distIn=="t")
  { 
    v<-matrix(0,1,k)
    v[1,1:k]<-param[(length(param)-k-k+1):(length(param)-k)]
  }

  # Retrieving sigma, switching and non switching independent variables
  
  param_sigma<-matrix(0,1,k)
  param_sigma[1,]<-param[1:k]
  param_indep_nS<-matrix(0,n_nS,1)
  param_indep_nS[,1]<-param[(k+1):(k+n_nS)]

  param_indep_S<-matrix(0,n_S,k)
  for (i in 0:k-1)
      param_indep_S[,i+1]<-param[(1+k+n_nS+i*n_S):(k+n_nS+n_S+n_S*i)] 
  
  # Building coeff structure as a list class
  
  if (distIn=="Normal")
    Coeff<-list(sigma=param_sigma,indep_nS=param_indep_nS,indep_S=param_indep_S,P=P)
  else
    Coeff<-list(sigma=param_sigma,indep_nS=param_indep_nS,indep_S=param_indep_S,P=P,v=v)
        
  # prealocation of large matrices
  
  e<-matrix(nrow=nr,ncol=k)
  condMean<-matrix(nrow=nr,ncol=k)
  
  for (i in 1:k)    # building conditional mean in each state
  { 
    condMean[,i]<-indep_S%*%(Coeff$indep_S[,i])+ indep_nS%*%(Coeff$indep_nS)
    e[,i]<-dep-condMean[,i]
  }

  lik<-matrix(nrow=nr,ncol=k)
  
  for (ik in 1:k)
  { 
    if (distIn=="Normal")
    {
        lik[,ik]<-1/(Coeff$sigma[ik]*sqrt(2*pi))*exp(-(e[,ik]^2)/(2*Coeff$sigma[ik]^2))

    }    
    if (distIn=="t")
    {
        lik[,ik]=( gamma(.5*(Coeff$v[1,ik]+1)) )/( (gamma(.5*Coeff$v[1,ik]))*sqrt(pi*Coeff$v[1,ik]*Coeff$sigma[1,ik]^2))* 
                ((1+(e[,ik]^2)/(Coeff$v[1,ik]*Coeff$sigma[1,ik]^2))^(-.5*(Coeff$v[1,ik]+1)))
    }
  }
  
  
  filtProb_t<-matrix(nrow=nr,ncol=k)
  filtProb_t[1,]<-1/k
  
  filtProb_t_1<-matrix(nrow=nr,ncol=k)
  filtProb_t_1[1,]<-1/k

  fullLik<-matrix(nrow=nr,ncol=1);
  onesVec<-matrix(1,1,k)
  f<-matrix(0,nr,1)

  for (i in 2:nr)   # Markov Switching Filter (see Hamilton (1994))
  {     
       f[i,1]<-onesVec%*%(t((filtProb_t[i-1,]%*%Coeff$P)*lik[i,]))
       filtProb_t[i,]<-(t((filtProb_t[i-1,]%*%Coeff$P)*lik[i,]))/f[i,1]
  }

  LL<-sum(log(f[-1]))
  
  if (is.infinite(LL))  # control for optimization
    LL<--10000
    
  if (is.nan(LL))  # control for optimization
    LL<--10000
    
  cat("Sum of log Likelihood for MS-Regression Model ->",sprintf("%4.4f",LL),"\n")
  
  if (typeCall=="optim")
    return(LL)
    
  if (typeCall=="filtering")
    {
    
    specOut<-list(LL=LL ,
                  filtProb=filtProb_t,
                  condMean=(condMean*filtProb_t)%*%matrix(1,k,1),
                  Coeff=Coeff,
                  condStd=filtProb_t%*%t(Coeff$sigma))
    return(specOut)
    }
    
  }
  
