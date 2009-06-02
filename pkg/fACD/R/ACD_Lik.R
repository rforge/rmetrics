`ACD_Lik` <-
function(param,x,qLag,pLag,distrib,typeACD,typeCall='maxLik')
{

#browser()
nr<-nrow(x)

if (distrib=='exp')
{
    if ( (typeACD=='ACD')|(typeACD=='log') )
        Coeff<-list(w=param[1],q=param[2:(1+qLag)],p=param[(2+qLag):(1+qLag+pLag)])

    if ( (typeACD=='BC')|(typeACD=='EX') )
        Coeff<-list(w=param[1],q=param[2:(1+qLag)],p=param[(2+qLag):(1+qLag+pLag)],delta=param[(2+qLag+pLag):(1+2*qLag+pLag)])
}

if (distrib=='weibull')
{
    if ( (typeACD=='ACD')|(typeACD=='log') )
        Coeff<-list(w=param[1],q=param[2:(1+qLag)],p=param[(2+qLag):(1+qLag+pLag)],y=param[2+qLag+pLag])
    if ( (typeACD=='BC')|(typeACD=='EX') )
        Coeff<-list(w=param[1],q=param[2:(1+qLag)],p=param[(2+qLag):(1+qLag+pLag)],delta=param[(2+qLag+pLag):(1+2*qLag+pLag)],y=param[2+2*qLag+pLag])
}

#browser()

condDur<-matrix(0,nr,1)
condDur[1:max(qLag,pLag),1]<-mean(x)

e<-matrix(0,nr,1)
#browser()

firstIdx=max(qLag,pLag)+1
durOut<-matrix(0,nr,1)
#browser()

#library.dynam("ACD_Filter", package = "fACD")
#dyn.load('fACD.dll')

if (typeACD=='ACD')
{     
condDur<-.C("ACD_Filter", as.double(x)  , 
                      as.integer(nr),
                      as.double(Coeff$w)  ,
                      as.double(Coeff$q)  ,
                      as.double(Coeff$p)  ,
                      as.integer(pLag),
                      as.integer(qLag),
                      as.double(durOut),PACKAGE="fACD")[[8]]
}

if (typeACD=='log')
{     
condDur<-.C("log_ACD_Filter", as.double(x)  , 
                      as.integer(nr),
                      as.double(Coeff$w)  ,
                      as.double(Coeff$q)  ,
                      as.double(Coeff$p)  ,
                      as.integer(pLag),
                      as.integer(qLag),
                      as.double(durOut),PACKAGE="fACD")[[8]]
}

if (typeACD=='BC')
{     
condDur<-.C("BC_ACD_Filter", as.double(x)  , 
                      as.integer(nr),
                      as.double(Coeff$w)  ,
                      as.double(Coeff$q)  ,
                      as.double(Coeff$p)  ,
                      as.double(Coeff$delta) ,
                      as.integer(pLag),
                      as.integer(qLag),
                      as.double(durOut),PACKAGE="fACD")[[9]]
}

if (typeACD=='EX')
{     
condDur<-.C("EX_ACD_Filter", as.double(x)  , 
                      as.integer(nr),
                      as.double(Coeff$w)  ,
                      as.double(Coeff$q)  ,
                      as.double(Coeff$p)  ,
                      as.double(Coeff$delta) ,
                      as.integer(pLag),
                      as.integer(qLag),
                      as.double(durOut),PACKAGE="fACD")[[9]]
}

                      
condDur=as.matrix(condDur)                    

if ( (typeACD=='log')|(typeACD=='BC')|(typeACD=='EX') )
    condDur<-exp(condDur)

if  (distrib=='exp')
    loglik<-log(1/condDur*exp(-x/condDur))

if (distrib=='weibull')
    loglik<-log(Coeff$y/x*((x*gamma(1+1/Coeff$y))/condDur)^Coeff$y*exp(-(((x*(gamma(1+1/Coeff$y))/condDur)^Coeff$y))))
    
LL<-sum(loglik)

myStr<-sprintf('%4.4f',LL)
cat('\nSum of Log Likelihood for ACD model:',myStr)

if (typeCall=='maxLik')
  return(LL)
  
if (typeCall=='filtering')
{
    specOut<-list(Coeff=Coeff,condDur=condDur,LL=LL)
}

}

