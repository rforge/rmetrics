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

for (i in firstIdx:nr)
{
    if (typeACD=='ACD')
        condDur[i,1]<-Coeff$w + t(x[(i-qLag):(i-1),1])%*%(Coeff$q) + t(condDur[(i-pLag):(i-1),1])%*%(Coeff$p)
    if (typeACD=='log')
        condDur[i,1]<-Coeff$w + t(log(x[(i-qLag):(i-1),1]))%*%(Coeff$q) + t(condDur[(i-pLag):(i-1),1])%*%(Coeff$p)

    if (typeACD=='BC')
    {
        condDur[i,1]<-Coeff$w + t(((e[(i-qLag):(i-1),1]^Coeff$delta)-1)/Coeff$delta)%*%(Coeff$q) + t(condDur[(i-pLag):(i-1),1])%*%(Coeff$p)
        #browser()
        e[i,1]<-x[i]/exp(condDur[i,1])
    }
    if (typeACD=='EX')
    {
      condDur[i,1]<-Coeff$w + t(e[(i-qLag):(i-1),1])%*%(Coeff$q) +t(abs(e[(i-qLag):(i-1),1]-1))%*%(Coeff$delta)+ t(condDur[(i-pLag):(i-1),1])%*%(Coeff$p)
      #browser()
      e[i,1]<-x[i]/exp(condDur[i,1])
    }


}

if ( (typeACD=='log')|(typeACD=='BC')|(typeACD=='EX') )
    condDur<-exp(condDur)

#browser()

if  (distrib=='exp')
    loglik<-log(1/condDur*exp(-x/condDur))

#browser()
if (distrib=='weibull')
    loglik<-log(Coeff$y/x*((x*gamma(1+1/Coeff$y))/condDur)^Coeff$y*exp(-(((x*(gamma(1+1/Coeff$y))/condDur)^Coeff$y))))
    
#loglik(:,1)=log(Coeff.y./x(:,1).*((x(:,1).*gamma(1+1/Coeff.y))./h(:,1)).^Coeff.y.*exp(-(((x(:,1).*(gamma(1+1/Coeff.y))./h(:,1)).^Coeff.y))));


#loglik(:,1)=log(1./h(:,1).*exp(-x(:,1)./h(:,1))); % this is the log likelihood

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

