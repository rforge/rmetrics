`ACD_Simul` <-
function(nr,Coeff,distrib='exp',typeACD='ACD')
{

if (!any(distrib=='exp',distrib=='weibull'))
    stop('The distrib input should be exp or weibull.')    

if (!any(typeACD=='ACD',typeACD=='log',typeACD=='BC',typeACD=='EX'))
    stop('The typeACD input should be: ACD, log, BC or EX.')        
    
if (nr<0)
    stop('The nr input should be a positige integer.')    
    


pLag<-length(Coeff$p)
qLag<-length(Coeff$q)

if (distrib=='exp')
    randVec<-rexp(nr, rate = 1)

if (distrib=='weibull')
    randVec<-rweibull(nr, Coeff$y, scale = 1)

#browser()

condDur<-matrix(0,nr,1)
simDur<-matrix(0,nr,1)
e<-as.matrix(randVec)

firstIdx<-max(pLag,qLag)

condDur[1:firstIdx,1]=1
simDur[1:firstIdx,1]=1


for (i in (firstIdx+1):nr)
{
    if (typeACD=='ACD')
        condDur[i,1]<-Coeff$w + t(simDur[(i-qLag):(i-1),1])%*%(Coeff$q) + t(condDur[(i-qLag):(i-1),1])%*%(Coeff$p)
    
    if (typeACD=='log')
        condDur[i,1]<-Coeff$w + t(log(simDur[(i-qLag):(i-1),1]))%*%(Coeff$q) + t(condDur[(i-qLag):(i-1),1])%*%(Coeff$p)
    
    if (typeACD=='BC')
        condDur[i,1]<-Coeff$w + t(((e[(i-qLag):(i-1),1]^Coeff$delta)-1)/Coeff$delta)%*%(Coeff$q) + t(condDur[(i-qLag):(i-1),1])%*%(Coeff$p)

    if (typeACD=='EX')
        condDur[i,1]<-Coeff$w + t(e[(i-qLag):(i-1),1])%*%(Coeff$q) + t(abs(e[(i-qLag):(i-1),1]-1))%*%(Coeff$delta) + t(condDur[(i-qLag):(i-1),1])%*%(Coeff$p)        

    if ( (typeACD=='log')|(typeACD=='BC')|(typeACD=='EX')      )
        simDur[i,1]=exp(condDur[i,1])*randVec[i]

    if (typeACD=='ACD')
        simDur[i,1]=(condDur[i,1])*randVec[i]        
}

return(simDur)                   

}

