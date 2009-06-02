`ACD_Fit` <-
function(x,qLag=1,pLag=1,distrib='exp',typeACD='ACD')
{

if ((pLag<0)|(qLag<0))
    stop('The input pLag and qLag should be positive integers')

if (!any(distrib=='exp',distrib=='weibull'))
    stop('The distrib input should be exp or weibull.')    
    
if (!any(typeACD=='ACD',typeACD=='log',typeACD=='BC',typeACD=='EX'))
    stop('The typeACD input should be: ACD, log, BC or EX.')        

x<-as.matrix(x)

if (distrib=='exp')
{
    if ( (typeACD=='ACD')|(typeACD=='log') )
        {
        param0<-matrix(data = NA,nrow=1,ncol=(1+pLag+qLag))
        }

    if (typeACD=='BC')
       {
       param0<-matrix(data = NA,nrow=1,ncol=(1+2*pLag+qLag))
       param0[(2+qLag+pLag):(1+2*qLag+pLag)]<-.4
       }

    if (typeACD=='EX')
       {
       param0<-matrix(data = NA,nrow=1,ncol=(1+2*pLag+qLag))
       param0[(2+qLag+pLag):(1+2*qLag+pLag)]<--0.02
       }
}

if (distrib=='weibull')
{
    if ( (typeACD=='ACD')|(typeACD=='log') )
        {
        param0<-matrix(data = NA,nrow=1,ncol=(2+pLag+qLag))
        param0[(2+qLag+pLag)]<-.8
        }

    if (typeACD=='BC')
       {
       param0<-matrix(data = NA,nrow=1,ncol=(2+2*pLag+qLag))
       param0[(2+qLag+pLag):(1+2*qLag+pLag)]<-.4
       param0[(2+2*qLag+pLag)]<-.8
       }

    if (typeACD=='EX')
       {
       param0<-matrix(data = NA,nrow=1,ncol=(2+2*pLag+qLag))
       param0[(2+qLag+pLag):(1+2*qLag+pLag)]<--0.02
       param0[(2+2*qLag+pLag)]<--.02
       }
}

param0[1]<-.1
param0[2:(1+qLag)]<-.05/qLag
param0[(2+qLag):(1+qLag+pLag)]<-.95/pLag

options(warn=-1)
fittedParam<-optim(param0,
                   ACD_Lik,
                   gr = NULL,
                   x,qLag,pLag,distrib,typeACD,typeCall='maxLik',
                   method ="BFGS" ,
                   control = list(maxit=5000,fnscale=-1),
                   hessian = TRUE)
options(warn=1)

typeCall='filtering'
specOut<-ACD_Lik(fittedParam$par,x,qLag,pLag,distrib,typeACD,typeCall)

stdParam<-sqrt(diag(solve(-fittedParam$hessian)))
pValues<-2*(1-pt(abs(fittedParam$par/stdParam),length(x)-length(stdParam)))

if (distrib=='exp')
{
    if ( (typeACD=='ACD')|(typeACD=='log') )
    {
        Coeff_Std<-list(w=stdParam[1],q=stdParam[2:(1+qLag)],p=stdParam[(2+qLag):(1+qLag+pLag)])
        Coeff_pValues<-list(w=pValues[1],q=pValues[2:(1+qLag)],p=pValues[(2+qLag):(1+qLag+pLag)])
    }    
    if ( (typeACD=='BC')|(typeACD=='EX') )
    {
        Coeff_Std<-list(w=stdParam[1],q=stdParam[2:(1+qLag)],p=stdParam[(2+qLag):(1+qLag+pLag)],delta=stdParam[(2+qLag+pLag):(1+2*qLag+pLag)])
        Coeff_pValues<-list(w=pValues[1],q=pValues[2:(1+qLag)],p=pValues[(2+qLag):(1+qLag+pLag)],delta=pValues[(2+qLag+pLag):(1+2*qLag+pLag)])
}   }

if (distrib=='weibull')
{
    if ( (typeACD=='ACD')|(typeACD=='log') )
    {
        Coeff_Std<-list(w=stdParam[1],q=stdParam[2:(1+qLag)],p=stdParam[(2+qLag):(1+qLag+pLag)],y=stdParam[2+qLag+pLag])
        Coeff_pValues<-list(w=pValues[1],q=pValues[2:(1+qLag)],p=pValues[(2+qLag):(1+qLag+pLag)],y=pValues[2+qLag+pLag])
    }
    if ( (typeACD=='BC')|(typeACD=='EX') )
    {
        Coeff_Std<-list(w=stdParam[1],q=stdParam[2:(1+qLag)],p=stdParam[(2+qLag):(1+qLag+pLag)],delta=stdParam[(2+qLag+pLag):(1+2*qLag+pLag)],y=stdParam[(2+2*qLag+pLag)])
        Coeff_pValues<-list(w=pValues[1],q=pValues[2:(1+qLag)],p=pValues[(2+qLag):(1+qLag+pLag)],delta=pValues[(2+qLag+pLag):(1+2*qLag+pLag)],y=pValues[(2+2*qLag+pLag)])
    }
}

sizeModel<-list(nr=nrow(x),nParameters=length(stdParam),pLag=pLag,qLag=qLag)
modelOut<-new("acdModel",
               x=x                        ,
               qLag=qLag                  ,
               pLag=pLag                  ,
               paramVec=fittedParam$par   ,
               Coeff=specOut$Coeff        ,
               Coeff_Std=Coeff_Std        ,
               Coeff_pValues=Coeff_pValues,
               condDur=specOut$condDur    ,
               sizeModel=sizeModel        ,
               distrib=distrib            ,
               typeACD=typeACD            ,
               LL=specOut$LL              ,
               timeRun=date()             )
cat('\n\n')
return(modelOut)
}

