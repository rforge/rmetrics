`print.acdModel` <-
function(modelOut)
{

qLag<-modelOut@qLag
pLag<-modelOut@pLag

cat('\nPrinting ACD
 Model\n')
cat('\nType of ACD model:',modelOut@typeACD)
cat('\nDistribution Assumption:',modelOut@distrib)
cat('\nDate and Time of Estimation:',modelOut@timeRun)

cat('\nLag Structure:\n')
cat('   q lags=',modelOut@sizeModel$qLag,'\n')
cat('   p lags=',modelOut@sizeModel$pLag)
cat('\nNumber of Parameters:',modelOut@sizeModel$nParameters)
cat('\nFinal log Likelihood:',modelOut@LL)

cat('\n\nFitted Parameters - Coefficient (standard error, p value):')
cat('\nOmega:',sprintf('%4.4f (%4.2f, %4.4f)',modelOut@Coeff$w,modelOut@Coeff_Std$w,modelOut@Coeff_pValues$w) )

for (i in 1:modelOut@qLag)
    cat('\nAlpha',i,':',sprintf('%4.4f (%4.2f, %4.4f)',modelOut@Coeff$q[(qLag-i+1)],modelOut@Coeff_Std$q[(qLag-i+1)],modelOut@Coeff_pValues$q[(qLag-i+1)]) )
    
for (i in 1:modelOut@pLag)
    cat('\nBeta ',i,':',sprintf('%4.4f (%4.2f, %4.4f)',modelOut@Coeff$p[pLag-i+1],modelOut@Coeff_Std$p[pLag-i+1],modelOut@Coeff_pValues$p[pLag-i+1]) )

if ( (modelOut@typeACD=='BC')|(modelOut@typeACD=='EX'))
{   
    for (i in 1:modelOut@qLag)
        cat('\ndelta',i,':',sprintf('%4.4f (%4.2f, %4.4f)',modelOut@Coeff$delta[qLag-i+1],modelOut@Coeff_Std$delta[qLag-i+1],modelOut@Coeff_pValues$delta[qLag-i+1]) )    
}

if (modelOut@distrib=='weibull')
    cat('\ny:',sprintf('%4.4f (%4.2f, %4.4f)',modelOut@Coeff$y,modelOut@Coeff_Std$y,modelOut@Coeff_pValues$y) )    


}

