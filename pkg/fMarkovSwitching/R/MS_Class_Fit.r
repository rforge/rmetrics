# R file for MS_Model S4 object definition and implementation

setClass("MS_Model",representation(
                    filtProb="matrix"   ,   # filtered probabilities (t¦t)
                    smoothProb="matrix" ,   # smoothed prob
                    Coeff="list"        ,   # all coefficients
                    condMean="matrix"   ,   # conditional Mean
                    condStd="matrix"    ,   # conditional standard deviation
                    Coeff_Std="list"    ,   # Standard errors for coefficients
                    LL="numeric"        ,   # final log likelihood
                    k="numeric"         ,   # number of states
                    paramVec="numeric"  ,   # vector of parameters (has the same values as Coeff)
                    stateDur="numeric"  ,   # The expected duration of each state 
                    nParameter="numeric",   # number of parameters in the model
                    sizeModel="list"    ,   # a list with the size of the model (number of indep var, etc)
                    distrib="character" ))  # Assumed distribution for ML estimation
                    
print.MS_Model<-function(MS_Model_In)
{

n_S      =MS_Model_In@sizeModel$n_S
n_nS     =MS_Model_In@sizeModel$n_nS
nIndep   =MS_Model_In@sizeModel$nIndep
S_S      =MS_Model_In@sizeModel$S_S
S_nS     =MS_Model_In@sizeModel$S_nS
Coeff    =MS_Model_In@Coeff
Coeff_Std=MS_Model_In@Coeff_Std
k        =MS_Model_In@k
distrib  =MS_Model_In@distrib
stateDur =MS_Model_In@stateDur
nr	   =MS_Model_In@sizeModel$nr



# Sending output to R

cat('\n\n***** Numerical Optimization for MS Model Converged *****\n\n')
cat('Final log Likelihood:',MS_Model_In@LL,'\n')
cat('Number of parameters:',MS_Model_In@nParameter,'\n')
cat('Distribution Assumption ->',MS_Model_In@distrib,'\n')

#cat('Method for standard error calculation -> ',num2str(std_method),'\n']); IMPLEMENT LATER OTHER STD CALC

cat('\n***** Final Parameters *****\n');
cat('\n---> Non Switching Parameters <---\n');

if (n_nS==0)
    cat('\nThere was no Non Switching Parameters. Skipping this result')
else
{
    for (i in 1:n_nS)
        {
        cat('\n Non Switching Parameter at Indep  Column ', S_nS[i]);
        cat('\n      Value:    ', sprintf("%4.4f",Coeff$indep_nS[i]))
        cat('\n      Std error:', sprintf("%4.4f (%4.2f)",Coeff_Std$indep_nS[i],2*(1-pt(abs(Coeff$indep_nS[i]/Coeff_Std$indep_nS[i]),nr-MS_Model_In@nParameter)) ) )
        }
}

cat('\n\n--->   Switching Parameters   <---\n');

for (i in 1:k)
{
    cat('\n  State', i);
    cat('\n      Model Standard Deviation:', sprintf("%4.4f",Coeff$sigma[i]))
    cat('\n      Std Error:               ', sprintf("%4.4f (%4.2f)",Coeff_Std$sigma[i],2*(1-pt(Coeff$sigma[i]/Coeff_Std$sigma[i],nr-MS_Model_In@nParameter)) ) )

    if (distrib=="t")
    {
        cat('\n      Degrees of Freedom:         ', sprintf("%4.4f",Coeff$v[i]))
        cat('\n      Std Error:                  ', sprintf("%4.4f (%4.2f)",Coeff_Std$v[i],2*(1-pt(Coeff$v[i]/Coeff_Std$v[i],nr-MS_Model_In@nParameter)) ) )
    }
}

for (i in 1:n_S)
{
    cat('\n\n  Switching Parameters for Indep  Column ', S_S[i],'\n');

    for (j in 1:k)
    {
        cat('\n  State ', j);
        cat('\n     Value:     ',sprintf("%4.4f",Coeff$indep_S[i,j]))
        cat('\n     Std error: ',sprintf("%4.4f (%4.2f)",Coeff_Std$indep_S[i,j],2*(1-pt(abs(Coeff$indep_S[i,j]/Coeff_Std$indep_S[i,j]),nr-MS_Model_In@nParameter)) ))
    }
}

cat('\n\n---> Transition Probabilities Matrix <---\n');

for (i in 1:k)
{
    cat('\n      ')
    for (j in 1:k)
    {
        str<-sprintf('%4.2f   ',Coeff$P[i,j])
        cat(str)
    }
}

cat('\n\n---> Expected Duration of Regimes <---\n\n');


for (i in 1:k)
{
    str<-sprintf('     Expected duration of Regime #%i: %4.2f time periods\n',i,stateDur[i])
    cat(str)
}

}
# FUNCTION FOR SIZE OF MS MODEL

dim.MS_Model<-function(MS_Model_In)
{
nr        =MS_Model_In@sizeModel$nr
nParameter=MS_Model_In@nParameter
n_S       =MS_Model_In@sizeModel$n_S
n_nS      =MS_Model_In@sizeModel$n_nS
nIndep    =MS_Model_In@sizeModel$nIndep
S_S       =MS_Model_In@sizeModel$S_S
S_nS      =MS_Model_In@sizeModel$S_nS
k         =MS_Model_In@k
distrib   =MS_Model_In@distrib

cat('\n***** Dimension of Markov Switching Model *****\n\n')

cat('Number of Observations:',nr,'\n')
cat('Number of Parameters:',nParameter,'\n')
cat('Number of States:',k,'\n')
cat('Fitted Distribution:',distrib,'\n')
cat('Number of Independent Variables:\n')
cat('     Switching States:',n_S,'\n')
cat('     Non-Switching States:',n_nS,'\n')
cat('     Total:',(n_S+n_nS),'\n')

return()
}

# PLOTTING FUNCTION FOR MARKOV SWITCHING MODEL

plot.MS_Model<-function(MS_Model_In)
{
k<-MS_Model_In@k    

# fixing up the colors

niceColors<-c("red","blue","green","black")
allColors<-colors()

for (i in 1:length(niceColors))
{
    idx<-which(niceColors[i]==allColors)
    allColors=allColors[-idx]
}

if (k>4)
    myColors<-c(niceColors,sample(allColors,k-length(niceColors)))
else
    myColors<-niceColors[1:k]

col<-myColors 

# Fixing up the strings in the legend

myStr<-c("State 1")
for (i in 2:k)
{   
    str<-sprintf("State %i",i)
    myStr<-c(myStr,str)
}

# 4 plots in one window
    
par(mfrow=c(2,2))   

matplot(MS_Model_In@filtProb,
        xlab="Time",
        ylab="Filtered Probabilities",
        type='l',
        col=col)
title("Filtered Probabilities for each State")
legend('topright',myStr,col=col,lty=1)

matplot(MS_Model_In@smoothProb,
        xlab="Time",
        ylab="Smoothed Probabilities for each State",
        type='l',
        col=c('red','blue'))
title("Smoothed Probabilities")
legend('topright',myStr,col=col,lty=1)

plot(MS_Model_In@condMean,
     xlab="Time",
     ylab="Fitted Conditional Mean",
     type='l')
title("Fitted Conditional Mean")

plot(MS_Model_In@condStd,
     xlab="Time",
     ylab="Conditional Standard Deviation",
     type='l')
title("Fitted Conditional Standard Deviation")

}
