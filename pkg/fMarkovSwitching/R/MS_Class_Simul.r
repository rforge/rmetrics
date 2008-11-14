setClass("MS_Simul",representation(
                    nr="numeric"       ,
                    dep="matrix"        ,
                    indep="matrix"      ,
                    Coeff="list"        ,
                    S="numeric"         ,
                    trueStates="matrix" ,
                    k="numeric"         ,
                    distrib="character" ))
                    
print.MS_Simul<-function(MS_Simul_In)
{
nr       =MS_Simul_In@nr
S        =MS_Simul_In@S
n_S      =sum(MS_Simul_In@S)
n_nS     =length(S)-n_S
nIndep   =ncol(MS_Simul_In@indep)
S_S      =which(S==1)
S_nS     =which(S==0)
Coeff    =MS_Simul_In@Coeff
k        =MS_Simul_In@k
distrib  =MS_Simul_In@distrib

# Sending output to R

cat('\n\n***** Simulated Markov Switching Model *****\n\n')
cat('Number of simulated observations:',nr,'\n')
cat('Number of explanatory variables:',nIndep,'\n')
cat('Distribution Assumption:',distrib,'\n')

cat('\n***** True Parameters *****\n');

cat('\n---> Non Switching Parameters <---\n');

if (n_nS==0)
    cat('\nThere was no Non Switching Parameters. Skipping this result')
else
{
    for (i in 1:n_nS)
        {
        cat('\n Non Switching Parameter at Indep collumn', S_nS[i]);
        cat('\n      Value:    ', Coeff$nS_param[i])
        }
}

cat('\n\n--->   Switching Parameters   <---\n');

for (i in 1:k)
{
    cat('\n  State', i);
    cat('\n      Model Standard Deviation:', Coeff$sigma[i])

    if (distrib=="t")
    {
        cat('\n      Degrees of Freedom:         ', Coeff$v[i])
    }
}

for (i in 1:n_S)
{
    cat('\n\n  Switching Parameters for Indep collumn', S_S[i],'\n');

    for (j in 1:k)
    {
        cat('\n  State ', j);
        cat('\n     Value:     ',Coeff$S_param[i,j])
    }
}

cat('\n\n---> Transition Probabilities Matrix <---\n');

for (i in 1:k)
{
    cat('\n      ')
    for (j in 1:k)
    {
        str<-sprintf('%4.2f    ',Coeff$P[i,j])
        cat(str)
    }
}
cat('\n')
}

# Plotting function for MS_Simul S4 Object

plot.MS_Simul<-function(MS_Simul_In)
{
k<-MS_Simul_In@k

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
    
par(mfrow=c(2,1))

plot(MS_Simul_In@dep,
     xlab="Time",
     ylab="Simulated Time Series",
     type='l')
title("Simulated Time Series")


matplot(MS_Simul_In@trueStates,
        xlab="Time"           ,
        ylab="True States"    ,
        type='l',
        col=col)
title("True States")

myStr<-c("State 1")
for (i in 2:MS_Simul_In@k)
{   
    str<-sprintf("State %i",i)
    myStr<-c(myStr,str)
}

legend('topright',myStr,col=col,lty=1)
}
