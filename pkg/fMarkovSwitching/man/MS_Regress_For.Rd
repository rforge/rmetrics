\name{MS_Regress_For}
\alias{MS_Regress_For}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to Forecast a Markov Switching Model in t+1}
\description{This function will take as input a MS_Model object, the new set of explanatory variables and then forecast in t+1 
the conditional mean and standard deviation of the model based on the filtered probabilities (conditional on t-1 (t¦t-1))
}
\usage{MS_Regress_For(myModel, newIndep)}
\arguments{
  \item{myModel}{A S4 object obtained with MS_Regress_Fit}
  \item{newIndep}{A new set of independent/explanatory variables} 
}

\value{
  The function returns a list with the following items:
  \item{$condMean}{Conditional Mean for t+1}
  \item{$condStd}{Conditional Standard Deviation in t+1}
}
\references{
\cite{ALEXANDER, C. (2008) 'Market Risk Analysis: Practical Financial Econometrics' Wiley}\cr
\cite{HAMILTON, J., D. (1994) 'Time Series Analysis' Princeton University Press}\cr
\cite{HAMILTON, J., D. (2005) 'Regime Switching Models' Palgrave Dictionary of Economic}\cr
\cite{KIM, C., J., NELSON, C., R. (1999) 'State Space Model with Regime Switching: Classical and Gibbs-Sampling Approaches with Applications' The MIT press}
}
\author{Marcelo Perlin - ICMA/UK <\email{marceloperlin@gmail.com}>}

\seealso{\code{\link{MS_Regress_Simul}},\code{\link{MS_Regress_Fit}},\code{\link{MS_Regress_Lik}}  }
\examples{

data(dep)
data(indep)

dep=as.matrix(dep)
indep=as.matrix(indep)

# input arguments

S=c(1,0,0)
distrib<-"Normal"
k<-2

# new dep and indep (without last observation, which will be forecasted)

dep=dep[-nrow(dep)]        
myNewIndep=indep[-nrow(indep),]

# Fit the model with ex ante data

myModel<-MS_Regress_Fit(dep,myNewIndep,S,k,distrib)

# new indep matrix is build as the last observations

newIndep_For=as.matrix(t(indep[nrow(indep),])) 

# forecast with MS_Regress_For

myFor<-MS_Regress_For(myModel,newIndep_For)

cat("\nForecast for conditional Mean in t+1= ",myFor$condMean,"\n")
cat("Forecast for conditional Standard deviation (sigma) in t+1= ",myFor$condStd,"\n")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}
