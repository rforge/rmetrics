\name{MS_Regress_Simul}
\alias{MS_Regress_Simul}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to Simulate a Time Series based on Markov Switching Process}
\description{This function simulates a time series based on the Markov Switching Model specified by the user}
\usage{MS_Regress_Simul(nr, Coeff, k, distrib = "Normal")}

\arguments{
  \item{nr}{Number of time periods to simulate (nr=number of rows)}
  \item{Coeff}{A list with all coefficients (see example for details)}
  \item{k}{Number of states in the model (positive integer) (e.g. \code{k=2}}
  \item{distrib}{Assumed distribution for residue ("Normal" or "t") e.g \code{distrib<-"Normal"}}
}

\value{
Returns a S4 Object of Class ""MS_Simul" with the following slots:
  \item{@nr }{Number of time periods (same as input)}
  \item{@dep}{The simulated dependent Variable (a vector of size nr)}
  \item{@Coeff}{A structure with all coefficients of the simulated series (same as input)}
  \item{@trueStates}{A matrix with the time varying states of the simulated series (which state at each time t)}
  \item{@indep}{A matrix with the independed variables (simulated normal random numbers)}
  \item{@k}{Number of states in Model (the dimension of Coeff$P matrix)}
  \item{@S}{The input argument that controls which variable switches (see example)}
}
\references{
\cite{ALEXANDER, C. (2008) 'Market Risk Analysis: Practical Financial Econometrics' Wiley}\cr
\cite{HAMILTON, J., D. (1994) 'Time Series Analysis' Princeton University Press}\cr
\cite{HAMILTON, J., D. (2005) ‘Regime Switching Models’ Palgrave Dictionary of Economic}\cr
\cite{KIM, C., J., NELSON, C., R. (1999) ‘State Space Model with Regime Switching: Classical and Gibbs-Sampling Approaches with Applications’ The MIT press}
}
\author{Marcelo Perlin - ICMA/UK}

\seealso{\code{\link{MS_Regress_Fit}},\code{\link{MS_Regress_For}},\code{\link{MS_Regress_Lik}}}
\examples{
nr=500
distrib<-"Normal"
k<-2

PVec<-c(.8 ,.2,
        .1 ,.9)
        
P<-matrix(PVec,k,k)
S<-c(1,0,0)

nS_param<-matrix(c(.5,-.2),2,1)    # Setting up the coefficients at non switching parameters
S_param<-matrix(0,sum(S),k)
S_param[,1]= .2             # Setting up the coefficients at switching parameters
S_param[,2]=-.3

sigma<-matrix(0,1,k)
sigma[1,1]=.05               # Setting up the standard deviavion of the model at State 1
sigma[1,2]=.01               # Setting up the standard deviavion of the model at State 2

Coeff<-list(P=P               , # Build Coeff as a list
            S=S               ,
            nS_param=nS_param ,
            S_param=S_param   ,
            sigma=sigma       )

mySimul<-MS_Regress_Simul(nr,Coeff,k,distrib)

print(mySimul)
plot(mySimul)
}

\keyword{models}
\keyword{regression}
