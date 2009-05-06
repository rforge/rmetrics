\name{ACD_Fit}
\alias{ACD_Fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for Estimation of an ACD Model}
\description{This function estimates, based on maximum likelihood, the parameters of an 
ACD (autoregressive conditional duration) model.}
\usage{ACD_Fit(x, qLag = 1, pLag = 1, distrib = "exp", typeACD = "ACD")}

\arguments{
  \item{x}{Vector with observed durations}
  \item{qLag}{Maximum lag for alpha parameter}
  \item{pLag}{Maximum lag for beta parameter}
  \item{distrib}{Assumed distribution ('exp' or 'weibull')}
  \item{typeACD}{Functional form of conditional duration equation (possible value = 'ACD','log','BC' or 'EX')}
}

\value{Returns a S4 object with the following slots:
  \item{x}{Observed Durations}
  \item{qLag}{Maximum lag for alpha parameter}
  \item{pLag}{Maximum lag for beta parameter}  
  \item{condDur}{Conditional Duration Series}  
  \item{Coeff}{A list with all estimated coefficients}
  \item{Coeff_Std}{A list with all standard errors for coefficients}
  \item{Coeff_pValues}{A list with all p values of coefficients}
  \item{LL}{Value of maximum log likelihood}
  \item{paramVec}{A vector with all coefficients (same values as in Coeff)}
  \item{nParameter}{Number of parameters in the model}
  \item{sizeModel}{A list with the size of the model (number of indep var, etc)}
  \item{distrib}{Assumed distribution for ML estimation}  
  \item{typeACD}{Assumed functional form of ACD filter}    
  \item{timeRun}{Time of estimation of model}    
}

\author{Marcelo Perlin - ICMA/UK <\email{marceloperlin@gmail.com}>}

\seealso{\code{\link{ACD_Simul}},\code{\link{acdModel-class}}}
\examples{

data(myDur)


x=myDur
pLag=1
qLag=1
distrib='exp'

typeACD='ACD'

modelOut<-ACD_Fit(x,qLag,pLag,distrib,typeACD)
}

\keyword{models}
