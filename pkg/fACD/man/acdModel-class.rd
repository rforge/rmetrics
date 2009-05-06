\name{acdModel-class}
\docType{class}
\alias{acdModel-class}

\title{A Class for an ACD Model}
\description{This class is the output from the use of function \code{ACD_Fit}. Normally you'll never call the object contructor 
to build this class}

\section{Slots}{
	 \describe{
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
}
\section{Methods}{
    \describe{
    \item{plot}{Plots an object of class 'acdModel'}
    \item{print}{Prints to screen the coefficients of the model (the 'acdModel' object)}
    }
}

\author{Marcelo Perlin - ICMA/UK <\email{marceloperlin@gmail.com}>}
\examples{showClass("acdModel")}
\keyword{classes}
