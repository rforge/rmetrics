\name{MS_Model-class}
\docType{class}
\alias{MS_Model-class}
\title{Definitions for Class "MS_Model"}
\description{
This is the class of a fitted Markov Switching Model S4 object.
This is a output object from \code{MS_Regress_Fit} so normally you wont create MS_Model objects directly
}
\section{Slots}{
	 \describe{
    \item{\code{filtProb}:}{Class:\code{"matrix"} - Filtered States Probabilities from the model (number of columns equal to k)}
    \item{\code{smoothProb}:}{Class:\code{"matrix"} - Smoothed States Probabilities from the model by means of Kim's Algorithm}
    \item{\code{Coeff}:}{Class:\code{"list"} - A list with all estimated coefficients}
    \item{\code{condMean}:}{Class:\code{"matrix"}  - The fitted conditional Mean (based on t¦t)}
    \item{\code{condStd }:}{Class:\code{"matrix"} - The fitted conditional Standard Deviation (based on t¦t)} 
    \item{\code{Coeff_Std}:}{Class:\code{"list"}  - A list with the standard error of coefficients (same slots as @Coeff)}
    \item{\code{LL}:}{Class:\code{"numeric"} - The final log likelihood of the model}
    \item{\code{k}:}{Class:\code{"numeric"} - Number of States in the model}
    \item{\code{paramVec}:}{Class:\code{"numeric"} - The vector of parameters with the same values as Coeff, but in vector notation (you probably wont use it)}
    \item{\code{stateDur}:}{Class:\code{"numeric"} - The expected duration of each regime (how long it take to change states)}
    \item{\code{nParameter}:}{Class:\code{"numeric"} - Number of parameters in the model}
    \item{\code{sizeModel}:}{Class:\code{"list"} - A  list with information regarding the size of the model}
    \item{\code{distrib}:}{Class:\code{"character"} - The assumed distribution (Normal or r)}
  }
}
\section{Methods}{
    \describe{
    \item{plot}{Plots an object of class 'MS_Model'}
    \item{print}{Prints to screen the fitted MS_Model}
    \item{dim}{Prints to screen the dimension/size of MS_Model}
    }
}

\author{Marcelo Perlin - ICMA/UK <\email{marceloperlin@gmail.com}>}

\seealso{\code{\linkS4class{MS_Simul}} }
\examples{
showClass("MS_Model")
}
\keyword{classes}
