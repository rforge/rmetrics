\name{MS_Simul-class}
\docType{class}
\alias{MS_Simul-class}

\title{A Class for the Simulation of a Markov Switching Time Series Process}
\description{This class is the output from the use of function \code{MS_Regress_Simul}. Normally you'll never call the object contructor 
to build this class as this task is already done by \code{MS_Regress_Simul}}

\section{Slots}{
	 \describe{
    \item{\code{nr}:}{Class:\code{"numeric"} - Number of time periods simulated}
    \item{\code{dep}:}{Class:\code{"matrix"} - Simulated Explained/dependent Variable (a Markov Switching Process)}
    \item{\code{indep}:}{Class:\code{"matrix"} - Simulated explanatory variables (arbitrarily normal random numbers)}
    \item{\code{Coeff}:}{Class:\code{"list"} - A list with all coefficients inputed to the function \code{MS_Regress_Simul}}
    \item{\code{S}:}{Class:\code{"numeric"} - The input that controls where to switch (from the call to \code{MS_regress_Simul}}
    \item{\code{trueStates}:}{Class \code{"matrix"} - The "true" states of the model which where build according to transation matrix (Coeff$P)}
    \item{\code{k}:}{Class:\code{"numeric"} - Number of states in the model}
    \item{\code{distrib}:}{Class:\code{"character"} - Distribution assumption for residue}
  }
}
\section{Methods}{
    \describe{
    \item{plot}{Plots an object of class 'MS_Simul'}
    \item{print}{Prints to screen the simulated model (the 'MS_Simul' object)}
    }
}

\author{Marcelo Perlin - ICMA/UK <\email{marceloperlin@gmail.com}>}

\seealso{\code{\linkS4class{MS_Model}} for links to other classes
}
\examples{
showClass("MS_Simul")
}
\keyword{classes}
