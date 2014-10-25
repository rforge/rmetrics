\name{MS_Regress_Lik}
\alias{MS_Regress_Lik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The likelihood function for a General Markov Switching Model}
\description{This function calculates the log likelihood of the model and is maximized by Rdonlp2. You'll probably never have to call this function (MS_Regress_Fit does that for you..)}
\usage{
MS_Regress_Lik(param)
}
\arguments{
  \item{param}{a vector with parameters, only. All the rest of the input arguments are passed as global variables (see rdonlp2 documentation)}
}

\value{Returns the sum of log likelihood. It may also return the filtered series of the model (see MS_Regress_Fit code)}
\references{
\cite{ALEXANDER, C. (2008) 'Market Risk Analysis: Practical Financial Econometrics' Wiley}\cr
\cite{HAMILTON, J., D. (1994) 'Time Series Analysis' Princeton University Press}\cr
\cite{HAMILTON, J., D. (2005) ‘Regime Switching Models’ Palgrave Dictionary of Economic}\cr
\cite{KIM, C., J., NELSON, C., R. (1999) ‘State Space Model with Regime Switching: Classical and Gibbs-Sampling Approaches with Applications’ The MIT press}
}
\author{Marcelo Perlin - ICMA/UK <\email{marceloperlin@gmail.com}>}
\seealso{\code{\link{MS_Regress_Simul}},\code{\link{MS_Regress_For}},\code{\link{MS_Regress_Fit}}}
\examples{# No Example. This function should normally not be called}

\keyword{models}
\keyword{regression}
