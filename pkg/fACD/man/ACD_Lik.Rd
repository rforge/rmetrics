\name{ACD_Lik}
\alias{ACD_Lik}
\title{Function for calculation of log likelihood of ACD model}
\description{This function calculates the conditional duration and resulting log likelihood of ACD models. 
This function is called by \code{ACD_Fit}.}

\usage{ACD_Lik(param, x, qLag, pLag, distrib, typeACD, typeCall = "maxLik")}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param}{parameter vector}
  \item{x}{Observed duration series}
  \item{qLag}{Maximum lag for alpha parameter}
  \item{pLag}{Maximum lag for beta parameter}
  \item{distrib}{Distribution assumption}
  \item{typeACD}{Type of ACD model}
  \item{typeCall}{Type of Call to function} }

\value{Returns the log likelihood of the model, or filtered series (depending on typeCall).}
\author{Marcelo Perlin - ICMA/UK <\email{marceloperlin@gmail.com}>}