  % --- Source file: parametervg.Rd ---
\name{vgParam}
\alias{smallShape}
\alias{largeShape}
\alias{smallParam}
\alias{largeParam}

\title{Parameter Sets for Variance Gamma Distribution}

\description{
  These objects store different parameter sets of the Variance Gamma distribution
  for testing or demonstrating purpose as matrixes. Specifically, the parameter 
  sets \code{smallShape} and \code{largeShape} have constant (standard) location
  and spread parameters of \eqn{c}{c}=0 and \eqn{\sigma}{sigma}=1; 
  where asymmetry and shape parameters vary from \eqn{\theta}{theta}=(-2, 0, 2)
  and \eqn{\nu}{nu}=(0.5, 1, 2) for \code{smallShape} and 
  \eqn{\theta}{theta}=(-4, -2, 0, 2, 4)
  and \eqn{\nu}{nu}=(0.25, 0.5, 1, 2, 4) for \code{largeShape}.
  
  The parameter sets \code{smallParam} and \code{largeParam} have varied 
  values of all 4 parameters. \code{smallParam} contains all of the parameter 
  combinations from \eqn{c}{c}=(-2, 0, 2), \eqn{\sigma}{sigma}=(0.5, 1, 2),
  \eqn{\theta}{theta}=(-2, 0, 2) and \eqn{\nu}{nu}=(0.5, 1, 2). 
  \code{largeParam} contains all of the parameter combinations from 
  \eqn{c}{c}=(-4, -2, 0, 2, 4), \eqn{\sigma}{sigma}=(0.25, 0.5, 1, 2, 4),
  \eqn{\theta}{theta}=(-4, -2, 0, 2, 4) and \eqn{\nu}{nu}=(0.25, 0.5, 1, 2, 4).    
}

\usage{
  smallShape
  largeShape
  smallParam
  largeParam  
}

\format{
  \code{smallShape}: a 9 by 4 matrix;
  \code{largeShape}: a 25 by 4 matrix;
  \code{smallParam}: a 81 by 4 matrix;
  \code{largeParam}: a 625 by 4 matrix.
}

\author{David Scott \email{d.scott@auckland.ac.nz}, Christine Yang Dong}

\examples{
data(vgParam)
## Testing the accuracy of vgMean  
for (i in 1:nrow(smallParam)) {
    param <- smallParam[i,]  
    x <- rvg(10000,param = param)             
    sampleMean <- mean(x)
    funMean <- vgMean(param = param)
    difference <- abs(sampleMean - funMean) 
    print(difference)
}

}


                                                                                                         