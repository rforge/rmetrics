\name{fMultivar-package}

\alias{fMultivar-package}
\alias{fMultivar}

\docType{package}

\title{Modelling Multivariate Distributions Package}


\description{

  Package for modelling multivariate distributions. The functions  
  may be useful in the analysis of bivariate and multivariate
  fincnial return series.
  
}


\details{

  \tabular{ll}{
    Package:    \tab fMultivar \cr
    Type:       \tab Package \cr
    Version:    \tab 270.73 \cr
    Date:       \tab 2008 \cr
    License:    \tab GPL (>= 2) \cr
    Copyright:  \tab (c) 1999-2014 Diethelm Wuertz and Rmetrics Association \cr
    URL:        \tab \url{https://www.rmetrics.org}
  }
  

}


\section{Bivariate Distribution functions}{

  contains functions to model bivariate density, probability,
  quantile functions, and to generate random numbers.
  
  Functions:
  
  \tabular{ll}{
    \code{*cauchy2d} \tab Bivariate Cauchy Distribution \cr 
    \code{*elliptical2d} \tab Bivariate Elliptical Disributions \cr 
    \code{*norm2d} \tab Bivariate Normal Disribution \cr 
    \code{*t2d} \tab Bivariate Student-t Disribution
    }
  
}


\section{Multivariate Distribution Functions}{

  contains functions to model multivariate density, probability,
  quantile functions, and to generate random numbers.
  
  Functions:
  
  \tabular{ll}{
    \code{*mvnorm} \tab Multivariate Normal Distribution \cr 
    \code{*mvt} \tab Multivariate Student-t Distribution \cr
    \code{*msn} \tab Multivariate Skew Normal Distribution \cr 
    \code{*msc} \tab Multivariate Skew Cauchy Distribution\cr 
    \code{*mst} \tab Multivariate Skew Student-t Distribution
    }
    
}


\section{Utility Functions}{

  contains utility functions to model bi- and multivariate 
  distribution functions.
  
  Functions:
  
  \tabular{ll}{
    \code{grid2d} \tab Bivariate Square Grid of Coordinates \cr 
    \code{binning2d} \tab Bivariate Square/Hexagonal Binning Plot \cr 
    \code{density2d} \tab Bivariate Kernel Density Plot \cr 
    \code{hist2d} \tab Bivariate Histogram Plot\cr 
    \code{gridData} \tab Bivariate gridded data set \cr
    \code{persp} \tab Perspective plot from a gridded data set \cr
    \code{contour} \tab Contour plot from a gridded data set \cr
    \code{integrate2} \tab Bivariate Integration \cr
    \code{adapt} \tab Multivariate adaptive Quadratur
    }
  
}


\author{

  Diethelm Wuertz and Rmetrics Core Team.
  
}


\keyword{package}

