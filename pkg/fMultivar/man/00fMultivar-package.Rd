\name{fMultivar-package}

\alias{fMultivar-package}
\alias{fMultivar}


\docType{package}


\title{Modelling Multivariate Return Distributions}


\description{

  The Rmetrics "fMultivar"" package is a  collection of functions 
  to manage, to investigate and to analyze bivariate and multivariate 
  data sets of financial returns.
  
}


\details{

    \tabular{ll}{
        Package:    \tab fMultivar\cr
        Type:       \tab Package\cr
        Version:    \tab R 3.0.1\cr
        Date:       \tab 2014\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2014 Rmetrics Assiciation\cr
        URL:        \tab \url{https://www.rmetrics.org}
  }

}


\section{1 Introduction}{

  The package \code{fMultivar} was written to explore and investigate
  bivariate and multivariate financial return series. 
  The bivariate modeling allows us the comparison of financial returns
  from two investments or from one investment and its benchmark. When 
  it comes to the investigation of multiple investment returns from 
  funds or portfolios we are concerned with the multivariate case.
  
  In the case of bivariate distribution functions we provide functions
  for the 2-dimensional Cauchy, Normal, and Student-t distributions.
  A generalisation is made for the family of 2-dimensional elliptical
  distributions. In this case we provide density functions for the 
  \code{"norm"}, \code{"cauchy"}, \code{"t"}, \code{"logistic"}, 
  \code{"laplace"}, \code{"kotz"}, and \code{"epower"} densities.
  
  In the case of multivariate distribution functions from the skew-normal 
  (SN) family and some related ones we recommend to 
  use the density funtions, probability functions and random number
  generators provided by Azzalini's contributed package \code{"sn"}.
  The family of his SN-distributions cover the skew Cauchy, the skew
  Normal, and the skew Student-t distributions. For parameter fitting
  we have added three simple wrapper Functions for an easy to use
  approach to estimate the distributional parameters.
  
  In the case of multivariate distribution functions from the generalized
  hyperbolic (GHYP) family and some related ones we recommend to 
  use the density funtions, probability functions and random number
  generators provided by David Luethi and Wolfgang Breymann's contributed 
  package \code{"ghyp"}.
  The family of their GHYP-distributions cover beside the General
  Hyperbolic distribution (GHYP) also the special cases for the
  Hyperbolic distribution (HYP), for the Normal Inverse Gaussian 
  distribution (NIG), for the Variance Gamma distribution (VG), and 
  for the skewed Student-t distribution (GHST).
}


\section{2 Bivariate Distributions}{

  contains functions to model bivariate density, probability,
  quantile functions, and to generate random numbers.
  
  \preformatted{
  [dpr]cauchy2d         Bivariate Cauchy Distribution
  [dpr]norm2d           Bivariate Normal Disribution
  [dpr]t2d              Bivariate Student-t Disribution
  
  delliptical2d         Bivariate Elliptical Densities
  }
  
}


\section{3 Multivariate SN Distributions}{

  We use the functions from the contributed package \code{"sn"} package 
  to model multivariate density and probability functions, and to 
  generate random numbers for the skew Cauchy, Normal and Student-t
  distributions. Note the symmetric case is also included in these
  functions. The functions are:
  
  \preformatted{
  [dpr]msc              Multivariate Skew Cauchy Distribution
  [dpr]msn              Multivariate Skew Normal Distribution
  [dpr]mst              Multivariate Skew Student-t Distribution
  }
    
  Note the functions are not part of the \code{fMultivar} package they
  depend on the \code{"sn"} package and are loaded when \code{fMultivar} 
  is loaded.
  
  NOTE: In the new version of the \code{fMultivar} package the following 
  two distribution functions \code{*mvsnorm} (multivariate Normal 
  distribution) and \code{*mvst} (multivariate Student-t Distribution)
  will become obsolete together with the \code{mvFit} parameter
  estimation function. The functionality is fully covered by the 
  \code{"sn"} package. (They will be most likely deprecated in the 
  future.)
  
  For parameter estimation please use the simple wrapper functions:
  
  \preformatted{
  mscFit                Multivariate Skew Cauchy Fit
  msnFit                Multivariate Skew Normal Fit
  mstFit                Multivariate Skew Student-t Fit
  }
  
  Thes parameter estimation functions will be in the same style
  as all the other fitting functions in other Rmetrics packages.
  
}


\section{4 Multivariate GHYP Distributions}{

  We refer to the package \code{"ghyp"} authored by
  David Luethi and  Wolfgang Breymann,

}


\section{5 Utility Functions}{

  We have also added some very useful utility functions for the
  bivariate case, these include 2-D grid generation, squared and
  hexagonal binned histograms, 2-D kernel density estimates, 
  bivariate histogram plots:
  
  \preformatted{
  grid2d                Bivariate Square Grid of Coordinates
  binning2d             Bivariate Square/Hexagonal Binning Plot
  density2d             Bivariate Kernel Density Plot
  hist2d                Bivariate Histogram Plot
  gridData              Bivariate gridded data set
  }
   
  For integration we have added two quadratur routines a simple one
  for the bivariate case and an adaptive one for the multivariate case:
  
  \preformatted{
  integrate2d           Bivariate Integration
  adapt                 Multivariate adaptive Quadratur
  }
  
  The function \code{adapt} is a wrapper to the function 
  \code{adaptIntegrate} from the new contributed package
  \code{cubature} authored by Stephan G. Johnson.
  
}


\section{About Rmetrics:}{

    The \code{fImport} Rmetrics package is written for educational 
    support in teaching "Computational Finance and Financial Engineering" 
    and licensed under the GPL.
    
}


\keyword{package}


