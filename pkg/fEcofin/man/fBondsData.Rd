\name{BondsData}

\alias{BondsData}


\alias{bundesbankNSS}
\alias{mk.zero2}
\alias{mk.maturity}    
            

\title{Bonds Data Sets}


\description{

    A collection and description of data sets 
    from the Rmetrics Package fBonds.
    \cr
    
    The data sets are:
    
    \tabular{ll}{   
    \code{bundesbankNSS} \tab Nelson-Siegel-Svensson Coefficients, \cr
    \code{mk.zero2} \tab US zero-coupon yield curve, \cr
    \code{mk.maturity} \tab US term structure maturities. }
    
}


\details{
            
    \bold{Bundesbank Nelson-Siegel-Svensson Coefficients:}\cr
    \cr
    \code{bundesbankNSS}
    coefficients for the Nelson-Siegel-Svensson yield curve.
        from the German Bundesbank.\cr
    The data set ranges from 1973-01-03 to 1996-07-23.\cr
    The columns are named: BMW.RET.\cr
    Source: German Bundesbank.\cr
    
    
    \bold{US zero-coupon yield curve:}\cr
    \cr
    \code{mk.zero2} 
    is a data set with a 67 x 55 values representing the US zero-coupon 
        yield curve. \cr
    The data set ranges from August 1985 to February 1991.\cr
    The columns are named:\cr
    Source: \cr
    
    
    \bold{US term structure maturities:}\cr
    \cr
    \code{mk.maturity} 
        is a numeric vector of length 55, giving the fifty-five maturities 
        in terms of years for the term structure.\cr
    The data set ranges from August 1985 to February 1991.\cr
    The columns are named:\cr
    Source: \cr
    
}


\format{
    
    Time series files are in CSV Excel spreadsheet format. 
    The delimiter is a semicolon. 
    
}


\references{

McCulloch J. H. (1990);
    \emph{US term structure data: 1946-87}, 
    Handbook of Monetary Economics, Friedman B.M. and Hahn F.H. (eds.), 
    Elsevier Science.  
  
McCulloch J. H. and Kwon, H.C. (1993);
    \emph{US term structure data: 1947-1991}, 
    Working Paper No. 93-6, Department of Economics, 
    Ohio State University.
      
}


\keyword{datasets}

