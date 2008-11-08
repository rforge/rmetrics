\name{TermStructure}

\alias{TermStructure}


\alias{NelsonSiegel}
\alias{Svensson}  
            

\title{Term Structure Modelling}


\description{

    A collection and description of functions
    for term structure modelling.
    \cr
    
    The functions are:
    
    \tabular{ll}{   
    \code{NelsonSiegel} \tab Nelson-Siegel Term Structure, \cr
    \code{Svensson} \tab Nelson-Siegel-Svensson Term Structure. }
    
}


\usage{
NelsonSiegel(rate, maturity, doplot = TRUE)
Svensson(rate, maturity, doplot = TRUE)
}


\arguments{

    \item{doplot}{
        a logical. Should a plot be displayed?
        }
    \item{maturity}{
        a numeric vector of maturities on an annual scale.
        }
    \item{rate}{
        a numeric vector of forward rates.
        }
        
}



\value{
    
    a list object with entries returned from the optimization function
    \code{nlminb}.
    
}


\references{

McCulloch J. H. (1990);
    \emph{US Term Structure Data: 1946-87}, 
    Handbook of Monetary Economics, Friedman B.M. and Hahn F.H. (eds.), 
    Elsevier Science.  
  
McCulloch J. H. and Kwon, H.C. (1993);
    \emph{US Term Structure Data: 1947-1991}, 
    Working Paper No. 93-6, Department of Economics, 
    Ohio State University.
   
Zivot E., Wang J.;
    \emph{Modeling Financial Time Series with S-Plus}. 
      
}



\keyword{datasets}

