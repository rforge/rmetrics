\name{rollMean}                     
                                    
                                    
\title{Rolling Statistics}


\alias{rollMean}
\alias{rollMin}
\alias{rollMax}
\alias{rollMedian}


\description{
    
    Computes rolling mean, min, max and median for a 'timeSeries' object.      
    
}
    
    
\usage{
rollMean(x, k, na.pad = FALSE, 
    align = c("center", "left", "right"), \dots)
rollMin(x, k, na.pad = FALSE, 
    align = c("center", "left", "right"), \dots)
rollMax(x, k, na.pad = FALSE, 
    align = c("center", "left", "right"), \dots)
rollMedian(x, k, na.pad = FALSE, 
    align = c("center", "left", "right"), \dots)
}


\arguments{

    \item{x}{
        an uni- or multivariate 'timeSeries' object.
        }
    \item{k}{
        an integer width of the rolling window. Must be odd for 
        \code{rollMedian}.
        }
    \item{na.pad}{
        a logical flag. Should NA padding be added at beginning?
        By default \code{FALSE}.
        }
    \item{align}{
        a character string specifying whether the index of the 
        result should be left- or right-aligned or centered 
        compared to the rolling window of observations. The
        default choice is set to \code{align="center"}.
        }
    \item{\dots}{
        optional arguments to be passed.
        }
        
}


\value{

    returns an object of class 'timeSeries'.
        
}

    
\details{

    The code in the core of the functions \code{rollMean}, \code{rollMin},
    \code{rollMax}, and \code{rollMedian} was borrowed from the package
    \code{zoo} authored by Achim Zeileis, Gabor Grothendieck and 
    Felix Andrews.
    
}

   
\author{

    Achim Zeileis, Gabor Grothendieck and Felix Andrews 
    for code from the contributed R package \code{zoo} used in the functions 
    \code{rollMean}, \code{rollMin}, \code{rollMax}, and \code{rollMedian}.
     
}


\examples{
## Use Swiss Pension Fund Data Set of Returns - 
   head(LPP2005REC)
   SPI <- LPP2005REC[, "SPI"]
   head(SPI)
   
## Plot Drawdowns - 
   rmean <- rollMean(SPI, k = 10)
   plot(rmean)
}


\keyword{chron}



 