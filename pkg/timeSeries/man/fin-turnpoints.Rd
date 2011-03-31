\name{turns}


\title{Turning Points of a Time Series}


\alias{turns}
\alias{turnsStats}


\description{
    
    Extracts and analyzes turn points of an univariate \code{timeSeries} 
    object.   
    
}
    
    
\usage{
turns(x, \dots)

turnsStats(x, doplot = TRUE)
}


\arguments{

    \item{x}{
        an univariate 'timeSeries' object of financial indices or prices.
        }
    \item{\dots}{
        optional arguments passed to the function \code{na.omit}.
        }
    \item{doplot}{
        a logical flag, should the results be plotted? By default TRUE.    
        }
        
}


\details{


    The function \code{turns} determines the number and the position of 
    extrema (turning points, either peaks or pits) in a regular time series. 
    
    The function \code{turnsStats} calculates the quantity of information
    associated to the observations in this series, according to Kendall's 
    information theory.
    
    The functions are borrowed from the contributed R package \code{pastecs}
    and made ready for working together with univariate \code{timeSeries} 
    objects. You need not to load the R package \code{pastecs}, the code parts
    we need here are builtin in the \code{timeSeries} package.
    
    We have renamed the function \code{turnpoints} to \code{turns} to
    distinguish between the original function in the contributed R package 
    \code{pastecs} and our Rmetrics function wrapper. 
    
    For further details please consult the help page from the contributed R
    package \code{pastecs}.
    
} 


\value{

    \code{turns}\cr
    returns an object of class \code{timeSeries}.
    
    \code{turnsStats}\cr
    returns an object of class \code{turnpoints} with the following entries:\cr 
    \code{data} - The dataset to which the calculation is done.\cr
    \code{n} - The number of observations.\cr
    \code{points} - The value of the points in the series, after elimination of ex-aequos.\cr
    \code{pos} - The position of the points on the time scale in the series (including ex-aequos).\cr
    \code{exaequos} - Location of exaequos (1), or not (0).\cr
    \code{nturns} - Total number of tunring points in the whole time series.\cr
    \code{firstispeak} - Is the first turning point a peak (TRUE), or not (FALSE).\cr
    \code{peaks} - Logical vector. Location of the peaks in the time series without ex-aequos.\cr
    \code{pits} - Logical vector. Location of the pits in the time series without ex-aequos.\cr
    \code{tppos} - Position of the turning points in the initial series (with ex-aequos).\cr
    \code{proba} - Probability to find a turning point at this location.\cr 
    \code{info} - Quantity of information associated with this point.\cr
       
}

    

\author{

    Frederic Ibanez and Philippe Grosjean for code from the contributed R package 
    \code{pastecs} and Rmetrics for the function wrapper.
    
}


\references{

    Ibanez, F., 1982, Sur une nouvelle application de la theorie de l'information a 
    la description des series chronologiques planctoniques. J. Exp. Mar. Biol. Ecol., 
    4, 619--632

    Kendall, M.G., 1976, Time Series, 2nd ed. Charles Griffin and Co, London.
    
}


\examples{
## Load Swiss Equities Series -  
   SPI.RET <- LPP2005REC[, "SPI"]
   head(SPI.RET)

## Cumulate and Smooth the Series -
   SPI <- smoothLowess(cumulated(SPI.RET), f=0.05)
   plot(SPI)
   
## Plot Turn Points Series - 
   SPI.SMOOTH <- SPI[, 2]
   tP <- turns(SPI.SMOOTH)
   plot(tP)
   
## Compute Statistics -
   turnsStats(SPI.SMOOTH)
}


\keyword{chron}

