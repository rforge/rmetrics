\name{ReturnSeriesGUI}

\alias{returnSeriesGUI}


\title{Return Series Plots}


\description{
    
    A graphical user interface to display
    finanical time series plots.
    \cr

    List of Functions:
    
    \tabular{ll}{
    \code{returnSeriesGUI} \tab Opens a GUI for return series plots.}
    
}
    

\usage{ 
returnSeriesGUI(x)
}


\arguments{
  
    \item{x}{
        an object of class \code{"timeSeries"} or any other object which
        can be transformed by the function \code{as.timeSeries} into an
        object of class \code{timeSeries}. The latter case, other then
        \code{timeSeries} objects, is more or less untested.
        }  
    
}


\value{
       
    \code{returnSeriesGUI}
    \cr
    For the \code{returnSeriesGUI} function, beside the graphical
    user interface no values are returned.\cr
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## 
}


\keyword{programming}

