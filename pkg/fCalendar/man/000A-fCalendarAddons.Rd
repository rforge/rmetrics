\name{fCalendarAddons}

\alias{fCalendarAddons}

\alias{datax}
            

\title{Calendar Addons}


\description{
    
    A collection and description of functions which
    adds additional functionality to R's base installation.
    \cr
    
}


\usage{
datax(\dots, list = character(0), package = NULL, lib.loc = NULL,
    verbose = getOption("verbose"), envir = .GlobalEnv)
}


\arguments{

    \item{list, package, lib.loc, verbose, envir}{
        [data] - \cr
        additional arguments to the \code{data} function.
        }
    \item{\dots}{
        a sequence of names or literal character strings.
        }
        
}


\details{

    The function \code{datax} is an extended version of R's
    function \code{data}. It comes with the same functionality
    beside the fact, that data files loaded from the demo
    directories are transformed into S4 \code{"timeSeries"}
    objects if the data structure allows this.
    
    
    For details for the function \code{data} we refer to the 
    original help page.
    
}
  

\examples{
## SOURCE("fCalendar.000A-CalendarAddons")

## datax -
   datax(DowJones30)
   class(DowJones30)
   print(head(DowJones30))
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}


\keyword{programming}

