\name{CalendarTools}


\alias{CalendarTools}


\alias{datax}

\alias{myFinCenter}
\alias{myUnits}
\alias{currentYear}

\alias{xmpCalendar}
\alias{xmpfCalendar}

\alias{modify}
\alias{modify.default}
\alias{atoms}
\alias{atoms.default}

\title{fCalendar Tools}


\description{

    A collection and description of functions and tools 
    used in the 'fCalendar' package. This includes the
    setting of the local environment and the management
    of examples.
    \cr
    
}


\usage{
datax(\dots, list = character(0), package = NULL, lib.loc = NULL,
    verbose = getOption("verbose"), envir = .GlobalEnv)

myFinCenter
currentYear
myUnits

xmpfCalendar()
xmpCalendar(prompt = "")

}


\arguments{

    \item{currentYear}{
        the current year, formatted as \code{CCYY}.
        }
    \item{list, package, lib.loc, verbose, envir}{
        [datax] - \cr
        additional arguments to the \code{datax} function.
        }
    \itam{myFinCenter}{
        a character string denoting my financial center settings,
        by default \code{"Zurich"}.
        }
    \itam{myUnits}{
        a character string denoting current time units, one from
        "secs", "mins", "hours", "days", or "weeks".
        }
    \item{prompt}{
        the string printed when prompting the user for input. 
        } 
    \item{\dots}{
        [datax] - \cr
        a sequence of names or literal character strings.
        }
          
}


\value{
  
    xmpfCalendar
    \cr
    Popups the example menu.
    \cr
    
    xmpCalendar
    \cr
    Nothing, the default, or the the prompt if you have set
    \code{xmpCalendar = readline} on the command prompt. 
    
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
  


\note{

    The example in the manual pages may be interactive and ask for
    input from the user. To achieve this you have to type on the
    command line: \code{xmpCalendar = readline}

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.

}


\examples{
## SOURCE("fCalendar.7A-CalendarTools")

## datax -
   # datax(DowJones30)
   # class(DowJones30)
   # head(DowJones30)

## myFinCenter -
   myFinCenter
   
## currentYear -
   currentYear
   
## date -
   date()
}


\keyword{programming}

