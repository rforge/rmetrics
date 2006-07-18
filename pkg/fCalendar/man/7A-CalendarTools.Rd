\name{CalendarTools}


\alias{CalendarTools}

\alias{xmpCalendar}
\alias{xmpfCalendar}

\title{fCalendar Tools}


\description{

    A collection and description of functions and tools 
    used in the 'fCalendar' package. This includes the
    setting of the local environment and the management
    of examples.
    \cr
    
}


\usage{

xmpfCalendar()
xmpCalendar(prompt = "")

}


\arguments{

 
    \item{prompt}{
        the string printed when prompting the user for input. 
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
   
}


\keyword{programming}

