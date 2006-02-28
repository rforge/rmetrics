\name{CalendarTools}


\alias{CalendarTools}

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


\note{

	The example in the manual pages may be interactive and ask for
	input from the user. To achieve this you have to type on the
	command line: \code{xmpCalendar = readline}

}


\author{

	Diethelm Wuertz for the Rmetrics \R-port.

}


\examples{
## SOURCE("fCalendar.xmpTools")

## myFinCenter -
   myFinCenter
   
## currentYear -
   currentYear
   
## date -
   date()
}


\keyword{programming}

