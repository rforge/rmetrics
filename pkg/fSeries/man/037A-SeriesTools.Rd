\name{SeriesTools}


\alias{SeriesTools}

\alias{xmpSeries}
\alias{xmpfSeries}


\title{fSeries Tools}


\description{

	Popups the example menu and starts or continues the example 
	and demo programs.
	\cr
	
}


\usage{
xmpfSeries()
xmpSeries(prompt = "")
}


\arguments{

  	\item{prompt}{
		the string printed when prompting the user for input. 
		}	
}


\value{
  
	\code{xmpfSeries}
	\cr
	Popups the example menu.
	\cr
	
	\code{xmpSeries}
	\cr
	Nothing, the default, or the the prompt if you have set
	\code{xmpSeries = readline} on the command prompt. 
	
}


\details{

	The example in the manual pages may be interactive and ask for
	input from the user. To achieve this you have to type on the
	command line: \code{xmpSeries = readline}

}


\author{

	Diethelm Wuertz for the Rmetrics \R-port.

}


\examples{
### SOURCE("fBasics.xmpTools")

\dontrun{
## xmpfSeries -
   # Popup the examples menu:
   xmpfSeries()
}
}


\keyword{programming}

