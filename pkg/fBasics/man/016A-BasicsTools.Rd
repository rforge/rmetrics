\name{fBasicsTools}


\alias{fBasicsTools}

\alias{xmpBasics}
\alias{xmpfBasics}


\title{fBasics Tools}


\description{

	Tools used in the fBasics library.
	\cr
}


\usage{
xmpfBasics()
xmpBasics(prompt = "")
}


\arguments{

  	\item{prompt}{
		the string printed when prompting the user for input. 
	}
}


\value{
  
	xmpfBasics
	\cr
	Popups the example menu.
	\cr
	
	xmpBasics
	\cr
	Nothing, the default, or the the prompt if you have set
	\code{xmpBasics = readline} on the command prompt. 
	
}


\note{

	The example in the manual pages may be interactive and ask for
	input from the user. To achieve this you have to type on the
	command line: \code{xmpBasics = readline}

}


\author{

	Diethelm Wuertz for the Rmetrics \R-port.

}


\examples{
## SOURCE("fBasics-xmpTools")
}


\keyword{programming}

