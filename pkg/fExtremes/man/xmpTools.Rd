\name{ExtremesTools}


\alias{ExtremesTools}

\alias{xmpExtremes}
\alias{xmpfExtremes}


\title{fExtremes Tools}


\description{

	Popups the example menu and starts or continues the example 
	and demo programs.
	
}


\usage{
xmpfExtremes()
xmpExtremes(prompt = "")
}


\arguments{

  	\item{prompt}{
		the string printed when prompting the user for input. 
		}	
}


\value{
  
	xmpfExtremes
	\cr
	Popups the example menu.
	\cr
	
	xmpExtremes
	\cr
	Nothing, the default, or the the prompt if you have set
	\code{xmpExtremes = readline} on the command prompt. 
	
}


\details{

	The example in the manual pages may be interactive and ask for
	input from the user. To achieve this you have to type on the
	command line: \code{xmpExtremes = readline}

}


\examples{
\dontrun{
## xmpfExtremes -
   # Popup the examples menu:
   xmpfExtremes()
}
}


\author{

	Diethelm Wuertz for this R-Port.

}


\keyword{programming}

