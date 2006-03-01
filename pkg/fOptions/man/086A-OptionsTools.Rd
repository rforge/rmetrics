\name{OptionsTools}


\alias{OptionsTools}

\alias{xmpOptions}
\alias{xmpfOptions}


\title{fOptions Tools}


\description{

    Popups the example menu and starts or continues 
    the example and demo programs.
    \cr
    
}


\usage{
xmpfOptions()
xmpOptions(prompt = "")
}


\arguments{

    \item{prompt}{
        the string printed when prompting the user for input. 
        }   
}


\value{
  
    xmpfOptions
    \cr
    Popups the example menu.
    \cr
    
    xmpOptions
    \cr
    Nothing, the default, or the the prompt if you have set
    \code{xmpOptions = readline} on the command prompt. 
    
}


\details{

    The example in the manual pages may be interactive and ask for
    input from the user. To achieve this you have to type on the
    command line: \code{xmpOptions = readline}

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.

}


\examples{
## SOURCE("fOptions.086A-OptionsTools")

\dontrun{
## xmpfOptions -
   # Popup the examples menu:
   xmpfOptions()
}}


\keyword{programming}

