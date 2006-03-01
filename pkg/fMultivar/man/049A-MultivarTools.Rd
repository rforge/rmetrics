\name{MultivarTools}


\alias{MultivarTools}

\alias{xmpMultivar}
\alias{xmpfMultivar}


\title{fMultivar Tools}


\description{

    Popups the example menu and starts or continues the example 
    and demo programs.
    \cr
    
}


\usage{
xmpfMultivar()
xmpMultivar(prompt = "")
}


\arguments{

    \item{prompt}{
        the string printed when prompting the user for input. 
        }   
}


\value{
  
    \code{xmpfMultivar}
    \cr
    Popups the example menu.
    \cr
    
    \code{xmpMultivar}
    \cr
    Nothing, the default, or the the prompt if you have set
    \code{xmpMultivar = readline} on the command prompt. 
    
}


\details{

    The example in the manual pages may be interactive and ask for
    input from the user. To achieve this you have to type on the
    command line: \code{xmpMultivar = readline}

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.

}


\examples{
## SOURCE("fMultivar.049A-MultivarTools")

\dontrun{
## xmpfMultivar -
   # Popup the examples menu:
   xmpfMultivar()
}
}


\keyword{programming}

