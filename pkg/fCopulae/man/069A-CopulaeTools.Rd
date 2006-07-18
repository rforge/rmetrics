\name{CopulaeTools}


\alias{CopulaeTools}


\alias{qlogisticTable}

\alias{xmpCopulae}
\alias{xmpfCopulae}


\title{fCopulae Tools}


\description{

    Popups the example menu and starts or continues the example 
    and demo programs.
    
}


\usage{
xmpfCopulae()
xmpCopulae(prompt = "")
}


\arguments{

    \item{prompt}{
        the string printed when prompting the user for input. 
        }   
}


\value{
  
    xmpfCopulae
    \cr
    Popups the example menu.
    \cr
    
    xmpCopulae
    \cr
    Nothing, the default, or the the prompt if you have set
    \code{xmpCopulae = readline} on the command prompt. 
    
}


\details{

    The example in the manual pages may be interactive and ask for
    input from the user. To achieve this you have to type on the
    command line: \code{xmpCopulae = readline}

}


\author{

    Diethelm Wuertz for this R-Port.

}


\examples{
## SOURCE("fCopulae.069A-CopulaeTools")
\dontrun{
## xmpfCopulae -
   # Popup the examples menu:
   xmpfCopulae()
}
}


\keyword{programming}

