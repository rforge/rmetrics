\name{fPortfolioTools}


\alias{fPortfolioTools}

\alias{xmpPortfolio}
\alias{xmpfPortfolio}


\title{fPortfolio Tools}


\description{

    Tools used in the 'fPortfolio' package.
    \cr
    
}


\usage{
xmpfPortfolio()
xmpPortfolio(prompt = "")
}


\arguments{

    \item{prompt}{
        the string printed when prompting the user for input. 
        }   
}


\value{
  
    \code{xmpfPortfolio}
    \cr
    Popups the example menu.
    \cr
    
    \code{xmpPortfolio}
    \cr
    Nothing, the default, or the the prompt if you have set
    \code{xmpPortfolio = readline} on the command prompt. 
    
}


\note{

    The example in the manual pages may be interactive and ask for
    input from the user. To achieve this you have to type on the
    command line: \code{xmpPortfolio = readline}

}


\author{

    Diethelm Wuertz for the \R-Port.

}


\examples{
## SOURCE("fPortfolio.109B-PortfolioTools")

\dontrun{
## xmpfPortfolio -
   # Popup the examples menu:
   xmpfPortfolio()
}

}


\keyword{programming}

