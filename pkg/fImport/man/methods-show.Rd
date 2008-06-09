\name{show-methods}


\docType{methods}


\alias{show-methods}
\alias{show,ANY-method}
\alias{show,fWEBDATA-method}


\title{WEBDATA Download Show Methods}


\description{
    
    Show methods for WEBDATA downloads. 
   
}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function.
        }
    \item{object = "fWEBDATA"}{
        Print function for objects of class \code{"fWEBDATA"}.
        }   
        
    }
}


\examples{
\dontrun{
## print --
   IBM = yahooImport("IBM")
   print(IBM)
}
}


\keyword{data}

