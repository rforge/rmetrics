\name{comment}

\alias{comment,timeSeries-method}
\alias{comment<-,timeSeries-method}

\title{comment for timeSeries objects}


\description{
    Print or assign new comment to a \code{timeSeries} object.
}


\usage{
\S4method{comment}{timeSeries}(x)
\S4method{comment}{timeSeries}(x) <- value

}


\arguments{
  
    \item{x}{
        a \code{timeSeries} object.
        }
    \item{value}{
        a character string - the comment.
    }
    
}


\examples{
## Get Description from timeSeries -
   comment(LPP2005REC)
   
## Add User to comment -
   comment(LPP2005REC) <- paste(comment(LPP2005REC), "by User Rmetrics")
   comment(LPP2005REC)
}


\keyword{chron}

