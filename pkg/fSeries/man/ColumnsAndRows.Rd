\name{ColumnsAndRows}


\alias{ColumnsAndRows}


\alias{dim.timeSeries}
\alias{dimnames.timeSeries}
\alias{colnames<-.timeSeries}
\alias{rownames<-.timeSeries}
\alias{is.array.timeSeries}
  

\title{timeSeries Columns and Rows}


\description{
    
    A collection and description of functions 
    and methods dealing with columns and rows of 
    'timeSeries' objects. 
    \cr
    
    \tabular{ll}{
    \code{dim} \tab Returns the dimension of a 'timeSeries' object, \cr
    \code{dimnames} \tab Returns the dimension names of a 'timeSeries' object, \cr
    \code{colnames<-} \tab Assigns column names to a 'timeSeries' object, \cr
    \code{rownames<-} \tab Assigns row names to a 'timeSeries' object, \cr
    \code{is.array} \tab Allows that NCOL and NROW work properly. }
    
}


\usage{

\method{dim}{timeSeries}(x)
\method{dimnames}{timeSeries}(x)

colnames<-.timeSeries(x) <- value
rownames<-.timeSeries(x) <- value

\method{is.array}{timeSeries}(x)
}


\arguments{
  
   
    \item{value}{
        a valid value for column names component of \code{dimnames(x)}. 
        For a \code{"timeSeries"} object this is either \code{NULL} or a 
        character vector of length the column dimension. Not, row names
        cannot be assigne for a \code{"timeSeries"} object, the function
        \code{rownames()} will stop and return an error message.
        }
    \item{x}{
        an object of class \code{timeSeries}.
        }
       
}


\value{

    NA
    
}



\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## NYI
}


\keyword{chron}

