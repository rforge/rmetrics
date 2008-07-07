\name{subset}


\alias{subset}

\alias{[.timeDate}
\alias{[<-.timeDate}

\title{Subsetting a 'timeDate' Object}


\description{
    
    Extracts or replaces subsets from 'timeDate' objects.
       
}


\usage{
\method{[}{timeDate}(x, \dots, drop = TRUE)
}


\arguments{
  
    \item{x}{
        an object of class \code{timeDate}.
        }
    \item{\dots}{
        arguments passed to other methods.
        }
    \item{drop}{
        a logical flag, by default \code{TRUE}.
        }         
}


\value{
   
    returns an object of class \code{timeDate}.

}


\examples{  
## timeCalendar -
   tC = timeCalendar(2008)
 
## [ Subsetting by Integer Indexes ... 
   tC[9:12] 
   
## [ Subsetting by Python Like Indexing ... 
   tC["2008-09::"]
   
## [ Subsetting by Spans ... 
   tC["2008-09::"]
   
## [<-
   # Replacing:  
}


\keyword{chron}

