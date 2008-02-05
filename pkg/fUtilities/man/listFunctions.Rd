\name{listFunctions}


\alias{listFunctions}
\alias{countFunctions}


\title{Functions Listing}


\description{

    Lists and counts functions from packages.
    
}


\usage{
listFunctions(package)
countFunctions(package)
}


\arguments{

    \item{package}{
      a literal character or a character string denoting the name of the
      package to be listed. 
        }
        
}


\value{

    prints a list and counts of functions.

}


\examples{
## listFunctions -
   listFunctions("fUtilities")
   
## countFunctions -
   countFunctions("fUtilities")
}


\keyword{programming}

