\name{listFunctions}


\alias{listFunctions}
\alias{countFunctions}


\title{Functions Listing}


\description{

    Lists and counts functions from packages.
    
}


\usage{
listFunctions(package, character.only = FALSE)
countFunctions(package, character.only = FALSE)
}


\arguments{

    \item{package}{
        a literal character or a character string denoting the name of the
        package to be listed. 
    }
    \item{character.only}{
        a logical indicating whether 'package' can be
        assumed to be character strings.
    }
}


\value{

    prints a list and counts of functions.

}


\seealso{
    
    \code{\link{listFunctions}},
    \code{\link{listIndex}}.
    
}


\examples{
## listFunctions -
   listFunctions("fBasics")
   
## countFunctions -
   countFunctions("fBasics")
}


\keyword{programming}

