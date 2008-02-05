\name{listIndex}


\alias{listIndex}


\title{Index File Listing}


\description{

    Lists the content of an index file. 
    
}


\usage{
listIndex(package = "Rmetrics", character.only = FALSE)
}


\arguments{

  \item{package}{
    a literal character string or a character string denoting the name
    of the package to be listed.
  }
  \item{character.only}{
    a logical indicating whether 'package' can be
    assumed to be character strings.
  }
  
}


\value{

    prints the index file.

}


\examples{
## listIndex -
   listIndex("fUtilities")
}


\keyword{programming}

