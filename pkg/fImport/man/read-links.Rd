\name{read.links}


\alias{read.links}


\title{Links Browser interface}


\description{

    Uses the Links Text Browser to read a web page. 
    
}


\usage{
read.links(url, intern = TRUE, bin = NULL, pipe = FALSE, \dots)
}


\arguments{

    \item{url}{
        a character string specifying the URL of the web page.
    }
    \item{intern}{
        a logical which indicates whether to make the output 
        of the command an R object. 
    }
    \item{bin}{
        a string with the path of your lynx binary or NULL if lynx
        binary is available in the operating system path.
      }
      \item{pipe}{
    a logical which indicates whether the result should be returned
        as a \code{pipe()} commmand.
    }
    \item{\dots}{
        optional arguments passed to links binary.
    }
}


\value{

    the downloaded text.

}


\references{

Diethelm Wuertz, Yohan Chalabi, and Andrew Ellis, (2010);
    \emph{Financial Market Data for R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich,
    www.rmetrics.org.
    
}


\keyword{programming}

