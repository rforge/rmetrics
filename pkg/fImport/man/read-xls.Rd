\name{read.xls}


\alias{read.xls}


\title{Read from an xls file}


\description{

    Reads a sheet from an xls file. 
    
}


\usage{
read.xls(url, sheet=1, lines=-1, verbose=FALSE, encoding="unknown")
}
    
    
\arguments{

    \item{url}{
        a character string specifying the URL of the web page.
    }
    \item{sheet}{
        an integer denoting which sheet should be extracted,
        by default the first. 
    }
    \item{lines}{
        a negative integer with the lines to be skipped, by
        default the first.
    }
    \item{verbose}{
        a logical decides about verbose mode, by default FALSE.
    }
    \item{encoding}{
        a character string with the type of encoding, by
        defaul \code{"unknown"}.
    }
}


\value{

    the downloaded text.

}


\keyword{programming}

