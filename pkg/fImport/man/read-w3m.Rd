\name{read.w3m}


\alias{read.w3m}


\title{w3m Browser interface}


\description{

    Uses the w3m Browser to read a web page. 
    
}


\usage{
read.w3m(url, intern = TRUE, bin = NULL, pipe = FALSE, \dots)
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
        a string with the path of your w3m binary or NULL if w3m
        binary is available in the operating system path.
      }
      \item{pipe}{
	a logical which indicates whether the result should be returned
        as a \code{pipe()} commmand.
	}
    \item{\dots}{
        optional arguments passed to w3m binary. 
        For a list of options, see the w3m manual page.
    }
}


\value{

    the downloaded text.

}


\keyword{programming}

