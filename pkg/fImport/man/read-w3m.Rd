\name{read-w3m}


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


\references{

    Diethelm Wuertz, Yohan Chalabi, and Andrew Ellis, (2010);
    \emph{Financial Market Data for R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich,
    www.rmetrics.org.
    
}


\seealso{
       
    Alternative text browser functions are the Rmetrics functions
    \code{\link{read.links}} and \code{\link{read.lynx}}.
     
    To download \code{xls} and \code{xlsx} spread sheets use the
    functions \code{gdata::read.xls} and \code{xlsx::read.xlsx} 
    from the contributed packages \code{gdata} and \code{xlsx}, 
    respectively.
    
    To download text files line by line use the Rmetrics function
    \code{\link{read.lines}} which wraps the function {readLines}
    from R's base evironment.
    
    To postprocess downloaded files use the Rmetrics functions
    \code{\link{indexGrep}}, \code{\link{dataSplit}}, 
    \code{\link{charvecSplit}}, and \code{\link{stringSplit}}.
    
}


\keyword{programming}

