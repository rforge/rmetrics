\name{read-lynx}


\alias{read.lynx}


\title{Lynx Browser interface}


\description{

    Uses the Lynx Browser to read a web page. 
    
}


\usage{
read.lynx(url, intern = TRUE, bin = NULL, pipe = FALSE, \dots)
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
        optional arguments passed to lynx binary. For example
        \code{accept_all_cookies = TRUE} or
        \code{cookie_file="~/.lynx_cookies"}. For a list of options, see
        the lynx manual page.
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
    \code{\link{read.links}} and \code{\link{read.w3m}}.
    
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

