\name{read.lynx}


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


\keyword{programming}

