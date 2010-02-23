\name{read.lynx}


\alias{read.lynx}


\title{Lynx Browser interface}


\description{

    Uses the Lynx Browser to read a web page. 
    
}


\usage{
read.lynx(url, intern=TRUE, 
    bin="C:/cygwin/bin/lynx.exe -dump -dont_wrap_pre", \dots)
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
        a string with the path of your lynx binary. The default
        setting expects a cygwin installation under Windows.
    }
    \item{\dots}{
        optional arguments passed to the internally called
        function \code{system}.
    }
}


\value{

    the downloaded text.

}


\keyword{programming}

