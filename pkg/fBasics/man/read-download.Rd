\name{readDownload}


\alias{composeURL}
\alias{indexGrep}


\title{Download Utilities}


\description{

    Some helpful download utilities. 
    
}


\usage{
composeURL(\dots, prefix="http://")
indexGrep(pattern, x, \dots)
}


\arguments{

    \item{\dots}{
        [composeURL]\cr
        character strings from which the URL will be composed.\cr
        [indexGrep]\cr
        optional arguments to be passed to the function \code{grep}.  
    }
    \item{prefix}{
        a character string specifying the prefix of the URL.
    }
    \item{pattern}{
        a character string containing a regular expression to be 
        matched in the given character vector. 
    }
    \item{x}{
        a character vector where matches are sought.
    }
    
}


%\value{}


\keyword{programming}

