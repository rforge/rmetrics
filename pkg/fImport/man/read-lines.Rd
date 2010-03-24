\name{read.lines}


\alias{read.lines}


\title{Read from a text file line by line}


\description{

    Reads from a text file line by line. 
    
}


\usage{
read.lines(con=stdin(), n=-1, ok=TRUE, warn=FALSE, encoding="unknown") 
}
    
    
\arguments{

    \item{con}{
        a connection object or a character string.
    }
    \item{n}{
        an integer, the (maximal) number of lines to read. 
        Negative values indicate that one should read up 
        to the end of input on the connection.
    }
    \item{ok}{
        a logical, is it OK to reach the end of the connection 
        before n > 0 lines are read? If not, an error will be 
        generated.
    }
    \item{warn}{
        a logical, warn if a text file is missing a final EOL.
        The default is FALSE, note different from function
        \code{readLines}.
    }
    \item{encoding}{
        a character string, the encoding to be assumed for input 
        strings. 
    }
}


\value{

    the downloaded text.

}


\keyword{programming}

