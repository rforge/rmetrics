\name{characterTable}


\alias{characterTable}


\title{Table of Characters}


\description{
    
    Displays a table of numerical equivalents to Latin characters.
    
}
    
    

\usage{ 
characterTable(font = 1, cex = 0.7)
}


\arguments{
  
    \item{cex}{
        a numeric value, determines the character size, the default 
        size is 0.7.
        }  
    \item{font}{
        an integer value, the number of the \code{font}, by default font 
        number 1.
        }  
    
}


\value{
    
    \code{characterTable}\cr
    \cr
    
    displays a table with the characters of the requested font.
    The character on line "xy" and column "z" of the table has 
    code \code{"\xyz"}, e.g \code{cat("\\126")} prints: V for font 
    number 1. These codes can be used as any other characters.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## Character Table for Font 2:
   characterTable(font = 1)
}


\keyword{programming}

