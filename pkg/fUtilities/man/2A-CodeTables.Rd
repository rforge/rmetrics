\name{CodeTables}

\alias{CodeTables}

\alias{characterTable}
\alias{symbolTable}
\alias{colorTable}


\title{Charaqcter, Symbol and Color Tables}


\description{
    
    A collection and description of functions which
    are useful utilities and tools in selectings
    characters, symbols and colors for plotting.
    \cr

    The plot utility functions are:
    
    \tabular{ll}{
    \code{characterTable} \tab Table of Numerical Equivalents to Latin Characters, \cr
    \code{symbolTable} \tab Table of plot characters, plot symbols, \cr
    \code{colorTable} \tab Table of Color Codes and Plot Colors itself. }
    
}
    
    

\usage{ 
characterTable(font = 1, cex = 0.7)
symbolTable(font = par('font'), cex = 0.7)
colorTable(cex = 0.7)
}


\arguments{
  
    \item{font, cex}{
        [*Table] - \cr
        an integer value, the number of the \code{font}, by default font 
        number 1, the standard font for the \code{characterTable}
        or the current plot character font for the 
        \code{plotcharacterTable}. The character size is determined by
        the numeric value \code{cex}, the default size is 0.7.
        }  
    
}


\value{
    
    \code{characterTable}
    \cr
    displays a table with the characters of the requested font.
    The character on line "xy" and column "z" of the table has 
    code \code{"\xyz"}, e.g \code{cat("\\126")} prints: V for font 
    number 1. These codes can be used as any other characters.
    \cr 
    
    \code{symbolTable}
    \cr
    displays a table with the plot characters numbered from 0 to 255.
    \cr
    
    \code{colorTable}
    \cr
    displays a table with the plot colors with the associated color number.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## Character Table for Font 2:
   characterTable(font = 1)
   
## Default Symbol Table:
   symbolTable()
    
## Color Table:
   colorTable()
}


\keyword{programming}

