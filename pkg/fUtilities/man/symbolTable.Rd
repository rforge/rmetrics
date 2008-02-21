\name{symbolTable}


\alias{symbolTable}


\title{Table of Symbols}


\description{
    
    Displays a Table of plot characters and symbols.
    
}
    
    

\usage{ 
symbolTable(font = par('font'), cex = 0.7)
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

    \code{symbolTable}
    
    displays a table with the plot characters and symbols numbered from 
    0 to 255 and returns invisible the name of the font.
       
}


\seealso{

    \code{link{characterTable}},
    \code{link{colorTable}}.
    
}


\examples{  
## symbolTable - 
   # Default Symbol Table:
   symbolTable()
}


\keyword{programming}

