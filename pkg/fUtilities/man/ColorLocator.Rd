\name{ColorLocator}

\alias{colorLocator}
\alias{colorMatrix}


\title{Color Management}


\description{
    
    Plots R's 657 named colors for selection
 
}
    
    

\usage{ 
colorLocator(locator = FALSE)
colorMatrix(locator = TRUE)
}


\arguments{
   
    \item{locator}{
        a logical flag,
        activates \code{locator} for interactive selection of color names,
        default is \code{FALSE}.
        }
    
}


\value{

    Color Locator:
    
    \code{colorsLocator} generates a plot with R colors and, when 
    \code{locator=TRUE}, returns matrix with graph coordinates and 
    names of colors selected. \code{colorsMatrix} quietly returns 
    matrix of names.
    
}


\details{

    Color Locator:
    
    The \code{colorLocator} function plots R's 657 named colors.  If 
    \code{locator=TRUE} then you can interactively point and click to 
    select the colors for which you want names. To end selection, right 
    click on the mouse and select 'Stop', then R returns the selected 
    color names. 
       
}
    
    
\author{

    Diethelm Wuertz for the Rmetrics \R-port, \cr
    Tomas Aragon, aragon@berkeley.edu, for the color locator and matrix.
    
}


\examples{
## colorLocator -
   colorLocator()
}


\keyword{programming}

