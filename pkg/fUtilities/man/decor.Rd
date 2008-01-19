\name{DecorFunctions}

\alias{decor}
\alias{hgrid}
\alias{vgrid}
\alias{boxL}
\alias{box_}
\alias{copyright}

\title{Decor Functions}


\description{

    Functions for decorating plots.
    \cr
      
    The plot utility functions are:
    
    \tabular{ll}{
    \code{decor} \tab simple decoration function, \cr
    \code{hgrid} \tab creates horizontal grid lines, \cr
    \code{vgrid} \tab creates vertical grid lines, \cr
    \code{boxL} \tab creates a L-shaped box, \cr
    \code{box_} \tab creates a bogttom line box, \cr
    \code{copyright} \tab adds Rmetrics copyright to a plot. }
    
}


\usage{
decor()

hgrid(ny = NULL, ...) 
vgrid(nx = NULL, ...) 

boxL(col = "white") 
box_(col = c("white", "black")) 

copyright()
}


\arguments{

    \item{col}{
        the color of the background, \code{"black"} and foreground
        \code{"white"} lines of the box.
        }
    \item{nx, ny}{
        number of cells of the grid in x or y direction. When \code{NULL}, 
        as per default, the grid aligns with the tick marks on the 
        corresponding default axis (i.e., tickmarks as computed by axTicks). 
        } 
    \item{\dots}{
        additional arguments passed to the \code{grid()} function.
        }
        
}


\author{

    Diethelm Wuertz for this R port.
    
}


\examples{
## Test Plot Function:
   plot(x = rnorm(100), type = "l", col = "red", 
     xlab = "", ylab = "Variates", las = 1)
   title("Normal Deviates", adj = 0)
   hgrid()
   boxL()
   copyright()
}


\keyword{hplot}

