\name{ColorPalettes}

\alias{ColorPalettes}

\alias{greyPalette}


\title{Color and Grey Palettes}


\description{
    
    A collection and description of functions for
    using and managing color and grey palettes.
    \cr

    Color and Grey Palettes:
    
    \tabular{ll}{
    \code{greyPalette} \tab Creates a grey palette like rainbow does for colors.}
    
}
    
    

\usage{ 
greyPalette(n = 64, start = 255-n, end = 255)
}


\arguments{
   
    \item{n, start, end}{
        [greyPal] - cr
        \code{n} gives the number of greys to be constructed,
        \code{start} and \code{end} span the range of the color palette.
        By default 64 grey tones equidistant chosen from the color range 
        (191, 191, 191) to (255, 255, 255).
        } 
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fEcofin.1B-ColorPalettes")

## GreyPalette
   greyPalette()
}


\keyword{programming}

