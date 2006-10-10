\name{fBasicsUtilities}

\alias{fBasicsUtilities}

\alias{characterTable}
\alias{symbolTable}
\alias{colorTable}
\alias{greyPalette}

\alias{histPlot}
\alias{densityPlot}
\alias{qqbayesPlot}


\title{General Rmetrics Utilities}


\description{
    
    A collection and description of functions which
    are useful utilities and tools used by Rmetrics.
    \cr

    The plot utility functions are:
    
    \tabular{ll}{
    \code{characterTable} \tab Table of Numerical Equivalents to Latin Characters, \cr
    \code{symbolTable} \tab Table of plot characters, plot symbols, \cr
    \code{colorTable} \tab Table of Color Codes and Plot Colors itself, \cr
    \code{greyPalette} \tab Creates a grey palette like rainbow does for colors.}
    
    The tailored plot functions are:
    
    \tabular{ll}{
    \code{histPlot} \tab Returns a tailored histogram plot, \cr
    \code{densityPlot} \tab Returns a tailored kernel density estimate plot, \cr
    \code{qqbayesPlot} \tab Returns a tailored quantile-quantile plot.}
    

}
    
    

\usage{ 
characterTable(font = 1, cex = 0.7)
symbolTable(font = par('font'), cex = 0.7)
colorTable(cex = 0.7)
greyPalette(n = 64, start = 255-n, end = 255)

histPlot(x, col = "steelblue", border = "white", main = x@units, add.fit = TRUE, \dots) 
densityPlot(x, col = "steelblue", main = x@units, add.fit = TRUE, \dots)
qqbayesPlot(x, labels = TRUE, \dots) 
}


\arguments{
  
    \item{add.fit}{
        [*Plot] - \cr
        a logical, should a fit added to the Plot?
        }
    \item{col, border, main}{
        [*Plot] - \cr
        plot parameters.
        }
    \item{font, cex}{
        [*table] - \cr
        an integer value, the number of the \code{font}, by default font 
        number 1, the standard font for the \code{characterTable}
        or the current plot character font for the 
        \code{plotcharacterTable}. The character size is determined by
        the numeric value \code{cex}, the default size is 0.7.
        }  
    \item{labels}{
        a logical, should labels be added to the plot?
        }
    \item{n, start, end}{
        [greyPal] - cr
        \code{n} gives the number of greys to be constructed,
        \code{start} and \code{end} span the range of the color palette.
        By default 64 grey tones equidistant chosen from the color range 
        (191, 191, 191) to (255, 255, 255).
        } 
    \item{x}{
        an object of class \code{"timeSeries"}.
        }
    \item{\dots}{
        optional arguments to be passed.
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
## SOURCE("fBasics.1A-fBasicsUtilities")
}


\keyword{programming}

