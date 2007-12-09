\name{ColorManagement}

\alias{ColorManagement}
\alias{ColorPalettes}

\alias{greyPalette}
\alias{colorsLocator}
\alias{colorsMatrix}


\title{Color Management}


\description{
    
    A collection and description of functions for
    using and managing colors.
    \cr

    Color and Grey Palettes:
    
    \tabular{ll}{
    \code{greyPalette} \tab Creates a grey palette like rainbow does for colors, \cr
    % COLORS LOCATOR:
    \code{colorsLocator} \tab Plots R's 657 named colors for selection, \cr
    \code{colorsMatrix} \tab Returns matrix of R's color names. }
 
}
    
    

\usage{ 
greyPalette(n = 64, start = 255-n, end = 255)

colorsLocator(locator = FALSE)
colorsMatrix(locator = TRUE)
}


\arguments{
   
    \item{locator}{
        a logical flag,
        activates \code{locator} for interactive selection of color names,
        default is \code{FALSE}.
        }
    \item{n, start, end}{
        [greyPal] - cr
        \code{n} gives the number of greys to be constructed,
        \code{start} and \code{end} span the range of the color palette.
        By default 64 grey tones equidistant chosen from the color range 
        (191, 191, 191) to (255, 255, 255).
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
    
    % The \code{.colorMmatrix} function is used by \code{colorLocator} to 
    % create the matrix of color names that corresponds to the graph 
    % created by colors.plot. \code{.colorMmatrix} can be used alone to 
    % create the matrix of name without generating a plot. To see the 
    % matrix it must be assigned an object name and then displayed. 
    % Visit http://www.epitools.net for the latest, \cr
       
}
    


\note{

    Additional undocumented hidden Functions:
    
    \tabular{ll}{
    % MORE COLOR PALETTES:
    \code{.timPalette} \tab Creates a cyan, yellow, to orange palette, \cr
    \code{.rainbowPalette} \tab Creates a rainbow color palette, \cr
    \code{.heatPalette} \tab Creates a heat color palette, \cr
    \code{.terrainPalette} \tab Creates a terrain color palette, \cr
    \code{.topoPalette} \tab Creates a topo color palette, \cr
    \code{.cmPalette} \tab Creates a cm color palette, \cr
    % RGB CONVERTER:
    \code{.asRGB} \tab Converts any R color to RGB (red/green/blue), \cr
    % CONVERSION HEXIMAL/DECIMAL:
    \code{.chcode} \tab Changes from one to another number system, \cr
    \code{.hex.to.dec} \tab Converts heximal numbers do decimal numbers, \cr
    \code{.dec.to.hex} \tab Converts decimal numbers do heximal numbers, \cr
    % COLOR RAMPS:
    \code{.blue2red} \tab Creates a blue to red color palette, \cr
    \code{.green2red} \tab Creates a blue to red color palette, \cr
    \code{.blue2green} \tab Creates a blue to red color palette, \cr
    \code{.purple2green} \tab Creates a blue to red color palette, \cr
    \code{.blue2yellow} \tab Creates a blue to red color palette, \cr
    \code{.cyan2magenta} \tab Creates a blue to red color palette. }

}
    
    
\author{

    Diethelm Wuertz for the Rmetrics \R-port, \cr
    Tomas Aragon, aragon@berkeley.edu, for the colors locator.
    
}


\examples{
## GreyPalette:
   greyPalette()
}


\keyword{programming}

