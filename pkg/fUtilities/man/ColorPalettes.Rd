\name{ColorPalettes}

\alias{ColorPalettes}

\alias{rainbowPalette} 
\alias{heatPalette}
\alias{terrainPalette}
\alias{topoPalette}
\alias{cmPalette}

\alias{greyPalette}
\alias{timPalette}

\alias{rampPalette}

\alias{seqPalette}
\alias{divPalette}
\alias{qualiPalette}

\alias{focusPalette}
\alias{monoPalette}


\title{Color Palettes}


\description{
    
    Creates color palettes.
 
}
    
    

\usage{ 
rainbowPalette(n = 64, \dots) 
heatPalette(n = 64, \dots) 
terrainPalette(n = 64, \dots) 
topoPalette(n = 64, \dots) 
cmPalette(n = 64, \dots) 

greyPalette(n = 64, \dots)
timPalette(n = 64)

rampPalette(n, name = c("blue2red", "green2red", "blue2green",     
    "purple2green", "blue2yellow", "cyan2magenta"))
    
seqPalette(n, name = c(
    "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", 
    "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", 
    "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"))
divPalette(n, name = c(
    "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", 
    "Spectral"))   
qualiPalette(n, name = c(
    "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", 
    "Set3")) 
    
focusPalette(n, name = c("redfocus", "greenfocus", "bluefocus"))
monoPalette(n, name = c("redmono", "greenmono", "bluemono"))
}


\arguments{
   
    \item{n}{
        an integer, giving the number of greys or colors to be constructed.
        } 
    \item{name}{
        a character string, the name of the color set.
        }    
    \item{\dots}{
        arguments to be passed, see the details section
        }    
}


\value{

    returns a character string of color strings.
   
}


\details{

    All Rmetrics' color sets are named as \code{fooPalette} where the 
    prefix \code{foo} denotes the name of the underlying color set.
    \cr
    
    
    \bold{R's Contiguous Color Palettes:}\cr
    \cr
    Palettes for \code{n} contiguous colors are implemented in the
    \code{grDevices} package. To be conform with Rmetrics' naming
    convention for color palettes we have build a wrapper around the
    underlying functions. These are the \code{rainbowPalette},  
    \code{heatPalette}, \code{terrainPalette}, \code{topoPalette}, and the  
    \code{cmPalette}. 
    Conceptually, all of these functions actually use (parts of) a line 
    cut out of the 3-dimensional color space, parametrized by the function
    \code{hsv(h,s,v,gamma)}, where \code{gamma=1} for the \code{fooPalette} 
    function, and hence, equispaced hues in RGB space tend to cluster at 
    the red, green and blue primaries. Some applications such as contouring 
    require a palette of colors which do not wrap around to give a final 
    color close to the starting one. To pass additional arguments to the
    underlying functions we refer to consult \code{help(rainbow)}.
    With rainbow, the parameters start and end can be used to specify 
    particular subranges of hues. Synonyme function calls are 
    \code{rainbow},  \code{heat.colors},\code{terrain.colors}, 
    \code{topo.colors}, and the \code{cm.colors}. 
    \cr
    
    
    \bold{R's Gamma-Corrected Gray Palette:}\cr
    \cr
    The function \code{grayPalette} chooses a series of \code{n} 
    gamma-corrected gray levels. The range of the gray levels can be
    optionally monitored through the \code{\dots} arguments, for
    details \code{help(gray.colors)}, which is a synonyme function call
    in the \code{grDevices} package.
    \cr
    
    
    \bold{Tim's Matlab like Color Palette:}\cr
    \cr
    The function \code{timPalette} creates a color set ranging from blue 
    to red, and passes through the colors cyan, yellow, and orange. It
    comes from the Matlab software, originally used in fluid dynamics
    simulations. The function here is a copy from R's contributed package
    \code{fields} doing a spline interpolation on \code{n=64} color points.
    \cr
    
    
    \bold{Color Ramp Palettes:}\cr
    \cr
    The function \code{rampPalette} creates several color ramps. The 
    function is implemented from Tim Keitt's contributed R package 
    \code{colorRamps}. 
    Supported through the argument \code{name} are the following color
    ramps: \code{"blue2red"}, \code{"green2red"}, \code{"blue2green"}, 
    \code{"purple2green"}, \code{"blue2yellow"}, \code{"cyan2magenta"}.
    \cr
    
    
    \bold{Color Brewer Palettes:}\cr
    \cr
    The functions \code{seqPalette}, \code{divPalette}, and
    \code{qualiPalette} create color sets according to R's 
    contributed \code{RColorBrewer} package. The first letter in the
    function name denotes the type of the color set: "s" for
    sequential palettes, 'd" for diverging palettes, and "q" for
    qualitative palettes.
    \cr 
    \emph{Sequential palettes} are suited to ordered data that progress  
    from low to high. Lightness steps dominate the look of these schemes, 
    with light colors for low data values to dark colors for high data 
    values. The sequential palettes names are:
    Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu,
    PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd.
    \cr
    \emph{Diverging palettes} put equal emphasis on mid-range critical values 
    and extremes at both ends of the data range. The critical class or 
    break in the middle of the legend is emphasized with light colors 
    and low and high extremes are emphasized with dark colors that have 
    contrasting hues.
    The diverging palettes names are:
    BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral.
    \cr
    \emph{Qualitative palettes} do not imply magnitude differences between 
    legend classes, and hues are used to create the primary visual 
    differences between classes. Qualitative schemes are best suited to 
    representing nominal or categorical data.
    The qualitative palettes names are:
    Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3.
    \cr
    In contrast to the original color brewer palettes, the palettes here
    are created by spline interpolation from the color variation with
    the most different values, i.e for the sequential palettes these
    are 9 values, for the diverging palettes these are 11 values, and
    for the qualitative palettes these are between 8 and 12 values
    dependeing on the color set.
    \cr

    
    \bold{Graph Color Palettes:}\cr
    \cr   
    The function \code{perfanPalette} creates color sets inspired 
    by R's cotributed package \code{Performance Analytics}. 
    These color palettes have been designed to create readable, 
    comparable line and bar graphs with specific objectives.
    \cr
    \emph{Focused Color Palettes:} Color sets designed to provide focus to 
    the data graphed as the first element. This palette is best used when 
    there is clearly an important data set for the viewer to focus on, 
    with the remaining data being secondary, tertiary, etc. Later elements 
    graphed in diminishing values of gray.
    \cr
    \emph{Monchrome Color Palettes:} These include color sets for monochrome 
    color displays.
   
}
    


\note{

    Additional undocumented hidden Functions:
    
    \tabular{ll}{
    % RGB CONVERTER:
    \code{.asRGB} \tab Converts any R color to RGB (red/green/blue), \cr
    % CONVERSION HEXIMAL/DECIMAL:
    \code{.chcode} \tab Changes from one to another number system, \cr
    \code{.hex.to.dec} \tab Converts heximal numbers do decimal numbers, \cr
    \code{.dec.to.hex} \tab Converts decimal numbers do heximal numbers.}

}
    
    
\author{

    Diethelm Wuertz for the Rmetrics \R-port, \cr
    Cynthia Brewer and Mark Harrower for the brewer palettes, \cr
    Peter Carl and  Brian G. Peterson for the "PerformanceAnalytics" package, \cr
    Tim Keitt for the "colorRamps" package, \cr
    Ross Ihaka for the "colorspace" package, \cr
    Tomas Aragon for the "epitools" package, \cr
    Doug Nychka for the "fields" package, \cr
    Erich Neuwirth for the "RColorBrewer" package.
}


\examples{
## GreyPalette:
   greyPalette()
}


\keyword{programming}

