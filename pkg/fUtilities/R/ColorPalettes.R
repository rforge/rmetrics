
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received A copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port:
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 COLOR PALETTES:
#  greyPalette               Creates a grey palette
#  .timPalette               Creates a cyan, yellow, to orange palette
#  .rainbowPalette           Creates a rainbow color palette          
#  .heatPalette              Creates a heat color palette
#  .terrainPalette           Creates a terrain color palette
#  .topoPalette              Creates a topo color palette 
#  .cmPalette                Creates a cm color palette
# FUNCTION:                 COLOR NAMES:
#  colorsLocator             Plots R's 657 named colors for selection
#  colorsMatrix              Returns matrix of R's color names
# FUNCTION:                 RGB CONVERTER:
#  .asRGB                    Converts any R color to RGB (red/green/blue)
# FUNCTION:                 CONVERSION HEXIMAL/DECIMAL:
#  .chcode                   Changes from one to another number system
#  .hex.to.dec               Converts heximal numbers do decimal numbers
#  .dec.to.hex               Converts decimal numbers do heximal numbers
# FUNCTION:                 COLOR RAMPS:
# .blue2red                  Creates a blue to red color palette
# .green2red                 Creates a blue to red color palette
# .blue2green                Creates a blue to red color palette
# .purple2green              Creates a blue to red color palette
# .blue2yellow               Creates a blue to red color palette
# .cyan2magenta              Creates a blue to red color palette
################################################################################


################################################################################
# FUNCTION:                 COLOR PALETTES:
#  greyPalette               Creates a grey palette


greyPalette <- 
function(n = 64, start = 255-n, end = 255)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a grey palette like rainbow does for colors

    # Arguments:
    #   n - the number of greys to be constructed
    #   start, end - the range of the color palette

    # Value:
    #   returns a grey palette like rainbow does
    #   for color palettes

    # FUNCTION:

    # Check Consistency:
    if (start < 0) stop("start must be greater or equal to 0")
    if (end > 255) stop("end must be smaller or equal to 255")
    if (start - end > 0) stop("start must be smaller than end")
    if (n > end-start) stop("n must be greater than end-start")

    # Palette:
    Greys = trunc(seq(start, end, length = n))
    Z = substr(.dec.to.hex(c(0:255)), 2, 3)[Greys]
    ans = paste("#", Z, Z, Z, sep = "")

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


if(FALSE) ## This function cannot work: it uses non-existing splint()
.timPalette <- 
function (n = 64)
{
    # Description:
    #   Creates a cyan, yellow, to orange palette
    
    # Notes:
    #   'Tim.colors' in 'fields' package goes from blue to red, and passes
    #   through the colors cyan, yellow, and orange. Also known as Jet
    #   color-map in Matlab. You can also easily design your own color map
    #   using 'rgb' function from 'gdDevices'.
    #   From:  <Jaroslaw.W.Tuszynski@saic.com>

    # FUNCTION:
    
    orig = c(
        "#00008F", "#00009F", "#0000AF", "#0000BF", "#0000CF",
        "#0000DF", "#0000EF", "#0000FF", "#0010FF", "#0020FF",
        "#0030FF", "#0040FF", "#0050FF", "#0060FF", "#0070FF",
        "#0080FF", "#008FFF", "#009FFF", "#00AFFF", "#00BFFF",
        "#00CFFF", "#00DFFF", "#00EFFF", "#00FFFF", "#10FFEF",
        "#20FFDF", "#30FFCF", "#40FFBF", "#50FFAF", "#60FF9F",
        "#70FF8F", "#80FF80", "#8FFF70", "#9FFF60", "#AFFF50",
        "#BFFF40", "#CFFF30", "#DFFF20", "#EFFF10", "#FFFF00",
        "#FFEF00", "#FFDF00", "#FFCF00", "#FFBF00", "#FFAF00",
        "#FF9F00", "#FF8F00", "#FF8000", "#FF7000", "#FF6000",
        "#FF5000", "#FF4000", "#FF3000", "#FF2000", "#FF1000",
        "#FF0000", "#EF0000", "#DF0000", "#CF0000", "#BF0000",
        "#AF0000", "#9F0000", "#8F0000", "#800000")
    if (n == 64) return(orig)
    rgb.tim = t(col2rgb(orig))
    temp = matrix(NA, ncol = 3, nrow = n)
    x = seq(0, 1, , 64)
    xg = seq(0, 1, , n)
    for (k in 1:3) {
        hold = splint(x, rgb.tim[, k], xg)
        hold[hold < 0] = 0
        hold[hold > 255] = 255
        temp[, k] = round(hold)
    }
    rgb(temp[, 1], temp[, 2], temp[, 3], maxColorValue = 255)
}


# ------------------------------------------------------------------------------
#  .rainbowPalette           Creates a rainbow color palette          
#  .heatPalette              Creates a heat color palette
#  .terrainPalette           Creates a terrain color palette
#  .topoPalette              Creates a topo color palette 
#  .cmPalette                Creates a cm color palette


.rainbowPalette = function(n = 64) rainbow(n = n)
.heatPalette = function(n = 64) heat.colors(n)
.terrainPalette = function(n = 64) terrain.colors(n)
.topoPalette = function(n = 64) topo.colors(n)
.cmPalette = function(n = 64) cm.colors(n)


################################################################################
# Package: epitools
# Version: 0.4-9
# Date: 2007-06-27
# Title: Epidemiology Tools
# Author: Tomas Aragon <aragon@berkeley.edu>
# Maintainer: Tomas Aragon <aragon@berkeley.edu>
# Depends: R (>= 2.1.0)
# Description: EpiTools: R Package for Epidemiologic Data and Graphics
# License: GPL version 2 or newer
# URL: http://www.epitools.net
# Packaged: Wed Jun 27 20:27:33 2007; Tomas


colorsLocator <- 
function(locator = FALSE) 
{   
    # A copy from contributed R-package epitools
    
    # Description:
    #   Plots R's 657 named colors for selection
    
    # FUNCTION:
    
    # Settings: 
    cex.axis = 0.7
    
    xx <- rep(1:30, 22)
    yy <- rep(1:22, rep(30, 22))
    yyy <- matrix(yy, ncol = 22)
    cm <- colorsMatrix()
    matplot(xx[1:30], yyy, pch = 15, type = "n", axes = FALSE, 
        xlab = "colorsMatrix[row, ]", ylab = "colorsMatrix[ , col]", 
        main = "Identify Color Names.")
    # title(sub = "Source: www.epitools.net", cex.sub = 0.7)
    points(xx, yy, type = "p", pch = 15, cex = 2, col = c(colors(), 
        NA, NA, NA))
    axis(1, at = c(0:30 + 0.5), labels = FALSE, tick = TRUE)
    axis(1, at = 1:30, label = 1:30, cex.axis = cex.axis, tick = FALSE)
    axis(2, at = c(0:22 + 0.5), labels = FALSE, tick = TRUE)
    axis(2, at = 1:22, label = 1:22, cex.axis = cex.axis, tick = FALSE, 
        las = 1)
        
    if (locator == TRUE) {
        lxy <- locator()
        xy <- round(data.frame(lxy))
        xym <- as.matrix(xy)
        located <- data.frame(xy, color.names = cm[xym])
        return(located)
    } else {
        return(invisible(cm))
    }
}


# ------------------------------------------------------------------------------


colorsMatrix <-
function()
{   
    # A copy from contributed R-package epitools

    # Description:
    #   Returns matrix of R's color names
    
    # FUNCTION:
    
    # Color Names:
    x <- matrix(c(colors(),NA,NA,NA), 30, 22)
    
    # return Value:
    invisible(x)
}



################################################################################


.asRGB <- 
function (col = .rainbowPalette(64), alpha = FALSE)
{   
    # A function implemented by Diethelm Wuertz   
    # Description:
    #   Converts any R color to RGB (red/green/blue)

    # Arguments:
    #   col - vector of any of the three kind of R colors, i.e., either a
    #       color name (an element of colors()), a hexadecimal string of
    #       the form "#rrggbb", or an integer i meaning palette()[i].
    #   alpha - a logical value indicating whether alpha channel values
    #       should be returned.

    # FUNCTION:
    
    # Color Conversion:
    result = .Internal(col2rgb(col))
    if (!alpha) result = result[1:3, , drop = FALSE]

    # Return Value:
    t(result)
}


################################################################################
# FUNCTION:                 CONVERSION HEXIMAL/DECIMAL:
#  .chcode                   Changes from one to another number system
#  .hex.to.dec               Converts heximal numbers do decimal numbers
#  .dec.to.hex               Converts decimal numbers do heximal numbers


.chcode <- 
function(b, base.in = 2, base.out = 10, digits="0123456789ABCDEF")
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Changes from one to another number system

    # Arguments:
    #   b - number specified in the input base
    #   b.in - input base
    #   b.out - output base
    #   digits - digits string

    # Value:
    #   returns the input in the form represented by the output base

    # Author:
    #   Peter Wolf Universitaet Bielefeld
    #   from: http://tolstoy.newcastle.edu.au/R/help/05/04/2085.html

    # FUNCTION:

    # Change Number System:
    digits = substring(digits,1:nchar(digits),1:nchar(digits))
    if (length(base.in) == 1)
        base.in = rep(base.in, max(nchar(b) - 1))
    if (is.numeric(b))
        b = as.character(as.integer(b))
    b.num = lapply(strsplit(b, ""),
        function(x) {match(x, digits)-1} )
    result = lapply(b.num,
        function(x) {cumprod(rev(c(base.in,1))[1:length(x)]) %*% rev(x)} )
    number = unlist(result)
    # DW Print Output Suppressed
    # cat("decimal representation:",number,"\n")
    if (length(base.out) == 1) {
        base.out<-rep(base.out, 1+ceiling(log(max(number), base = base.out)))
    }
    n.base = length(base.out)
    result = NULL
    for(i in n.base:1){
        result = rbind(number %% base.out[i], result)
        number = floor(number/base.out[i])
    }
    result[]<-digits[result+1]
    ans = apply(result, 2, paste, collapse = "")

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.hex.to.dec <- 
function(b)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts heximal numbers do decimal numbers

    # Arguments:
    #   b - a heximal number

    # Value:
    #   returns a heximal numbers as decimal numbers

    # FUNCTION:

    # Hex to Bin:
    ans = as.numeric(.chcode(b, base.in = 16, base.out = 10))

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.dec.to.hex <- 
function(b)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts decimal numbers do heximal numbers

    # Arguments:
    #   x - a decimal number

    # Value:
    #   returns a decimal numbers as heximal numbers

    # FUNCTION:

    # Decimal to Hex:
    ans = .chcode(b, base.in = 10, base.out = 16)

    # Return Value:
    ans
}


################################################################################
# Package: colorRamps
# Type: Package
# Title: Builds pleasing color tables
# Version: 1.0
# Date: 2007-04-05
# Author: Tim Keitt
# Maintainer: Tim Keitt <tkeitt@gmail.com>
# Description: Builds single and double gradient color maps
# License: GPL
# Packaged: Thu Apr  5 16:34:42 2007; tkeitt

# FUNCTION:                 COLOR RAMPS:
# .blue2red                  Creates a blue to red color palette
# .green2red                 Creates a blue to red color palette
# .blue2green                Creates a blue to red color palette
# .purple2green              Creates a blue to red color palette
# .blue2yellow               Creates a blue to red color palette
# .cyan2magenta              Creates a blue to red color palette


.blue2red <-  
function(n)
{
    # A copy from contributed R-package colorRamps
    
    # FUNCTION:
    
    # Color Ramp:
    n2 = ceiling(n / 2)
    red = rep(c(0, 1), each = n2)[1:n]
    green = 1 - abs(seq(-1, 1, length.out = n))
    blue = rev(red)
    
    # Return Value:
    rgb(red, green, blue)
}


# ------------------------------------------------------------------------------


.green2red <-  
function(n)
{
    # A copy from contributed R-package colorRamps
    
    # FUNCTION:
    
    # Color Ramp:
    n2 = ceiling(n / 2)
    red = rep(c(0, 1), each = n2)[1:n]
    blue = 1 - abs(seq(-1, 1, length.out = n))
    green = rev(red)
    
    # Return Value:
    rgb(red, green, blue)
  }


# ------------------------------------------------------------------------------


.blue2green <-  
function(n)
{
    # A copy from contributed R-package colorRamps
    
    # FUNCTION:
    
    # Color Ramp:
    n2 = ceiling(n / 2)
    green = rep(c(0, 1), each = n2)[1:n]
    red = 1 - abs(seq(-1, 1, length.out = n))
    blue = rev(green)
    
    # Return Value:
    rgb(red, green, blue)
}


# ------------------------------------------------------------------------------


.purple2green <-  
function(n)
{
    # A copy from contributed R-package colorRamps
    
    # FUNCTION:
    
    # Color Ramp:
    red = rep(0.5, length.out = n)
    green = seq(0, 1, length.out = n)
    blue = rev(green)
    
    # Return Value:
    rgb(red, green, blue)
}


# ------------------------------------------------------------------------------


.blue2yellow <-  
function(n)
{
    # A copy from contributed R-package colorRamps
    
    # FUNCTION:
    
    # Color Ramp:
    red = seq(0, 1, length.out = n)
    green = red
    blue = rev(red)
    
    # Return Value:
    rgb(red, green, blue)
}


# ------------------------------------------------------------------------------


.cyan2magenta <-  
function(n)
{
    # A copy from contributed R-package colorRamps
    
    # FUNCTION:
    
    # Color Ramp:
    red = seq(0, 1, length.out = n)
    green = rev(red)
    blue = rep(1, n)
    
    # Return Value:
    rgb(red, green, blue)
}


################################################################################