
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
#  .chcode                   Changes from one to another number system
#  .hex.to.dec               Converts heximal numbers do decimal numbers
#  .dec.to.hex               Converts decimal numbers do heximal numbers
################################################################################


greyPalette = 
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


.chcode = 
function(b, base.in = 2, base.out = 10, digits="0123456789ABCDEF")
{   # A function implemented by Diethelm Wuertz

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


.hex.to.dec =
function(b) 
{   # A function implemented by Diethelm Wuertz

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


.dec.to.hex = 
function(b) 
{   # A function implemented by Diethelm Wuertz

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

