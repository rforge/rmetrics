
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
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             DESCRIPTION:    
#  tsPlot                Returns a time series plot
#  histPlot              Returns a histogram plot
#  densityPlot           Returns a kernel density estimate plot
#  .splusLikePlot        Sets parameters that plots look more Splus like
# FUNCTION:             3-DIMENSIONAL PLOTS:
#  circlesPlot           Returns a scatterplot of circles indexing 3rd variable
#  perspPlot             Returns a perspective plot in 2 dimensions
#  contourPlot           Returns a contour plot in 2 dimensions
# FUNCTION:             TABLES AND PALETTES:
#  characterTable        Shows a table of character's numerical equivalents 
#  plotcharacterTable    Shows a table of plot characters and symbols
#  colorTable            Shows a table of plot color codes
#  .chcode               Changes from one to another number system
#  .hex.to.dec           Converts heximal numbers do decimal numbers
#  .dec.to.hex           Converts decimal numbers do heximal numbers
#  grey.pal              Creates a grey palette like rainbow does for colors
# FUNCTION:             SLIDER MENU:
#  .sliderMenu           Starts a slider menu
################################################################################


tsPlot = 
function(x, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns time series graphs in a common plot
  
    # FUNCTION:
    
    # Plot:
    plot(x, ylab = "", ...)
         
    # Return Value:
    invisible(x)
}
     
   
# ------------------------------------------------------------------------------


histPlot = 
function(x, col = "steelblue4", border = "white", 
main = x@units, add.fit = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a probability histogram plot for each column of a 
    #   timeSeries object
    
    # FUNCTION:
    
    # Settings:
    xlim = NULL
    
    # Transform 'timeSeries':
    units = x@units
    DIM = dim(x@Data)[2]
      
    # Construct output list:
    ans = paste( " hist", 1:DIM, " = NULL", sep = "", collapse = ",")
    ans = paste( "list(",  ans, ")" )
    ans = eval(parse(text = ans))
    
    # Histogram Plots:
    for (i in 1:DIM) {
        Values = as.vector(x@Data[, i])
        mean = mean(Values)
        sd = sd(Values)
        if (is.null(xlim)) 
            xlim = c(qnorm(0.001, mean, sd), qnorm(0.999, mean, sd)) 
        result = hist(x = Values, col = col, border = border, 
            breaks = "FD", main = main[i], xlim = xlim, probability = TRUE,
            ...)  
        if (add.fit) {
            s = seq(xlim[1], xlim[2], length = 201)
            lines(s, dnorm(s, mean, sd), col = "brown")
        }
        ans[[i]] = result  
    }
    names(ans) = units
    
    # Return Value:
    invisible(ans)
}  


# ------------------------------------------------------------------------------


densityPlot = 
function(x, col = "steelblue4", main = x@units, add.fit = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns density plots for each column of a 
    #   timeSeries object

    # FUNCTION:
    
    # Transform 'timeSeries':
    units = x@units
    DIM = dim(x@Data)[2]
    xlim = NULL
      
    # Construct output list:
    ans = paste( " hist", 1:DIM, " = NULL", sep = "", collapse = ",")
    ans = paste( "list(",  ans, ")" )
    ans = eval(parse(text = ans))
    
    # Histogram Plots:
    for (i in 1:DIM) {
        Values = as.vector(x@Data[, i])
        mean = mean(Values)
        sd = sd(Values)
        if (is.null(xlim)) 
            xlim = c(qnorm(0.001, mean, sd), qnorm(0.999, mean, sd)) 
        Density = density(Values, ...)
        plot(x = Density, xlim = xlim, col = col, type = "l", 
            main = main[i], ...)  
        ans[[i]] = Density  
        # Add Fit:
        if (add.fit) {
            s = seq(xlim[1], xlim[2], length = 201)
            lines(s, dnorm(s, mean, sd), col = "brown")
        }
        # Grid:
        grid()
    }

    # Names:
    names(ans) = units
    
    # Return Value:
    invisible(ans)
}


# ------------------------------------------------------------------------------


.splusLikePlot = 
function(scale = 0.8)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Sets parameters that plots look more Splus like
    
    # Arguments:
    #   cex.axis - The magnification to be used for axis annotation
    #       relative to the current.
    #   cex.lab - The magnification to be used for x and y labels relative
    #       to the current.
    #   cex.main - The magnification to be used for main titles relative
    #       to the current.
    #   cex.sub - The magnification to be used for sub-titles relative to
    #       the current.
    
    # Note: 
    #   * Scales plotting text and symbols relative to the default
    #     so that plots look more SplusLike.    
    #   * Further parameters will be added in the future.

    # FUNCTION:
    
    # Set par:
    par(cex.axis = scale, cex.lab = scale, cex.main = scale, cex.sub = scale)
    
    # Return value:
    invisible()
}


################################################################################


circlesPlot = 
function(x, y, size = 1, ...)
{   # A function implemented by GKS
    
    # Description:  
    #   Creates a scatterplot with circle size indexing a 
    #   third variable.
    
    # Example:
    #   circlesPlot(x = rnorm(50), y = rnorm(50), size = abs(rnorm(50)))

    # FUNCTION:
    
    # Settings:
    stopifnot(length(x) == length(x))
    if (length(size) != length(x))
    size = rep(size[1], times = length(x))
    
    # Circle Plot:
    plot(x, y, type = "n", ...)
    symbols(x, y, add = TRUE, circles = sqrt(size), inches = 0.25, ...)
    
    # Return Value:
    invisible(data.frame(x = x, y = y, size = size))
}


# ------------------------------------------------------------------------------


perspPlot = 
function(x, y, z, theta = -40, phi = 30, col = "steelblue4", ps = 9, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns a perspecvtive plot
    
    # Notes:
    #   A synonyme call for function 'persp'
    
    # FUNCTION:   
    
    # Settings:
    # if (SPLUSLIKE) .splusLikePlot(TRUE)
    
    # Perspective Plot:
    if (class(version) == "Sversion") {
        # we assume SPlus:
        ans = persp(x = x, y = y, z = z, ...) 
    } else {
        # R:
        par(ps = ps)
        if (!exists("ticktype")) ticktype = "detailed"
        if (!exists("expand")) expand = 0.6
        if (!exists("r")) r = 500
        ans = persp(x = x, y = y, z = z, theta = theta, phi = phi, 
            col = col, ticktype = ticktype, expand = expand, ...) 
    }
        
    # Return Value:
    invisible(ans)
}


# ------------------------------------------------------------------------------


contourPlot = 
function(x, y, z, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns a contour plot
    
    # Notes:
    #   A synonyme call for function 'contour'
    
    # FUNCTION:   
    
    # Settings:
    # if (SPLUSLIKE) .splusLikePlot(TRUE)
    
    # Contour Plot:
    if (class(version) == "Sversion") {
        # we assume SPlus:
        ans = contour(x = x, y = y, z = z, ...) 
    } else {
        # R:
        ans = contour(x = x, y = y, z = z, ...) 
    }
        
    # Return Value:
    invisible(ans)
}

                        
################################################################################


characterTable = 
function(font = 1, cex = 0.7) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Prints numeric equivalents to all latin characters.

    # Notes:
    #   The printed version doesn't allways corresponds to the 
    #   screen display. The character on line "xy" and column 
    #   "z" of the table has code "xyz". These codes can be 
    #   used as any other characters. 
    #     e.g. title("\347\340 et \340")
    #   Note on S:
    #   As the command line window of Splus can't print special 
    #   characters 
    #     cat("\347\340 et \340") 
    #   will not print the special characters, at least under 
    #   4.5 and under 2000.
    
    # Author:
    #   Source from Pierre Joyet, pierre.joyet@bluewin.ch

    # Example:
    #   for (i in 1:20) characterTable(font = i)

    # FUNCTION:
    
    # Table:
    v = 40:377
    v = v[v %% 100 < 80 & v %% 10 < 8]
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot(-1:7, seq(4, 33, length = 9), type = "n", axes = FALSE, 
        xlab = "", ylab = "", cex = cex, main = "Table of Characters")
    k = 1
    for(i in 4:31)
        for(j in 0:7) {
            text(j, 35 - i, eval(parse(text = paste("\"\\", v[k], "\"",
                    sep = ""))), font = font, cex = cex)
            k = k + 1 }
    
    text(0:7, rep(33, 7), as.character(0:7), font = 3, cex = cex)
    text(rep(-1, 28), 31:4, as.character(c(4:7, 10:17, 20:27, 
        30:37)), font = 3, cex = cex)
    
    # Return Value:
    invisible(font)
}


# ------------------------------------------------------------------------------


plotcharacterTable = 
function(font = par('font'), cex = 0.7) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Shows a table of plot characters.
    
    # Example:
    #   plotcharacterTable()
    
    # Author:
    #   Unknown, code found on the internet.

    # FUNCTION:
    
    # Table:
    plot(0, 0, xlim = c(-1, 11), ylim = c(0, 26), type = 'n', 
        axes = FALSE, xlab = '', ylab = '', 
        main = "Table of Plot Characters")
    j = -1
    for(i in 0:255) {
        if(i %% 25 == 0) {j = j+1; k = 26}
        k = k-1
        points(j, k, pch = i, font = font, cex = cex, col = 2)
        text(j+0.50, k, i, cex = cex) 
    }
    
    # Return Value:
    invisible(font)
}


# ------------------------------------------------------------------------------


colorTable = 
function(cex = 0.7) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays a table of plot colors.
    
    # Author:
    #   Unknown, code found on the internet.
    
    # Example:
    #   colorTable()

    # FUNCTION:
    
    # Plot:
    plot(0, 0, xlim = c(-1, 10), ylim = c(0, 10), type = 'n', axes = FALSE, 
        xlab = '', ylab = '', cex = cex, main = "Table of Color Codes")
    j = -1
    for(i in 0:99) {
        if(i %% 10 == 0) {j = j+1; k = 10}
        k = k-1
        points(j, k, pch = 15, col = i, cex = 2)
        text(j+0.45, k, i, cex = cex)}
    
    # Return Value:
    invisible()
}


################################################################################
    

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


# ------------------------------------------------------------------------------


greyPal = 
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


# ******************************************************************************


.sliderMenu =   
function(refresh.code, names, minima, maxima, resolutions, starts, 
title = "Slider", no = 0, set.no.value = 0)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Starts a slider menu
    
    # Source:
    #   Built on code written by Peter Wolf
    
    # FUNCTION:
    
    # Requirement:
    require(tcltk)
    
    # Environment:
    if (!exists("slider.env")) {
        slider.env <<- new.env() 
    }    
    if (no != 0) {
        options(show.error.messages = FALSE)
        ans = as.numeric(tclvalue(get(paste("slider", no, sep = ""),
            env = slider.env)))
        options(show.error.messages = TRUE)
        return(ans)
    }              
    if (set.no.value[1] != 0) { 
        try(eval(parse(text = paste("tclvalue(slider", set.no.value[1], 
            ")<-", set.no.value[2], sep = "")), env = slider.env),
            silent = TRUE)
        return(set.no.value[2]) 
    }
    
    # Toplevel:
    nt = tktoplevel()
    tkwm.title(nt, title)
    
    
    # Slider:
    for (i in seq(names)) {
        eval(parse(text = paste("assign(\"slider", i, "\", 
            tclVar(starts[i]), env = slider.env)", sep = "")))
        tkpack(fr<-tkframe(nt))
        lab = tklabel(fr, text = names[i])
        sc = tkscale(fr, command = refresh.code, from = minima[i], 
            to = maxima[i], showvalue = TRUE, resolution = 
            resolutions[i], orient = "horiz")
        assign("sc", sc, env = slider.env)
        tkgrid(sc, lab)
        eval(parse(text = paste("tkconfigure(sc, variable = slider", i, ")",
            sep = "")), env = slider.env)
    }
    tkpack(fr<-tkframe(nt)) 
    
    # Quit:
    quitButton = tkbutton(fr, text = "   Quit   ", 
        command = function() {
            tkdestroy(nt) 
        } )
    
    # Reset:
    resetButton = tkbutton(fr, text = "   Start | Reset   ", 
        command = function() {
            for (i in seq(starts)) eval(parse(text = 
                paste("tclvalue(slider", i, ")<-", starts[i], sep = "")),
                env = slider.env)
            refresh.code()    
        }  )
        
    # Compose:
    tkgrid(resetButton, quitButton, sticky = "sew")
}


################################################################################

