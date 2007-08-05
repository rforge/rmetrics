
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
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:
#  .header.ts
#  .showDataCmd
################################################################################


################################################################################
# .header.ts
# .showDataCmd


.head.ts = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Description: 
    #   Show the head of a ts object
    
    # FUNCTION:
    
    # Extract Header:
    if ((fr.x <- frequency(x)) != 1) {
        cat("Start =", deparse(start(x)), 
            "\nEnd =", deparse(end(x)), "\nFrequency =", 
            deparse(fr.x), "\n")
    } else {
        cat("Start =", format(tsp(x)[1]), 
            "\nEnd =", format(tsp(x)[2]), "\nFrequency =", 
            deparse(fr.x), "\n")
    }
    
    # Add Data:
    L = 6
    print(x[1:L])
    N = length(x)
    if (N > L) {
        cat("...\n")
        cat(paste("[", N-L+1, "]", sep = ""), x[(N-L+1):N], "\n") 
    }  
}


# ------------------------------------------------------------------------------


.showDataCmd =
function (dataframe, colname.bgcolor = "black", rowname.bgcolor = "grey90", 
body.bgcolor = "white", colname.textcolor = "white", 
rowname.textcolor = "darkred", body.textcolor = "black", 
font = "Courier 9", maxheight = 30, maxwidth = 80, title = NULL, 
rowname.bar = "left", colname.bar = "top", rownumbers = FALSE, 
placement = "-20-40") 
{   # A function copied from contributed R package relim

    # FUNCTION:
    
    object.name <- deparse(substitute(dataframe))
    if (!is.data.frame(dataframe)) 
        stop(paste(object.name, "is not a data frame"))
    if (is.numeric(rownumbers) && length(rownumbers) != nrow(dataframe)) 
        stop("rownumbers argument must be TRUE, FALSE or have length nrow(dataframe)")
    require(tcltk) || stop("Tcl/Tk support is absent")
    oldwidth <- options()$width
    options(width = 10000)
    conn <- textConnection("zz", "w")
    sink(conn)
    print(dataframe)
    sink()
    close(conn)
    options(width = oldwidth)
    base <- tktoplevel()
    tkwm.geometry(base, placement)
    tkwm.title(base, {
        if (is.null(title)) 
            object.name
        else title
    })
    nrows <- length(zz) - 1
    if (is.numeric(rownumbers)) 
        rowname.text <- paste(rownumbers, row.names(dataframe))
    else if (rownumbers) 
        rowname.text <- paste(1:nrows, row.names(dataframe))
    else rowname.text <- row.names(dataframe)
    namewidth = max(nchar(rowname.text))
    yy <- substring(zz, 2 + max(nchar(row.names(dataframe))))
    rm(zz, envir = .GlobalEnv)
    datawidth <- max(nchar(yy))
    winwidth <- min(1 + datawidth, maxwidth)
    hdr <- tktext(base, bg = colname.bgcolor, fg = colname.textcolor, 
        font = font, height = 1, width = winwidth)
    ftr <- tktext(base, bg = colname.bgcolor, fg = colname.textcolor, 
        font = font, height = 1, width = winwidth)
    textheight <- min(maxheight, nrows)
    txt <- tktext(base, bg = body.bgcolor, fg = body.textcolor, 
        font = font, height = textheight, width = winwidth, setgrid = 1)
    lnames <- tktext(base, bg = rowname.bgcolor, fg = rowname.textcolor, 
        font = font, height = textheight, width = namewidth)
    rnames <- tktext(base, bg = rowname.bgcolor, fg = rowname.textcolor, 
        font = font, height = textheight, width = namewidth)
    xscroll <- tkscrollbar(base, orient = "horizontal", repeatinterval = 1, 
        command = function(...) {
            tkxview(txt, ...)
            tkxview(hdr, ...)
            tkxview(ftr, ...)
        })
    string.to.vector <- function(string.of.indices) {
        string.of.indices <- tclvalue(string.of.indices)
        as.numeric(strsplit(string.of.indices, split = " ")[[1]])
    }
    tkconfigure(txt, xscrollcommand = function(...) {
        tkset(xscroll, ...)
        xy <- string.to.vector(tkget(xscroll))
        tkxview.moveto(hdr, xy[1])
        tkxview.moveto(ftr, xy[1])
    })
    tkconfigure(hdr, xscrollcommand = function(...) {
        tkset(xscroll, ...)
        xy <- string.to.vector(tkget(xscroll))
        tkxview.moveto(txt, xy[1])
        tkxview.moveto(ftr, xy[1])
    })
    tkconfigure(ftr, xscrollcommand = function(...) {
        tkset(xscroll, ...)
        xy <- string.to.vector(tkget(xscroll))
        tkxview.moveto(hdr, xy[1])
        tkxview.moveto(txt, xy[1])
    })
    yscroll <- tkscrollbar(base, orient = "vertical", repeatinterval = 1, 
        command = function(...) {
            tkyview(txt, ...)
            tkyview(lnames, ...)
            tkyview(rnames, ...)
        })
    tkconfigure(txt, yscrollcommand = function(...) {
        tkset(yscroll, ...)
        xy <- string.to.vector(tkget(yscroll))
        tkyview.moveto(lnames, xy[1])
        tkyview.moveto(rnames, xy[1])
    })
    tkconfigure(lnames, yscrollcommand = function(...) {
        tkset(yscroll, ...)
        xy <- string.to.vector(tkget(yscroll))
        tkyview.moveto(txt, xy[1])
        tkyview.moveto(rnames, xy[1])
    })
    tkconfigure(rnames, yscrollcommand = function(...) {
        tkset(yscroll, ...)
        xy <- string.to.vector(tkget(yscroll))
        tkyview.moveto(txt, xy[1])
        tkyview.moveto(lnames, xy[1])
    })
    tkbind(txt, "<B2-Motion>", function(x, y) {
        tkscan.dragto(txt, x, y)
    })
    tktag.configure(hdr, "notwrapped", wrap = "none")
    tktag.configure(ftr, "notwrapped", wrap = "none")
    tktag.configure(txt, "notwrapped", wrap = "none")
    tktag.configure(lnames, "notwrapped", wrap = "none")
    tktag.configure(rnames, "notwrapped", wrap = "none")
    tkinsert(txt, "end", paste(paste(yy[-1], collapse = "\n"), 
        sep = ""), "notwrapped")
    tkgrid(txt, row = 1, column = 1, sticky = "nsew")
    if ("top" %in% colname.bar) {
        tkinsert(hdr, "end", paste(yy[1], sep = ""), "notwrapped")
        tkgrid(hdr, row = 0, column = 1, sticky = "ew")
    }
    if ("bottom" %in% colname.bar) {
        tkinsert(ftr, "end", paste(yy[1], sep = ""), "notwrapped")
        tkgrid(ftr, row = 2, column = 1, sticky = "ew")
    }
    if ("left" %in% rowname.bar) {
        tkinsert(lnames, "end", paste(rowname.text, collapse = "\n"), 
            "notwrapped")
        tkgrid(lnames, row = 1, column = 0, sticky = "ns")
    }
    if ("right" %in% rowname.bar) {
        tkinsert(rnames, "end", paste(rowname.text, collapse = "\n"), 
            "notwrapped")
        tkgrid(rnames, row = 1, column = 2, sticky = "ns")
    }
    tkconfigure(hdr, state = "disabled")
    tkconfigure(ftr, state = "disabled")
    tkconfigure(txt, state = "disabled")
    tkconfigure(lnames, state = "disabled")
    tkconfigure(rnames, state = "disabled")
    if (maxheight < nrows) {
        tkgrid(yscroll, row = 1, column = 3, sticky = "ns")
    }
    if (maxwidth < datawidth) {
        tkgrid(xscroll, row = 3, column = 1, sticky = "ew")
    }
    tkgrid.rowconfigure(base, 1, weight = 1)
    tkgrid.columnconfigure(base, 1, weight = 1)
    tkwm.maxsize(base, 1 + datawidth, nrows)
    tkwm.minsize(base, 1 + nchar(names(dataframe)[1]), 1)
    invisible(NULL)
}


################################################################################

