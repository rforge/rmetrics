
################################################################################
# FUNCTION:
#  .header.ts
#  .showDataCmd
################################################################################


.head.ts = .header.ts =  
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
{
    # A function copied from contributed R package relim
    
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



# ------------------------------------------------------------------------------
    

.xMenu =
function(fun, params, infoName, tkoutput = FALSE, console = NULL,
title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
     
    argNames = names(params)
    Character = NULL
    Numeric = NULL
    Logical = NULL
    Null = NULL
    
    tt <- tktoplevel()
    for (i in 1:length(params) ) {
        Numeric = c( Numeric, is.numeric(params[[i]]) )
        Character = c( Character, is.character(params[[i]]) )
        Logical = c( Logical, is.logical(params[[i]]) )
        Null = c( Null, is.null(params[[i]]) )
        assign( argNames[i], tclVar(as.character(params[[i]])) )
        entry.Name <- tkentry(tt, width = "20", 
            textvariable = get(argNames[i]) )
        label.Name <- tklabel(tt, text = argNames[i])
        tkgrid(entry.Name, label.Name)
    }
    
    # Internal Function:
    OnOK <-
    function(...) {
        z = list()
        .n = length(argNames)
        z[[.n+1]] = NA
        z[[.n+1]] = NULL
        names(z) = argNames

        for (i in 1:.n ) {
            if (Character[i]) 
                z[[i]] = as.character(tclvalue(get(argNames[i])))
            if (Numeric[i]) 
                z[[i]] = as.numeric(tclvalue(get(argNames[i])))
            if (Logical[i]) 
                z[[i]] = as.logical(tclvalue(get(argNames[i]))) 
        }
        
        FUN = match.fun(fun)
        f = FUN
        formals(f) = z
        x <<- f()
        
        # Info:
        activeDataSet <<- paste("x =", infoName)             
        seriesTitle <<- infoName
        infoLabelText <<- tclVar(paste("Active Data Set:", activeDataSet))
        tkconfigure(infoLabel, textvariable = infoLabelText)
        tkgrid(infoLabel) 
        
        # Output:
        if (tkoutput) {
            if (!is.null(title)) tkTitle(title)
            .tkOutput(capture.output(object)) 
            if (!is.null(description)) .tkDescription(description)
        }
        if (!is.null(console)) {
            eval(parse(text = console))
            cat("...\n")
        }
    }
    
    okButton <- tkbutton(tt, text = "   Ok   ", 
        command = OnOK)
    
    quitButton <- tkbutton(tt, text = "   Quit   ", 
        command = function() tkdestroy(tt) )
    
    tkbind(entry.Name, "<Return>", OnOK)
    tkgrid(okButton, quitButton, sticky = "sew")
    tkfocus(tt)
}


################################################################################


.objectDataSet =
function(object)
{   # A function implemented by Diethelm Wuertz

    x <<- tkSaveAs(
        data = as.timeSeries(object), 
        infoName = "Object Data Slot",
        console = "print(head(data))",
        what = "x" )
    invisible()
}


# ------------------------------------------------------------------------------


.sp500DataSet =
function()
{   # A function implemented by Diethelm Wuertz

    data(singleIndex.dat)
    X = as.timeSeries(singleIndex.dat, format = "%d-%b-%Y")[, 2]
    x <<- tkSaveAs(
        data = X, 
        infoName = "SP500 Index",
        console = "print(head(data))" )
    plotTitle <<- "SP500 Index"
    invisible()
}


# ------------------------------------------------------------------------------


.sp500retDataSet =
function()
{   # A function implemented by Diethelm Wuertz

    data(singleIndex.dat)
    X = as.timeSeries(singleIndex.dat, format = "%d-%b-%Y")[, 2]
    x <<- tkSaveAs(
        data = returnSeries(X), 
        infoName = "SP500 Returns",
        console = "print(head(data))",
        what = "x" )
    plotTitle <<- "SP500 Returns"
    invisible()
}


# ------------------------------------------------------------------------------


.msftsp500DataSet =
function()
{   # A function implemented by Diethelm Wuertz

    X = as.timeSeries(singleIndex.dat, format = "%d-%b-%Y")
    x <<- tkSaveAs(
        data = X,
        infoName = "MSFT | SP500 Values",
        console = "print(head(data))",
        what = "x" )
    plotTitle <<- "MSFT | SP500 Values"
    invisible()
}


# ------------------------------------------------------------------------------


.msftsp500retDataSet =
function()
{   # A function implemented by Diethelm Wuertz

    X = as.timeSeries(singleIndex.dat, format = "%d-%b-%Y")
    x <<- tkSaveAs(
        data = returnSeries(X), 
        infoName = "MSFT | SP500 Returns",
        console = "print(head(data))",
        what = "x" )
    plotTitle <<- "MSFT | SP500 Returns"
    invisible()
}


# ------------------------------------------------------------------------------


.nyseDataSet = 
function(report = TRUE)
{   # A function implemented by Diethelm Wuertz

    data(nyse)
    X = as.timeSeries(nyse)
    X = returnSeries(X)
    x = X[X@Data < 1] # Correct for OLD/NEW Index
    attr(x, "data") <- "NYSE Returns"
    x <<- tkSaveAs(
        data = x, 
        infoName = "NYSE Returns",
        console = "print(head(data))",
        what = "x" )
    plotTitle <<- "NYSE Returns"
    if (report) {
        tkTitle(plotTitle)
        tkOutput(capture.output(head(x)))
        tkOutput("...")
        tkOutput(capture.output(tail(x))) 
    }
    invisible()
}


.dem2gbpDataSet = 
function(report = TRUE)
{   # A function implemented by Diethelm Wuertz

    data(dem2gbp)
    x = as.ts(as.vector(dem2gbp[, 1]))
    attr(x, "data") <- "DEMGBP Returns"
    x <<- tkSaveAs(
        data = x, 
        infoName = "DEMGBP Returns",
        console = "print(.head.ts(data))",
        what = "x" )
    plotTitle <<- "DEMGBP Returns"
    if (report) {
        tkTitle(plotTitle)
        tkOutput(capture.output(.head.ts(x)))
    }
    invisible()
}


.bmwDataSet = 
function()
{   # A function implemented by Diethelm Wuertz

    data(bmw)
    x = timeSeries(
        data = matrix(bmw[, 2], ncol = 1),
        charvec = as.character(bmw[, 1]),
        units = "BMW",
        format = "%Y-%m-%d", 
        zone = "GMT",
        FinCenter = "GMT")
    x <<- tkSaveAs(
        data = x, 
        infoName = "BMW Returns",
        console = "print(head(data))",
        what = "x" )
    plotTitle <<- "BMW Returns"
    invisible()
}


.bmwmaxDataSet = 
function()
{   # A function implemented by Diethelm Wuertz

    data(bmw)
    x = timeSeries(
        data = matrix(bmw[, 2], ncol = 1),
        charvec = as.character(bmw[, 1]),
        units = "BMW",
        format = "%Y-%m-%d", 
        zone = "GMT",
        FinCenter = "GMT")
    blocks = blocks(-x, block = "month")
    x = timeSeries(
        data = blocks[, 1],
        charvec = rownames(blocks),
        units = "BMW",
        format = "%Y-%m-%d", 
        zone = "GMT",
        FinCenter = "GMT")
    x <<- tkSaveAs(
        data = x, 
        infoName = "BMW Block Maxima Returns",
        console = "print(head(data))",
        what = "x" )
    plotTitle <<- "BMW Returns"
    invisible()
} 


.danishDataSet = 
function()
{   # A function implemented by Diethelm Wuertz

    data(danish)
    x = timeSeries(
        data = matrix(danish[, 2], ncol = 1),
        charvec = as.character(danish[, 1]),
        units = "DANISH",
        format = "%Y-%m-%d", 
        zone = "GMT",
        FinCenter = "GMT")
    x <<- tkSaveAs(
        data = x, 
        infoName = "Danish Fire Losses",
        console = "print(head(data))",
        what = "x" )
    plotTitle <<- "Danish Fire Losses"
    invisible()
}



# ------------------------------------------------------------------------------
################################################################################


.packagesMenu.loadPackagesCmd = 
function() {
    local({pkg <- select.list(sort(.packages(all.available = TRUE)))
        if(nchar(pkg)) library(pkg, character.only = TRUE)})
}
        
        
################################################################################



# ------------------------------------------------------------------------------
    
