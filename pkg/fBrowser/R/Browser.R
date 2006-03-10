
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
# fBrowser / .gui
#  Menues:
#   File
#   fBasics
#   fCalendar
#   fSeries
#   fMultivar
#   fExtremes
#   fOptions
#   Help
# .NotYetImplementedCmd
################################################################################

   
fBrowser = .gui =
function(menuToolbar =c("File", "fBasics", "fCalendar", "fSeries",
"fMultivar", "fExtremes", "fOptions", "Help"), guiTitle = "Rmetrics" )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Rmetrics GUI
    
    # FUNCTION:
    
    menuToolbar <<- menuToolbar
    
    # Active Objects:
    x <<- rnorm(1000)
    object <<- NULL
    fittedObject <<- NULL
    testedObject <<- NULL
    predictedObject <<- NULL
    
    # Function Settings:
    .fun <<- NULL
    .FUN <<- NULL
    
    # Global Variables:
    year <<- currentYear
    
    xTitle <<- "rnorm(1000)"
    activeDataSet <<- "x = rnorm(1000)"
    plotTitle <<- NULL
    seriesTitle <<- activeDataSet
    saveAs <<- NULL
    objectTitle <<- "NULL"
    activeObject <<- "object = NULL"
    saveTitle <<- "?" 
    saveTitle <<- ""
       
    .infoObject <<- function(data, infoName, console = NULL) {
        objectTitle <<- paste("object =", infoName)
        objectLabelText <<- tclVar(paste("Active Object:", objectTitle))
        tkconfigure(objectLabel, textvariable = objectLabelText)
        tkgrid(objectLabel)
        if (!is.null(console)) {
            eval(parse(text = console))
            cat("...\n")
        }
        data
    } 
     
        
    # Button Commands:
    fileName <<- NULL
    
    # Frames:
    base <<- tktoplevel()
    tkwm.title(base, guiTitle) 
    
    # Popup Frame:  
    popupFrame <<- tkmenu(base)  
    tkconfigure(base, menu = popupFrame)

    # Rmetrics Popup Menu:
    for (i in 1:length(menuToolbar)) {
        .fun.PopupMenu = match.fun(paste(".", menuToolbar[i], 
            ".PopupMenu", sep = ""))
        .fun.PopupMenu()
    }
        
    # Put things together - Focus ...
    tkfocus(popupFrame) 
    
    
    # Text Frame:    
    textFrame <<- tkframe(base, relief = "groove", borderwidth = 2)
    fontText <<- tkfont.create(family = "courier", size = 9)
    scr <<- tkscrollbar(textFrame, repeatinterval = 5, 
        command = function(...) tkyview(txt, ...))
    txt <<- tktext(textFrame, bg = "white", font = fontText, 
        yscrollcommand = function(...) tkset(scr,...))
    tkgrid(txt, scr)
    tkgrid.configure(scr, sticky = "ns")
    tkinsert(txt, "end", "Rmetrics, (C) 1999-2006, Diethelm Wuertz, GPL\n")
    tkinsert(txt, "end", "Version 2.2.1\n\n")  
    tkfocus(txt)
    
    # Active DataSet Info Line:
    infoFrame <<- tkframe(base, relief = "groove", borderwidth = 2)
    infoLabelText <<- tclVar(paste("Active Series Data: x = rnorm(1000)"))
    infoLabel <<- tklabel(infoFrame, text = tclvalue(infoLabelText))
    tkconfigure(infoLabel, textvariable = infoLabelText)
    tkgrid(infoLabel)
    
    # Active Object Info Line:
    objectFrame <<- tkframe(base, relief = "groove", borderwidth = 2)
    objectLabelText <<- tclVar(paste("Active Object: object = NULL"))
    objectLabel <<- tklabel(objectFrame, text = tclvalue(objectLabelText))
    tkconfigure(objectLabel, textvariable = objectLabelText)
    tkgrid(objectLabel)  
    
    # Test:
    testFrame <<- tkframe(base, relief = "groove", borderwidth = 2)
    testLabelText <<- tclVar(paste("Active Test: Tested Object = NULL"))
    testLabel <<- tklabel(testFrame, text = tclvalue(testLabelText))
    tkconfigure(testLabel, textvariable = testLabelText)
    tkgrid(testLabel)  
    
    # Fit:
    fitFrame <<- tkframe(base, relief = "groove", borderwidth = 2)
    fitLabelText <<- tclVar(paste("Active Fit: Fitted Object = NULL"))
    fitLabel <<- tklabel(fitFrame, text = tclvalue(fitLabelText))
    tkconfigure(fitLabel, textvariable = fitLabelText)
    tkgrid(fitLabel)  
    
    # Predict:
    predictFrame <<- tkframe(base, relief = "groove", borderwidth = 2)
    predictLabelText <<- tclVar(paste("Active Prediction: Predicted Object = NULL"))
    predictLabel <<- tklabel(predictFrame, text = tclvalue(predictLabelText))
    tkconfigure(predictLabel, textvariable = predictLabelText)
    tkgrid(predictLabel)
    
       
    # Compose: 
    tkpack(textFrame) 
    tkpack(infoFrame, objectFrame, 
        testFrame, fitFrame, predictFrame, fill = "x")    
}


# ------------------------------------------------------------------------------
    

tkNotYetImplemented = .NotYetImplementedCmd = 
function() 
{   # A function implemented by Diethelm Wuertz

    invisible()
}


################################################################################
# FUNCTION:
#  newToolbarMenu
#  addToolbarMenu
#  cascadeToolbarMenu
################################################################################


newToolbarMenu = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Create New Toolbar Menu:
    ans = tkmenu(popupFrame, tearoff = FALSE) 
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


addToolbarMenu =  
function(popupMenu, Label, subLabel, Command) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Add Toolbar Menu:
    if (is.null(subLabel)) {
        ans = tkadd(popupMenu, "command", label = Label, 
            command = match.fun(Command))
    } else { 
        Menu = tkmenu(popupMenu, tearoff = FALSE)
        for (i in 1:length(subLabel)) {
            tkadd(Menu, "command", label = subLabel[i], 
                command = match.fun(Command[i]))
        }
        ans = tkadd(popupMenu, "cascade", label = Label, menu = Menu)
    }
    
    # Return Value:
    ans
}
 

# ------------------------------------------------------------------------------

   
cascadeToolbarMenu = 
function (Menu, Label) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Cascade Toolbar Menu:
    ans = tkadd(popupFrame, "cascade", label = Label, menu = Menu) 
    
    # Return Value:
    ans 
}
       

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
# FUNCTION:                 DESCRIPTION:
#  tkExecute
#  .saveAs
#  .xMenu
#  .objectMenu
################################################################################


tkExecute =
function(fun = ".fun", params = NULL, infoName = "- missing -", 
tkoutput = TRUE, console = "print(object)", title = NULL, 
description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    if (is.null(params)) {
        FUN = match.fun(fun)
        object <<- FUN(x, ...)
        objectTitle <<- paste("object =", infoName)
        objectLabelText <<- tclVar(paste("Active Object:", objectTitle))
        tkconfigure(objectLabel, textvariable = objectLabelText)
        tkgrid(objectLabel)
        if (!is.null(console)) {
            eval(parse(text = console))
            cat("...\n")
        }  
        if (tkoutput) {
            ans = (capture.output(object))
            if (!is.null(title)) tkTitle(title)
            .tkOutput(ans)     
        }
    } else {
        .objectMenu(fun = fun, params = params, info = infoName, 
            tkoutput = tkoutput, console = console, title = title, 
            description = description)     
    }
    
    invisible()
        
}


# ------------------------------------------------------------------------------


tkSaveAs = .saveAs = 
function(data, infoName, console = NULL, 
what = c("x", "object", "tested", "fitted", "predicted"), tkoutput = FALSE)
{   # A function implemented by Diethelm Wuertz

    what = what[1]
    toPaste = c(
        x = "Active x:  ", 
        object = "Active object:  ", 
        tested = "Active Tested Object:  ", 
        fitted = "Active Fitted Object:  ", 
        predicted = "Active Predicted Object:  ")[what]
    
    # Assign:
    saveTitle <<- paste(toPaste, saveTitle)           
    
    # Active DataSet Info Line:
    if (what == "x") {
        # x <<- data
        infoLabelText <<- tclVar(saveTitle)
        tkconfigure(infoLabel, textvariable = infoLabelText)
        tkgrid(infoLabel)
    }
    
    # Active Object Info Line:
    if (what == "object") {
        # object <<- data
        objectLabelText <<- tclVar(saveTitle)
        tkconfigure(objectLabel, textvariable = objectLabelText)
        tkgrid(objectLabel)  
    }
        
    # Test:
    if (what == "tested") {
        tested <<- data
        testLabelText <<- tclVar(saveTitle)
        tkconfigure(htestLabel, textvariable = testLabelText)
        tkgrid(testLabel)  
    }
        
    # Fit:
    if (what == "fitted") {  
        # fitted <<- data
        fitLabelText <<- tclVar(saveTitle)
        tkconfigure(fitLabel, textvariable = fitLabelText)
        tkgrid(fitLabel)  
    }
            
    # Predict:
    if (what == "predicted") {    
        # predicted <<- data
        ffitLabelText <<- tclVar(saveTitle)
        tkconfigure(fitLabel, textvariable = fitLabelText)
        tkgrid(fitLabel)  
    }
      
    # tkoutput:
    if (tkoutput) {
        tkTitle(infoName)
        tkOutput(capture.output(data))
    }
      
    # Console:
    if (!is.null(console)) {
        eval(parse(text = console))
        cat("...\n")
    }
    
    # Return Value:
    data
}
    

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


# ------------------------------------------------------------------------------


.objectMenu = 
function(fun = ".fun", params, infoName, tkoutput = FALSE,
console = NULL, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    argNames = names(params)
    Character = NULL
    Numeric = NULL
    Logical = NULL
    Null = NULL
    
    # Parameters:
    tt <- tktoplevel()
    tkgrid(tklabel(tt, text = infoName))
    
    # Parameters:
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
        z[[.n+1]] = NA       # Don't remove this line!
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
        object <<- f()
        
        # Info:
        activeObject <<- paste("x =", infoName)             
        objectTitle <<- paste("object = ", infoName)
        objectLabelText <<- tclVar(paste("Active Object:", objectTitle))
        tkconfigure(objectLabel, textvariable = objectLabelText)
        tkgrid(objectLabel) 
        
        # Save:
        if (length(z$object2x) == 1) {
            if (z$object2x) 
                # DEBUG: cat("\n\n object2x:", z$object2x, "\n")
                x <<- tkSaveAs(object, infoName, console = NULL, what = "x")
        }
        
        # Print Specification overwrites tkoutput:
        if (length(z$report) == 1)  {
            # DEBUG: cat("\n\n object2x:", z$report, "\n")
            tkoutput = z$report
        }
        
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

 
tkTitle = .tkTitle =
function(title)
{   # A function implemented by Diethelm Wuertz

    # Output Title Function:
    tkinsert(txt, "end", "\n\nTitle:\n ")
    tkinsert(txt, "end", title) 
    tkinsert(txt, "end", "\n\n")
}




tkPar = 
function(...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # par():
    par(...)
}


# ------------------------------------------------------------------------------


tkSummary =
function(object, title = NULL)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Summary:
    if (!is.null(title)) tkTitle(title)
    tkOutput(capture.output(summary(object))) 
}


# ------------------------------------------------------------------------------


tkParameters = .tkParameters = 
function(parameters)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Output Function for Parameters:
    parameters = cbind(parameters)              
    parameters = as.data.frame(parameters)
    colnames(parameters) = "Value:"
    output <- capture.output(parameters)    
    tkinsert(txt, "end", "Parameters:\n")
    for (i in 1:length(output)) 
        tkinsert(txt, "end", paste(output[i], "\n"))
}


# ------------------------------------------------------------------------------


tkOutput = .tkOutput = 
function(output)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Output Function - Object:
    for (i in 1:length(output)) 
        tkinsert(txt, "end", paste(output[i], "\n"))            
}


# ------------------------------------------------------------------------------


.tkDescription = 
function(description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Output Function - Description:
    if (is.null(description)) 
        description = as.character(date())
    tkinsert(txt, "end", "\nDescription:\n ")
    tkinsert(txt, "end", description)
    tkinsert(txt, "end", "\n\n")
}


################################################################################


################################################################################


.tkReport =
function(title)
{   # A function implemented by Diethelm Wuertz

    tkTitle(title)
    .tkOutput(capture.output(object))
    .tkDescription()  
}
      

# ------------------------------------------------------------------------------


tkGetClass = .getClass =
function(class)
{   # A function implemented by Diethelm Wuertz

    # Class:
    ans = (capture.output(getClass(class)))[-1]
    
    # Output:
    tkTitle(paste(class, "Class Representation"))
    .tkOutput(ans)   
}



tkGetTime =  
function()
{   # A function implemented by Diethelm Wuertz

    # Time:
    ans = (capture.output(Sys.timeDate()))
    
    # Output:
    tkTitle("Current Date and Time")
    tkOutput(ans) 
}


tkGetFinCenters =
function()
{   # A function implemented by Diethelm Wuertz

    # Time:
    ans = (capture.output(listFinCenter()))
    
    # Output:
    tkTitle("List of Financial Centers")
    .tkOutput(ans) 
}



tkFit = .mleFit = 
function(fun, infoName = "Distribution", ...)
{
    # MLE Fit:
    fit = match.fun(fun)
    object <<- fit(as.vector(x), ...)
    objectTitle <<- paste("object =", infoName)
    objectLabelText <<- tclVar(paste("Active Object:", objectTitle))
    tkconfigure(objectLabel, textvariable = objectLabelText)
    tkgrid(objectLabel) 
    ans = (capture.output(object))
    tkTitle(infoName)
    .tkOutput(ans)     
}





# ------------------------------------------------------------------------------


.normFit = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    X = as.vector(x)
    list(estimate = c(mean = mean(X), sd = sd(X))) 
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


tkGetData = 
function(Data, infoName, report = TRUE )
{
    # tkExampleData(Data = "nyse", asTimeSeries = TRUE, asReturnSeries = TRUE)
    # tkExampleData(Data = "nyse", asTimeSeries = TRUE, asReturnSeries = TRUE, asVector = TRUE)
    # tkExampleData(Data = "nyse", asTimeSeries = TRUE, asReturnSeries = TRUE, asTS = TRUE)
    
    command = paste("data(", Data, ")", sep = "")
    eval(parse(text = command))
    x <<- eval(parse(text = Data))
    consoleCmd = "print(head(x)); print(tail(x))"
    
    if (dim(x)[2] != 1) x <<- as.timeSeries(x)
    
    x <<- tkSaveAs(
        data = x, 
        infoName = infoName,
        console = consoleCmd,
        what = "x",
        tkoutput = FALSE )
    attr(x, "data") = Data
        
    plotTitle <<- infoName
    if (report) {
        tkTitle(plotTitle)
        tkOutput(capture.output(head(x)))
        tkOutput("...")
        tkOutput(capture.output(tail(x))) 
    }
    
    invisible()
}

tkGetDataFrame = 
function(Data, infoName, report = TRUE )
{
    
    command = paste("data(", Data, ")", sep = "")
    eval(parse(text = command))
    x <<- eval(parse(text = Data))
    consoleCmd = "print(head(x)); print(tail(x))"
    attr(x, "data") <<- Data
    
    x <<- tkSaveAs(
        data = x, 
        infoName = infoName,
        console = consoleCmd,
        what = "x" )
        
    plotTitle <<- infoName
    if (report) {
        tkTitle(plotTitle)
        tkOutput(capture.output(head(x)))
        tkOutput("...")
        tkOutput(capture.output(tail(x))) 
    }
    
    invisible()
}


tkDataExample = 
function(Data = "nyse", asTimeSeries = FALSE, asReturnSeries = FALSE, 
asVector = FALSE, asTS = FALSE, infoName = Data, plotTitle = Data, report = FALSE )
{
    # tkExampleData(Data = "nyse", asTimeSeries = TRUE, asReturnSeries = TRUE)
    # tkExampleData(Data = "nyse", asTimeSeries = TRUE, asReturnSeries = TRUE, asVector = TRUE)
    # tkExampleData(Data = "nyse", asTimeSeries = TRUE, asReturnSeries = TRUE, asTS = TRUE)
    
    command = paste("data(", Data, ")", sep = "")
    eval(parse(text = command))
    x <<- eval(parse(text = Data))
    consoleCmd = "print(head(x))"
    
    if (asTimeSeries) {
        x <<- as.timeSeries(x)
    }
        
    if (asReturnSeries) {
        x <<- returnSeries(x, digits = 12)
        if (Data == "nyse") 
            x <<- x[x@Data < 1] # Correct for OLD/NEW NYSE Index
    }
    
    if (asVector) {
        x <<- as.vector(x)
    }
    
    if (asTS) {
        x <<- as.ts(as.vector(x))
        consoleCmd = "print(.head.ts(x))"
    }
    
    # Add Attribute
    attr(x, "data") <<- Data
    
    x <<- tkSaveAs(
        data = x, 
        infoName = infoName,
        console = consoleCmd,
        what = "x" )
        
    plotTitle <<- Data
    
    if (report) {
        tkTitle(plotTitle)
        tkOutput(capture.output(head(x)))
        tkOutput("...")
        tkOutput(capture.output(tail(x))) 
    }
    
    invisible()
}


tkDataSlot =
function(object)
{   # A function implemented by Diethelm Wuertz

    # Extract Data Slot from object:
    x <<- object@Data
    x <<- tkSaveAs(
        data = x, 
        infoName = "Object Data Slot",
        console = "print(head(data))",
        what = "x" )
    invisible()
}



tkData = .dataSet =
function(which, report = TRUE)
{   # A function implemented by Diethelm Wuertz

    if (which == "object@data")
        return(.objectDataSet(object@data))
        
    if (which == "sp500r")
        return(.sp500DataSet())
        
    if (which == "msftsp500")
        return(.msftsp500DataSet())
        
    if (which == "sp500ret")
        return(.sp500retDataSet())
        
    if (which == "msftsp500ret")
        return(.msftsp500retDataSet())
        
    if (which == "nyse")
        return(.nyseDataSet(report = report))
    
    if (which == "dem2gbp")
        return(.dem2gbpDataSet(report = report))
        
    if (which == "bmw")
        return(.bmwDataSet())
        
    if (which == "bmwmax")
        return(.bmwmaxDataSet())
        
    if (which == "danish")
        return(.danishDataSet())
}


################################################################################


.packagesMenu.loadPackagesCmd = 
function() {
    local({pkg <- select.list(sort(.packages(all.available = TRUE)))
        if(nchar(pkg)) library(pkg, character.only = TRUE)})
}
        
        
################################################################################



