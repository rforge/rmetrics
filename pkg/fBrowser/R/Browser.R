
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
################################################################################

   
fBrowser = .gui =
function(menuToolbar =c("File", "fBasics", "fCalendar", "fSeries",
"fMultivar", "fExtremes", "fOptions", "Help"), guiTitle = "Rmetrics" )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Rmetrics GUI
    
    # FUNCTION:
    
    fontSize = 9
    fontFamily = "Courier New"
    
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
    fontText <<- tkfont.create(family = fontFamily, size = fontSize)
  
    txt <<- tktext(textFrame, bg = "white", fg = "blue", font = fontText,
          width = 80, height = 25, wrap = "none")
    yscr <<- tkscrollbar(textFrame, repeatinterval = 5,
          command = function(...) tkyview(txt, ...))
    xscr <<- tkscrollbar(textFrame, repeatinterval = 5, orient = "horizontal",
          command = function(...) tkxview(txt, ...))
    tkconfigure(txt, yscrollcommand = function(...) tkset(yscr, ...))
    tkconfigure(txt, xscrollcommand = function(...) tkset(xscr, ...))
   
    tkgrid(txt, yscr)
    tkgrid(xscr)
    tkgrid.configure(yscr, sticky = "ns")
    tkgrid.configure(xscr, sticky = "ew")

    tkinsert(txt, "end", "Rmetrics, (C) 1999-2006, Diethelm Wuertz, GPL\n")
    tkinsert(txt, "end", "Version 2.2.1\n\n")  
    tkfocus(txt)

    
    # Command Console:
    commandFrame = tkframe(base, relief = "groove", borderwidth = 2) 
    commandFont <<- tkfont.create(family = fontFamily, size = fontSize)
    Command <- tclVar("z <- date()")
    entryCommand <- tkentry(commandFrame, width = "82", 
        textvariable = Command, fg = "red", font = commandFont)
    tkgrid(entryCommand)
    CommandOnOK <- function() {
        commandVal = tclvalue(Command)
        ans = eval(parse(text = commandVal), parent.frame())
        tkinsert(txt, "end", paste(">", commandVal, "\n"))
        tktag.add(txt, "currentLine", "end - 2 lines linestart", "end - 2 lines lineend")
        tktag.configure(txt, "currentLine", foreground = "red")
        if (length(grep("<-", commandVal)) == 0) {
            tkOutput(capture.output(ans)) 
            tkyview.moveto(txt, 1)
        }
        invisible() 
    }
    tkbind(entryCommand, "<Return>", CommandOnOK)
    
        
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
    
    # Active Test Object:
    testFrame <<- tkframe(base, relief = "groove", borderwidth = 2)
    testLabelText <<- tclVar(paste("Active Test: Tested Object = NULL"))
    testLabel <<- tklabel(testFrame, text = tclvalue(testLabelText))
    tkconfigure(testLabel, textvariable = testLabelText)
    tkgrid(testLabel)  
    
    # Active Fit Object:
    fitFrame <<- tkframe(base, relief = "groove", borderwidth = 2)
    fitLabelText <<- tclVar(paste("Active Fit: Fitted Object = NULL"))
    fitLabel <<- tklabel(fitFrame, text = tclvalue(fitLabelText))
    tkconfigure(fitLabel, textvariable = fitLabelText)
    tkgrid(fitLabel)  
    
    # Active Predict Object:
    predictFrame <<- tkframe(base, relief = "groove", borderwidth = 2)
    predictLabelText <<- tclVar(paste("Active Prediction: Predicted Object = NULL"))
    predictLabel <<- tklabel(predictFrame, text = tclvalue(predictLabelText))
    tkconfigure(predictLabel, textvariable = predictLabelText)
    tkgrid(predictLabel)
       
    # Compose: 
    tkpack(textFrame, fill = "both", expand = TRUE)  
    tkpack(commandFrame)
    tkpack(infoFrame, objectFrame, testFrame, fitFrame, predictFrame, 
        fill = "x")    
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
# FUNCTION:                 DESCRIPTION:
#  tkExecute
#  tkSaveAs 
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
        # Settings:  
        info = infoName
        argNames = names(params)
        Character = NULL
        Numeric = NULL
        Logical = NULL
        Null = NULL
        # Parameters:
        tt <- tktoplevel()
        tkgrid(tklabel(tt, text = infoName))
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
                tkOutput(capture.output(object)) 
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
    invisible()
        
}


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


tkSummary =
function(object, title = NULL)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Summary:
    if (!is.null(title)) tkTitle(title)
    tkOutput(capture.output(summary(object))) 
}


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



tkOutput = .tkOutput = 
function(output)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Output Function - Object:
    for (i in 1:length(output)) { 
        tkinsert(txt, "end", paste(output[i], "\n"))   
    }        
}


tkDescription = .tkDescription = 
function(description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Output Function - Description:
    if (is.null(description)) 
        description = as.character(date())
    tkinsert(txt, "end", "\nDescription:\n ")
    tkinsert(txt, "end", description)
    tkinsert(txt, "end", "\n\n")
}



tkReport = .tkReport =
function(title)
{   # A function implemented by Diethelm Wuertz

    tkTitle(title)
    .tkOutput(capture.output(object))
    .tkDescription()  
}
      

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


tkGetData = 
function(Data, infoName, report = TRUE )
{
    
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


tkNotYetImplemented = .NotYetImplementedCmd = 
function() 
{   # A function implemented by Diethelm Wuertz

    invisible()
}

