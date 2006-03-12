
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
# fBrowser / .gui

   
fBrowser = .gui =
function(menuToolbar =c("File", "fBasics", "fCalendar", "fSeries",
"fMultivar", "fExtremes", "fOptions", "Help"), 
fontSize = 9, fontFamily = "Courier New",

guiTitle = "Rmetrics" )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Rmetrics GUI
    
    # FUNCTION:
    
    # Active Objects:
    x <<- as.ts(rnorm(1000))
    xTitle <<- plotTitle <<- "rnorm(1000)"
    object <<- NULL
    objectTitle <<- "NULL"
    
    # Frames:
    base <<- tktoplevel()
    tkwm.title(base, guiTitle) 
    popupFrame <<- tkmenu(base)  
    tkconfigure(base, menu = popupFrame)
    for (i in 1:length(menuToolbar)) {
        cmd = paste(".", menuToolbar[i], ".PopupMenu", sep = "")
        fun.PopupMenu = match.fun(cmd)
        fun.PopupMenu()
    }
    
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
    tkinsert(txt, "end", "\nRmetrics (C) 1999-2006, Diethelm Wuertz, GPL\n")
    tkinsert(txt, "end", "Version 2.2.1\n\n")
    tkinsert(txt, "end", "Rmetrics is free software and comes with ABSOLUTELY NO WARRANTY.\n\n")
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
        tktag.add(txt, "currentLine", "end - 2 lines linestart", 
            "end - 2 lines lineend")
        tktag.configure(txt, "currentLine", foreground = "red")
        if (length(grep("<-", commandVal)) == 0) {
            tkOutput(capture.output(ans)) }
        invisible() }
    tkbind(entryCommand, "<Return>", CommandOnOK)  
    
    # Active Input/Value Data Frame:
    activeFrame <<- tkframe(base, relief = "groove", borderwidth = 2)
    infoLabelText <<- tclVar(paste("INPUT: ", xTitle))
    objectLabelText <<- tclVar(paste("VALUE: ", objectTitle))
    infoLabel <<- tkbutton(activeFrame, relief = "ridge",
        command = function() tkOutput(capture.output(x)),
        text = tclvalue(infoLabelText), fg = "blue")     
    classXLabel <<- tkbutton(activeFrame, relief = "ridge",
        command = function() x <<- tkGetClass(class(x)[1]),
        text = "Slots", fg = "blue")    
    objectLabel <<- tkbutton(activeFrame, relief = "ridge",
        command = function() tkOutput(capture.output(object)),
        text = tclvalue(objectLabelText), fg = "darkgreen")
    classLabel <<- tkbutton(activeFrame, relief = "ridge",
        command = function() x <<- tkGetClass(class(object)[1]),
        text = "Slots", fg = "darkgreen")
    summaryLabel <<- tkbutton(activeFrame, relief = "ridge",
        command = function() x <<- tkGetSummary(capture.output(object)),
        text = "Summary", fg = "darkgreen")
    copyLabel <<- tkbutton(activeFrame, relief = "ridge",
        command = function() x <<- tkObjectToX(object),
        text = "Object to x", fg = "darkgreen")
    tkgrid(activeFrame)
    tkgrid(infoLabel, classXLabel, 
        objectLabel, classLabel, summaryLabel, copyLabel)  
       
    # Compose: 
    tkpack(textFrame, fill = "both", expand = TRUE)  
    tkpack(commandFrame)
    tkpack(activeFrame, fill = "x")
}


################################################################################
#  newToolbarMenu
#  addToolbarMenu
#  cascadeToolbarMenu


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
    tkfocus(popupFrame)
    
    # Return Value:
    ans 
}
 

################################################################################               
# tkExecute
# tkSaveAsX 
# tkSaveAsObject
# tkObjectToX


tkExecute =
function(fun = ".fun", params = NULL, infoName = "- missing -", 
tkoutput = FALSE, console = NULL, title = NULL, 
description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Set Object Title"
    objectTitle <<- infoName
    
    if (is.null(params)) {
        FUN = match.fun(fun)
        object <<- FUN(x, ...)
        # What is object?
        what = paste(as.character(class(object)), ":", sep = "")
        objectLabelText <<- tclVar(paste(what, objectTitle))
        tkconfigure(objectLabel, textvariable = objectLabelText)
        tkgrid(objectLabel)  
        if (tkoutput) {
            ans = (capture.output(object))
            if (!is.null(title)) tkTitle(title)
            tkOutput(ans)     
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
        tkgrid(tklabel(tt, text = infoName, fg = "blue"))
        for (i in 1:length(params) ) {
            Numeric = c( Numeric, is.numeric(params[[i]]) )
            Character = c( Character, is.character(params[[i]]) )
            Logical = c( Logical, is.logical(params[[i]]) )
            Null = c( Null, is.null(params[[i]]) )
            assign( argNames[i], tclVar(as.character(params[[i]])) )
            entry.Name <- tkentry(tt, width = "25", fg = "red",
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
            # What is object?
            what = paste(as.character(class(object)), ":", sep = "")
            # Info:            
            objectLabelText <<- tclVar(paste(what, objectTitle))
            tkconfigure(objectLabel, textvariable = objectLabelText)
            tkgrid(objectLabel) 
            # Save:
            if (length(z$object2x) == 1) {
                if (z$object2x) 
                    # DEBUG: cat("\n\n object2x:", z$object2x, "\n")
                    x <<- tkSaveAsX(object, infoName)
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
                if (!is.null(description)) tkDescription(description)
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


tkSaveAsX =  
function(data, infoName)
{   # A function implemented by Diethelm Wuertz

    # What is X?
    what = paste(as.character(class(data)), ":", sep = "")
    
    # Save X:
    xTitle <<- infoName
    infoLabelText <<- tclVar(paste(what, infoName))
    tkconfigure(infoLabel, textvariable = infoLabelText, fg = "blue")
    tkgrid(infoLabel)
    
    # Return Value:
    data
}


tkSaveAsObject =  
function(data, infoName)
{   # A function implemented by Diethelm Wuertz

    # What is Object?
    what = paste(as.character(class(data)), ":", sep = "")
    
    # Save Object: 
    objectTitle <<- infoName
    infoLabelText <<- tclVar(paste(what, infoName))
    tkconfigure(infoLabel, textvariable = infoLabelText)
    tkgrid(infoLabel)
    
    # Return Value:
    data
}


tkObjectToX =  
function(object)
{   # A function implemented by Diethelm Wuertz
       
    # What is Object?
    what = paste(as.character(class(object)), ":", sep = "")
    
    # Active DataSet Info Line:
    xTitle <<- objectTitle
    infoLabelText <<- tclVar(paste(what, objectTitle))
    tkconfigure(infoLabel, textvariable = infoLabelText)
    tkgrid(infoLabel)
    
    # Return Value:
    object
}


################################################################################
# tkTitle
# tkOutput
# tkDescription


tkTitle = 
function(title, col = "blue")
{   # A function implemented by Diethelm Wuertz

    # Output Title Function:
    lines = c("\n", "\n", "Title", "\n ", title, "\n", "\n")
    for (line in lines) {
        tkinsert(txt, "end", line)
        tktag.add(txt, "currentLine", "end - 2 lines linestart", 
            "end - 2 lines lineend")
        tktag.configure(txt, "currentLine", foreground = col)
    }
    tkyview.moveto(txt, 1)
}


tkOutput =
function(output)
{   # A function implemented by Diethelm Wuertz
    
    # Output Function - Object:
    for (i in 1:length(output)) { 
        tkinsert(txt, "end", paste(output[i], "\n"))   
        tkyview.moveto(txt, 1)     
    }   
}


tkDescription = 
function(description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Output Function - Description:
    if (is.null(description)) 
        description = as.character(date())
    tkinsert(txt, "end", "\nDescription:\n ")
    tkinsert(txt, "end", description)
    tkinsert(txt, "end", "\n\n")
    tkyview.moveto(txt, 1)
}


################################################################################
# tkGet* Functions


tkGetSummary =
function(object, title = NULL)
{   # A function implemented by Diethelm Wuertz
    
    # Summary:
    if (!is.null(title)) tkTitle(title)
    tkOutput(capture.output(summary(object))) 
}


tkGetParameters = 
function(parameters)
{   # A function implemented by Diethelm Wuertz
    
    # Output Function for Parameters:
    parameters = cbind(parameters)              
    parameters = as.data.frame(parameters)
    colnames(parameters) = "Value:"
    output <- capture.output(parameters)    
    tkinsert(txt, "end", "Parameters:\n")
    for (i in 1:length(output)) 
        tkinsert(txt, "end", paste(output[i], "\n"))
    tkyview.moveto(txt, 1)
}
     

tkGetClass = 
function(class)
{   # A function implemented by Diethelm Wuertz

    # Class:
    ans = (capture.output(getClass(class)))[-1]
    
    # Output:
    tkTitle(paste(class, "Class Representation"))
    tkOutput(ans)   
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
    tkOutput(ans) 
}


tkGetData = 
function(Data, infoName, description = NULL, report = TRUE )
{
    
    # Get and Save:
    command = paste("data(", Data, ")", sep = "")
    eval(parse(text = command))
    x <<- eval(parse(text = Data))
    if (dim(x)[2] != 1) x <<- as.timeSeries(x)
    
    x <<- tkSaveAsX(data = x, infoName = infoName)
    attr(x, "data") = Data
        
    # Report
    plotTitle <<- infoName
    if (report) {
        tkTitle(plotTitle)
        tkOutput(capture.output(head(x)))
        tkOutput("...")
        tkOutput(capture.output(tail(x))) 
        tkDescription()
    }
    
    # Return Value:
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
    
    x <<- tkSaveAsX(data = x, infoName = infoName)
        
    plotTitle <<- infoName
    if (report) {
        tkTitle(plotTitle)
        tkOutput(capture.output(head(x)))
        tkOutput("...")
        tkOutput(capture.output(tail(x))) 
    }
    
    invisible()
}


tkGetFit = 
function(fun, infoName = "Distribution", ...)
{
    # MLE Fit:
    fit = match.fun(fun)
    object <<- fit(as.vector(x), ...)
    objectTitle <<- infoName
    objectLabelText <<- tclVar(paste("VALUE: ", objectTitle))
    tkconfigure(objectLabel, textvariable = objectLabelText)
    tkgrid(objectLabel) 
    ans = (capture.output(object))
    tkTitle(infoName)
    tkOutput(ans)     
}


################################################################################