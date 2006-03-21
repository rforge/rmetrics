
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
function(toolbar = c(".filePopup", ".fBasicsPopup", ".fCalendarPopup", 
".fSeriesPopup", ".fMultivarPopup", ".fExtremesPopup", ".fOptionsPopup", 
".helpPopup"), fontSize = 9, fontFamily = "Courier New", guiTitle = 
"Rmetrics" )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Rmetrics GUI
    
    # FUNCTION:
    
    # Global Objects:
    x <<- as.ts(rnorm(1000))
    xTitle <<- "rnorm(1000)"
    object <<- NULL
    info <<-NULL
    object2recover <<- NULL
    info2recover <<-NULL
    objectTitle <<- "NULL"
    helpTopic <<- "help"
    
    # Frames:
    base <<- tktoplevel(width=800)
    tkwm.title(base, guiTitle) 
    tkfocus()
    
    popupFrame <<- tkmenu(base)  
    tkconfigure(base, menu = popupFrame)
    for (popup in toolbar) {
        funPopupMenu = match.fun(popup)
        funPopupMenu()
    }
    tkfocus()
    
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
    # tkfocus(txt)
    tkfocus()
    
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
        # Display Prompt in another Color:
        tktag.add(txt, "currentLine", "end - 2 lines linestart", 
            "end - 2 lines lineend")
        tktag.configure(txt, "currentLine", foreground = "red")
        if (length(grep("<-", commandVal)) == 0) {
            tkOutput(capture.output(ans)) }
        invisible() }
    tkbind(entryCommand, "<Return>", CommandOnOK)  
    
    # Active Input/Value Data Frame:
    activeFrame <<- tkframe(base, relief = "groove", borderwidth = 2)
    infoLabelText <<- tclVar(paste("ts: ", xTitle))
    objectLabelText <<- tclVar(paste("null: ", objectTitle))
    infoLabel <<- tkbutton(activeFrame, relief = "ridge",
        command = .onInfo, text = tclvalue(infoLabelText), fg = "blue")     
    classXLabel <<- tkbutton(activeFrame, relief = "ridge",
        command = .onClassX, text = "Info", fg = "blue")    
    objectLabel <<- tkbutton(activeFrame, relief = "ridge",
        command = .onObject, text = tclvalue(objectLabelText), fg = "red")
    classObjectLabel <<- tkbutton(activeFrame, relief = "ridge",
        command = .onClassObject, text = "Info", fg = "red")
    summaryLabel <<- tkbutton(activeFrame, relief = "ridge",
        command = .onSummary, text = "Summary", fg = "red")
    plotLabel <<- tkbutton(activeFrame, relief = "ridge",
        command = .onPlot, text = "Plot", fg = "red")
    copyLabel <<- tkbutton(activeFrame, relief = "ridge",
        command = .onCopy, text = "object to x", fg = "darkgreen")
    recoverLabel <<- tkbutton(activeFrame, relief = "ridge",
        command = .onRecover, text = "Recover", fg = "darkgreen")
    tkgrid(activeFrame)
    tkgrid(infoLabel, classXLabel, objectLabel, classObjectLabel, 
        summaryLabel, plotLabel, copyLabel, recoverLabel)  
       
    # Compose: 
    tkpack(textFrame, fill = "both", expand = TRUE)  
    tkpack(commandFrame)
    tkpack(activeFrame, fill = "x")  
    tkfocus()
}


# ------------------------------------------------------------------------------


.onInfo = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    tkOutput(capture.output(x))
}


# ------------------------------------------------------------------------------


.onClassX = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    x <<- .tkGetClass(class(x)[1])
}


# ------------------------------------------------------------------------------


.onObject =
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    tkOutput(capture.output(object))
}


# ------------------------------------------------------------------------------


.onClassObject =
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    x <<- .tkGetClass(class(object)[1])
}


# ------------------------------------------------------------------------------


.onSummary = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    x <<- .tkGetSummary(object)
}


# ------------------------------------------------------------------------------


.onPlot = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    plot(object)
}


# ------------------------------------------------------------------------------


.onCopy =
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    x <<- tkObjectToX(object)
}


# ------------------------------------------------------------------------------


.onRecover = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    object <<- object2recover
    info <<- info2recover
    what = paste(as.character(class(object)), ":", sep = "")
    objectLabelText <<- tclVar(paste(what, info))
    tkconfigure(objectLabel, textvariable = objectLabelText)
    tkdestroy(tt)
}


################################################################################
# tkNewPopup
# tkAddPopupMenu
# tkSeparator
# tkCascadePopup


tkNewPopup = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Create New Toolbar Menu:
    Menu = tkmenu(popupFrame, tearoff = FALSE) 
    # tkfocus(Menu)
}


# ------------------------------------------------------------------------------


tkAddPopupMenu =  
function(Menu, Label, subLabel, Command) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Add Toolbar Menu:
    if (is.null(subLabel)) {
        tkadd(Menu, "command", label = Label, 
            command = match.fun(Command))
    } else { 
        subMenu = tkmenu(Menu, tearoff = FALSE)
        for (i in 1:length(subLabel)) {
            tkadd(subMenu, "command", label = subLabel[i], 
                command = match.fun(Command[i]))
        }
        tkadd(Menu, "cascade", label = Label, menu = subMenu)
    }
}


# ------------------------------------------------------------------------------


tkSeparator =
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Return Value:
    invisible()
}
 

# ------------------------------------------------------------------------------

   
tkCascadePopup = 
function(Menu, Label) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Cascade Toolbar Menu:
    tkadd(popupFrame, "cascade", label = Label, menu = Menu) 
}
 

################################################################################               
# tkExecute
# tkEval
# tkSplit
# tkSaveAsX 
# tkSaveAsObject
# tkObjectToX


tkExecute =
function(fun, prototypes, subject = "")
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Set Object Title"
    objectTitle <<- subject
    object2recover <<- object
    info2recover <<- info
    info <<- subject 
    tkoutput = FALSE

    # Settings:  
    argNames = names(prototypes)
    Character = NULL
    Numeric = NULL
    Logical = NULL
    Null = NULL
    # Parameters:
    tt <- tktoplevel()
    tkgrid(tklabel(tt, text = subject, fg = "blue"))
    for (i in 1:length(prototypes) ) {
        Numeric = c( Numeric, is.numeric(prototypes[[i]]) )
        Character = c( Character, is.character(prototypes[[i]]) )
        Logical = c( Logical, is.logical(prototypes[[i]]) )
        Null = c( Null, is.null(prototypes[[i]]) )
        if (Logical[i]) {           
            assign( argNames[i], tclVar(as.integer(prototypes[[i]])) )    
            entry.Name <- tkcheckbutton(tt, variable = get(argNames[i]),
                anchor = "e" )
        } else { 
            assign( argNames[i], tclVar(prototypes[[i]]) )    
            entry.Name <- tkentry(tt, width = "25", fg = "red", 
                textvariable = get(argNames[i]) )
        }
        # print( get(argNames[i]) ) 
        label.Name <- tklabel(tt, text = argNames[i])
        tkgrid(label.Name, entry.Name)
        tkgrid.configure(label.Name, sticky = "e")
        tkgrid.configure(entry.Name, sticky = "w")
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
                # z[[i]] = as.logical(tclvalue(get(argNames[i]))) 
                z[[i]] = as.logical(as.integer(tclvalue(get(argNames[i]))))
            # print(z[[i]])
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
            if (z$object2x) x <<- tkSaveAsX(object, subject)
        }
        # Print Specification overwrites tkoutput:
        if (length(z$report) == 1)  {
            tkoutput = z$report
        }
        # Output:
        if (tkoutput) {
            # if (!is.null(title)) tkTitle(title)
            CO = capture.output(object)
            N.CO = length(CO)
            if ( N.CO > 100 ) {
                tkOutput(capture.output(object)[1:10])
                tkOutput("...")
                tkOutput(capture.output(object)[(N.CO-9):N.CO])
            } else {
                tkOutput(capture.output(object)) 
            }
            # if (!is.null(description)) tkDescription(description)
        }
    }

    okButton <- tkbutton(tt, text = "   Ok   ", 
        command = OnOK)
    quitButton <- tkbutton(tt, text = "   Quit   ", 
        command = function() tkdestroy(tt) )
        
    tkbind(entry.Name, "<Return>", OnOK)
    tkgrid(okButton, quitButton, sticky = "sew")

    
    helpButton <- tkbutton(tt, text = "   Help   ",
         command = function() print(help(helpTopic)) )
    tkgrid(helpButton, sticky = "sew")
    
    tkfocus(tt)
    invisible()     
}


# ------------------------------------------------------------------------------


tkEval =
function(what)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Return Value:
    eval(parse(text = what))
}


# ------------------------------------------------------------------------------


tkSplit = 
function(what)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Return Value:
    sub(" ", "", strsplit(what, "&")[[1]][1])
}


# ------------------------------------------------------------------------------


tkSaveAsX =  
function(data, subject)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # What is X?
    what = paste(as.character(class(data)), ":", sep = "")
    
    # Save X:
    xTitle <<- subject
    infoLabelText <<- tclVar(paste(what, subject))
    tkconfigure(infoLabel, textvariable = infoLabelText, fg = "blue")
    tkgrid(infoLabel)
    
    # Return Value:
    data
}


# ------------------------------------------------------------------------------


tkSaveAsObject =  
function(data, subject)
{   # A function implemented by Diethelm Wuertz

    # What is Object?
    what = paste(as.character(class(data)), ":", sep = "")
    
    # Save Object: 
    objectTitle <<- subject
    infoLabelText <<- tclVar(paste(what, subject))
    tkconfigure(infoLabel, textvariable = infoLabelText)
    tkgrid(infoLabel)
    
    # Return Value:
    data
}


# ------------------------------------------------------------------------------


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

    # FUNCTION:
    
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


# ------------------------------------------------------------------------------


tkOutput =
function(output)
{   # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Output Function - Object:
    for (i in 1:length(output)) { 
        tkinsert(txt, "end", paste(output[i], "\n"))       
    }   
    tkyview.moveto(txt, 1) 
}


# ------------------------------------------------------------------------------


tkDescription = 
function(description = NULL)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Output Function - Description:
    if (is.null(description)) 
        description = as.character(date())
    tkinsert(txt, "end", "\nDescription:\n ")
    tkinsert(txt, "end", description)
    tkinsert(txt, "end", "\n\n")
    tkyview.moveto(txt, 1)
}


################################################################################
# .tkGetSummary
# .tkGetParameters
# .tkGetClass
# .tkGetTime
# .tkGetFinCenters
# .tkGetDemoData
# .tkGetData
# .tkGetDataFrame


.tkGetSummary =
function(object, title = NULL)
{   # A function implemented by Diethelm Wuertz
    
    # Summary:
    if (!is.null(title)) tkTitle(title)
    tkOutput(capture.output(summary(object))) 
}


# ------------------------------------------------------------------------------


.tkGetParameters = 
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
    tkyview.moveto(txt, 1)
}
     

# ------------------------------------------------------------------------------


.tkGetClass = 
function(class)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Class:
    ans = (capture.output(getClass(class)))[-1]
    
    # Output:
    tkTitle(paste(class, "Class Representation"))
    tkOutput(ans)   
}


# ------------------------------------------------------------------------------


.tkGetTime =  
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Time:
    ans = (capture.output(Sys.timeDate()))
    
    # Output:
    tkTitle("Current Date and Time")
    tkOutput(ans) 
}


# ------------------------------------------------------------------------------


.tkGetFinCenters =
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Financial Centers:
    ans = (capture.output(listFinCenter()))
    
    # Output:
    tkTitle("List of Financial Centers")
    tkOutput(ans) 
}


# ------------------------------------------------------------------------------


.tkGetDemoData = 
function(Data, report, FUN = "as.timeSeries")
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Get and Save:
    command = paste("data(", Data, ")", sep = "")
    tkEval(command)
    ans = tkEval(Data)
    if (class(FUN) == "function") {
        fun = FUN
    } else {
        fun = match.fun(FUN)
    }
    ans = fun(ans)
    attr(ans, "control") <- c(data = Data)
    
    # Title:
    if (report) tkTitle(paste(Data, "Demo Data Set"))
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.tkGetData = 
function(Data, subject, description = NULL, report = TRUE )
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Get and Save:
    command = paste("data(", Data, ")", sep = "")
    eval(parse(text = command))
    x <<- eval(parse(text = Data))
    if (dim(x)[2] != 1) x <<- as.timeSeries(x)
    
    x <<- tkSaveAsX(data = x, subject = subject)
    attr(x, "data") = Data
        
    # Report
    if (report) {
        tkTitle(subject)
        tkOutput(capture.output(head(x)))
        tkOutput("...")
        tkOutput(capture.output(tail(x))) 
        tkDescription()
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


..tkGetDataFrame = 
function(Data, subject, report = TRUE )
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    command = paste("data(", Data, ")", sep = "")
    eval(parse(text = command))
    x <<- eval(parse(text = Data))
    attr(x, "data") <<- Data
    
    x <<- tkSaveAsX(data = x, subject = subject)
        
    if (report) {
        tkTitle(subject)
        tkOutput(capture.output(head(x)))
        tkOutput("...")
        tkOutput(capture.output(tail(x))) 
    }
    
    invisible()
}


################################################################################

