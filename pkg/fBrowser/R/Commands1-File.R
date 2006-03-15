
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# A copy of the GNU General Public License is available via WWW at
# http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
# writing to the Free Software Foundation, Inc., 59 Temple Place,
# Suite 330, Boston, MA  02111-1307  USA. 

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
# File Popup
           
    
.listActiveDataSet =  
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # List Active Data Set
    .showDataCmd(as.data.frame(x)) 
}    


# ------------------------------------------------------------------------------


.listObjects =  
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # List Active Data Set
    tkTitle("List of Objects")
    tkOutput(capture.output(ls())) 
}    


# ------------------------------------------------------------------------------


.listTypeOfObjects =  
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # List Active Data Set
    tkTitle("List of Objects with Class Type")
    List = ls()
    L = length(List)
    Type = NULL
    for ( i in 1:L ) {
        Type[i] = class(eval(parse(text = List[i])))
    }
    tkOutput(capture.output(data.frame(cbind(List, Type))))
}    


# ------------------------------------------------------------------------------
   

.menuInstallPackages =
function()
{    # A function implemented by Diethelm Wuertz

     # FUNCTION:
    
     # Install:
     utils:::menuInstallPkgs()
}


# ------------------------------------------------------------------------------
   

.menuUpdatePackages =
function()
{    # A function implemented by Diethelm Wuertz

     # FUNCTION:
    
     # Update:
     update.packages(ask='graphics')
}
 

# ------------------------------------------------------------------------------
    

.quitBrowser =  
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Quit:
    tkdestroy(base) 
}       


################################################################################

        
.openFile =  
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
        
    # Open File:
    # Arguments - Read Table:
    # read.table(file, header = FALSE, sep = "", quote = "\"'", dec = ".",
    #   row.names, col.names, as.is = FALSE, na.strings = "NA",
    #   colClasses = NA, nrows = -1,
    #   skip = 0, check.names = TRUE, fill = !blank.lines.skip,
    #   strip.white = FALSE, blank.lines.skip = TRUE,
    #   comment.char = "#")
    argNames <<- c("Header", "Separator", "Skip Lines")
    params <<- c("TRUE", ";", "0")
    names(params) = argNames
    
    # Internal Function:
    .start <- function()
    {
        # Get values:
        Values = NULL
        for ( i in 1:length(argNames) ) 
            Values <- c( Values, as.character(tclvalue(get(valueNames[i]))) )       
        
        # Arguments - Read Table:
        header = as.logical(Values[1])
        sep = Values[2]
        skip = as.integer(Values[3])
        # Open File:
        file <- as.character(tclvalue(tkgetOpenFile()))
        print(file)
        # Execute:
        x <<- read.table(file = file, header = header, sep = sep, 
            skip = skip)
        activeDataSet <<- paste("x =", file)    
        
        tkinsert(txt, "end", "\nTitle:\nOpen File\n")
        tkinsert(txt, "end", paste("\nFile:\n", activeDataSet, "\n"))
        tkinsert(txt, "end", paste("\nDimension:\n", capture.output(dim(x)), 
            "\n"))
        tkinsert(txt, "end", "\nHead of Data File:\n")
        output = capture.output(head(x))
        for (i in 1:length(output)) 
            tkinsert(txt, "end", paste(output[i], "\n"))
        tkinsert(txt, "end", paste("\nDescription:\n", capture.output(date()), 
            "\n"))
        
        # Info:
        seriesTitle <<- activeDataSet
        infoLabelText <<- tclVar(paste("Active Data Set:", activeDataSet))
        tkconfigure(infoLabel, textvariable = infoLabelText)
        tkgrid(infoLabel)       
    }
    
    # Frame:
    parameterTitle = "Open File"
    parameterBase <- tktoplevel()
    tkwm.title(parameterBase, parameterTitle)
    parameterFrame <- tkframe(parameterBase, relief = "groove", 
        borderwidth = 2)
    tkpack(tklabel(parameterBase, text = "Open File")) 
    
    # Get Parameter Values:
    labelNames <<- paste("label.", argNames, sep = "")
    entryNames <<- paste("entry.", argNames, sep = "")
    valueNames <<- paste("value.", argNames, sep = "")
    for ( i in 1:length(argNames) ) { 
        assign( argNames[i], params[i] )
        assign( valueNames[i], tclVar(get(argNames[i])) )
        tkgrid(
            tklabel(parameterFrame, text = argNames[i] ),
            tkentry(parameterFrame, width = "20", 
                textvariable = get(valueNames[i])) )
    }
    
    # Buttons:
    startButton <- tkbutton(
        parameterFrame, 
        text = "Start", 
        command = .start )
    quitButton <- tkbutton(
        parameterFrame, 
        text = "Quit", 
        command = function() tkdestroy(parameterBase) ) 
    helpButton <- tkbutton(
        parameterFrame, 
        text = "Help", 
        command = function() help(read.table) ) 
    tkgrid(startButton, quitButton, helpButton)
    tkfocus(parameterFrame)
    tkpack(parameterFrame)      
}


################################################################################

