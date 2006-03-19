\name{fBrowser}

\alias{fBrowser}

\alias{tkNewPopup}
\alias{tkAddPopupMenu}
\alias{tkSeparator}
\alias{tkCascadePopup}

\alias{tkExecute}
\alias{tkEval}
\alias{tkSplit}
\alias{tkSaveAsX}
\alias{tkSaveAsObject}
\alias{tkObjectToX}

\alias{tkTitle}
\alias{tkOutput}
\alias{tkDescription}
  
 
\title{Rmetrics Browser and Graphical User Interface}


\description{ 

    A browser and graphical user interface for the Rmetrics packages.
    \cr
    
}


\usage{
fBrowser(toolbar = c(".filePopup", ".fBasicsPopup", ".fCalendarPopup", 
    ".fSeriesPopup", ".fMultivarPopup", ".fExtremesPopup", ".fOptionsPopup", 
    ".helpPopup"), fontSize = 9, fontFamily = "Courier New", guiTitle = 
    "Rmetrics") 
    
tkNewPopup()
tkAddPopupMenu(Menu, Label = NULL, subLabel, Command)
tkSeparator()
tkCascadePopup(Menu, Label)

tkExecute(fun, prototypes = NULL, subject = "- missing -", tkoutput = FALSE)
tkEval()
tkSplit()
tkSaveAsX(data, subject)
tkSaveAsObject(data, subject)
tkObjectToX()

tkTitle()
tkOutput()
tkDescription()
}


\details{

    We have created a browser and graphical user interfaces for the
    Rmetrics packages to better support teaching in financial engineering
    and computation finance. The GUI is started from the R console by 
    the command \code{fBrowser()}, the rest is self-explaining.
    
    The GUI is divided into three major parts: on top a menu bar, in 
    the middle a text window for the output, and on the bottom an info 
    bar with five text fields.
    
    The menu bar on top has currently 7 Popup menus including \cr
    File, fBasics/fCalendar, fSeries, fMultivar, fExtremes, fOptions, Help.
    
    In the text window the standard output is returned, warnings, errors,
    and other information is printed on the R console.
    
    The info bar has five text fields which hold the following information:
    The active data series, the active object, and optional test, fit and 
    prediction objects.
    
    
    *** How to create your own Popup Menu ? ***
    
    newToolbarMenu()
    
    addToolbarMenu(fileMenu, Label, subLabel, Command) 
    
    cascadeToolbarMenu(Menu = fileMenu, Label = "File")  
    
    
    *** How to write your own Command Function ? ***
    
    myFunction = function(..., object2x, report) { }
    
    tkExecute(fun = myFunction, params, subject)
    
    
    *** About Parameter Prototypes ***
    
    numeric
    character
        evaluable - tkEval
        splitable - tkSplit
    logical
    null
        
}


\arguments{

    \item{toolbar}{
        [fBrowser] - \cr
        a vector of character strings naming the popup menus in the
        toolbar.
        }
    \item{fontSize}{
        [fBrowser] - \cr
        an integer value, the font size used by the GUI. The default
        is \code{9}.
        } 
    \item{fontFamily}{
        [fBrowser] - \cr
        a character string, the font used by the GUI. The default
        font is \code{"Couurier New"}.
        }
    \item{guiTitle}{
        [fBrowser] - \cr
        a character string, the title appearing on top of the GUI.
        }
    \item{Menu}{
        [tkAddPopupMenu] - \cr
        ...
        }
    \item{Label}{
        [tkAddPopupMenu] - \cr
        ...
        }
    \item{subLabel}{
        [tkAddPopupMenu] - \cr
        ...
        }
    \item{Command}{
        [tkAddPopupMenu] - \cr
        ...
        }
    \item{fun}{
        [tkExecute] - \cr
        ...
        }
    \item{prototypes}{
        [tkExecute] - \cr
        ...
        }
    \item{subject}{
        [tkExecute] - \cr
        ...
        }
    \item{tkoutput}{
        [tkExecute] - \cr
        ...
        }
    \item{data}{
        ...
        }  

}


\note{

    The Browser and GUI is still experimental and still under current 
    development.

}


\author{
 
    Diethelm Wuertz.
    
}


\keyword{programming}

