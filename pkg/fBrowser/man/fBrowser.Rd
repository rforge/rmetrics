\name{fBrowser}

\alias{fBrowser}

\alias{newToolbarMenu}
\alias{addToolbarMenu}
\alias{cascadeToolbarMenu}

\alias{tkDescription}
\alias{tkEval}
\alias{tkExecute}
\alias{tkGetClass}
\alias{tkGetData}
\alias{tkGetDataFrame}
\alias{tkGetDemoData}
\alias{tkGetFinCenters}
\alias{tkGetFit}
\alias{tkGetParameters}
\alias{tkGetSummary}
\alias{tkGetTime}
\alias{tkObjectToX}
\alias{tkOutput}
\alias{tkSaveAsObject}
\alias{tkSaveAsX}
\alias{tkSplit}
\alias{tkTitle}

\alias{besselSlider}
\alias{gevSlider}
\alias{gpdSlider}
\alias{heavisideSlider}
\alias{kummerSlider}
\alias{newToolbarMenu}
\alias{sgedSlider}
\alias{snormSlider}
\alias{sstdSlider}
\alias{unitrootSlider}
\alias{whittakerSlider}
    
     

\title{Rmetrics Browser and Graphical User Interface}


\description{ 

    A browser and graphical user interface for the Rmetrics packages.
    \cr
    
}


\usage{
tkEval(what)
tkSplit(what)

newToolbarMenu()
addToolbarMenu(popupMenu, Label, subLabel, Command)
cascadeToolbarMenu(Menu, Label)

tkExecute(fun = ".fun", params = NULL, infoName = "- missing -", tkoutput = 
    FALSE, console = NULL, title = NULL, description = NULL, ...)
tkSaveAsX(data, infoName)
tkSaveAsObject(data, infoName)



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
    
    tkExecute(fun = myFunction, params, infoName)
    
    
    *** About Parameter Prototypes ***
    
    numeric
    character
        evaluable - tkEval
        splitable - tkSplit
    logical
    null
        
    
    
}


\note{

    The Browser and GUI is still experimental and still under current 
    development. Just try it ...

}
    
    

\usage{ 
fBrowser()
}


\author{
 
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


%\examples{
%## SOURCE("A1.fCommander")
%}


\keyword{programming}

