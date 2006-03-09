\name{fBrowser}

\alias{fBrowser}


\title{Rmetrics Browser and Graphical User Interface}


\description{ 

    A browser and graphical user interface for the Rmetrics packages.
    \cr
    
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

