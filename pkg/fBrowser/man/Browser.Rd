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

    A configurable browser and graphical user interface 
    for the Rmetrics packages.
    \cr
    
}


\usage{
fBrowser(toolbar = c(".filePopup", ".fBasicsPopup", ".fCalendarPopup", 
    ".fSeriesPopup", ".fMultivarPopup", ".fExtremesPopup", ".fOptionsPopup", 
    ".helpPopup"), fontSize = 9, fontFamily = "Courier New", guiTitle = 
    "Rmetrics") 
    
tkNewPopup()
tkAddPopupMenu(Menu, Label, subLabel, Command)
tkSeparator()
tkCascadePopup(Menu, Label)

tkExecute(fun, prototypes, subject = "")
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

    The Rmetrics browser is a configurable graphical user interfaces for the
    Rmetrics packages to better support teaching in financial engineering
    and computation finance. The GUI is started from the R console by 
    the command \code{fBrowser()}, the rest is self-explaining.
    
    \bold{Browsewr Design:}
    \cr\cr
    The GUI is divided into three major parts: on top a toolbar with
    popup menus, in the middle a text window for the output, and on 
    the bottom a command line input field and a taskbar with five buttons
    returning additional information.
    \cr
    The menu bar on top currently 7 Popup menus including a File Menu, 
    the fBasics, fCalendar, fSeries, fMultivar, fExtremes, and fOptions 
    Menus, and a Help Menu.
    In the text window the standard output is returned, warnings, errors,
    and other information is printed on the R console.
    The command line input field allows to execute R commands.
    The info bar has five buttons which can return on click the following 
    information: The first button prints the active data set \code{x}, the
    second button returns information about the active data set \code{x}, 
    the third button prints the active \code{object}, the fourth 
    information about the active \code{object}. The fivth button returns
    a summary report about the active \code{object}, and the last two 
    buttons allow to copy the \code{object} to \code{x}, and to retrieve 
    the previous \code{object}.
    \cr
    
    \bold{Key Figures}
    \cr\cr
    The Rmetrics Browser allows a comfortable execution of functions and 
    scripts implemented in the Rmetrics financial packages with default 
    settings and example data sets. 
    The browser also allows to build task sequences which can be 
    repeated for several different parameter settings.
    Furthermore the browser can be used as a powerful demonstration tool  
    in lectures teaching financial engineering and computational finance.
    Finally the browser is configurable, the user can add to the system 
    personal popup menus with user tailored commands.
    For details we refer to Wuertz [2006].
      
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
        a character string, the name of the popup menu to which a
        new entry should be added.
        }
    \item{Label}{
        [tkAddPopupMenu] - \cr
        a character string, the label of a new entry to be added.
        }
    \item{subLabel}{
        [tkAddPopupMenu] - \cr
        a character vector, the labels of optional subentries to
        be added.
        }
    \item{Command}{
        [tkAddPopupMenu] - \cr
        a character string, the name of the comman to be aexecuted
        when an entry of an popuup menu is clicked.
        }
    \item{fun}{
        [tkExecute] - \cr
        the name of the function which will be executed.
        }
    \item{prototypes}{
        [tkExecute] - \cr
        a list with prototype argument settings for the function
        \code{fun}.
        }
    \item{subject}{
        [tkExecute] - \cr
        a character string which holds a subject title characterizing
        an entry of a popup menu.
        }
    \item{data}{
        a character string, the name of a demo data set.
        }  

}


\note{

    The browser is still experimental and still under current 
    development.

}


\references{

Wuertz, D. (2006); 
    \emph{The Rmetrics Browser}, 
    Presentation, www.rmetrics.org.

}


\author{
 
    Diethelm Wuertz.
    
}


\keyword{programming}

