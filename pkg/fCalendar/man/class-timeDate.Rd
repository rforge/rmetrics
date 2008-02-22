\name{timeDate-class}


\docType{class}


\alias{timeDate-class}


\title{Class "timeDate"}


\description{ 

    The class 'timeDate' represents date and time objects.
    
}


\section{Objects from the Class}{

    Objects can be created for example by calls of the functions 
    \code{timeDate}, \code{timeCalender} and \code{timeCalendar}
    among others.
    
}


\section{Slots}{
    \describe{

    \item{\code{Data}:}{Object of class \code{"POSIXct"}: 
        a vector of POSIXct dates and times always related to
        "GMT".
        } 
    \item{\code{Dim}:}{Object of class \code{"numeric"}: 
        an integer value with the number (length) of the 
        time stamp vector Data.
        }
    \item{\code{format}:}{Object of class \code{"character"}: 
        a character string denoting the format specification of 
        the input Data character vector.
        }
    \item{\code{FinCenter}:}{Object of class \code{"character"}: 
        a character string with the the location of the
        financial center named as "continent/city", or just "city".
        }
    
    }
}   



\section{Methods}{
    \describe{
    
    %\item{plot}{\code{signature(x = "fGARCH", y = "missing)}: 
    %    plots an object of class 'fGARCH'.
    %    }
    
    \item{show}{\code{signature(object = "fGARCH")}: 
        prints an object of class 'fGARCH'.
        }  
            
    %\item{summary}{\code{signature(object = "fGARCH")}: 
    %    summarizes an object of class 'fGARCH'.
    %    }
   
    }
}  


\references{

Bateman R., (2000);
    \emph{Time Functionality in the Standard C Library},
    Novell AppNotes, September 2000 Issue, 73--85.

ISO-8601, (1988);
    \emph{Data Elements and Interchange Formats -
    Information Interchange, Representation of Dates and Time},
    International Organization for Standardization,
    Reference Number ISO 8601, 14 pages.

James D.A., Pregibon D. (1992),
    \emph{Chronological Objects for Data Analysis},
    Reprint.

Ripley B.D., Hornik K. (2001);
    \emph{Date-Time Classes},
    R-News, Vol. 1/2 June 2001, 8--12.

Zivot, E., Wang J. (2003);
    \emph{Modeling Financial Time Series with S-Plus},
    Springer, New-York.
}


\keyword{chron}

