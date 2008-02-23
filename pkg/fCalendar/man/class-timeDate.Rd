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
    
    \item{show}{\code{signature(object = "fGARCH")}: 
        prints an object of class 'fGARCH'.
        }  
   
    }
}  


\keyword{chron}

