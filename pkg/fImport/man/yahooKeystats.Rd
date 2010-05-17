\name{YahooKeystats}


\alias{yahooKeystats}


\title{Import Key Statistics Data from Yahoo}


\description{
    
    Imports key statistics from chart.yahoo.com.
   
}


\usage{ 
yahooKeystats(query, file = "tempfile", source = NULL, save = FALSE, 
    try = TRUE) 
}


\arguments{

    \item{file}{
        a character string with filename, usually having extension 
        ".csv", where to save the downloaded data.
        }
    \item{query}{
        a character string, denoting the location of the data at the 
        web site.
        }
    \item{save}{
        a logical value, if set to TRUE the downloaded data file will 
        be stored under the path and file name specified by the
        string \code{file}. By default FALSE.
        }
    \item{source}{a character string setting the URL of the source.
        If \code{NULL}, then the URL will be set automatically to its
        default value.
        }
    \item{try}{
        a logical value, if set to TRUE the Internet access will be
        checked.
        }
        
}
  
  
\value{

    returns a data frame with 
    key statistics downloaded from yahoo's web site.
    
}


\note{

    Function \code{yohooKeystats} has been renamed from \code{keystatsImport}.

}
    

\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}


\references{

Diethelm Wuertz, Yohan Chalabi, and Andrew Ellis, (2010);
    \emph{Financial Market Data for R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich,
    www.rmetrics.org.
    
}


\keyword{data}

