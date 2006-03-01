\name{MillsData}

\alias{MillsData}


\alias{RS}  
\alias{R20}     
\alias{RSQ}     
\alias{R20Q}    
\alias{RSQREAL}     
\alias{FTAPRICE} 
\alias{FTADIV}  
\alias{FTARET}  
\alias{RPI}     
\alias{EXCHD}   
\alias{EXCHQ}   
\alias{SP500}   
\alias{SP500R}  
\alias{SP500D}  
\alias{FT30}    
\alias{FTSE100}     
\alias{CTLD}    
\alias{LGEN}    
\alias{PRU}     
            

\title{Terence Mill's Data Sets}


\description{

    A collection and description of data sets 
    from the book 'The Econometric Modelling of 
    Financial Time Series" written by Terence C. 
    Mills.
    \cr
    
    The data sets are:
    
    \tabular{ll}{   
    \code{RS.txt} \tab Monthly 91 day Treasury Bill rate, \cr
    \code{R20.txt} \tab Monthly Yield on 20 Year UK Gilts, \cr
    \code{RSQ.txt} \tab Quarterly 91 day Treasury Bill rate, \cr
    \code{R20Q.txt} \tab Quarterly Yield on 20 Year UK Gilts, \cr
    \code{RSQREAL.txt} \tab Quarterly real 91 day Treasury Bill rate, \cr
    \code{FTAPRICE.txt} \tab FTA All Share Price Index, \cr
    \code{FTADIV.txt} \tab FTA All Share Dividend Index, \cr
    \code{FTARET.txt} \tab FTA All Share Nominal Returns, \cr
    \code{RPI.txt} \tab UK Retail Price Index, \cr
    \code{EXCHD.txt} \tab Dollar/Sterling Exchange Rate, \cr
    \code{EXCHQ.txt} \tab Dollar/Sterling Exchange Rate, \cr
    \code{SP500.txt} \tab SP 500 Annual Data Index, \cr
    \code{SP500R.txt} \tab SP 500 Real Returns, \cr
    \code{SP500D.txt} \tab SP 500 Daily Data Index, \cr
    \code{FT30.txt} \tab FT 30 Index, \cr
    \code{FTSE100.txt} \tab FTSE 100 Index, \cr
    \code{CTLD.txt} \tab Courtaulds Share Price, \cr
    \code{LGEN.txt} \tab Legal and General Share Price, \cr
    \code{PRU.txt} \tab Prudential Share Price. }
    
}


\details{
            
    \bold{RS - 91 Day Treasury Bill Rate:}
    \cr\cr
    Monthly data values starting in March 1952 and ending December 1995. 
    The data set includes one column with 526 observations.
    \cr
        
    \bold{R20 - Yield on 20 Year UK Gilts:}
    \cr\cr
    Monthly data values starting in March 1952 and ending December 1995. 
    The data set includes one column with 526 observations.
    \cr
    
    \bold{RSQ - 91 day Treasury Bill Rate:}
    \cr\cr
    Quarterly data values starting in Q1 1952 and ending Q4 1995.
    The data set includes one column with 176 observations.
    \cr
    
    \bold{R20Q - Yield on 20 Year UK Gilts:}
    \cr\cr
    Quarterly data values starting in Q1 1952 and ending Q4 1995.
    The data set includes one column with 176 observations. 
    \cr
    
    \bold{RSQREAL - Real 91 day Treasury Bill Rate:}
    \cr\cr
    Quarterly data values starting in Q1 1952 and ending Q3 1995.
    The data set includes one column with 175 observations.
    \cr
    
    \bold{FTAPRICE - FTA All Share Price Index:}
    \cr\cr
    Monthly data values starting in January 1965 and ending December 1995. 
    The data set includes one column with 372 observations. 
    \cr
    
    \bold{FTADIV - FTA All Share Dividend Index:}
    \cr\cr
    Monthly data values starting in January 1965 and ending December 1995. 
    The data set includes one column with 372 observations.
    \cr
    
    \bold{FTARET - FTA All Share Nominal Returns:}
    \cr\cr
    Monthly data values starting in January 1965 and ending December 1995. 
    The data set includes one column with 372 observations.
    \cr
    
    \bold{RPI - UK Retail Price Index:}
    \cr\cr
    Monthly data values starting in January 1965 and ending December 1995. 
    The data set includes one column with 372 observations.
    \cr
    
    \bold{EXCHD - Dollar/Sterling Exchange Rate:}
    \cr\cr
    Daily data values ranging from 1974 to 1994 
    The data set includes one column with 5'192 observations.
    \cr
    
    \bold{EXCHQ - Dollar/Sterling Exchange Rate:}
    \cr\cr
    Quarterly data values starting in Q1 1972 and ending Q4 1996.
    The data set includes one column with 100 observations.
    \cr
    
    \bold{SP500 - SP 500 Index:}
    \cr\cr
    Annual data values ranging from 1871 to 1997. 
    The data set includes one column with 127 observations.
    \cr
    
    \bold{SP500R - SP 500 Real Returns:}
    \cr\cr
    Annual data values ranging from 1872 to 1995. 
    The data set includes one column with 124 observations.
    \cr
    
    \bold{SP500D - SP 500 Index:}
    \cr\cr
    Daily data values ranging from 1928 to 1991.
    The data set includes one column with 17'054 observations.
    \cr
    
    \bold{FT30 - FT 30 Index:}
    \cr\cr
    Daily data values ranging from 1935 to 1994. 
    The data set includes one column with 15'003 observations.
    \cr
    
    \bold{FTSE100 - FTSE 100 Index:}
    \cr\cr
    Weekly data values ranging from 1984 to 1993.
    The data set includes one column with 521 observations.
    \cr
    
    \bold{CTLD - Courtaulds Share Price:}
    \cr\cr
    Weekly data values ranging from 1984 to 1993.
    The data set includes one column with 521 observations.
    \cr
    
    \bold{LGEN - Legal and General Share Price:}
    \cr\cr
    Weekly data values ranging from 1984 to 1993.
    The data set includes one column with 521 observations.
    \cr
    
    \bold{PRU - Prudential Share Price:}
    \cr\cr
    Weekly data values ranging from 1984 to 1993.
    The data set includes one column with 521 observations.

}

\references{

Mills, T.C. (1999);
    \emph{The Econometric Modelling of Financial Time Series,}
    Second Edition, Cmbridge University Press.
    
}


\examples{
## SOURCE("fSeries.036B-Data")

## RS.txt - 
   data(RS)
   plot(as.ts(RS))
}


\keyword{datasets}

