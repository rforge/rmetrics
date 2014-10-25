\name{WFEStatistics}

\alias{WFEStatistics}

%DATA

\alias{wfe1}
\alias{wfe2}
\alias{wfe3}
\alias{wfe4}
\alias{wfe5}
\alias{wfe6}
            

\title{WFE Statistics}


\description{
    
    Data sets of financial and economic market 
    statistics from the data available from the exchange 
    data collected by the World Federation of Stock 
    Exchanges.
    \cr
    
    To load statistics from the WFE:
    
    \tabular{ll}{
    \code{data(wfe1)} \tab Market capitalization of domestic companies, \cr
    \code{data(wfe2)} \tab Total number of companies with shares listed, \cr
    \code{data(wfe3)} \tab Total value of share trading, \cr 
    \code{data(wfe4)} \tab Market value of bonds listed, \cr
    \code{data(wfe5)} \tab Total value of bond trading, and \cr
    \code{data(wfe6)} \tab Price earning ratio an gross dividend yield. }
    
}

\references{
  
WFE, 2004,
    \emph{World Federation of Stock Exchanges, Focus 2004},
    http://www.world-exchanges.org.

}
    
 
\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}

   
\examples{
## Barplot from WFE Capitalization Statistics:
   # Extract Capitalization of/at:
   # NYSE: 7, Tokyo: 37, London: 22, Frankfurt: 15
   # 1991 - 2003 triannual: 3,6,9,12,15
   data(wfe1)
   Table = t(wfe1[c(7,37,22,15),c(3,6,9,12,15)])/1e6
   colnames(Table) = c("NewYork", "Tokyo", "London", "Frankfurt")
   rownames(Table) = as.character(seq(1991, 2003, by = 3))
   Table
   # Create Barplot:
   barplot(Table, beside = TRUE, legend = rownames(Table),
     col = c("lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk"))
   title(main = "Stock Market Capitalization\n 1991 - 2003")
   mtext("Source: World Federation of Exchanges", side = 4, 
     line = -2, cex = 0.7) 
}


\keyword{data}

