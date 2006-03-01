\name{fBasicsData}

\alias{fBasicsData}

\alias{audusd}
\alias{usdchf}
\alias{usddem30u}
\alias{usdthb}
\alias{fdax9710}
\alias{fdax97m}
\alias{bmwres}
\alias{nyse}
\alias{nyseres}

\alias{jbLM}
\alias{jbALM}

\alias{.PhiStable}

\alias{ciaFactbook}

\alias{wfe1}
\alias{wfe2}
\alias{wfe3}
\alias{wfe4}
\alias{wfe5}
\alias{wfe6}



\title{fBasics Data Sets}


\description{

    A collection and description of data sets used in the  
    examples. Included are data files for high frequency FX
    data, time and sales data for financial futures, and 
    market returns for selected stock and market indexes.
    Included are also tables for the finite sample Jarque-Bera
    test.
    \cr
    
    The data sets are:
    
    \tabular{ll}{   
    \code{audusd.csv} \tab Reuters Tick-by-Tick AUDUSD rates 1997-10, \cr
    \code{usdthb.csv} \tab Reuters Tick-by-Tick USDTHB rates 1997, \cr
    \code{usddem30u.csv} \tab Reuter 30 min USDDEM rate in upsilon time, \cr 
    \code{usdchf.csv} \tab Reuters 30 min USDCHF Rates 199604-200103, \cr
    \code{fdax9710.csv} \tab Minute-by-Minute DAX Futures Prices for 1997-10*, \cr
    \code{fdax97m.csv} \tab Minutely Time and Sales DAX Futures for 1997, \cr
    \code{bmwres.csv} \tab Daily log Returns of German BMW Stock Proces, \cr
    \code{nyse.csv} \tab Daily Values of the NYSE Composite Index, \cr
    \code{nyseres.csv} \tab Daily log Returns of the NYSE Composite Index, \cr
    \code{jbLM} \tab Table for the Jarque Bera Lagrange Multiplier test, \cr
    \code{jbALM} \tab Table for the JB Augmented LM finite sample test, \cr
    \code{PhiStable} \tab Table of Contours for Stable Parameter Estimation. }
    
    The CIA Factbook:
    
    \tabular{ll}{   
    \code{ciaFactbook.R} \tab Macroeconomic data from the CIA Factbook. } 
    
    The Tables from World Federarion of Exchanges , WFE:
    
    \tabular{ll}{   
    \code{wfe1.csv} \tab Table 1, market capitalization of domestic companies, \cr
    \code{wfe2.csv} \tab Table 2, total number of companies with shares listed, \cr
    \code{wfe3.csv} \tab Table 3, total value of share trading, \cr 
    \code{wfe4.csv} \tab Table 4, market value of bonds listed, \cr
    \code{wfe5.csv} \tab Table 5, total value of bond trading, \cr
    \code{wfe6.csv} \tab Table 6, price earning ratio an gross dividend yield. }
            
    *The file \code{fdax97m.csv} is too large and therefore not part of 
    the \code{fBasics} distribution. Please contact \emph{inf@rmetrics.org}.
    
}


\details{

    \bold{High Frequency Data for the AUDUSD and USDTHB:}
    \cr\cr
    \code{audusd} and \code{usdthb} archive high frequency exchange rates 
    for the Australian / US Dollar exchange rate in October 1997 and exchange 
    rates for the US Dollar / Thailand Bhat exchange rate in June 1997:
    A comma delimited CSV file with 6 columns. The first column,
    named \code{XDATE}, contains date/time entries in ISO-8601 format 
    as [CCYYMMDDhhmm], the second column, named \code{DELAY}, gives 
    the delay in minutes between the time stamp of Reuter's data
    record and arrival time at the local database, the third column
    named \code{CONTRIBUTOR} is Reuter's identification, a 4 character
    code, the fourth and fifth column, named \code{BID} and \code{ASK}
    are the bid and ask price quotations, and finally the sixth
    column, named \code{FLAG}, is not used and has zeros as entries.
    \cr
    
    \bold{30 Minutes Data for the USDDEM Rate in Upsilon time:}
    \cr\cr
    \code{usddem30u} archives 30 min USDDEM bid and ask for the US Dollar 
    German Mark exchange rate ranging from 1992-10-01 00:09 until 
    1997-05-30 21:49.
    A comma delimited CSV file with 3 columns. The first column,
    named \code{"\%Y\%m\%d\%H\%M"}, contains date/time entries in 
    ISO-8601 format as [YYYYMMDDhhmm], the second column, named 
    \code{BID}, gives the bid prices, and the third column named
    \code{ASK}, gives the ask prices retrieved from the Reuter's data
    records.
    \cr 
    
    \bold{30 Minutes Data for the USDCHF Rate:}
    \cr\cr
    \code{usdchf} archives 30 min USDCHF midprices for the US Dollar 
    Swiss Franc exchange rate ranging from 1996-04-01 until 2001-03-30.
    A comma delimited CSV file with 2 columns. The first column,
    named \code{"\%Y\%m\%d\%H\%M"}, contains date/time entries in 
    ISO-8601 format as [YYYYMMDDhhmm], the second column, named 
    \code{USDCHF}, gives the prices retrieved from the Reuter's data
    records.
    \cr 
    
    \bold{DAX Futures Data:}
    \cr\cr
    \code{fdax9710} archives returns of minute-by-minute prices for 
    Dax Futures in October 1997:
    A comma delimited CSV file with 2 columns. The first column,
    named \code{XDATE}, contains date/time entries in ISO-8601 format 
    as [CCYYMMDDhhmm], the second column, named \code{FDAX}, gives 
    an averaged price of the Dax Futures, i.e. the mean of
    all volume weighted time and sales within the same minute.
    \cr
    \code{fdax97m} archives returns for minute-by-minute prices for Dax 
    Futures in 1997: A comma delimited CSV file with 2 columns. The first 
    column, named \code{XDATE}, contains date/time entries in ISO-8601 
    format as [CCYYMMDDhhmm], the second column, named \code{FDAX}, gives 
    a minutely averaged price during opening hours of the exchange, 
    i.e. the mean of all volume weighted time and sales within the 
    same minute.
    \cr
    
    \bold{Log Returns for BMW Shares and NYSE Composite Index:}
    \cr\cr
    \code{bmwres} and \code{nyseres} archive log returns of the German 
    BMW stock listed in the German  DAX30 and log returns of the NYSE 
    Composite Index, both on a daily trading day time scale just numbering 
    the log returns: A one column CSV file with column names \code{BMW} 
    or \code{NYSERES}, respectively. The entries are the differences
    of the logarithmic prices on two succeeding trading days. \code{nyse}
    contains two column data recors, the date and the NYSE Composite Index.
    \cr
    
    \bold{Jarque Bera Normality Test:}
    \cr\cr
    \code{jbLM} and \code{jbALM} are finite sample tables for the
    Jarque Bera Lagrange multiplier and augmented Lagrange multiplier
    normality test. The columns denote the sample sizes and the row the
    probabilities.
    \cr
    
    \bold{Stable Parameter Estimation:}
    \cr\cr
    \code{.PhiStable} is a list object containing two tables for the
    estimation of the parameters of a stable distribution using
    McCullochs approach.
    \cr
    
}


\source{

    \code{audusd}\cr
    \code{usdthb}\cr
    \code{usdthb}
    \cr
    The data were collected by D. Wuertz and R. Schnidrig from the 
    Reuter's data feed.
    \cr
    
    \code{fdax9710}\cr
    \code{fdax97m}
    \cr
    The data were extracted from time and sales data records from
    the Frankfurt Futures Exchange.
    \cr
    
    \code{bmwres}
    \cr
    The data were published in the EVIS software package.
    \cr
    
    \code{nyse}\cr
    \code{nyseres}
    \cr
    The data were downloaded from the web site of the New York Stock 
    Exchange and the residuals were calculated as logarithmic price 
    differences.\cr
    \emph{http://www.nyse.com}.
    \cr
    
    \code{jbLM}\cr
    \code{jbALM}
    \cr
    Monte Carlo Simulations by D. Wuertz and H.G. Katzgraber.
    
}


\examples{
## SOURCE("fBasics.012C-fBasicsData")

##  plot -
    xmpBasics("\nStart: Plot Residuals NYSE Composite Index > ")
    data(nyseres)
    x = as.ts(nyseres)
    par(mfrow = c(2, 1), cex = 0.75)
    plot(100*x, type = "l", col = "steelblue4",
      main = "NYSE Composite Index")
    grid()
    plot(cumsum(x), type = "l", col = "steelblue4",
      main = "Cumulated NYSE Index")
    grid()
}


\keyword{datasets}

