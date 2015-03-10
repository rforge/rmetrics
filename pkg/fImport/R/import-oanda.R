
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################
# FUNCTION:             DESCRIPTION:
#  oandaImport           Downloads market data from www.oanda.com
#  oandaSeries           Easy to use download from www.oanda.com
################################################################################


oandaImport <-
  function(query, file = "tempfile", source = NULL,
           frequency = "daily",
           from = NULL, to = Sys.timeDate(), nDaysBack = 366,
           save = FALSE, sep = ";", try = TRUE)
  {
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Downloads market data from www.oanda.com
    
    # Example:
    #   oandaImport("USD/EUR")
    
    # Value:
    #   An One Column data frame with row names denoting the dates
    #   given in the POSIX format "%Y%m%d". The column lists the
    #   downloaded data records.
    
    # FUNCTION:
    
    # Settings:
    stopifnot(length(query) == 1)
    
    # Check:
    stopifnot(frequency == "daily")
    
    # Source:
    if (is.null(source))
      source <- "http://www.oanda.com/currency/historical-rates/download?&"
    
    # Download:
    if (try) {
      # Try for Internet Connection:
      z <- try(oandaImport(query, file, source, frequency, from, to,
                           nDaysBack, save, sep, try = FALSE))
      if (inherits(z, "try-error") || inherits(z, "Error")) {
        return("No Internet Access")
      } else {
        return(z)
      }
    } else {
      # Download File:
      to <- as.timeDate(to)
      to <- trunc(to, "days")
      
      if (is.null(from))
        from <- to - nDaysBack * 24 * 3600
      else
        from <- as.timeDate(from)
      
      from.date <- as.character(from)
      to.date <- as.character(to)
      
      ccy.pair <- strsplit(toupper(query),"/")[[1]]
      tmp <- tempfile()
      
      url <- paste0("http://www.oanda.com/currency/historical-rates/download?", 
                    "quote_currency=", ccy.pair[1], 
                    "&end_date=", to.date, "&start_date=", from.date, 
                    "&period=daily", "&display=absolute", "&rate=0", 
                    "&data_range=", "c", "&price=mid", "&view=table", 
                    "&base_currency_0=", ccy.pair[2],
                    "&base_currency_1=", "&base_currency_2=", "&base_currency_3=", "&base_currency_4=",
                    "&download=csv")
      
      download.file(url=url, destfile=tmp)
      # add an EOL to the file to avoid the warning message
      cat("\n",file=tmp,append=TRUE)
      
      ## Compose Time Series:
      # fl <- readLines(file, n=20)
      # begin <- grep(paste(ccy.pair,collapse="/"),fl)
      
      fx <- read.csv(tmp, skip = 4, as.is = TRUE, header = TRUE)
      dates <- !is.na(as.Date(fx[,1]))
      fx <- fx[dates,1:2]
      
      data <- rev(as.numeric(fx[,2]))
      charvec <- rev(as.character((fx[,1])))
      X <- timeSeries(data, charvec, units = "USD/CHF")
    }
    
    # Save to file:
    if (save) {
      write.table(as.data.frame(X), file = file, sep = sep)
    } else {
      unlink(file)
    }
    
    # Result:
    ans <- new("fWEBDATA",
               call = match.call(),
               param = c(
                 "Instrument" = query,
                 "Frequency " = frequency),
               data = X,
               title = "Data Import from www.oanda.com",
               description = description() )
    
    # Return Value:
    ans
  }


# ------------------------------------------------------------------------------


oandaSeries <-
  function(symbols, from = NULL, to = Sys.timeDate(), nDaysBack = 366,  ...)
  {
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Easy to use download from www.oanda.com
    
    # Arguments:
    #   symbols - a character vector of symbol names
    #   from - from date
    #   to - to date
    #   nDaysBack - number of n-days back
    #   ... - arguments passed to the *Import()
    
    # Example:
    #   oandaSeries("USD/EUR")
    #   oandaSeries(c("USD/EUR", "USD/JPY"), nDaysBack = 10)
    
    # FUNCTION:
    
    # Download:
    if (is.null(from)) 
      from <- to - nDaysBack * 24 * 3600
    else
      from <- as.timeDate(from) 
    to <- as.timeDate(to)
    to <- trunc(to,"days")  
    # The maximum number of observations allowed by Oanda is 500
    # We need a loop to dowload the series sequencially 
    if ( (to-from) > 400)
    {
      getMore <- TRUE
      to2 <- from+399*24*3600
      X = oandaImport(query = symbols[1], from = from, to = to2, ...)@data
      while (getMore)
      {
        from2 <- to2+24*3600
        if ( (to-from2) < 400)
        {
          X <- rbind(X, oandaImport(query = symbols[1], 
                                    from = from2, to = to, ...)@data)
          getMore <- FALSE
        }
        else
        {
          to2 <- from2+399*24*3600
          X <- rbind(X, oandaImport(query = symbols[1], 
                                    from = from2, to = to2, ...)@data)
        }   
      }
    }
    else
      X = oandaImport(query = symbols[1], from = from, to = to, ...)@data
    names(X) <- symbols[1]  
    
    N = length(symbols)
    if (N > 1) {
      for (i in 2:N) {
        if ( (to-from) > 400)
        {
          getMore <- TRUE
          to2 <- from+399*24*3600
          X2 = oandaImport(query = symbols[i], from = from, to = to2, ...)@data
          while (getMore)
          {
            print(from2)
            from2 <- to2+24*3600
            if ( (to-from2) < 400)
            {
              X2 <- rbind(X2, oandaImport(query = symbols[i], 
                                          from = from2, to = to, ...)@data)
              getMore <- FALSE
            }
            else
            {
              to2 <- from2+399*24*3600
              X2 <- rbind(X2, oandaImport(query = symbols[i], 
                                          from = from2, to = to2, ...)@data)
            }   
          }
        }
        else
          X2 = oandaImport(query = symbols[i], from = from, to = to, ...)@data
        names(X2) <- symbols[i]
        X = cbind(X, X2)
      }
    }
    # Return Value:
    X
  }


################################################################################

