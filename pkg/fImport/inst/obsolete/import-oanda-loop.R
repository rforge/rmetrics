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
