#
# Examples from the Monograph:
#   "Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 2.4
#   Rmetrics 'timeSeries' Objects
#
# List of Examples:                
#   
#   Example: Monthly Data
#   Example: Daily Data
#
# Author:
#   (C) 1997-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################
# Functions and methods for 'timeSeries' objects


    # Description:
    #   These are test cases for 'timeSeries' objects. We used these
    #   examples to check if the functions and methods included in the
    #   two files 'timeDateClass.R', 'timeDateMethods' and 'timeSeries'
    #   work properly. 
    # References:
    #   POSIXt from R's "base" package.


# ------------------------------------------------------------------------------


### Load Packages:

    require(fCalendar)
    ###
    

# ------------------------------------------------------------------------------


### Example: Monthly Data

    # Use JohnsonJohnson from 'stats'
    # Quarterly earnings (dollars) per Johnson & Johnson share 1960–80
    require(stats)
    data(JohnsonJohnson)
    JohnsonJohnson
    ###
      
    # Create a 'timeSeries' object
    args(timeSeries)
    myFinCenter = "GMT"
    dates = timeCalendar(y = rep(1960:1980, each = 4),
     m = rep(c(3,6,9,12), 21), d = rep(c(31,30,30,31), 21))
    ts = timeSeries(as.vector(JohnsonJohnson), dates, units = "JJ")
    class(ts)
    is.timeSeries(ts)
    ts[1:3, ]
    ###
      
    # 'timeSeries' Data and Positions:
    ts.mat = ts@Data
    class(ts.mat)
    ts.mat[1:3, ]
    positions = ts@positions
    positions
    class(positions)
    # ts.df = as.data.frame(ts)
    # ts.df[1:3, ]
    ###
      
    # Apply - Aggregate Annually:
    c(start(ts), end(ts))
    from = timeCalendar(y = 1960:1980, m = 1)
    to = timeCalendar(y = 1960:1980, m = 12, d = 31)
    merge(from, to)[1:3, ]
    applySeries(ts, from, to, FUN = sum)
    ###
       
    # Merge 'timeSeries' with matrix:
    args(merge)
    ts2 = merge(x = ts, y = log(ts))
    ts2@units = c("JJ", "logJJ")
    colnames(ts2@Data) = ts2@units
    ts2[1:3, ]
    ###
     
    # Cut Out a Piece from a 'timeSeries':
    args(cut)
    # The Last 2 years:
    cut(ts2, from = "1979-01-31", to = "2001-12-31")
    ###
    
  
# ------------------------------------------------------------------------------


### Example: Daily Data
    
    
    # Create a 'timeSeries' Object:
    data(msftdat)
    MSFT.OPEN = msftdat[,"Open"]
    CHARVEC = as.character(msftdat[, 1])
    ts = timeSeries(data = MSFT.OPEN, charvec = CHARVEC, 
        units = "MSFT.OPEN", tz = "GMT", FinCenter = "GMT")
    class(ts)
    ts[1:3, ]
    c(start(ts), end(ts))
    ###
    
    # You can also use:
    ts = as.timeSeries(msftdat)
        
    # Cut out April Data from 2001:
    ts.Apr01 = cut(ts, "2001-04-01", "2001-04-30") 
    ts.Apr01
    ###
        
    # Compute Returns:
    args(returnSeries)
    # Continuous Returns:
    returnSeries(ts.Apr01)
    # Discrete Returns:
    returnSeries(ts.Apr01, type = "discrete")
    # Don't trim:
    returnSeries(ts.Apr01, trim = FALSE)
    # Use Percentage Values:
    returnSeries(ts.Apr01, percentage = TRUE, trim = FALSE)
    ###
    
    # Merge Series with Returns:
    # Include last Day from March:
    ts.Apr01 = cut(ts, "2001-03-31", "2001-04-30") 
    ts.merged = merge(x = ts.Apr01[, "Open"],
        y = returnSeries(ts.Apr01[, "Open"], trim = FALSE),
        units = c("MSFT.PRICE", "MSF.RETURN"))
    ts.merged
    ###
        
    # Align with NA:
    args(alignDailySeries)
    ts.ret = returnSeries(ts.Apr01, trim = TRUE)
    GoodFriday(2001)   # is a holiday
    EasterMonday(2001) # is not a holiday !?
    alignDailySeries(ts.ret, method = "fillNA")
    alignDailySeries(ts.ret, method = "fillNA", include.weekends = TRUE)
    ###
        
    # Interpolate:
    ts.ret
    alignDailySeries(ts.ret, method = "interp")
    alignDailySeries(ts.ret, method = "interp", include.weekend = TRUE)
    ###
        
    # Aggregate weekly:
    GoodFriday(2001)
    to = timeSequence(from = "2001-04-06", length.out = 3, by = "week") 
    from = to - 6*24*3600
    applySeries(ts.ret, from, to, FUN = colSums)
    ###
        
    # Plot:
    plot(ts[, "Open"], col = "steelblue", type = "o", pch = 19,
        xlab = "Year", ylab = "Index", main = "MSFT")
    grid()
    ###
    
    
################################################################################

