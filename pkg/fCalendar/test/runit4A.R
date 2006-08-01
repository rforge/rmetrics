
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

# Copyrights (C)
# for this R-port: 
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:            GENERATION OF TIME SERIES OBJECTS:
#  'timeSeries'         S4 Class definition for a 'timeSeries' object
#  timeSeries           Creates a 'timeSeries' object from scratch
#  readSeries           Reads from a spreadsheet and creates a 'timeSeries'
#  returnSeries         Computes returns from a 'timeSeries' object  
#  applySeries          Applies a function to blocks of a 'timeSeries'
#  orderStatistics      Compute order statistic of a 'timeSeries'
# FUNCTION:            DATA SLOT AND CLASSIFICATION OF TIME SERIES OBJECTS:
#  seriesData           Extracts data slot from 'timeSeries' object
#  isUnivariate         Tests if an object of class 'timeSeries' is univariate
#  isMultivariate       Tests if an object of class 'timeSeries' is multivariate
# METHODS:             PRINT AND PLOT FUNCTIONS:
#  print.timeSeries     S3: Print method for a 'timeSeries' object
#  plot.timeSeries      S3: Plot method for a 'timeSeries' object
#  lines.timeSeries     S3: Lines method for a 'timeSeries' object
#  points.timeSeries    S3: Points method for a 'timeSeries' object
# FUNCTION:            FOR DAILY OPERATIONS:
#  dummyDailySeries     Creates a dummy daily 'timeSeries' object
#  alignDailySeries     Aligns a 'timeSeries' object to new positions 
#  ohlcDailyPlot        Plots open–high–low–close bar chart         
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(TimeSeriesClass); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.createTimeSeries = 
function()
{    
    #  timeSeries           Creates a 'timeSeries' object from scratch
    #  readSeries           Reads from a spreadsheet and creates a 'timeSeries'
    #  returnSeries         Computes returns from a 'timeSeries' object  
    #  applySeries          Applies a function to blocks of a 'timeSeries'
    #  orderStatistics      Compute order statistic of a 'timeSeries' 
    
    # Settings:
    myFinCenter <<- "GMT"
    set.seed(4711)
    data = matrix(round(rnorm(12), 3))
    data
    class(data)
    charvec = format(timeCalendar(2006))
    charvec
    class(charvec)
    
    # Univariate daily random sequence
    myFinCenter <<- "GMT"
    uTS = timeSeries(data, charvec, units = "uTS")
    uTS@Data
    print(uTS)
    
    # FinCenter Functionality:
    timeSeries(data, charvec, units = "uTS", zone = "GMT", FinCenter = "GMT")
    timeSeries(data, charvec, units = "uTS", zone = "Zurich", FinCenter = "Zurich")
    timeSeries(data, charvec, units = "uTS", zone = "GMT", FinCenter = "Zurich")
    timeSeries(data, charvec, units = "uTS", zone = "Zurich", FinCenter = "GMT")
   
    # Read Data Frame:
    read.table("library/fBasics/data/msft.dat.csv", sep = ";") 
    
    # Read Time Series:
    X = readSeries("library/fBasics/data/msft.dat.csv")  
    X = X[1:12, ]
    class(X)
    
    # Read Series:
    x = readSeries("src/library/fBasics/data/DowJones30.csv")
    head(x)[,1:5]
    head(x[,1:5])
    head(x[,1:5], 8)
    class(x)
    
    # returnSeries:
    OPEN = X[,1]
    OPEN
    returnSeries(OPEN)
    
    # applySeries:
    
    # ORDER STATISTICS:
    orderStatistics(OPEN)
    orderStatistics(X[, -5])
    orderStatistics(X[, -5])$Open
  
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.classification = 
function()
{    
    #  seriesData       Extracts data slot from 'timeSeries' object
    #  isUnivariate     Tests if an object of class 'timeSeries' is univariate
    #  isMultivariate   Tests if an object of class 'timeSeries' is multivariate

    # Data:
    X = as.timeSeries(MSFT)
    X = X[1:12, ]
    class(X)
    
    # Return Series:
    OPEN = X[, 1]
    OPEN
    returnSeries(OPEN)
    
    # Volatility Series:
    abs(returnSeries(OPEN))
    
    # Data Matrix:
    seriesData(OPEN)
    Y = seriesData(X)
    Y
    class(Y)
    
    # Position Vector:
    PO = seriesPositions(OPEN)
    PO
    PX = seriesPositions(X)
    PX
    class(PX)
    checkEquals(
        target = sum(as.integer(PO - PX)),
        current = 0)

    # Univariate / Multivariate:
    checkTrue(!isUnivariate(X))
    checkTrue(isMultivariate(X))
    checkTrue(isUnivariate(OPEN))
    checkTrue(!isMultivariate(OPEN))
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.displayMethods = 
function()
{    
    #  print.timeSeries     Print method for a 'timeSeries' object
    #  plot.timeSeries      Plot method for a 'timeSeries' object
    #  lines.timeSeries     Lines method for a 'timeSeries' object
    #  points.timeSeries    Points method for a 'timeSeries' object

    # Data:
    X = as.timeSeries(MSFT)
    X = X[1:12, ]
    OPEN = X[, 1]
    
    # Print:
    print(X)
    print(OPEN)
    
    # Plot:
    par(mfrow = c(1, 1))
    plot(OPEN)
    plot(X[, 1:4])
    
    # GMT - Plot:
    tC = timeCalendar(2006, 1, 1, 0:23, 0, 0, zone = "GMT", FinCenter = "GMT")
    tS = timeSeries(data = matrix(rnorm(24), ncol = 1), charvec = tC)
    plot(tS)
    
    # Zurich - Plot:
    tC = timeCalendar(2006, 1, 1, 0:23, 0, 0, zone = "GMT", FinCenter = "Zurich")
    tS = timeSeries(data = matrix(rnorm(24), ncol = 1), charvec = tC,
        zone = "GMT", FinCenter = "Zurich")
    plot(tS)
    
    # New York - Plot:
    tC = timeCalendar(2006, 1, 1, 0:23, 0, 0, zone = "GMT", FinCenter = "NewYork")
    tS = timeSeries(data = matrix(rnorm(24), ncol = 1), charvec = tC,
        zone = "GMT", FinCenter = "NewYork")
    plot(tS, type = "h")
    lines (tS, col = "red",  lty = 3)
    points(tS, col = "blue", pch = 19)
    abline(h=0, col = "grey")
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.dailyOps = 
function()
{    
    #  dummyDailySeries     Creates a dummy daily 'timeSeries' object
    #  alignDailySeries     Aligns a 'timeSeries' object to new positions 
    #  ohlcDailyPlot        Plots open–high–low–close bar chart 

    # Create Dummy Time Series:
    myFinCenter = "GMT"
    dummyDailySeries(rnorm(12))

    # alignDailySeries()
    
    # ohlcDailyPlot(X)

    # Return Value:
    return()    
}
 

# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCalendar/test/runit4A.R")
    printTextProtocol(testResult)
}


################################################################################
   
    