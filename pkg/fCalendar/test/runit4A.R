
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
   
    # Microsoft Data:
    MSFT.df = data.frame(matrix(c(
    20010326, 57.1250, 57.5000, 55.5625, 56.0625,  31559300,
    20010327, 56.0625, 58.5625, 55.8750, 58.2500,  47567800,
    20010328, 57.3750, 57.9375, 55.3750, 55.5625,  39340800,
    20010329, 55.3750, 57.1875, 54.5625, 55.3750,  43492500,
    20010330, 55.7500, 56.1875, 53.8750, 54.6875,  45600800,
    20010402, 54.8125, 56.9375, 54.6250, 55.8125,  37962000,
    20010403, 55.3125, 55.3125, 52.7500, 53.3750,  47093800,
    20010404, 53.3750, 55.0000, 51.0625, 51.9375,  52023300,
    20010405, 53.7500, 57.3750, 53.5000, 56.7500,  56682000,
    20010406, 56.3750, 57.1875, 55.0625, 56.1875,  46311000,
    20010409, 56.5700, 57.4200, 55.6600, 57.1500,  28147800,
    20010410, 57.9500, 60.0900, 57.7800, 59.6800,  54599700,
    20010411, 60.6500, 61.5000, 59.7000, 60.0400,  54939800,
    20010412, 59.5600, 62.3100, 59.3500, 62.1800,  43760000,
    20010416, 61.4000, 61.5800, 60.1200, 60.7900,  32928700,
    20010417, 60.5200, 62.1100, 60.0400, 61.4800,  42574600,
    20010418, 63.3900, 66.3100, 63.0000, 65.4300,  78348200,
    20010419, 65.8100, 69.0000, 65.7500, 68.0400,  79687800,
    20010420, 70.3000, 71.1000, 68.5000, 69.0000,  96459800,
    20010423, 68.1100, 68.4700, 66.9000, 68.2500,  46085600,
    20010424, 68.2000, 69.9300, 67.1400, 67.5500,  44588300,
    20010425, 67.5700, 69.7900, 67.2500, 69.6900,  38372000,
    20010426, 70.0700, 71.0000, 68.2500, 69.1300,  59368800,
    20010427, 69.5300, 69.6800, 66.2100, 67.1200,  60786200,
    20010430, 68.5300, 69.0600, 67.6800, 67.7500,  37184100,
    20010501, 67.6600, 70.3000, 67.6000, 70.1700,  41851400,
    20010502, 71.0000, 71.1500, 69.3500, 69.7600,  46432200,
    20010503, 69.2500, 70.1800, 68.1400, 68.5300,  33136700,
    20010504, 68.0000, 71.0500, 67.9600, 70.7500,  59769200,
    20010507, 70.8300, 72.1500, 70.7000, 71.3800,  54678100), 
    byrow = TRUE, ncol = 6))
    colnames(MSFT.df) = c("YYMMDD", "Open", "High", "Low", "Close", "Volume")
    X = as.timeSeries(MSFT.df)
    
    # Read Data Frame:
    # read.table("library/fBasics/data/msft.dat.csv", sep = ";") 
    
    # Read Time Series:
    # X = readSeries("library/fBasics/data/msft.dat.csv")  
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

    # Microsoft Data:
    MSFT.df = data.frame(matrix(c(
    20010326, 57.1250, 57.5000, 55.5625, 56.0625,  31559300,
    20010327, 56.0625, 58.5625, 55.8750, 58.2500,  47567800,
    20010328, 57.3750, 57.9375, 55.3750, 55.5625,  39340800,
    20010329, 55.3750, 57.1875, 54.5625, 55.3750,  43492500,
    20010330, 55.7500, 56.1875, 53.8750, 54.6875,  45600800,
    20010402, 54.8125, 56.9375, 54.6250, 55.8125,  37962000,
    20010403, 55.3125, 55.3125, 52.7500, 53.3750,  47093800,
    20010404, 53.3750, 55.0000, 51.0625, 51.9375,  52023300,
    20010405, 53.7500, 57.3750, 53.5000, 56.7500,  56682000,
    20010406, 56.3750, 57.1875, 55.0625, 56.1875,  46311000,
    20010409, 56.5700, 57.4200, 55.6600, 57.1500,  28147800,
    20010410, 57.9500, 60.0900, 57.7800, 59.6800,  54599700,
    20010411, 60.6500, 61.5000, 59.7000, 60.0400,  54939800,
    20010412, 59.5600, 62.3100, 59.3500, 62.1800,  43760000,
    20010416, 61.4000, 61.5800, 60.1200, 60.7900,  32928700,
    20010417, 60.5200, 62.1100, 60.0400, 61.4800,  42574600,
    20010418, 63.3900, 66.3100, 63.0000, 65.4300,  78348200,
    20010419, 65.8100, 69.0000, 65.7500, 68.0400,  79687800,
    20010420, 70.3000, 71.1000, 68.5000, 69.0000,  96459800,
    20010423, 68.1100, 68.4700, 66.9000, 68.2500,  46085600,
    20010424, 68.2000, 69.9300, 67.1400, 67.5500,  44588300,
    20010425, 67.5700, 69.7900, 67.2500, 69.6900,  38372000,
    20010426, 70.0700, 71.0000, 68.2500, 69.1300,  59368800,
    20010427, 69.5300, 69.6800, 66.2100, 67.1200,  60786200,
    20010430, 68.5300, 69.0600, 67.6800, 67.7500,  37184100,
    20010501, 67.6600, 70.3000, 67.6000, 70.1700,  41851400,
    20010502, 71.0000, 71.1500, 69.3500, 69.7600,  46432200,
    20010503, 69.2500, 70.1800, 68.1400, 68.5300,  33136700,
    20010504, 68.0000, 71.0500, 67.9600, 70.7500,  59769200,
    20010507, 70.8300, 72.1500, 70.7000, 71.3800,  54678100), 
    byrow = TRUE, ncol = 6))
    colnames(MSFT.df) = c("YYMMDD", "Open", "High", "Low", "Close", "Volume")
   
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

    # Microsoft Data:
    MSFT.df = data.frame(matrix(c(
    20010326, 57.1250, 57.5000, 55.5625, 56.0625,  31559300,
    20010327, 56.0625, 58.5625, 55.8750, 58.2500,  47567800,
    20010328, 57.3750, 57.9375, 55.3750, 55.5625,  39340800,
    20010329, 55.3750, 57.1875, 54.5625, 55.3750,  43492500,
    20010330, 55.7500, 56.1875, 53.8750, 54.6875,  45600800,
    20010402, 54.8125, 56.9375, 54.6250, 55.8125,  37962000,
    20010403, 55.3125, 55.3125, 52.7500, 53.3750,  47093800,
    20010404, 53.3750, 55.0000, 51.0625, 51.9375,  52023300,
    20010405, 53.7500, 57.3750, 53.5000, 56.7500,  56682000,
    20010406, 56.3750, 57.1875, 55.0625, 56.1875,  46311000,
    20010409, 56.5700, 57.4200, 55.6600, 57.1500,  28147800,
    20010410, 57.9500, 60.0900, 57.7800, 59.6800,  54599700,
    20010411, 60.6500, 61.5000, 59.7000, 60.0400,  54939800,
    20010412, 59.5600, 62.3100, 59.3500, 62.1800,  43760000,
    20010416, 61.4000, 61.5800, 60.1200, 60.7900,  32928700,
    20010417, 60.5200, 62.1100, 60.0400, 61.4800,  42574600,
    20010418, 63.3900, 66.3100, 63.0000, 65.4300,  78348200,
    20010419, 65.8100, 69.0000, 65.7500, 68.0400,  79687800,
    20010420, 70.3000, 71.1000, 68.5000, 69.0000,  96459800,
    20010423, 68.1100, 68.4700, 66.9000, 68.2500,  46085600,
    20010424, 68.2000, 69.9300, 67.1400, 67.5500,  44588300,
    20010425, 67.5700, 69.7900, 67.2500, 69.6900,  38372000,
    20010426, 70.0700, 71.0000, 68.2500, 69.1300,  59368800,
    20010427, 69.5300, 69.6800, 66.2100, 67.1200,  60786200,
    20010430, 68.5300, 69.0600, 67.6800, 67.7500,  37184100,
    20010501, 67.6600, 70.3000, 67.6000, 70.1700,  41851400,
    20010502, 71.0000, 71.1500, 69.3500, 69.7600,  46432200,
    20010503, 69.2500, 70.1800, 68.1400, 68.5300,  33136700,
    20010504, 68.0000, 71.0500, 67.9600, 70.7500,  59769200,
    20010507, 70.8300, 72.1500, 70.7000, 71.3800,  54678100), 
    byrow = TRUE, ncol = 6))
    colnames(MSFT.df) = c("YYMMDD", "Open", "High", "Low", "Close", "Volume")
   
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


test.modelSeries =
function()
{
    Matrix = cbind(X = rnorm(10), Y = rnorm(10))
    Matrix = cbind(Matrix, Z = Matrix[,"Y"] - Matrix[, "X"])
    TS = dummyDailySeries(Matrix, units = c("X", "Y", "Z") )
    head(TS)
            
    #.modelSeries(Y ~ ar(2), data = TS, lhs = TRUE)  
    #.modelSeries(log(abs(Z)) ~ lm(X + sin(Y)), data = TS, fake = TRUE)
    #.modelSeries(log(abs(Z)) ~ lm(X + sin(Y)), data = TS, lhs = TRUE)
    
    #.modelSeries(Y ~ ar(2), data = as.data.frame(TS), lhs = TRUE)  
    #.modelSeries(log(abs(Z)) ~ lm(X + sin(Y)), data = TS, fake = TRUE)
    #.modelSeries(log(abs(Z)) ~ lm(X + sin(Y)), data = TS, lhs = TRUE)
    
    #.modelSeries(Y ~ ar(2), data = rnorm(10)) 
    #.modelSeries(Y ~ ar(2), data = as.ts(rnorm(10))) 
    #.modelSeries(x ~ arima(2, 0, 1), data = armaSim(n=10))
    
    #.modelSeries(~ ar(2), rnorm(10))
    
    #attach(TS)
    #.modelSeries(Y ~ ar(2), lhs = TRUE) 
    
    #.modelSeries(Y ~ ar(2) + garch(1,1), data = rnorm(10))
    #.modelSeries(Y ~ ar(2) + garch(1,1), data = rnorm(10), lhs = TRUE)
    #.modelSeries(Y ~ ar(2) + garch(1,1), data = TS, lhs = TRUE)
    
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
   
    