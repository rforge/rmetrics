
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
# METHODS:             CREATE A TIMESERIES FROM OTHER OBJECTS:
#  is.timeSeries        S3: Tests for a 'timeSeries' object
#  as.timeSeries        S3: Defines method for a 'timeSeries' object
#  as.timeS*.default    S3: Returns the input
#  as.timeS*.numeric    S3: Transforms a numeric vector into a 'timeSeries'
#  as.timeS*.data.frame S3: Transformas a 'data.frame' into a 'timeSeries'
#  as.timeS*.matrix     S3: Transformas a 'matrix' into a 'timeSeries'
#  as.timeS*.ts         S3: Transforms a 'ts' object into a 'timeSeries'
#  as.timeS*.character  S3: Loads and transformas from a demo file
#  as.timeS*.zoo        S3: Transforms a 'zoo' object into a 'timeSeries'
# METHODS:             TRANSFORM A TIMESERIES INTO OTHER OBJECTS:
#  as.vector.timeS*     S3: Converts a univariate 'timeSeries' to a vector
#  as.matrix.timeS*     S3: Converts a 'timeSeries' to a 'matrix'
#  as.data.frame.t*     S3: Converts a 'timeSeries' to a 'data.frame'
#  as.ts.timeSeries     S3: Converts a 'timeSeries' to a 'ts'     
# NEW METHODS 
#  .as.vector.zoo
#  .as.matrix.zoo
#  .quantile.zoo
#  .t.timeSeries
#  .mergeSeries
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(timeSeriesCoercion); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.asTimeSeries = 
function()
{
    # as.timeSeries.default     Returns the input
    # as.timeSeries.numeric     Transforms a numeric vector into a 'timeSeries'
    # as.timeSeries.data.frame  Transformas a 'data.frame' into a 'timeSeries'
    # as.timeSeries.matrix      Trasformas a 'matrix' into a 'timeSeries'
    # as.timeSeries.ts          Tranf orms a 'ts' object into a 'timeSeries'
    # as.timeSeries.character   Loads and transformas from a demo file
    # as.timeSeries.zoo         Transforms a 'zoo' object into a 'timeSeries'
    

    set.seed(4711)
    data = round(rnorm(12), 3)
    charvec = timeCalendar(2006)
    uTS = timeSeries(data, charvec, units = "uTS")
    uTS
    checkTrue(inherits(uTS, "timeSeries"))
    checkTrue(is.timeSeries(uTS))

    x = rnorm(12)
    as.timeSeries(x)
    timeSeries(x)
    
    x = data.frame(rnorm(12))
    as.timeSeries(x)
    
    x = matrix(rnorm(12))
    as.timeSeries(x)
    timeSeries(x)
      
    x = as.ts(rnorm(12))
    as.timeSeries(x)
    timeSeries(x)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.asTimeSeriesDJ1 = 
function()
{   
    # Load Data:
    require(fBasics)
    data(DowJones30) 
    
    # Taking Dates from First Column:
    DJ = DowJones30[21:30, c(1,11:15)]
    DJ
    class(DJ) 
    as.timeSeries(DJ)
   
    # Adding Dates through Rownames Assignment:
    DJ = DowJones30[21:30, c(11:15)]
    rownames(DJ)<-DowJones30[21:30, 1]
    DJ
    as.timeSeries(DJ)
   
    # Missing Dates - Using Dummy Dates:
    DJ = DowJones30[21:30, c(11:15)]
    DJ
    class(DJ)
    as.timeSeries(DJ)
   
    # With recordIDs:  
    if (FALSE) {
        DJ = DowJones30[21:30, c(1,11:15)]
        DJ = cbind(DJ, LETTERS[1:10])
        class(DJ)
        tsDJ = as.timeSeries(DJ)
        tsDJ
        tsDJ@recordIDs
    }
   
    DJ = DowJones30[21:30, c(11:15)]
    rownames(DJ) = DowJones30[21:30, 1]
    DJ = cbind(DJ, LETTERS[1:10])
    tsDJ = as.timeSeries(DJ)
    tsDJ
    tsDJ@recordIDs
   
    DJ = DowJones30[21:30, c(11:15)]
    DJ =cbind(DJ, LETTERS[1:10])
    tsDJ = as.timeSeries(DJ)
    tsDJ
    tsDJ@recordIDs

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.fromTimeSeriesUV = 
function()
{     
    # as.vector.timeSeries      Converts a univariate 'timeSeries' to a vector
    # as.matrix.timeSeries      Converts a 'timeSeries' to a 'matrix'
    # as.data.frame.timeSeries  Converts a 'timeSeries' to a 'data.frame'
    # as.ts.timeSeries          Converts a 'timeSeries' to a 'ts'
    
   
    # Univariate Case:
    set.seed(4711)
    data = round(rnorm(12), 3)
    charvec = timeCalendar(2006)
    uTS = timeSeries(data, charvec, units = "uTS")
    uTS
    
    # Vector:
    VEC = as.vector(uTS) 
    head(VEC)
    class(VEC)
    checkIdentical(class(VEC), "numeric")
    
    # Numeric:
    VEC = as.numeric(uTS) 
    head(VEC)
    class(VEC)
    checkIdentical(class(VEC), "numeric")
    
    # Matrix:
    MAT = as.matrix(uTS)  
    head(MAT)
    class(MAT)
    checkIdentical(class(MAT), "matrix")
    checkIdentical(target = MAT[,1], current = VEC)
    
    # Data Frame:
    DF = as.data.frame(uTS)  
    head(DF)
    checkIdentical(class(DF), "data.frame")
    checkIdentical(target = as.matrix(DF)[,1], current = VEC)
    
    # Time Series:
    TS = as.ts(uTS)
    head(TS)
    class(TS)
    checkIdentical(class(TS), "ts")
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.fromTimeSeriesMV = 
function()
{     
    # as.vector.timeS*     Converts a univariate 'timeSeries' to a vector
    # as.matrix.timeS*     Converts a 'timeSeries' to a 'matrix'
    # as.data.frame.t*     Converts a 'timeSeries' to a 'data.frame'
    # as.ts.timeSeries     Converts a 'timeSeries' to a 'ts'
    
   
    # Multivariate Case:
    set.seed(4711)
    data = matrix(round(rnorm(24), 3), ncol = 2)
    charvec = timeCalendar(2006)
    mTS = timeSeries(data, charvec)
    mTS
     
    # Matrix:
    MAT = as.matrix(mTS)  
    head(MAT)
    class(MAT)
    checkIdentical(
        target = class(MAT), 
        current = "matrix")
    checkIdentical(
        target = as.vector(MAT[, 1]), 
        current = as.numeric(MAT)[1:12])
    
    # Data Frame:
    DF = as.data.frame(mTS)  
    head(DF)
    class(DF)
    checkIdentical(
        target = class(DF), 
        current = "data.frame")
    
    # Time Series:
    TS = as.ts(mTS)
    head(TS)
    class(TS)
    checkIdentical(
        target = class(TS), 
        current = c("mts", "ts"))
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCalendar/test/runit4D.R")
    printTextProtocol(testResult)
}


################################################################################
   
    