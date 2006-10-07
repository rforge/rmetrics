
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
# You should have received A copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                    DESCRIPTION
#  log                           log has become a generic function
#  log.default                   log default method
#  round                         round has become a generic function
#  round.default                 round default method
#  sample                        sample has become a generic function
#  sample.default                sample default method
#  sort                          sort has become a generic function
#  sort.default                  sort default method
#  var                           var has become a generic function
#  var.default                   var default method
# FUNCTION:                     DESCRIPTION:
#  "rownames<-"                  rownames<- has become a generic function
#  "rownames<-.default"          rownames<- default method
#  "colnames<-"                  colnames<- has become a generic function
#  "colnames<-.default"          colnames<- default method
# FUNCTION:                     DESCRIPTION:
#  as.matrix.ts                  Converts univariate ts to 1-column matrix
#  as.matrix.mts                 Converts multivariate ts to matrix
# FUNCTION:                     DESCRIPTION:
#  .description                  Sets default description string
#  .unirootNA                    Computes zero without error exit    
#  .interp                       Does Akima Spline Interpolation
# FUNCTION:                     DESCRIPTION:
#  modify                        Modifies a 'timeSeries' object
#  modify.default                Default Method
#  atoms                         Extracts atoms from 'timeSeries' object
#  atoms.default                 Default Method
# FUNCTION:                     DESCRIPTION:
#  .datax                        Loads timeSeries objects from demo files
# FUNCTION/VALUE:               DESCRIPTION: 
#  currentYear                   Sets date of the current year
#  .currentYear                  Sets date of the current year
#  myUnits                       Sets date units
# DATA:                         DATA:
#  MSFT                          Microsoft data set from Yahoo
# FUNCTION:                     DESCRIPTION [REQUIRES DATE]:
#  .fjulian                      Transform formatted dates to julian day numbers
#  .julian                       Implements SPlus like 'julian'
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(RmetricsUtilities); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.genfun =
function()
{
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
    
    # Time Series:
    MSFT.TS = as.timeSeries(MSFT.df)
    print(head(log(MSFT.TS)))
    print(tail(log(MSFT.TS)))
    MEAN = mean(MSFT.TS@Data)
    checkEquals(
        target = MEAN, 
        current = 9875599, 
        tolerance = 1)
    
    # Return Value:
    return() 
}


# ------------------------------------------------------------------------------


if (FALSE) {
test.julian =
function()
{
    # Requirement:
    require(date)

    # .fjulian -
    fdates = c("8/11/73", "08-11-73", "August 11 1973", "Aug11/73")
    FJ = .fjulian(fdates) 
    print(FJ)
    checkIdentical(
        target = class(FJ), 
        current = "numeric")
    checkIdentical(
        target = FJ, 
        current = c(4971, 4971, 4971, 4971))
    
    # .fjulian -
    fdates = c("11/8/73", "11-08-73", "11 August 1973", "11Aug73")
    FJ = .fjulian(fdates, order = 'dmy')
    print(FJ) 
    checkIdentical(
        target = class(FJ), 
        current = "numeric")
    checkIdentical(
        target = FJ, 
        current = c(4971, 4971, 4971, 4971))
    
    # .julian - 
    # day.of.week -
    # The number of days from January 1, 1990 to each of:
    # January 15, 1990, February 15, 1991, March 15, 1992, etc.
    JULIAN = .julian(1:12, rep(15,12), 1990+(0:11), origin = c(1, 1, 1990))
    print(JULIAN)
    COUNTS = c(14,410,804,1200,1595,1991,2387,2783,3179,3574,3971,4366)
    checkIdentical(
        target = JULIAN, 
        current = COUNTS)
    # November 12, 98, was a Wednesday.
    DOW = .day.of.week(m = 11, d = 12, y = 98)
    print(DOW)
    COUNT = 3
    checkIdentical(
        target = DOW, 
        current = COUNT)
   
    # Return Value:
    return() 
}
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCalendar/test/runit1A.R")
    printTextProtocol(testResult)
}


# ------------------------------------------------------------------------------

