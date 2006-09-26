
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
        example(ISO8601Standard); return() }
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
    # Time Series:
    data(msftdat)
    MSFT.TS = as.timeSeries(msftdat)
    print(head(log(MSFT.TS)))
    print(tail(log(MSFT.TS)))
    MEAN = mean(MSFT.TS@Data)
    checkEquals(
        target = MEAN, 
        current = 8628750, 
        tolerance = 1)
    
    # Return Value:
    return() 
}


# ------------------------------------------------------------------------------


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


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCalendar/test/runit1A.R")
    printTextProtocol(testResult)
}


# ------------------------------------------------------------------------------

