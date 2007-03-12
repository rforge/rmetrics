
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
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# METHOS:              POSITIONS:
#  seriesPositions      Extracts positions slot from 'timeSeries' object
#  newPositions<-       Modifies positions of a 'timeSeries' object
# METHODS:             ORDERING:
#  sample.timeSeries    S3: Resamples a 'timeSeries' object in time
#  sort.timeSeries      S3: Sorts reverts a 'timeSeries' object in time
#  rev.timeSeries       S3: Reverts a 'timeSeries' object in time 
#  start.timeSeries     S3: Extracts start date of a 'timeSeries' object 
#  end.timeSeries       S3: Extracts end date of a 'timeSeries' object
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(TimeSeriesPositions, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.seriesPositions = 
function()
{
    # Generate nivariate daily random sequence
    set.seed(4711)
    data = round(rnorm(12), 2)
    charvec = timeCalendar(2006)
    uTS = timeSeries(data, charvec, units = "uTS")
    uTS
    
    # Get Positions:
    POS = seriesPositions(uTS)
    POS
    charvec = as.vector(as.character(POS))
    checkTrue(inherits(POS, "timeDate"))
    attr(uTS@positions, "control")<-NULL
    checkIdentical(target = charvec, current = uTS@positions)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.seriesPositions = 
function()
{
    # Generate nivariate daily random sequence
    set.seed(4711)
    data = round(rnorm(12), 2)
    charvec = timeCalendar(2006)
    uTS = timeSeries(data, charvec, units = "uTS")
    uTS
    
    # Add one Day to Positions:
    newPositions(uTS) <- POS + 24*3600          # CHECK
    uTS
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.timeSeriesOrdering = 
function()
{
    #  sample.timeSeries    Resamples a 'timeSeries' object in time
    #  sort.timeSeries      Sorts reverts a 'timeSeries' object in time
    #  rev.timeSeries       Reverts a 'timeSeries' object in time 
    #  start.timeSeries     Extracts start date of a 'timeSeries' object 
    #  end.timeSeries       Extracts end date of a 'timeSeries' object
    
    # Generate univariate monthly random sequence:
    set.seed(4711)
    data = cbind(1:12, round(rnorm(12), 2))
    positions = timeCalendar(2006)
    uTS = timeSeries(data, positions)
    uTS
    
    # Sample/Sort: 
    target = uTS
    target
    # current = sort(sample(uTS))
    # current
    # checkIdentical(target, current)

    # Revert:
    target = uTS
    target
    current = rev(rev(uTS))
    current
    target - current
    
    # Start/End date of Series:
    start(uTS)  
    end(uTS)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCalendar/test/runit4C.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}


################################################################################
   
    

   