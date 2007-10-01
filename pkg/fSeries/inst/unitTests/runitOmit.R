
# Rmetrics is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# Rmetrics is distributed in the hope that it will be useful,
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
# FUNCTION:                 DESCRIPTION:
#  na.omit.timeSeries        Handles missing values in objects
#  .naOmitMatrix             Internal function called from na.omit.timeSeries
################################################################################


test.naOmitMatrix =
function()
{
    x = as.timeSeries(data(edhec))[1:20, 1:4]  
    colnames(x) = abbreviate(colnames(x), 6)
    
    x@Data[1, 1] = NA
    x@Data[3:4, 2] = NA
    x@Data[18:20, 4] = NA
    print(x)
    
    .naOmitMatrix(as.matrix(x))
    .naOmitMatrix(as.matrix(x), "s")
    .naOmitMatrix(as.matrix(x), "z")
    .naOmitMatrix(as.matrix(x), "ir")
    .naOmitMatrix(as.matrix(x), "iz")
    .naOmitMatrix(as.matrix(x), "ie")
  
    # Return Value:
    return()    
    
}


# ------------------------------------------------------------------------------


test.na.omit =
function()
{
    x = as.timeSeries(data(edhec))[1:20, 1:4]  
    colnames(x) = abbreviate(colnames(x), 6)
    
    x@Data[1, 1] = NA
    x@Data[3:4, 2] = NA
    x@Data[18:20, 4] = NA
    print(x)
    
    na.omit(x)
    na.omit(x, "s")
    na.omit(x, "z")
    na.omit(x, "ir")
    na.omit(x, "iz")
    na.omit(x, "ie")
    
    # Return Value:
    return()    
}


################################################################################

