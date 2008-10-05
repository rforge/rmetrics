
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
# FUNCTION:             ASSETS STATISTICS:
#  assetsStats           Computes basic statistics of a set of assets
# FUNCTION:             MEAN-COVARIANCE ESTIMATION:
#  assetsMeanCov         Estimates mean and variance for a set of assets
################################################################################


test.assetsStats =
function()
{
    # Time Series Object:
    Data <- as.timeSeries(LPP2005REC)[, 1:6]

    # Settings:
    class(Data)
    head(Data)

    # Statistics:
    assetsStats(as.matrix(Data))

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsMeanCov =
function()
{

    # Time Series Object:
    Data <- as.timeSeries(LPP2005REC)[, 1:6]

    # Settings:
    class(Data)

    # use "cov":
    args(assetsMeanCov)
    assetsMeanCov(Data)

    # use "shrink"
    assetsMeanCov(Data, "shrink")

    # Return Value:
    return()
}


################################################################################
