
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
    Data <- as.timeSeries(matrix(c(1.075,0.942,0.852,0.815,
                          0.698,1.023,0.851,1.677,
                          1.084,1.020,0.735,0.716,0.662,1.002,0.768,1.722,
                          1.061,1.056,1.371,1.385,1.318,1.123,1.354,0.760,
                          1.052,1.175,1.236,1.266,1.280,1.156,1.025,0.960,
                          1.055,1.002,0.926,0.974,1.093,1.030,1.181,1.200,
                          1.077,0.982,1.064,1.093,1.146,1.012,1.326,1.295,
                          1.109,0.978,1.184,1.256,1.307,1.023,1.048,2.212,
                          1.127,0.947,1.323,1.337,1.367,1.031,1.226,1.296,
                          1.156,1.003,0.949,0.963,0.990,1.073,0.977,0.688,
                          1.117,1.465,1.215,1.187,1.213,1.311,0.981,1.084,
                          1.092,0.985,1.224,1.235,1.217,1.080,1.237,0.872,
                          1.103,1.159,1.061,1.030,0.903,1.150,1.074,0.825,
                          1.080,1.366,1.316,1.326,1.333,1.213,1.562,1.006,
                          1.063,1.309,1.186,1.161,1.086,1.156,1.694,1.216,
                          1.061,0.925,1.052,1.023,0.959,1.023,1.246,1.244,
                          1.071,1.086,1.165,1.179,1.165,1.076,1.283,0.861,
                          1.087,1.212,1.316,1.292,1.204,1.142,1.105,0.977,
                          1.080,1.054,0.968,0.938,0.830,1.083,0.766,0.922,
                          1.057,1.193,1.304,1.342,1.594,1.161,1.121,0.958,
                          1.036,1.079,1.076,1.090,1.174,1.076,0.878,0.926,
                          1.031,1.217,1.100,1.113,1.162,1.110,1.326,1.146,
                          1.045,0.889,1.012,0.999,0.968,0.965,1.078,0.990),
                          byrow = TRUE, ncol = 8))
    rownames(Data) <- c("19731231", "19741231", "19751231",
                        "19761231", "19771231", "19781231",
                        "19791231", "19801231", "19811231",
                        "19821231", "19831231", "19841231",
                        "19851231", "19861231", "19871231",
                        "19881231", "19891231", "19901231",
                        "19911231", "19921231", "19931231",
                        "19941231")
    colnames(Data) <- c("TBills3m", "LongBonds", "SP500",
                        "Wilshire5000", "NASDAQComp", "LehmanBonds",
                        "EAFE", "Gold")

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
    Data <- as.timeSeries(matrix(c(1.075,0.942,0.852,0.815,
                          0.698,1.023,0.851,1.677,
                          1.084,1.020,0.735,0.716,0.662,1.002,0.768,1.722,
                          1.061,1.056,1.371,1.385,1.318,1.123,1.354,0.760,
                          1.052,1.175,1.236,1.266,1.280,1.156,1.025,0.960,
                          1.055,1.002,0.926,0.974,1.093,1.030,1.181,1.200,
                          1.077,0.982,1.064,1.093,1.146,1.012,1.326,1.295,
                          1.109,0.978,1.184,1.256,1.307,1.023,1.048,2.212,
                          1.127,0.947,1.323,1.337,1.367,1.031,1.226,1.296,
                          1.156,1.003,0.949,0.963,0.990,1.073,0.977,0.688,
                          1.117,1.465,1.215,1.187,1.213,1.311,0.981,1.084,
                          1.092,0.985,1.224,1.235,1.217,1.080,1.237,0.872,
                          1.103,1.159,1.061,1.030,0.903,1.150,1.074,0.825,
                          1.080,1.366,1.316,1.326,1.333,1.213,1.562,1.006,
                          1.063,1.309,1.186,1.161,1.086,1.156,1.694,1.216,
                          1.061,0.925,1.052,1.023,0.959,1.023,1.246,1.244,
                          1.071,1.086,1.165,1.179,1.165,1.076,1.283,0.861,
                          1.087,1.212,1.316,1.292,1.204,1.142,1.105,0.977,
                          1.080,1.054,0.968,0.938,0.830,1.083,0.766,0.922,
                          1.057,1.193,1.304,1.342,1.594,1.161,1.121,0.958,
                          1.036,1.079,1.076,1.090,1.174,1.076,0.878,0.926,
                          1.031,1.217,1.100,1.113,1.162,1.110,1.326,1.146,
                          1.045,0.889,1.012,0.999,0.968,0.965,1.078,0.990),
                          byrow = TRUE, ncol = 8))
    rownames(Data) <- c("19731231", "19741231", "19751231",
                        "19761231", "19771231", "19781231",
                        "19791231", "19801231", "19811231",
                        "19821231", "19831231", "19841231",
                        "19851231", "19861231", "19871231",
                        "19881231", "19891231", "19901231",
                        "19911231", "19921231", "19931231",
                        "19941231")
    colnames(Data) <- c("TBills3m", "LongBonds", "SP500",
                        "Wilshire5000", "NASDAQComp", "LehmanBonds",
                        "EAFE", "Gold")


    # Settings:
    class(Data)

    # Test Standard Mean-Covariance:
    args(assetsMeanCov)
    assetsMeanCov(Data)

    # uses "mve" from [MASS]
    assetsMeanCov(Data, FUN = "mveEstimate")

    # uses "mcd" from [MASS]
    assetsMeanCov(Data, FUN = "mcdEstimate")

    # Return Value:
    return()
}


################################################################################
