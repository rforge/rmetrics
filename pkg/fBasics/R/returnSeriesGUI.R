
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                GRAPHICAL USER INTERFACE:
#  returnSeriesGUI          pens a GUI for return series plots
################################################################################


returnSeriesGUI <-
    function(x)
{
    # A function implemented by Diethelm Wuertz

    # Descriptions:
    #   Opens a GUI for return series plots

    # Arguments:
    #   x - an uni- or multivariate timeSeries object

    # FUNCTION:

    # Check:
    stopifnot(class(x) == "timeSeries")

    # Settings:
    N = ceiling(sqrt(ncol(x)))
    mfrow = c(N, N)

    returnSeriesRefreshCode =
    function(...)
    {
        # Settings:
        selectedAsset  = fBasics:::.tdSliderMenu(no = 1)
        type = as.integer(fBasics:::.tdSliderMenu(obj.name = "returnSeriesType"))
        Unit = colnames(x)

        # Print Basic Return Statistics:
        if (type == 1) {
            if (selectedAsset == 0) {
                print(basicStats(x))
            } else {
                print(basicStats(x[, selectedAsset]))
            }
        }

        # Return Series Plot:
        if (type == 2) {
            if (selectedAsset == 0) {
                par(mfrow = mfrow)
                seriesPlot(x)
            } else {
                par(mfrow = c(1, 1))
                seriesPlot(x[, selectedAsset])
            }
        }

        # Cumulate Return Series Plot
        if (type == 3) {
            if (selectedAsset == 0) {
                par(mfrow = mfrow)
                seriesPlot(100*exp(colCumsums(x)))
            } else {
                par(mfrow = c(1, 1))
                seriesPlot(100*exp(colCumsums(x[, selectedAsset])))
                abline(h = 100, col = "grey")
            }
        }

        # Histogram Plot:
        if (type == 4) {
            if (selectedAsset == 0) {
                par(mfrow = mfrow)
                histPlot(x, skipZeros = TRUE)
            } else {
                par(mfrow = c(1, 1))
                histPlot(x[, selectedAsset], skipZeros = TRUE)
            }
        }

        # Density Plot:
        if (type == 5) {
            if (selectedAsset == 0) {
                par(mfrow = mfrow)
                densityPlot(x)
            } else {
                par(mfrow = c(1, 1))
                densityPlot(x[, selectedAsset])
            }
        }

        # Normal QQ Plot:
        if (type == 6) {
            if (selectedAsset == 0) {
                par(mfrow = mfrow)
                qqnormPlot(x)
            } else {
                par(mfrow = c(1, 1))
                qqnormPlot(x[, selectedAsset])
            }
        }

    }

    nAssets = dim(x)[2]

    fBasics:::.tdSliderMenu(
        returnSeriesRefreshCode,

        names       = c("Selected Asset"),
        minima      = c(      0),
        maxima      = c(      nAssets),
        resolutions = c(      1),
        starts      = c(      0),

        but.functions = list(
            function(...){
                fBasics:::.tdSliderMenu(obj.name = "returnSeriesType", obj.value = "1")
                returnSeriesRefreshCode()},
            function(...){
                fBasics:::.tdSliderMenu(obj.name = "returnSeriesType", obj.value = "2")
                returnSeriesRefreshCode()},
            function(...){
                fBasics:::.tdSliderMenu(obj.name = "returnSeriesType", obj.value = "3")
                returnSeriesRefreshCode()},
            function(...){
                fBasics:::.tdSliderMenu(obj.name = "returnSeriesType", obj.value = "4")
                returnSeriesRefreshCode()},
            function(...){
                fBasics:::.tdSliderMenu(obj.name = "returnSeriesType", obj.value = "5")
                returnSeriesRefreshCode()},
            function(...){
                fBasics:::.tdSliderMenu(obj.name = "returnSeriesType", obj.value = "6")
                returnSeriesRefreshCode()}
        ),

        but.names = c(
            "1 Basic Return Statistics Table",
            "2 Return Series Plot",
            "3 Cumulated Return Series Plot",
            "4 Histogram Plot",
            "5 Density Plot",
            "6 Normal Quantile-Quantile Plot"),

        title = "Return Series GUI"
        )

   fBasics:::.tdSliderMenu(obj.name = "type", obj.value = "1", no = 1)

   # Return Value()
   invisible()
}


################################################################################

