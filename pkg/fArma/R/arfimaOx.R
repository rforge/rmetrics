
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
# FUNCTION:               DESCRIPTION:
#  * fARMA                 Class representation for "fARMA" objects
#  * armaSim               Simulates a time series process from the ARIMA family
#    arfimaOxFit           Fits parameters for AR(FI)MA time series processes
# S3 METHOD:              PREDICTION:
#  * predict.fARMA         S3: Predicts from an ARMA time series prrocess
# S3 METHOD:              RINT - PLOT - SUMMARY METHODS:
#  * show.fARMA            S4: Prints a fitted ARMA time series object
#  * plot.fARMA            S3: Plots stylized facts of a fitted ARMA object
#  * summary.fARMA         S3: Summarizes a fitted ARMA time series object
# S3 METHOD:              ADDON METHODS:
#  * coef.fARMA            S3: Returns coefficidents from a fitted ARMA object
#  * coefficients.fARMA    S3: Synonyme for coef.fARMA
#  * fitted.fARMA          S3: Returns fitted values from a fitted ARMA object
#  * residuals.fARMA       S3: Returns residuals from a fitted ARMA object
#  *                      Asterisked Functions are in arma*.R
################################################################################


# ------------------------------------------------------------------------------


arfimaOxFit =
function(formula, data, method = c("mle", "nls", "mpl"), trace = TRUE,
title = NULL, description = NULL)
{
print("arfimaOxFit is no more available")
print("If you are interested to use, please contact us")
invisible()
}


# ------------------------------------------------------------------------------

