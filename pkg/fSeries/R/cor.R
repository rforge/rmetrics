#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  ../../COPYING


################################################################################
# FUNCTION:                 COLUMN STATISTICS IN FUTILITIES:
#  cov.timeSeries            Returns  covariance for a 'timeSeries' object
#  cor.timeSeries            Returns correlations for a 'timeSeries' object
################################################################################


# Defined in fUtilites/BasicExtensions.R
# cov = function() UseMethod("cov")
# cov.default <- stats::cov
# cor = function() UseMethod("cor")
# cov.default <- stats::cov


# Add robust methods ...


## cov.timeSeries <-
##     function(x, y = NULL,
##              use = "all.obs", method = c("pearson", "kendall", "spearman"))
## {   # A function implemented by Diethelm Wuertz

##     # Description:
##     #   Returns variance/covariance for a 'timeSeries' object

##     # FUNCTION:

##     # Settings:
##     x = x@Data
##     if (!is.null(y)) y = y@Data

##     # CoVariance:
##     ans = cov.default(x, y, use = use, method = method)

##     # Return Value:
##     ans
## }


## # ------------------------------------------------------------------------------


## cor.timeSeries =
## function(x, y = NULL,
## use = "all.obs",
## method = c("pearson", "kendall", "spearman"))
## {   # A function implemented by Diethelm Wuertz

##     # Description:
##     #   Returns correlations for a 'timeSeries' object

##     # FUNCTION:

##     # Settings:
##     x = x@Data
##     if (!is.null(y)) y = y@Data

##     # CoVariance:
##     ans = cor.default(x, y, use = use, method = method)

##     # Return Value:
##     ans
## }


################################################################################

