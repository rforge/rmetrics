
# This R package is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This R package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this R package; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port:
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
#__ and Martin Maechler, ETH Zurich __
#
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTIONS:            STABLE DISTRIBUTION:
#  stableMode            Computes stable mode
#  dstable               Returns density for stable DF
#  pstable               Returns probabilities for stable DF
#  qstable               Returns quantiles for stable DF
#  rstable               Returns random variates for stable DF
################################################################################

## FIXME / TODO:  need unit tests  for   stableMode()  !!


if(do.stable.rUnitTest <-
   Sys.getenv("USER") == "maechler" && require("fBasics") &&
   ## need the newer distCheck():
   packageDescription("fBasics")$Version >= package_version("2110.79"))
{
    ## fBasics:  for  .distCheck()
    distCheck <- fBasics:::.distCheck
    environment(distCheck) <- asNamespace("stabledist")
    ## and re-attach "stabledist" as its contents is now masked by fBasics::dstable:
    if((P <- "package:stabledist") %in% search())
	detach(P, character.only=TRUE)
    stopifnot(require("stabledist"),
	      ## check that indeed we get stabledist's functions, not fBasics:
	      identical(dstable, stabledist::dstable))
}

test.stableS0 <- function()
{
    if (do.stable.rUnitTest) {
        ## "FIXME" in distCheck() -- would like to use  'tol = .005' for density --
        ##         but then it would have to work for rstable() too
        # stable - Parameterization S0:
        test <- distCheck("stable", alpha = 1.8, beta = 0.3)
        print(test)
        ## the 3rd test -- matching (mean, var)  typically fails for stable -- as Var(.) == Inf !
        checkTrue(mean(test[1:2]) == 1)

        # stable - Parameterization S0:
        test <- distCheck("stable", alpha = 1.2, beta = -0.3)
        print(test)
        checkTrue(mean(test[1:2]) == 1)

        # stable - Parameterization S0:
        test <- distCheck("stable", alpha = 0.6, beta = 0)
        print(test)
        checkTrue(mean(test[1:2]) == 1)
    }

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.stableS1 <- function()
{
    if (do.stable.rUnitTest) {
        # stable - Parameterization S1:
        test <- distCheck("stable", alpha = 1.8, beta = 0.3, pm = 1)
        print(test)
        checkTrue(mean(test[1:2]) == 1)

        # stable - Parameterization S1:
        test <- distCheck("stable", alpha = 1.2, beta = -0.3, pm = 1)
        print(test)
        checkTrue(mean(test[1:2]) == 1)

        # stable - Parameterization S1:
        test <- distCheck("stable", alpha = 0.6, beta = 0, pm = 1)
        print(test)
        checkTrue(mean(test[1:2]) == 1)
    }

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.stableS2 <- function()
{
    if (do.stable.rUnitTest) {
        # stable - Parameterization S2:
        test <- distCheck("stable", alpha = 1.8, beta = 0.3, pm = 2)
        print(test)
        checkTrue(mean(test[1:2]) == 1)

        # stable - Parameterization S2:
        test <- distCheck("stable", alpha = 1.2, beta = -0.3, pm = 2)
        print(test)
        checkTrue(mean(test[1:2]) == 1)

        # stable - Parameterization S2:
        test <- distCheck("stable", alpha = 0.6, beta = 0, pm = 2)
        print(test)
        checkTrue(mean(test[1:2]) == 1)
    }

    # Return Value:
    return()
}

################################################################################

