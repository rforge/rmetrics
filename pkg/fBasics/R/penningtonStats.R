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
# FUNCTION:                  DESCRIPTION:
#  penningtonStats            Computes Pennington statistics
################################################################################


## .penningtonStats =
##     function(x, ...)
## {
##     # A function implemented by Diethelm Wuertz

##     # Description:
##     #   Computes Pennington statistics

##     # Arguments:
##     #   x - an univariate timeSeries object, e.g. a price or index series
##     #   ... - arguments passed to the function na.omit()

##     # Example:
##     #   .penningtonStats(as.timeSeries(data(msft.dat))[, 4])

##     # FUNCTION:

##     # Settings:
##     stopifnot(isUnivariate(x))

##     # Handle Missing Values:
##     x = na.omit(x, ...)

##     # Convert to Vector:
##     X = x
##     x = as.vector(x)

##     # Result:
##     stats = t(t(round(.stat.pen.pastecs(x), 6)))
##     colnames(stats) <- colnames(X)
##     data.frame(stats)
## }


## ################################################################################
## # Package: pastecs
## # Title: Package for Analysis of Space-Time Ecological Series
## # Version: 1.3-4
## # Date: 2006-11-28
## # Author: Frederic Ibanez <ibanez@obs-vlfr.fr>,
## #   Philippe Grosjean <phgrosjean@sciviews.org> &
## #   Michele Etienne <etienne@obs-vlfr.fr>
## # Description: Regulation, decomposition and analysis of space-time series.
## #   The pastecs library is a PNEC-Art4 and IFREMER
## #   (Benoit Beliaeff <Benoit.Beliaeff@ifremer.fr>) initiative
## #   to bring PASSTEC 2000
## #   (http://www.obs-vlfr.fr/~enseigne/anado/passtec/passtec.htm)
## #   functionnalities to R.
## # URL: http://www.sciviews.org/pastecs
## # Maintainer: Philippe Grosjean <phgrosjean@sciviews.org>
## # License: GNU Public Licence 2.0 or above at your convenience
## # Depends: boot, stats
## # Packaged: Tue Nov 28 15:33:42 2006; Philippe Grosjean


## .stat.pen.pastecs <-
##     function(x, basic=FALSE, desc=FALSE)
## {
##     # This is the body of stat.pen
##     Basic <- basic
##     Desc <- desc
##     # If x is a vector, stat.pen returns a vector with results
##     if (is.vector(x)) {
##         .stat.pen.vec.pastecs(x, Basic, Desc)
##     } else {
##         # If x is not a vector, it is treated as a data frame
##         # A result will be returned in a data frame with corresponding columns
##         x <- as.data.frame(x)
##         # We keep the same column headers
##         NamesV <- names(x)
##         StatM <- NULL
##         # Calculation is performed alternatively on each column
##         for (i in 1:ncol(x)) {
##             StatV <- stat.pen.vec(x[i], Basic, Desc)
##             # The next if condition to avoid error at the first step
##             if (is.null(StatM)==TRUE) StatM <- data.frame(StatV) else
##                 StatM <- cbind(StatM, StatV)
##         }
##         # We change names of columns to match the original data frame
##         names(StatM) <- NamesV
##         StatM
##     }
## }


## # ------------------------------------------------------------------------------


## .stat.pen.vec.pastecs <-
##     function(x, basic, desc)
## {
##     # This function performs all calculations on a single vector
##     # Missing data allowed and stripped out before calculations

##     # If x is a list, we transform it into a vector
##     x <- unlist(x)
##     if (!is.numeric(x)) {   # Not a numeric vector!
##         Nbrval <- NA; Nbrnull <- NA; Nbrna <-NA
##         Median <- NA; Mean <- NA; StdDev <- NA
##         if (basic==TRUE) {
##             Res1 <- list(nbr.val=NA, nbr.null=NA, perc.numm=NA, nbr.na=NA)
##         } else Res1 <- NULL
##         if (desc==TRUE) {
##             Res2 <- list(median=NA, mean=NA, var=NA, std.dev=NA,
##                 pos.median=NA, pos.mean=NA, pos.var=NA, pos.std.dev=NA,
##                 geo.mean=NA)
##         } else Res2 <- NULL
##         Res3 <- list(pen.mean=NA, pen.var=NA, pen.std.dev=NA, pen.mean.var=NA)
##     } else {            # Vector contains numbers, we can perform calcs
##         Nbrna <- sum(as.numeric(is.na(x)))
##         # We could use na.rm=TRUE everywhere, but it is faster
##         # to remove all missing values once at the beginning
##         x <- x[!is.na(x)]
##         Nbrval <- length(x)
##         Nbrnull <- sum(as.numeric(x==0))
##         if (basic==TRUE) {
##             Percnull <- Nbrnull/Nbrval*100
##             Res1 <- list(nbr.val=Nbrval, nbr.null=Nbrnull, percnull=Percnull,
##                 nbr.na=Nbrna)
##         } else Res1 <- NULL
##         if (desc==TRUE) {
##             Median <- median(x); names(Median) <- NULL  # To correct a bug!?
##             Mean <- mean(x)
##             Var <- var(x)
##             StdDev <- sqrt(Var)
##             xpos <- x[x>0]
##             if (length(xpos)==0) {  # No positive values!
##                 # If at least one zero, everything is 0, else everything is NA
##                 if (NbrNull>0) {
##                     PosMedian <- 0; PosMean <- 0; PosVar <- 0; PosStdDev <- 0;
##                         GeoMean <- 0
##                 } else {
##                     PosMedian <- NA; PosMean <- NA; PosVar <- NA;
##                         PosStdDev <- NA; GeoMean <- NA
##                 }
##             } else {
##                 PosMedian <- median(xpos); names(PosMedian) <- NULL
##                 PosMean <- mean(xpos)
##                 PosVar <- var(xpos)
##                 PosStdDev <- sqrt(PosVar)
##                 GeoMean <- exp(mean(log(xpos)))
##             }
##             Res2 <- list(median=Median, mean=Mean, var=Var, std.dev=StdDev,
##                 pos.median=PosMedian, pos.mean=PosMean, pos.var=PosVar,
##                 pos.std.dev=PosStdDev, geo.mean=GeoMean)
##         } else Res2 <- NULL
##         Pen <- pennington(x, calc="all")
##         names(Pen) <- NULL
##         PMean <- Pen[1]
##         PVar <- Pen[2]
##         PStdDev <- sqrt(PVar)
##         PMeanVar <- Pen[3]
##         Res3 <- list(pen.mean=PMean, pen.var=PVar, pen.std.dev=PStdDev,
##             pen.mean.var=PMeanVar)
##     }
##     # We collect all results together
##     Res <- unlist(c(Res1, Res2, Res3))
##     Res
## }


################################################################################
