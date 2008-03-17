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


# experimental

members <- c("cov", "cor")

# template definition
templateXY <-  c("{",
               "finCenter <- finCenter(x)",
               "x <- as.matrix(x)",
               "if (!is.null(y)) y <- as.matrix(y)",
               "ans <- callNextMethod()",
               "ans",
               "}")

# set the Methods in a loop
for (f in members) {
    def <- function()NULL
    formals(def) <- formals(f)
    body(def) <- parse(text = templateXY)
    setMethod(f, "timeSeries", def)
}

# ------------------------------------------------------------------------------

members <- c("quantile", "dnorm", "dcauchy", "dt")

# template definition
templateXXX <-  c("{",
                  "finCenter <- finCenter(XXX)",
                  "XXX <- as.matrix(XXX)",
                  "ans <- callNextMethod()",
                  "if (is.matrix(ans))",
                  "    ans <- timeSeries(ans, zone = finCenter,",
                  "                      FinCenter = finCenter)",
                  "ans",
                  "}")

# set the Methods in a loop
for (f in members) {
    def <- function()NULL
    formals(def) <- formals(f)
    bodyText <- gsub("XXX", names(formals(f))[1], templateXXX)
    body(def) <- parse(text = bodyText)
    setMethod(f, "timeSeries", def)
}
