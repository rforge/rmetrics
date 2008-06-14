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


# experimental

.members <- c("mean", "summary")

# TODO : list functions in base package to be used
# TODO : candidates : "mean", "colSums", "rowSums", "colMeans", "rowMeans"
# TODO : NOTE : check conflicts with functions defined in fUtilities

# template definition
.templateANY <-  c("{",
                  "finCenter <- finCenter(ANY)",
                  "ANY <- as.matrix(ANY)",
                  "ans <- callNextMethod()",
                  "if (is.matrix(ans))",
                  "    ans <- timeSeries(ans, zone = finCenter,",
                  "                      FinCenter = finCenter)",
                  "ans",
                  "}")

# set the Methods in a loop
for (.f in .members) {
    .def <- function()NULL
    formals(.def) <- formals(.f)
    .bodyText <- gsub("ANY", names(formals(.f))[1], .templateANY)
    body(.def) <- parse(text = .bodyText)
    setMethod(.f, "timeSeries", .def)
}


################################################################################

