
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


# experimental

members <- c("dnorm", "dcauchy", "dt")

# template definition
template <-  c("{",
               "finCenter <- XXX@FinCenter",
               "XXX <- as.matrix(XXX)",
               "ans <- callGeneric()",
               "if (is.matrix(ans))",
               "    ans <- as.timeSeries(ans, finCenter, finCenter)",
               "ans",
               "}")

# set the Methods in a loop
for (f in members) {
    def <- function()NULL
    formals(def) <- formals(f)
    bodyText <- gsub("XXX", names(formals(f))[1], template)
    body(def) <- parse(text = bodyText)
    setMethod(f, "timeSeries", def)
}
