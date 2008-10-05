
# This library is free software, you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation, either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library, if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA 02111-1307 USA


################################################################################
# FUNCTION:             DESCRIPTION:
#  .mvnorme
################################################################################


.mvnorm.e <-
    function(x)
{
    z <- scale(x, scale = FALSE)
    ev <- eigen(var(x), symmetric = TRUE)
    P <- ev$vectors
    y <- z %*% (P %*% diag(1 / sqrt(ev$values)) %*% t(P))
    e <- .C("mvnEstat", y = as.double(t(y)), byrow = as.integer(TRUE),
        nobs = as.integer(nrow(x)), dim = as.integer(ncol(x)),
        stat = as.double(0), PACKAGE = "fAssets")$stat

    # Return Value:
    e
}


################################################################################

