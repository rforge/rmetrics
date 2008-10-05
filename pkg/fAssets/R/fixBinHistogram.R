
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
# FUNCTION:                   SIMILARITY PLOTS:
#  fixBinHistogram             Returns histogram with fixed bins
################################################################################


.hist <- 
    function (x, nbins) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns histogram with fixed bins
    
    # FUNCTION:
    
    # Classes:
    nclass = nbins + 1
    n = length(x)
    xname = paste(deparse(substitute(x), 500), collapse = "\n")
    
    # Breaks:
    breaks = seq(min(x), max(x), length = nclass)
    nB = length(breaks)
    h = diff(breaks)
    
    # Compute Counts:
    counts = .C("bincount", as.double(x), as.integer(n), as.double(breaks), 
        as.integer(nB), counts = integer(nB - 1), right = FALSE, 
        include = TRUE, naok = FALSE, NAOK = FALSE, DUP = FALSE, 
        PACKAGE = "base")$counts
    dens = counts/(n * h)
    mids = 0.5 * (breaks[-1] + breaks[-nB])
    
    # Histogram:
    r = structure(list(breaks = breaks, counts = counts, intensities = dens, 
        density = dens, mids = mids, xname = xname, equidist = TRUE), 
        class = "histogram")
}


################################################################################

