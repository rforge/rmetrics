
#*******************************************************************************
# fSeries - A SOFTWARE COLLECTION FOR FINANCIAL ENGINEERS
# The Dynamical Process Behind Financial Markets
#
# collected by Diethelm Wuertz
# Version 0.9
#*******************************************************************************


# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2005, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################


.First.lib = 
function(lib, pkg)
{   # A function implemented by Diethelm Wuertz
    
    # Package:
    cat("\nRmetrics, (C) 1999-2007, Diethelm Wuertz, GPL")
    cat("\nfSeries: The Dynamical Process Behind Financial Markets\n")

    # Load Libraries:
    sink("@sink@")
    library(mgcv) # gam
    library(nnet) # nnet
    sink()
    unlink("@sink@")
    
    # Load dll:
    library.dynam("fSeries", pkg, lib)
    
    # Load urcaBuiltin Package:
    sinkFile <- file("@sink@", open = "wt")
    sink(sinkFile)
    sink(sinkFile, type = "message")
    .urcaBuiltin()
    sink(type = "message")
    sink()
    unlink("@sink@")
}


################################################################################

