
#*******************************************************************************
# fBasics - A SOFTWARE COLLECTION FOR FINANCIAL ENGINEERS
# PART I: Markets, Basic Statistics, Date and Time Management
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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
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
    cat("\nRmetrics, (C) 1999-2005, Diethelm Wuertz, GPL")
    cat("\nfBasics: Markets, Basic Statistics, Hypothesis Testing\n")

    # Load dll:
    library.dynam("fBasics", pkg, lib)
    
    # Example time series for internal use:
    # data(singleIndex.dat)
    # .univTS = timeSeries(
    # 	data = matrix(singleIndex.dat[, 3], ncol = 1),
    #	charvec = as.character(singleIndex.dat[, 1]),
    #	units = "SP500",
    #	format = "%d-%b-%Y", 
    #	zone = "GMT",
    #	FinCenter = "GMT")
    #.bivTS = timeSeries(
    #	data = as.matrix(singleIndex.dat[, 2:3]),
    #	charvec = as.character(singleIndex.dat[, 1]),
    #	units = c("MSFT", "SP500"),
    #	format = "%d-%b-%Y", 
    #	zone = "GMT",
    #	FinCenter = "GMT") 	
    
    .Sys.timezone = function() Sys.getenv("TZ")[[1]]

}


################################################################################

