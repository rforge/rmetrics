
# Rmetrics is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# Rmetrics is distributed in the hope that it will be useful,
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
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file 


################################################################################
# FUNCTION:             DESCRIPTION:
#  fredImport            Downloads monthly data from research.stlouisfed.org
#  fredSeries            Easy to use download from research.stlouisfed.org 
################################################################################
       

test.fredSeries = 
function()
{     
    # http://research.stlouisfed.org/fred2/series/
    
    args(fredSeries)
    # function (symbols, from = NULL, to = Sys.timeDate(), nDaysBack = 366, ...) 
    
    if (FALSE) {

         # Daily Series:
         fredSeries("DPRIME")[1:10, ]
         fredSeries("DAAA")[1:10, ]
         
         # Monthly:
         fredSeries("AAA")[1:10, ]
         
         # Weekly:
         head(fredSeries("WAAA"))
         
         # Daily + Daily Series:
         fredSeries(c("DPRIME", "DAAA"))[1:10, ]
         
         # Mixing Weekly and Monthly:
         fredSeries(c("WAAA", "AAA"))[1:20, ]
         
         # nDaysBack:
         fredSeries("DPRIME", to = Sys.timeDate(), nDaysBack = 20)
         
         # from, to:
         #  Note express last 20 days as 20*24*3600 ...
         fredSeries("DPRIME", 
            from = Sys.timeDate() - 20*24*3600, to = Sys.timeDate())
    }
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.fredImport = 
function()
{     
    
    # http://research.stlouisfed.org/fred2/series/
    
    args(fredImport)
    # function (query, file = "tempfile", frequency = "daily", from = NULL, 
    #   to = Sys.timeDate(), nDaysBack = NULL, save = FALSE, sep = ";", 
    #   try = TRUE) 

    if (FALSE) {
        
        # Daily Series:
        fredImport("DPRIME") 
        fredImport("DAAA") 
         
        # Monthly:
        fredImport("AAA")
          
    }
    
    # Return Value:
    return()
}

   
################################################################################

