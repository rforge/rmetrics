
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:         DESCRIPTION:
#  yahooExchanges    Lists exchanges and symbol modifications supported by Yahoo
################################################################################


yahooExchanges <-
    function()
{
    # Description:
    #   Lists exchanges and symbol modifications supported by Yahoo 
    
    # Example:
    #   .yahooExchanges()
    
    # FUNCTION:
    
    # Load Data:
    x = readLines("http://uk.biz.yahoo.com/sd/index.html")
    
    # Convert to matrix:
    table = x[grep("<td bgcolor=ffffff>", x)]
    table = sub(" <td bgcolor=ffffff>", "", table)
    table = sub("</td> ", "", table)
    table = matrix(table, byrow = TRUE, ncol = 5)
    colnames(table) = c("Country", "Exchange", "Suffix", "Delay", "DataProvider")
    rownames(table) = 1:NROW(table)
    
    # Return Value:
    table
}


################################################################################

