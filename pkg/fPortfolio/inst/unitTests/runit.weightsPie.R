
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
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION:
#  test.weightsPie.portfolio
#  test.weightsPie.frontier
################################################################################


test.weightsPie.portfolio <-  
    function()
{  
    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    
    # Specification:
    spec = portfolioSpec()
    
    # Constraints:
    constraints = "LongOnly"
    
    # Portfolio:
    portfolio = tangencyPortfolio(data, spec, constraints)
    portfolio
    
    # Graph Frame:
    par(mfrow = c(2, 2))
    
    # Pie Plot 1:
    weightsPie(portfolio)
    
    # Pie Plot 2:
    weightsPie(portfolio, col = seqPalette(4, "Blues"), box = FALSE)
    boxL()
    
    # Pie Plot 3:
    weightsPie(portfolio, labels = letters[1:4], init.angle = -17, 
        col = qualiPalette(4, "Accent"), radius = 0.7, box = FALSE)
    mtext("Portfolio Weights", line = 1, adj = 0, font = 4)
    box_()
    copyright()
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.weightsPie.frontier <-  
    function()
{  
    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    
    # Specification:
    spec = portfolioSpec()
    
    # Constraints:
    constraints = "LongOnly"
    
    # Portfolio:
    frontier = portfolioFrontier(data, spec, constraints)
    frontier
    
    # Graph Frame:
    par(mfrow = c(2, 2))
    
    # Pie Plot 2:
    for (pos in 10*(1:4)) {
        weightsPie(frontier, labels = FALSE, pos = pos, 
            col = qualiPalette(4, "Pastel1"), 
            box = FALSE, radius = 0.6, cex = 0.8, font = 4) 
        copyright()
        Return = round(100*getTargetReturn(frontier)[pos,], 2)
        mtext(paste("Return", Return, "%"), 
            side = 3, line = 0.5, adj= 0, font = 2)
        abline(h = 1.07, lwd = 2)
        box_()
    }
    
    # Return Value:
    return()
}


################################################################################

