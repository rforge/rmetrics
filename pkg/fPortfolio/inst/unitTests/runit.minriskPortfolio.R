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


################################################################################
# FUNCTION:
#  test.minriskPortfolio
################################################################################


test.minriskPortfolio <- function()
{
    # If solver does not converged to a solution, replace value of
    # objective function by maximum machine number. It helps in
    # situation where the worst and best return are not feasible
    # with the constraints.

    # Example:
    Data <- 252*as.timeSeries(data(LPP2005REC))[,1:6]
    Spec <- portfolioSpec()
    Constraints <- c("minW[1:6]=0.05", "maxsumW[c('SBI','LMI')]=0.6")

    # MV Markowitz Frontier:
    minriskPortfolio(Data, Spec, Constraints)
}
