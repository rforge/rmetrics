
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA 02111-1307 USA

# Copyrights (C)
# for this R-port:
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION:                     DESCRIPTION:
#  portfolioSpec                 Specifies a portfolio to be optimized
#  .checkWeights                 Sets extremely small weights to zero
################################################################################


portfolioSpec <-
function(
    model = list(
         type = "MV",                   # Alt: "LPM", "CVaR"
         optimize = "minRisk",          # Alt: "maxReturn"
         estimator = "covEstimator",    # Alt: "shrinkEstimator", 
                                        #      "lpmEstimator"
         tailRisk = list(),
         params = list(alpha = 0.05, a = 1)),
    portfolio = list(
         weights = NULL,
         targetReturn = NULL,
         targetRisk = NULL,
         riskFreeRate = 0,
         nFrontierPoints = 50,
         status = 0),
    optim = list(
         solver = "solveRquadprog",     # Alt: "solveRdonlp2" 
                                        #      "solveRlpSolve", 
                                        #      "solveRsocp"
         objective = NULL,
         params = list(),
         control = list(),
         trace = FALSE)
    )
{
    # A function implemented by Diethelm Wuertz and Oliver Greshake

    # Description:
    #   Specifies a portfolio to be optimized

    # Example:
    #   portfolioSpec(portfolio = list(targetReturn = 1.5))

    # FUNCTION:

    # Compose Checklists:
    # model.type = c("MV", "CVaR")
    # model.estimator.mean = "mean"
    # model.estimator.cov = c("cov", "mcd", "Mcd", "shrink")
    # optim.solver = c("quadprog", "Rdonlp2", "lpSolve")
    # optim.trace = FALSE

    # Check Arguments:
    # stopifnot(model$type %in% model.type)
    # stopifnot(model$estimator[1] %in% model.estimator.mean)
    # stopifnot(model$estimator[2] %in% model.estimator.cov)
    # stopifnot(optim$solver %in% optim.solver)

    # Model Slot:
    Model = list(
        type = "MV",
        optimize = "minRisk",
        estimator = "covEstimator",
        tailRisk = NULL,
        params = list())
    model$type = model$type[1]
    Model[(Names <- names(model))] <- model

    # Portfolio Slot:
    Portfolio = list(
        weights = NULL,
        targetReturn = NULL,
        targetRisk = NULL,
        riskFreeRate = 0,
        nFrontierPoints = 50,
        status = 0)
    Portfolio[(Names <- names(portfolio))] <- portfolio

    # Check Portfolio - weights, targetReturn, targetRisk:
    # ... at least two of them must be set to NULL!
    checkPortfolio = 0
    if(!is.null(portfolio$weights)) checkPortfolio = checkPortfolio + 1
    if(!is.null(portfolio$targetReturn)) checkPortfolio = checkPortfolio + 1
    stopifnot(checkPortfolio <= 1)

    # Optim Slot:
    Optim = list(
        solver = "solveRquadprog",
        trace = FALSE)
    Optim[(Names <- names(optim))] <- optim

    # Return Value:
    new("fPFOLIOSPEC",
        model = Model,
        portfolio = Portfolio,
        optim = Optim)
}


# ------------------------------------------------------------------------------


.checkWeights <-
    function(weights, eps = sqrt(.Machine$double.eps))
{    
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Sets extremely small weights to zero
    
    # Arguments:
    #   weights - a numeric vector of portfolio weights
    #   eps - a numeric value, lower bounds of weigths
    
    # FUNCTOION:
    
    # Check:
    for(i in 1:length(weights)) {
        if(abs(weights[i]) < eps) weights[i] = 0
    }
    
    # Return Value:
    weights
}


################################################################################

