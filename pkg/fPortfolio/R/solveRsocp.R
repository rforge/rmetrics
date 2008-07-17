
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
# FUNCTION:                    DESCRIPTION:
#  solveRsocp                   Second order cone programming solver
#  .SqrtMatrix                  Returns square Root of a quadratic matrix
################################################################################


# Test Implementation for "LongOnly" MV Portfolio


# ------------------------------------------------------------------------------


solveRsocp <-
    function(data, spec, constraints)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Second Order Cone Programming

    # Arguments:
    #   data - portfolio of assets
    #   spec - specification of the portfolio
    #   constraints - string of constraints

    # Note:
    #   This function is thought to maximize MV return for a fixed risk

    # FUNCTION:

    # Test Implementation for "LongOnly" MV Portfolio
    # stopifnot(constraints == "LongOnly")

    # Load Rsocp:
    if (!require(Csocp)) {
        cat("\n\nRsocp Package missing")
        cat("\nPlease download package from Rmetrics Server\n")
    }

    # Transform Data and Constraints:
    data = portfolioData(data, spec)
    if (class(constraints) == "fPFOLIOCON")
        constraints = constraints@stringConstraints

    # Scale Data:
    Data = data@data$series
    Scale = 1e6 * sd(as.vector(Data))

    # Trace:
    trace = getTrace(spec)
    if(trace) cat("\nPortfolio Optimiziation:\n Using Rsocp ...\n\n")

    # Get Specifications:
    mu = getMu(data) /Scale
    Sigma = getSigma(data) / Scale^2

    # Number of Assets:
    nAssets = getNAssets(data)

    # Extracting Target Risk from Specification:
    targetRisk = getTargetRisk(spec) / Scale
    stopifnot(is.numeric(targetRisk))

    # Optimize Portfolio:
    #if (nAssets == 2) {
    # Two Assets Portfolio:
    # ...
    #} else {

    # Objective Function:
    lambda = 10
    f <- -mu - lambda * length(mu) * max(abs(mu))

    # Setting the constraints matrix and vector:
    tmpConstraints <- .setConstraints(data = data, spec = spec,
                                      constraints = constraints)

    # NOTE : tmpConstraints[1, ] "Budget"
    # NOTE : tmpConstraints[2, ] "Return"

    C1 <- rep(0, nAssets)                                       # xCx
    C2 <- tmpConstraints[1, -(nAssets + 1)]                     # sum(x)
    C3 <- tmpConstraints[- c(1,2), -(nAssets + 1)]              # x[i]>0

    d1 <- targetRisk                                            # xCx = risk
    d2 <- tmpConstraints[1, (nAssets + 1)]                      # sum(x) <= 1
    d3 <- tmpConstraints[- c(1,2), (nAssets + 1)]               # x[i] > 0

    A1 <- .SqrtMatrix(Sigma)
    A2 <- matrix(0, ncol = nAssets)
    A3 <- matrix(0, nrow = nrow(C3), ncol = nAssets)

    b1 <- rep(0, nAssets)                                       # xCx
    b2 <- 0                                                     # sum(x)
    b3 <- rep(0, nrow(C3))                                      # x[i]>0

    N1 <- nAssets
    N2 <- 1
    N3 <- rep(1, nrow(C3))

    # Combine constraints for socp
    A <- rbind(A1,
               A2,
               A3)

    b <- c(b1,      # xCx
           b2,      # sum(x)
           b3)      # x[i]>0

    C <- rbind(C1,  # xCx
              -C2,  # sum(x)
               C3)  # x[i]>0
    d <- c(d1,      # xCx = risk
           d2,      # sum(x) <= 1
          -d3)      # x[i] > 0

    N <- c(N1,      # dim(C)
           N2,      # Full Investment
           N3)      # Long

    # Control List:
    #   abs_tol = 1e-8, rel_tol = 1e-6, target = 0,
    #   max_iter = 500, Nu = 10, out_mode = 0,
    #   BigM_K = 2, BigM_iter = 5

    # Optimize:
    fit <- rsocp(f, A, b, C, d, N, control =
        list(
            abs.tol = 1e-12,
            rel.tol = 1e-12,
            target = 0,
            max.iter = 1000,
            Nu = 10,
            out.mode = 0,
            BigM.K = 2,
            BigM.iter = 5
    ))

    # Prepare Output List:
    #   YC: status slot required by efficientPortfolio
    #   DW: solver = "socp" replaced
    ans <- list(
        # Obligatory Entries ...
        solver = "solveRsocp",
        weights = fit$x,
        status = as.integer(fit$convergence),
        targetReturn = NA,
        targetRisk = targetRisk * Scale,
        objective = NA,
        # Optional entries ...
        fit = fit)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.SqrtMatrix <-
function(x)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Square Root of a quadratic Matrix:

    # Example:
    #   A = matrix(c(1,.2,.2,.2,1,.2,.2,.2,1), ncol = 3)
    #   round(Sqrt(A) %*% Sqrt(A) - A, digits = 12)

    # FUNCTION:

    # Check if matrix is square:
    stopifnot(NCOL(x) == NROW(x))

    # One-dimensional ?
    if (NCOL(x) == 1) return(sqrt(as.vector(x)))

    # Square Root of a matrix:
    e <- eigen(x)
    V <- e$vectors
    ans <- V %*% diag(sqrt(e$values)) %*% t(V)

    # Return Value:
    ans
}


################################################################################

