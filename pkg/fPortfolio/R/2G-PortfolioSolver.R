
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
# FUNCTION:                    DESCRIPTION:  
#  solveRQuadprog               Calls Goldfarb and Idnani's QP solver
#  solveRDonlp2                 Calls Spelucci's donlp2 solver
# FUNCTION:                    DESCRIPTION:
#  setSolver                    Sets the desired solver
#  setSolver<-                  Sets the desired solver
################################################################################



solveRQuadprog =
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Calls Goldfarb and Idnani's QP solver
    
    # FUNCTION:
    
    # Get Statistics:
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Number of Assets:
    dim = length(mu)

    # Extracting data from spec:
    targetReturn = spec@portfolio$targetReturn  
    stopifnot(is.numeric(targetReturn)) 

    # Setting the constraints matrix and vector:   
    tmp.ans = setConstraints(data = data, spec = spec,
        constraints = constraints)
    Dmat = Sigma
    dvec = rep(0, dim)
    A = tmp.ans[, -(dim+1)]
    Amat = t(A)
    b0 = tmp.ans[, (dim+1)] 
    bvec = t(b0)
    meq = 2
    n = nrow(Dmat)
    q = ncol(Amat)
    r = min(n, q)
    work = rep(0, 2 * n + r * (r + 5)/2 + 2 * q + 1)
    res1 = .Fortran("qpgen2", 
        as.double(Dmat), 
        dvec = as.double(dvec), 
        as.integer(n), 
        as.integer(n), 
        sol = as.double(rep(0, n)), 
        crval = as.double(0), 
        as.double(Amat), 
        as.double(bvec), 
        as.integer(n), 
        as.integer(q), 
        as.integer(meq), 
        iact = as.integer(rep(0, q)), 
        nact = as.integer(0), 
        iter = as.integer(rep(0, 2)), 
        work = as.double(work), 
        ierr = as.integer(0), 
        PACKAGE = "fPortfolio")
    ans = list(
        solution = res1$sol, 
        value = res1$crval, 
        unconstrainted.solution = res1$dvec, 
        ierr = res1$ierr, 
        iterations = res1$iter, 
        iact = res1$iact[1:res1$nact],
        solver = "quadprog")
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


solveRDonlp2 =
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Calls Spelucci's donlp2 solver
    
    # Note:
    #   Requires package "Rdonlp2" to be loaded
    
    # FUNCTION:
    
    # Get Statistics:
    mu = data$statistics$mu
    Sigma <<- data$statistics$Sigma
    
    # Number of Assets:
    dim = length(mu)

    # Extracting data from spec:
    targetReturn = spec@portfolio$targetReturn  
    stopifnot(is.numeric(targetReturn)) 
    
    # Donlp2 Settings - Variables and Function:
    par = rep(1/dim, dim)
    fn = function(x) x %*% Sigma %*% x
    
    # Donlp2 Settings - Box Constraints:
    A.mat = setConstraints(data, spec, constraints)
    upperNames = paste("maxW", 1:dim, sep = "")
    par.upper = -A.mat[upperNames, "Exposure"]
    lowerNames = paste("minW", 1:dim, sep = "")
    par.lower = A.mat[lowerNames, "Exposure"]
    
    # Donlp2 Settings - Sector Constraints:
    Rows = (1:nrow(A.mat))
    names(Rows) = rownames(A.mat)
    Rows[c(lowerNames, upperNames)]
    A = A.mat[-Rows[c(lowerNames, upperNames)], ]
    M = nrow(A)
    mNames = rownames(A)
    lin.upper = lin.lower = rep(NA, M)
    lin.upper[1] = lin.lower[1] = A[1, dim+1]
    lin.upper[2] = lin.lower[2] = A[2, dim+1]
    if (M > 2) {
        for (i in 3:M) {
            if (mNames[i] == "minsumW") {
                lin.lower[i] = A[i, dim+1]
                lin.upper[i] = Inf
            } else if (mNames[i] == "maxsumW") {
                lin.lower[i] = -Inf
                lin.upper[i] = -A[i, dim+1]
                A[i, 1:N] = -A[i, 1:dim]
            }
        }
    }
    A = A[, -(dim+1)]
    
    # Optimize:
    ans = donlp2(
        par, fn, 
        par.lower = par.lower, par.upper = par.upper,
        A = A, lin.upper = lin.upper, lin.lower = lin.lower, 
        control = donlp2.control(silent = TRUE),
        name = "portfolio")
   ans$solution = ans$par
   ans$ierr = NA
   
   # Return Value:
   ans
}


# ------------------------------------------------------------------------------


setSolver = 
function (spec = portfolioSpec(), solver = c("RQuadprog", "Rdonlp2")) 
{   # A function implemented by Rmetrics

    # Description:
    
    # FUNCTION:
    
    # Set Solver:
    solver = match.arg(solver)
    spec@solver$type = solver
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setSolver<-" <- function(spec, value)
{   # A function implemented by Rmetrics

    # Description:
    
    # FUNCTION:
    
    # Set Solver:
    spec@solver$type = value
    
    # Return Value:
    spec
}


################################################################################

