
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
#  solveRlpSolve                Calls linear programming solver
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
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Number of Assets:
    dim = length(mu)

    # Extracting data from spec:
    targetReturn = spec@portfolio$targetReturn  
    stopifnot(is.numeric(targetReturn)) 

    # Setting the constraints matrix and vector:   
    tmpConstraints = .setConstraints(
        data = data, spec = spec, constraints = constraints)
    Dmat = Sigma
    dvec = rep(0, dim)
    A = tmpConstraints[, -(dim+1)]
    Amat = t(A)
    b0 = tmpConstraints[, (dim+1)] 
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
        
    # Prepare output list:
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
    
    # FUNCTION:
    
    # Get Statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu <<- data$statistics$mu
    Sigma <<- data$statistics$Sigma
    
    # Number of Assets:
    dim = length(mu)

    # Extracting data from spec:
    targetReturn = spec@portfolio$targetReturn  
    stopifnot(is.numeric(targetReturn)) 
    
    # Donlp2 Settings - Variables and Function:
    par = rep(1/dim, dim)
    fn = function(x) { x %*% Sigma %*% x }
    
    # Donlp2 Settings - Box/Group Constraints:
    A.mat = .setConstraints(data, spec, constraints, type = "BoxGroup")
    upperNames = paste("maxW", 1:dim, sep = "")
    par.upper = -A.mat[upperNames, "Exposure"]
    lowerNames = paste("minW", 1:dim, sep = "")
    par.lower = A.mat[lowerNames, "Exposure"]
    
    # Donlp2 Settings - Group Constraints:
    Rows = (1:nrow(A.mat))
    names(Rows) = rownames(A.mat)
    Rows[c(lowerNames, upperNames)]
    A = A.mat[-Rows[c(lowerNames, upperNames)], ]
    M = nrow(A)
    mNames = rownames(A)
    lin.upper = lin.lower = rep(NA, M)
    lin.upper[1] = lin.lower[1] = A[1, dim+1]
    lin.upper[2] = lin.lower[2] = A[2, dim+1]
    
    # Further Group Constraints:
    if (M > 2) {
        for (i in 3:M) {
            if (mNames[i] == "minsumW") {
                lin.lower[i] = A[i, dim+1]
                lin.upper[i] = Inf
            } else if (mNames[i] == "maxsumW") {
                lin.lower[i] = -Inf
                lin.upper[i] = -A[i, dim+1]
                A[i, 1:dim] = -A[i, 1:dim]
            }
        }
    }
    A = A[, -(dim+1)]
    
    # Trace Solver:
    solver.trace = spec@solver$trace  
    
    # Check Constraint Strings for Risk Budgets:
    validStrings = c("minB", "maxB")
    usedStrings = unique(sort(sub("\\[.*", "", constraints)))
    usedStrings
    checkStrings = usedStrings %in% validStrings
    checkStrings
    includeRiskBudgeting = (sum(!checkStrings) > 0)
    if (solver.trace) cat("Include Risk Budgeting:", includeRiskBudgeting, "\n")
    
    if (includeRiskBudgeting) {
        nlcon <- function(x) {
            B1 = as.vector(x %*% Sigma %*% x)
            B2 = as.vector(x * Sigma %*% x)
            B = B2/B1
            B
        }
          
        # Compose non-linear functions:
        for (I in 1:dim) 
        eval( parse(text = paste(
            "nlcon", I, " = function(x) { nlcon(x)[", I, "] }", sep = "")) )
        nlinFunctions = paste("nlcon", 1:dim, sep = "", collapse = ",")
        nlinFunctions = paste("list(", nlinFunctions, ")")
        nlin = eval( parse(text = nlinFunctions) )
        
        # Constraints Vectors:
        B = .setConstraints(data, spec, constraints, type = "RiskBudget")
        nlin.lower = B[1, ]
        nlin.upper = B[2, ]
        
        # Optimize:
        ans = donlp2(
            par, fn, 
            par.l = par.lower, par.u = par.upper,
            A = A, lin.l = lin.lower, lin.u = lin.upper,  
            nlin = nlin, nlin.l = nlin.lower, nlin.u = nlin.upper,  
            control = donlp2.control(silent = !solver.trace),
            name = "portfolio")
    } else {
        # Optimize:
        ans = donlp2(
            par, fn, 
            par.l = par.lower, par.u = par.upper,
            A = A, lin.l = lin.lower, lin.u = lin.upper,  
            control = donlp2.control(silent = !solver.trace),
            name = "portfolio")
   }
   
   # Add:
   ans$solution = ans$par
   ans$ierr = ans$message
   if (solver.trace) cat("Rdonlp2 Message:", ans$message, "\n")
   
   # Return Value:
   ans
}


# ------------------------------------------------------------------------------


solveRlpSolve =
function(data, spec, constraints)
{   # A function implemented by Rmetrics

    # Description:
    #   Linear Solver from R package lpSolve
    
    # Note:
    #   This function requires to load the contributed R package
    #   lpSolve explicitely!
    #
    #   IMPORTANR:
    #   Only Zero-One Long-Only Constraints are implemented !!!
    
    # FUNCTION:
    
    # Get data statistics:
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma
    
    # Get quantile measure alpha:
    targetAlpha = spec@portfolio$targetAlpha
    
    # Get target Return:
    targetReturn = spec@portfolio$targetReturn
    
    # Scenarios:
    Data = data$series
    colNames = colnames(Data)
    rowNames = rownames(Data)
    DIM = dim(Data)
    m = DIM[1]
    w = DIM[2]

    # Compose objective function:
    Names = c("VaR", paste("e", 1:m, sep = ""), colNames)
    f.obj = c(1, rep(-1/(targetAlpha*m), m), rep(0, w))
    names(f.obj) = Names
    
    # Info on constraints - Constraint matrix:
    #   Example m=8 Data Records, and w=4 Assets
    #   
    #   VaR  es          weights          exposure
    #   x1   x2 ... x9   x10 ... x13
    #       
    #    0    0      0   mu1     mu4      = Mu
    #    0    0      0   1       1        = 1
    #               
    #   -1   x2      0   r1.1    r4.1     >= 0
    #   -1   0 x3    0   r1.2    r4.1     >= 0
    #   
    #   -1   0   x8  0   r1.8    r4.8     >= 0
    #   -1   0      x9   r1.9    r4.9     >= 0
    #   
    #   x2   >= 0       es    Not yet Implemented !!!
    #   ...
    #   x9   >= 0
    #   
    #   x10  >= 0       w     Not yet Implemented !!!
    #   ...
    #   x13  >= 0
    
    # Compose constraint matrix:
    nX = 1 + m + w
    nY = 2 + m
    f.con = matrix(rep(0, nX*nY), ncol = nX)
    rownames(f.con) = c("Budget", "Return", rowNames)
    colnames(f.con) = c("VaR", paste("e", 1:m, sep = ""), colNames) 
    f.con[1, (2+m):(2+m+w-1)] = 1 
    f.con[2, (2+m):(2+m+w-1)] = mu
    f.con[3:(m+2), (2+m):(2+m+w-1)] = seriesData(Data)
    f.con[3:(m+2), 1] = -1 
    f.con[3:(m+2), 2:(m+1)] = diag(m)
    
    # Set directions:
    f.dir = c("=", "=", rep(">=", m))
    names(f.dir) = rownames(f.con)
  
    # Compose right hand side vector:
    f.rhs = c(1, targetReturn, rep(0, m))
    names(f.rhs) = rownames(f.con)
    
    # Optimize:
    ans = lp("max", f.obj, f.con, f.dir, f.rhs)

    # Prepare output list:
    ans$Solution = ans$solution
    ans$solution = ans$solution[(m+2):(m+1+w)] 
    ans$ierr = ans$status
    ans$solver = "lpSolve"
    
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
    # solver = match.arg(solver)
    # spec@solver$type = solver
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


"setSolver<-" <- function(spec, value)
{   # A function implemented by Rmetrics

    # Description:
    
    # FUNCTION:
    
    # Valid Solvers:
    # validSolvers = c("RQuadprog", "RDonlp2")
    # stopifnot(value %in% validSolvers)
    
    # Set Solver:
    spec@solver$type = value
    
    # Return Value:
    spec
}


################################################################################

