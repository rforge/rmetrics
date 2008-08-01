
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
# FUNCTION:                 DESCRIPTION:
#  solveRsocp                Second order cone programming solver
#  .SqrtMatrix                Returns square Root of a quadratic matrix
# FUNCTION:                 DESCRIPTION:
#  rsocp                     Second order cone programming
#  .socp.phase1              Phase 1 method to finnd an initial primal point
#  .socp.phase2              Phase 2 cone variable selectors
#  rsocpControl              Control list for "rsocp" Solver
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
# FUNCTION:                 DESCRIPTION:
#  rsocp                     Second order cone programming
#  .socp.phase1              Phase 1 method to finnd an initial primal point
#  .socp.phase2              Phase 2 cone variable selectors
#  rsocpControl              Control list for "rsocp" Solver
################################################################################


# Package: Csocp
# Title: An R extenstion library to use SOCP from R.
# Version: 0.1
# Date: 2008-31-01
# Author: Yohan Chalabi and Diethelm Wuertz
# Maintainer: Yohan Chalabi <chalabi@phys.ethz.ch>
# Depends: R (>= 2.4.0), utils
# Description: Second-order cone programming solver 
#   written by M. Lobo, L. Vandenberghe, and S. Boyd. 
#   Rsocp is a wrapper library to use it from R.
# License: Rsocp - GPL |

# Requires Rmetrics Package: Csocp


rsocp  <-
    function(f, A, b, C, d, N, x = NULL, z = NULL, w = NULL, 
    control = list())
{
    # A function implemented by Yohan Chalabi and Diethelm Wuertz

    # Description:
    #   Second order cone programming
    
    # Arguments:
    #

    # Example:
    #   min x + y
    #   s.t. x^2 + y^2 <= 1
    #       y >= 0
    #
    #   f <- c(1,1)
    #   A <- matrix(c(1,0,0,0,1,0), ncol = 2)
    #   b <- c(0, 0, 0)
    #   C <- matrix(c(0,0, 0, 1), ncol = 2)
    #   d <- c(1,0)
    #   N <- c(2, 1)
    #   x <- c(0, 0.5)
    #   z <- c(1, 0, 0)
    #   w <- c(2,1)
    #   fit <- rsocp(f, A, b, C, d, N, x, z, w)

    # FUNCTION:

    # Check control list:
    cpos <- rsocpControl()
    if (length(control)) {
        nms <- names(control)
        if (!is.list(control) || is.null(nms))
            stop("control argument must be a named list")
        pos <- pmatch(nms, names(cpos))
        if (any(nap <- is.na(pos))) {
            warning(paste("unrecognized control element(s) named `",
                paste(nms[nap], collapse = ", "), "' ignored", sep = ""))
            pos <- pos[!nap]
            control <- control[!nap]
        }
        if (length(control))
            control <- c(control, cpos[-pos])
        else
            control <- cpos
    } else {
        control <- cpos
    }

    # Set control parameters:
    abs.tol <- as.double(control$abs.tol)
    rel.tol <- as.double(control$rel.tol)
    target <- as.double(control$target)
    max.iter <- as.integer(control$max.iter)
    Nu <- as.double(control$Nu)
    out.mode <- as.integer(control$out.mode)
    BigM.K <- as.integer(control$BigM.K)
    BigM.iter <- as.integer(control$BigM.iter)

    # Check control parameters:
    stopifnot(Nu >= 1)
    stopifnot(out.mode %in% 0:3)

    # Set parameters:
    L <- length(N)
    n <- length(f)
    iter <- max.iter
    mhist <- 0
    nhist <- 0
    ndbl <- 0
    nint <- 0
    info <- 0

    if (!is.null(d)) {
        # socp makes a recursive call for phase 1
        # with the data structure already changed
        # this is signaled by an empty d

        # check arguments
        stopifnot(any(dim(A) == c(sum(N), n)))
        stopifnot(length(b) == sum(N))
        stopifnot(any(dim(C) == c(L, n)))
        stopifnot(length(d) == L)
        if (!is.null(x)) stopifnot(length(x) == n)
        if (!is.null(z)) stopifnot(length(z) == sum(N))
        if (!is.null(z)) stopifnot(length(w) == L)

        # storage convention for socp.c routine
        Ni <- seq(from = 1, length = N[1])
        AA <- rbind(A[Ni,], C[1,])
        bb <- c(b[Ni], d[1])
        if (!is.null(z)) zz <- c(z[Ni], w[1])
        for (i in seq(L, from = 2)) {
            Ni <- seq(from = N[i-1] + 1, length = N[i])
            AA <- rbind(AA, A[Ni,], C[i,])
            bb <- c(bb, b[Ni], d[i])
            if (!is.null(z)) zz <- c(zz, z[Ni], w[i])
        }
        A <- AA
        b <- bb
        if (!is.null(z)) z <- zz
        N <- N + 1
    }

    if (is.null(x)) x <- .socp.phase1(f, A, b, N, control)
    if (is.null(z)) z <- .socp.phase2(f, A, b, N, x, control)

    # compute amount of workspace required
    socp.getwork <- .C("socp_getwork",
        as.integer(L),
        as.integer(N),
        as.integer(n),
        as.integer(max.iter),
        as.integer(out.mode),
        as.integer(mhist),
        as.integer(nhist),
        as.integer(ndbl),
        as.integer(nint),
        PACKAGE = "Csocp")
    mhist <- socp.getwork[[6]]
    nhist <- socp.getwork[[7]]
    ndbl <- socp.getwork[[8]]
    nint <- socp.getwork[[9]]

    # Initialise length of arrays
    hist <- rep(0, nhist*mhist + 1)
    dblwork <- rep(0, ndbl + 1)
    intwork <- rep(0, nint + 1)

    # Call socp.c routine
    socp <- .C("socp",
        as.integer(L),
        as.integer(N),
        as.integer(n),
        as.double(f),
        as.double(as.vector(A)),
        as.double(b),
        as.double(x),
        as.double(z),
        as.double(abs.tol),
        as.double(rel.tol),
        as.double(target),
        as.integer(iter),
        as.double(Nu),
        as.integer(info),
        as.integer(out.mode),
        as.double(hist),
        as.double(dblwork),
        as.integer(intwork),
        PACKAGE = "Csocp")
    ans <- list(x = socp[[7]])
    if (!is.null(d)) {
        idx <- cumsum(N)
        ans$z <- socp[[8]][-idx]
        ans$w <- socp[[8]][idx]
    } else {
        ans$z <- socp[[8]]
    }
    ans$iter <- socp[[12]]
    ans$hist <- socp[[16]]
    ans$convergence <- as.logical(if (socp[[14]] %in% 1:3) TRUE else FALSE)
    ans$info <- socp[[14]]
    ans$message <- switch(as.character(socp[[14]]),
        "0" = "Error (0)",
        "1" = "Absolute tolerance (1)",
        "2" = "Relative convergence (2)",
        "3" = "Target value (achieved or unachievable) (3)",
        "4" = "Maximum iterations (4)")

    # DIAGNOSTICS:
    if (ans$info == 1 | ans$info == 2) {
        e2 <- sum((t(A) %*% socp[[8]] - f)^2)^0.5 / (sum(socp[[8]]^2))^0.5
        if (e2 > 1.0e-6)
    warning("Dual is not in the feasible hyperplane, solution may be wrong.")
    }

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.socp.phase1 <-
    function(f, A, b, N, control)
{
    # A function implemented by Yohan Chalabi
    
    # Description:
    #   Phase 1 method to finnd an initial primal point
    
    # Note:
    #   Original Matlab code by mlobo@isl.stanford.edu

    # FUNCTION:

    # Set control parameters
    abs.tol <- as.double(control$abs.tol)
    rel.tol <- as.double(control$rel.tol)
    target <- as.double(control$target)
    max.iter <- as.integer(control$max.iter)
    Nu <- as.double(control$Nu)
    out.mode <- as.integer(control$out.mode)
    BigM.K <- as.integer(control$BigM.K)
    BigM.iter <- as.integer(control$BigM.iter)

    n <- length(f)

    # BUILD CONE VARIABLE SELECTORS
    m <- nrow(A)
    L <- length(N)

    SU <- matrix(0, nrow = m, ncol = L)
    ST <- matrix(0, nrow = m, ncol = L)
    T <- rep(0, m)
    k <- 1
    for (i in seq(L)) {
        if (N[i] > 1)
            SU[k:(k + N[i]-2), i] = matrix(1, nrow = N[i] - 1, ncol = 1)
        ST[k + N[i] - 1, i] <- 1
        T[k + N[i] - 1] <- 1
        k <- k + N[i]
    }

    # To define minimum initial slack
    # we take a guess at the order of mag. of u
    # (a bad guess makes for a very off-center initial point
    # so this may need to be edited for specific problems)

    slacka <- max(rowSums(abs(cbind(A,b))))
    
    # PICK ANY PRIMAL POINT so
    x <- rep(0, n)
    u0 <- A %*% x + b               # point in feasible hyperplane
    k <- -t(ST) %*% u0 + sqrt(t(SU) %*% (u0^2)) # cone "infeasibility" vector
    alpha <- slacka + max(k)        # to make all strictly feas.
    x0 <- qr.solve(A,T)
    if (sum(abs(T-A%*% x0)^2)^(1/2) <  10*.Machine$double.eps*sqrt(m)) {
        x <- alpha * x0
    } else {
        if (alpha >= 0) {
            x1 <- c(x, alpha)
            f1 <- c(rep(0,n), 1)
            A1 <- cbind(A,T)
            fit <- rsocp(f1, A1, b, NULL, NULL, N, x1, NULL, NULL,
                control = list(target = 0, rel.tol = -1))
            if (fit$x[n+1] >= 0)
                stop("Phase 1 failed, alpha>=0")
            x <- fit$x[1:n]
        }
    }

    # Return Value:
    x
}

# ------------------------------------------------------------------------------


.socp.phase2 <-
    function(f, A, b, N, x, control)
{
    # A function implemented by Yohan Chalabi
    
    # Description:
    #   Phase 2 cone variable selectors
    
    # Note:
    #   Original Matlab code by mlobo@isl.stanford.edu

    # FUNCTION:

    # Set control parameters:
    abs.tol <- as.double(control$abs.tol)
    rel.tol <- as.double(control$rel.tol)
    target <- as.double(control$target)
    max.iter <- as.integer(control$max.iter)
    Nu <- as.double(control$Nu)
    out.mode <- as.integer(control$out.mode)
    BigM.K <- as.integer(control$BigM.K)
    BigM.iter <- as.integer(control$BigM.iter)

    # BUILD CONE VARIABLE SELECTORS:
    m <- nrow(A)
    L <- length(N)

    SU <- matrix(0, nrow = m, ncol = L)
    ST <- matrix(0, nrow = m, ncol = L)
    T <- rep(0,m)
    k <- 1
    for (i in seq(L)) {
        if (N[i] > 1)
            SU[k:(k + N[i]-2), i] = rep(1, N[i] - 1)
        ST[k + N[i] - 1, i] <- 1
        T[k + N[i] - 1] <- 1
        k <- k + N[i]
    }

    BigM.repeat <- max.iter / BigM.iter

    # To define minimum initial slack
    #    we take a guess at the order of mag. of z
    #   (a bad guess makes for a very off-center initial point
    #   so this may need to be edited for specific problems)
    normA <- max(colSums(abs(A)))
    normf <- max(abs(f))

    slackb <- 0.1 * normf/normA

    # Primal Bound:
    u <- A %*% x + b            # point in feasible hyperplane
    B0 <- BigM.K * (T %*% u)    # bound on sum(t)

    # DUAL FEASIBLE POINT:
    z0 <- qr.solve(t(A),f)      # pick any point satisfying hyperplane constr.
    k <- -t(ST) %*% z0 + sqrt(t(SU)%*%(z0^2)) # cone "infeasibility" vector
    beta <- slackb + max(0,max(k)) # to make all strictly feas. (incl. beta>0)
    z = z0+T*beta;              # add beta to all w(i)
    z1 = c(z, beta)             # extend dual variable

    # EXTENDED PROBLEM:
    A1 <- rbind(A, -T %*% A)    # extend u: tB=B0-sum(t)
    b1 <- c(b, B0 - T %*% b)
    N1 <- c(N, 1)

    i <- 0
    info <- 4
    while (info == 4 & i < BigM.repeat) {
        i <- i + 1
        fit <- rsocp(f, A1, b1, NULL, NULL, N1, x, z1, NULL, control)
        info <- fit$info
        u <- A %*% fit$x + b
        t <- T %*% u
        if (BigM.K * t > B0) {
            b1[m+1] <- b1[m+1] + BigM.K*t-B0
            B0 <- BigM.K*t
        }
    }

    # Check quality of dual solution
    #   (choice of 1e-4 for displaying warnings is arbitrary)
    e1 <- fit$z[m+1] / ((sum(fit$z[1:m]^2))^(1/2))

    if ( (rel.tol >= 0) && (e1 > max(rel.tol,1e-4)))
        warning("Warning: big M bound appears to be active, solution may be wrong.")

    z <- fit$z[1:m]

    # Return Value:
    z
}


################################################################################

        
rsocpControl <- 
    function(abs.tol = 1e-18, rel.tol = 1e-16, target = 0, 
    max.iter = 500, Nu = 10, out.mode = 0, BigM.K = 2, BigM.iter = 5, 
    scale = 1, penalty = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   A copy of function socpControl from package Rsocp
    
    # Note:
    #   Here we have added the entries scale and penealty
    
    # FUNCTION:
    
    # Return Value:
    list(
        abs.tol = abs.tol, 
        rel.tol = rel.tol,
        target = target,
        max.iter = max.iter,
        Nu = Nu,
        out.mode = out.mode,
        BigM.K = BigM.K,
        BigM.iter = BigM.iter,
        scale = scale,
        penalty = penalty)
}


################################################################################

