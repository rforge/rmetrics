
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################
# FUNCTION:               DESCRIPTION:
#  .garchLLH               Computes log-likelihood function
#  .garchLLH.internal       Internal ARMA-APARCH recursion done by Fortran Code
#  .garchLLH.filter         Fast approach using the filter function in R
#  .garchLLH.testing        Simple double loops over time and order in R
#  .garchOptimizeLLH       Opimizes log-likelihood function
################################################################################


.garchLLH <-
function(params, trace = TRUE, fGarchEnv = FALSE)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute Log-Likelihood Function

    # Arguments:
    #   params - a named numeric vector with the model parameters
    #       to be optimized

    # Value:
    #   Returns the value of the max log-likelihood function.

    # Note:
    #   The variables '.series' and '.params' must be global available

    # FUNCTION:

    # DEBUG:
    DEBUG = FALSE
    if (DEBUG) print("Entering Function .garchLLH")

    # Get Global Variables:
    .series <- .getfGarchEnv(".series")
    .params <- .getfGarchEnv(".params")
    .garchDist <- .getfGarchEnv(".garchDist")
    .llh <- .getfGarchEnv(".llh")
    
    # How to calculate the LLH Function?
    if (DEBUG) print(.params$control$llh)

    if(.params$control$llh == "internal")  {
    
        if (DEBUG) print("internal")
        return(.garchLLH.internal(params, trace = TRUE, fGarchEnv = FALSE))
        
    } else if (.params$control$llh == "filter")  {
    
        if (DEBUG) print("filter")
        return(.garchLLH.filter(params, trace = TRUE, fGarchEnv = FALSE))
        
    } else if (.params$control$llh == "testing")  {
    
        if (DEBUG) print("testing")
        return(.garchLLH.testing(params, trace = TRUE, fGarchEnv = FALSE))  
     
    } else {
    
        stop("LLH is neither internal, testing, nor filter!")
    }   
        
}


# ------------------------------------------------------------------------------


.garchLLH.internal <-
function(params, trace = TRUE, fGarchEnv = FALSE)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal ARMA-APARCH recursion done by Fortran Code
   
    # Arguments:
    #   params - a named numeric vector with the model parameters
    #       to be optimized
    #   trace -
    #   fGarchEnv - 
    
    # Value:
    #   Returns the value of the max log-likelihood function.

    # Note:
    #   The variables '.series' and '.params' must be global available

    # FUNCTION:

    # DEBUG:
    DEBUG = FALSE
    if (DEBUG) print("Entering Function .garchLLH.internal")

    # Get Global Variables:
    .series <- .getfGarchEnv(".series")
    .params <- .getfGarchEnv(".params")
    .garchDist <- .getfGarchEnv(".garchDist")
    .llh <- .getfGarchEnv(".llh")
    
    # How to calculate the LLH Function?
    if (DEBUG) print(.params$control$llh)

    if(.params$control$llh == "internal") {

        INDEX <- .params$index
        MDIST <- c(norm = 10, QMLE = 10, snorm = 11, std = 20, sstd = 21,
            ged = 30, sged = 31)[.params$cond.dist]
        if(.params$control$fscale) NORM <- length(.series$x) else NORM = 1
        REC <- 1
        if(.series$init.rec == "uev") REC <- 2
        MYPAR <- c(
            REC   = REC,                                  # How to initialize
            LEV   = as.integer(.params$leverage),         # Include Leverage 0|1
            MEAN  = as.integer(.params$includes["mu"]),   # Include Mean 0|1
            DELTA = as.integer(.params$includes["delta"]),# Include Delta 0|1
            SKEW  = as.integer(.params$includes["skew"]), # Include Skew 0|1
            SHAPE = as.integer(.params$includes["shape"]),# Include Shape 0|1
            ORDER = .series$order,                        # Order of ARMA-GARCH
            NORM  = as.integer(NORM))

        # Now Estimate Parameters:
        MAX <- max(.series$order)
        NF <- length(INDEX)
        N <- length(.series$x)
        DPARM <- c(.params$delta, .params$skew, .params$shape)
        fit <- .Fortran(
            "garchllh",
            N = as.integer(N),
            Y = as.double(.series$x),
            # Z = as.double(rep(2, times = N)),
            # H = as.double(rep(0, times = N)),
            Z = as.double(.series$z),
            H = as.double(.series$h),
            NF = as.integer(NF),
            X = as.double(params),
            DPARM = as.double(DPARM),
            MDIST = as.integer(MDIST),
            MYPAR = as.integer(MYPAR),
            F = as.double(0),
            PACKAGE = "fGarch")

        llh <- fit[[10]]

        if(is.na(llh)) llh = .llh + 0.1*(abs(.llh))
        if(!is.finite(llh)) llh = .llh + 0.1*(abs(.llh))
        .setfGarchEnv(.llh = llh)

        if (fGarchEnv) {
            # Save h and z:
            .series$h <- fit[[4]]
            .series$z <- fit[[3]]
            .setfGarchEnv(.series = .series)
        }

    } else {

        stop("LLH is not internal!")

    }

    # Return Value:
    c(LogLikelihood = llh)
}


# ------------------------------------------------------------------------------


.garchLLH.filter <-
function(params, trace = TRUE, fGarchEnv = FALSE)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #  Fast approach using the filter function in R for ARMA-APARCH models

    # Arguments:
    #   params - a named numeric vector with the model parameters
    #       to be optimized
    #   trace - a logical, should the frunction output be traced ?
    #   fGarchEnv - 
    
    # Value:
    #   Returns the value of the max log-likelihood function.

    # Note:
    #   The variables '.series' and '.params' must be global available

    # FUNCTION:

    # DEBUG:
    DEBUG = FALSE
    if (DEBUG) print("Entering Function .garchLLH.filter")

    # Get Global Variables:
    .series <- .getfGarchEnv(".series")
    .params <- .getfGarchEnv(".params")
    .garchDist <- .getfGarchEnv(".garchDist")
    .llh <- .getfGarchEnv(".llh")
    
    # Which conditional distribution function should be used ?
    if(DEBUG) print(.garchDist)
    
    # How to calculate the LLH Function?
    if (DEBUG) print(c("testing ?", .params$control$llh))

    if(.params$control$llh == "filter") {

        # Retrieve From Initialized Series:
        x = .series$x
    
        # Get Order:
        u = .series$order[1]
        v = .series$order[2]
        p = .series$order[3]
        q = .series$order[4]
        max.order = max(u, v, p, q)
    
        # Get Start Conditions:
        h.start = .series$h.start
        llh.start = .series$llh.start
    
        # Get the Index Values and Add Names - Just to be Sure:
        index = .params$index
        names(params) = names(.params$params[index])
        Names = names(params)
    
        # Retrieve From Initialized Parameters:
        cond.dist = .params$cond.dist
    
        # Extracting the parameters by name ...
        alpha <- beta <- NULL
        mu = c(mu = .params$mu)
        delta = c(delta = .params$delta)
        skew = c(skew = .params$skew)
        shape = c(shape = .params$shape)
        leverage = c(leverage = .params$leverage)
        if(.params$includes["mu"]) mu = params["mu"]
        if(u > 0) ar = params[substr(Names, 1, 2) == "ar"]
        if(v > 0) ma = params[substr(Names, 1, 2) == "ma"]
        omega = params[substr(Names, 1, 5) == "omega"]
        if(p > 0) alpha = params[substr(Names, 1, 5) == "alpha"]
        if(p > 0 & leverage) gamma = params[substr(Names, 1, 5) == "gamma"]
        if(p > 0 & !leverage) gamma = rep(0, times = p)
        if(q > 0) beta  = params[substr(Names, 1, 4) == "beta"]
        if(.params$includes["delta"]) delta = params["delta"]
        if(.params$includes["skew"])  skew  = params["skew"]
        if(.params$includes["shape"]) shape = params["shape"]
        if(DEBUG) print(params)
        
        # Iterate z:
        N = length(x)
        z = rep(0, N)
        if(u > 0 & v > 0)
            for (i in (h.start):N)
                z[i] = x[i] - mu - sum(ar*x[i-(1:u)]) - sum(ma*z[i-(1:v)])
        if(u > 0 & v == 0)
            for (i in (h.start):N)
                z[i] = x[i] - mu - sum(ar*x[i-(1:u)])
        if(u == 0 & v > 0)
            for (i in (h.start):N)
                z[i] = x[i] - mu - sum(ma*z[i-(1:v)])
        if(u == 0 & v == 0)
            z = x - mu
    
        # Initialize Variance Equation:
        deltainv = 1/delta
        if(.series$model[2] == "garch") {
            persistence = sum(alpha) + sum(beta)
        } else if(.series$model[2] == "aparch") {
            persistence = sum(beta)
            for (i in 1:p)
                persistence = persistence + alpha[i]*garchKappa(cond.dist,
                    gamma[i], delta, skew, shape)
        }
        names(persistence) = "persistence"
        attr(persistence, "control") = NULL
        attr(persistence, "cond.dist") = NULL
        .params$persistence <- persistence
        .setfGarchEnv(.params = .params)
        mvar = mean(z^2)
        h = rep(omega + persistence*mvar, N)
    
        # Iterate Conditional Variances h:
        if(p == 0) {
            alpha = 0
            p = 1
        }
        if(q == 0) {
            beta = 0
            q = 1
        }
    
        # R Filter Representation:
        # Entirely written in S, and very effective ...
        # own filter method because as.ts and tsp time consuming...
       
        # Note, sometimes one of the beta's can become undefined
        # during optimization.
        if(!.params$leverage) gamma = rep(0, p)
        pq = max(p, q)
        edeltat = 0
        for (j in 1:p) {
            Filter = rep(0, length = p+1)
            Filter[j+1] = alpha[j]
            edelta = (abs(z) - gamma[j]*z)^delta
            edelta = .filter2(edelta, filter = Filter, sides = 1)
            edeltat = edeltat + edelta
        }
        c.init = omega/(1-sum(beta))
        h = c( h[1:pq], c.init + .filter2(edeltat[-(1:pq)], filter = beta,
             method = "recursive", init = h[q:1]-c.init))
             
        ### ? remove ? ### DW: May be not .
        if( sum(is.na(h)) > 0 ) {
            # We use the testing Version ...
            warning("Problems in Filter Representation")
            if(!.params$leverage) {
                for (i in (h.start):N) {
                    h[i] = omega +
                        sum(alpha * ( abs(z[i-(1:p)])) ^ delta ) +
                        sum(beta*h[i-(1:q)])
                }
            } else {
                for (i in (h.start):N) {
                    h[i] = omega +
                        sum(alpha * ( abs(z[i-(1:p)]) -
                        gamma * z[i-(1:p)])^delta ) + sum(beta*h[i-(1:q)])
                }
            }
        }
    
        # Calculate Log Likelihood:
        hh = (abs(h[(llh.start):N]))^deltainv
        zz = z[(llh.start):N]
        llh = -sum(log(.garchDist(z = zz, hh = hh, skew = skew, shape = shape)))
        if(DEBUG) cat("DEBUG - LLH:   ", llh, "\n")
        names(params) = names(.params$params[.params$index])
        if(is.na(llh)) llh = .llh + 0.1*(abs(.llh))
        if(!is.finite(llh)) llh = .llh + 0.1*(abs(.llh))
    
        # Print if LLH has Improved:
        if(llh < .llh) {
            diff = (.llh - llh)/llh
            if(trace & diff > 1e-2) {
                # cat(" LLH: ", llh, "   norm LLH: ", llh/N, "\n")
                # print(params)
                if(persistence > 1)
                    cat("Warning - Persistence:", persistence, "\n")
            }
            .setfGarchEnv(.llh = llh)
        }
    
        if (fGarchEnv) {
            # Save h and z:
            .series$h <- h
            .series$z <- z
            .setfGarchEnv(.series = .series)
        }

    } else {

        stop("LLH is not filter!")

    }
    
    # Return Value:
    if (DEBUG) print("Entering Function .garchLLH.filter")
    c(LogLikelihood = llh)
}


# ------------------------------------------------------------------------------


.garchLLH.testing <-
function(params, trace = TRUE, fGarchEnv = FALSE)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute Log-Likelihood Function for ARMA-APARCH models

    # Arguments:
    #   params - a named numeric vector with the model parameters
    #       to be optimized
    #   trace -
    #   fGarchEnv - 
    
    # Value:
    #   Returns the value of the max log-likelihood function.

    # Note:
    #   The variables '.series' and '.params' must be global available

    # FUNCTION:

    # DEBUG:
    DEBUG = FALSE
    if (DEBUG) print("Entering Function .garchLLH.testing")

    # Get Global Variables:
    .series <- .getfGarchEnv(".series")
    .params <- .getfGarchEnv(".params")
    .garchDist <- .getfGarchEnv(".garchDist")
    .llh <- .getfGarchEnv(".llh")
    if(DEBUG) print(.garchDist)
    
    # How to calculate the LLH Function?
    if (DEBUG) print(.params$control$llh)

    if(.params$control$llh == "testing") {

        # Retrieve From Initialized Series:
        x = .series$x
    
        # Get Order:
        u = .series$order[1]
        v = .series$order[2]
        p = .series$order[3]
        q = .series$order[4]
        max.order = max(u, v, p, q)
    
        # Get Start Conditions:
        h.start = .series$h.start
        llh.start = .series$llh.start
    
        # Get the Index Values and Add Names - Just to be Sure:
        index = .params$index
        names(params) = names(.params$params[index])
        Names = names(params)
    
        # Retrieve From Initialized Parameters:
        cond.dist = .params$cond.dist
        if(DEBUG) print(paste("Conditional Distribution:", cond.dist))
    
        # Extracting the parameters by name ...
        alpha <- beta <- NULL
        mu = c(mu = .params$mu)
        delta = c(delta = .params$delta)
        skew = c(skew = .params$skew)
        shape = c(shape = .params$shape)
        leverage = c(leverage = .params$leverage)
        if(.params$includes["mu"]) mu = params["mu"]
        if(u > 0) ar = params[substr(Names, 1, 2) == "ar"]
        if(v > 0) ma = params[substr(Names, 1, 2) == "ma"]
        omega = params[substr(Names, 1, 5) == "omega"]
        if(p > 0) alpha = params[substr(Names, 1, 5) == "alpha"]
        if(p > 0 & leverage) gamma = params[substr(Names, 1, 5) == "gamma"]
        if(p > 0 & !leverage) gamma = rep(0, times = p)
        if(q > 0) beta  = params[substr(Names, 1, 4) == "beta"]
        if(.params$includes["delta"]) delta = params["delta"]
        if(.params$includes["skew"])  skew  = params["skew"]
        if(.params$includes["shape"]) shape = params["shape"]
        if(DEBUG) print(params)
    
        # Iterate z:
        N = length(x)
        z = rep(0, N)
        if(u > 0 & v > 0)
            for (i in (h.start):N)
                z[i] = x[i] - mu - sum(ar*x[i-(1:u)]) - sum(ma*z[i-(1:v)])
        if(u > 0 & v == 0)
            for (i in (h.start):N)
                z[i] = x[i] - mu - sum(ar*x[i-(1:u)])
        if(u == 0 & v > 0)
            for (i in (h.start):N)
                z[i] = x[i] - mu - sum(ma*z[i-(1:v)])
        if(u == 0 & v == 0)
            z = x - mu
    
        # Initialize Variance Equation:
        deltainv = 1/delta
        if(.series$model[2] == "garch") {
            persistence = sum(alpha) + sum(beta)
        } else if(.series$model[2] == "aparch") {
            persistence = sum(beta)
            for (i in 1:p)
                persistence = persistence + alpha[i]*garchKappa(cond.dist,
                    gamma[i], delta, skew, shape)
        }
        names(persistence) = "persistence"
        attr(persistence, "control") = NULL
        attr(persistence, "cond.dist") = NULL
        .params$persistence <- persistence
        .setfGarchEnv(.params = .params)
        mvar = mean(z^2)
        h = rep(omega + persistence*mvar, N)
    
        # Initial Values to Iterate Conditional Variances h:
        if(p == 0) {
            alpha = 0
            p = 1
        }
        if(q == 0) {
            beta = 0
            q = 1
        }
    
        # Test Version Just a Simple Double 'for' Loop:
        # As You Can Imagine, Slow Version But Very Useful for Testing:
        if(!.params$leverage) {
            for (i in (h.start):N) {
                h[i] = omega +
                    sum(alpha * ( abs(z[i-(1:p)])) ^ delta ) +
                    sum(beta*h[i-(1:q)])
            }
        } else {
            for (i in (h.start):N) {
                h[i] = omega +
                    sum(alpha * ( abs(z[i-(1:p)]) -
                    gamma * z[i-(1:p)])^delta ) + sum(beta*h[i-(1:q)])
            }
        }
    
        # Calculate Log Likelihood:
        hh = (abs(h[(llh.start):N]))^deltainv
        zz = z[(llh.start):N]
        llh = -sum(log(.garchDist(z = zz, hh = hh, skew = skew, shape = shape)))
        if(DEBUG) cat("DEBUG - LLH:   ", llh, "\n")
        names(params) = names(.params$params[.params$index])
        if(is.na(llh)) llh = .llh + 0.1*(abs(.llh))
        if(!is.finite(llh)) llh = .llh + 0.1*(abs(.llh))
    
        # Print if LLH has Improved:
        if(llh < .llh) {
            diff = (.llh - llh)/llh
            if(trace & diff > 1e-2) {
                # cat(" LLH: ", llh, "   norm LLH: ", llh/N, "\n")
                # print(params)
                if(persistence > 1)
                    cat("Warning - Persistence:", persistence, "\n")
            }
            .setfGarchEnv(.llh = llh)
        }
    
        if (fGarchEnv) {
            # Save h and z:
            .series$h <- h
            .series$z <- z
            .setfGarchEnv(.series = .series)
        }
        
    } else {

        stop("LLH is not testing!")

    }

    # Return Value:
    if (DEBUG) print("Leaving Function .garchLLH.testing")
    c(LogLikelihood = llh)
}


# ------------------------------------------------------------------------------


.garchOptimizeLLH <-
function(hessian = hessian, robust.cvar, trace)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Opimizes the Log-Likelihood Function

    # Arguments:
    #   hessian - the Hessian matrix
    #   robust.cvar - a logical
    #   trace - a logical

    # FUNCTION:

    # DEBUG:
    DEBUG = FALSE
    if (DEBUG) print("Entering Function .garchOptimizeLLH")

    # get global variables
    .series <- .getfGarchEnv(".series")
    .params <- .getfGarchEnv(".params")

    # Initialization:
    INDEX = .params$index

    # Algorithm:
    algorithm = .params$control$algorithm[1]
    TOL1 = .params$control$tol1
    TOL2 = .params$control$tol2
    if(trace) {
        cat("\n\n--- START OF TRACE ---")
        cat("\nSelected Algorithm:", algorithm, "\n")
    }

    # First Method: 
    # Two Step Apparoach > Trust Region + Nelder-Mead Simplex
    if(algorithm == "nlminb" | algorithm == "nlminb+nm") {
        fit <- .garchRnlminb(.params, .series, .garchLLH, trace)
        .params$llh = fit$llh
        .params$params[INDEX] = fit$par
        .setfGarchEnv(.params = .params)
    }
    if(algorithm == "nlminb+nm") {
        fit <- .garchRnm(.params, .series, .garchLLH, trace)
            .params$llh = fit$llh
            .params$params[INDEX] = fit$par
            .setfGarchEnv(.params = .params)
    }

    # Second Method:
    # Two Step Approach > BFGS + Nelder-Mead Simplex
    if(algorithm == "lbfgsb" | algorithm == "lbfgsb+nm") {
        fit <- .garchRlbfgsb(.params, .series, .garchLLH, trace)
        .params$llh = fit$llh
        .params$params[INDEX] = fit$par
        .setfGarchEnv(.params = .params)
    }
    if(algorithm == "lbfgsb+nm") {
        fit <- .garchRnm(.params, .series, .garchLLH, trace)
        .params$llh = fit$llh
        .params$params[INDEX] = fit$par
        .setfGarchEnv(.params = .params)
    }

    # Save parameters:
    .params$llh = fit$llh
    .params$params[INDEX] = fit$par
    .setfGarchEnv(.params = .params)

    # Compute the Hessian:
    if (hessian == "ropt") {
        fit$hessian <- - .garchRoptimhess(par = fit$par, .params = .params,
            .series = .series)
        titleHessian = "R-optimhess"
    } else if (hessian == "rcd") {
        fit$hessian <- - .garchRCDAHessian(par = fit$par, .params = .params,
            .series = .series)
        titleHessian = "Central"
    } else if (hessian == "rts") {
        fit$hessian <- - .garchTSHessian(par = fit$par, .params = .params,
            .series = .series)
        titleHessian = "Two Sided"
    }

    # Rescale Parameters:
    if (.params$control$xscale) {
        .series$x <- .series$x * .series$scale
        if (.params$include["mu"])
            fit$coef["mu"] <- fit$par["mu"] <- .params$params["mu"] <-
                .params$params["mu"]*.series$scale
        if (.params$include["omega"])
            fit$coef["omega"] <- fit$par["omega"] <- .params$params["omega"] <-
                .params$params["omega"]*.series$scale^(.params$params["delta"])
        # save changes
        .setfGarchEnv(.params = .params)
        .setfGarchEnv(.series = .series)
    }

    # Rescale Hessian Matrix:
    if (.params$control$xscale) {
        if (.params$include["mu"]) {
            fit$hessian[,"mu"] <- fit$hessian[,"mu"] /  .series$scale
            fit$hessian["mu",] <- fit$hessian["mu",] /  .series$scale
        }
        if (.params$include["omega"]) {
            fit$hessian[,"omega"] <-
                fit$hessian[,"omega"] / .series$scale^(.params$params["delta"])
            fit$hessian["omega",] <-
                fit$hessian["omega",] / .series$scale^(.params$params["delta"])
        }
    }

    # Recalculate llh, h, z with Rescaled Parameters:
    .llh <- fit$llh <- fit$value <-
        .garchLLH(fit$par, trace = FALSE, fGarchEnv = TRUE)
    .series <- .getfGarchEnv(".series")

    # Compute the Gradient:
    # YC: needs to be after the calculation of h, z !
    if (robust.cvar)
        fit$gradient <- - .garchRCDAGradient(
            par = fit$par, .params = .params, .series = .series)
            
    # Compute Information Criterion Statistics:
    N = length(.series$x)
    NPAR = length(fit$par)
    fit$ics = c(
        AIC  = c((2*fit$value)/N + 2 * NPAR/N),
        BIC  = (2*fit$value)/N + NPAR * log(N)/N,
        SIC  = (2*fit$value)/N + log((N+2*NPAR)/N),
        HQIC = (2*fit$value)/N + (2*NPAR*log(log(N)))/N )
    names(fit$ics) <- c("AIC", "BIC", "SIC", "HQIC")

    # Print LLH if we trace:
    if(trace) {
        cat("\nFinal Estimate of the Negative LLH:\n")
        cat(" LLH: ", .llh, "   norm LLH: ", .llh/N, "\n")
        print(fit$par)
    }

    # Print Hessian Matrix if we trace:
    if(trace) {
        cat("\n", titleHessian, " Difference Approximated Hessian Matrix:\n",
            sep = "")
        print(fit$hessian)
        cat("\n--- END OF TRACE ---\n\n")
    }

    # Return Value:
    if (DEBUG) print("Entering Function .garchOptimizeLLH")
    fit
}


# ------------------------------------------------------------------------------


.filter2 <-
function (x, filter, method = c("convolution", "recursive"),
    sides = 2, circular = FALSE, init = NULL)
{
    # Description:
    #   Fast filter function based on top of stats::filter() 
    
    # Arguments:
    
    # Note: 
    #   This function is called from .garchLLH.filter 
    
    # FUNCTION:
    
    # DEBUG:
    DEBUG = FALSE
    if (DEBUG) print("Entering Function .filter2")
    
    # Settings:
    method <- match.arg(method)
    x <- as.matrix(x)
    n <- nrow(x)
    nser <- ncol(x)
    nfilt <- length(filter)
    if (any(is.na(filter))) stop("missing values in 'filter'")
    y <- matrix(NA, n, nser)
    
    # Convolution Filter:
    if (method == "convolution") {
        if (nfilt > n)
            stop("'filter' is longer than time series")
        if (sides != 1 && sides != 2)
            stop("argument 'sides' must be 1 or 2")
        for (i in 1:nser) y[, i] <-
            .C("filter1", 
            as.double(x[, i]),
               as.integer(n), 
               as.double(filter), 
               as.integer(nfilt),
               as.integer(sides), 
               as.integer(circular), 
               out = double(n),
               NAOK = TRUE, 
               PACKAGE = "stats")$out
    
    # Recursive Filter:
    } else {
        if (missing(init)) {
            init <- matrix(0, nfilt, nser)
        } else {
            ni <- NROW(init)
            if (ni != nfilt)
                stop("length of 'init' must equal length of 'filter'")
            if (NCOL(init) != 1 && NCOL(init) != nser)
                stop(gettextf("'init'; must have 1 or %d cols",
                    nser), domain = NA)
            if (!is.matrix(init))
                init <- matrix(init, nfilt, nser)
        }
        for (i in 1:nser) y[, i] <-
            .C("filter2", 
                as.double(x[, i]),
                as.integer(n), 
                as.double(filter), 
                as.integer(nfilt),
                out = as.double(c(rev(init[, i]), 
                double(n))), 
                NAOK = TRUE,
                PACKAGE = "stats")$out[-(1:nfilt)]
    }
    
    # Return Value:
    if (DEBUG) print("Entering Function .filter2")
    y
}


################################################################################

