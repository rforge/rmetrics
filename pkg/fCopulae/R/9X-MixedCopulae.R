
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
# TAIL DEPENDENCE ESTIMATOR 
#   derived from the "Mixed Gumbel-SurvivalGumbel-Normal Copula" approach
#   supported Marginals: Normal, NIG and Generalized Hyperbolic Student-t


################################################################################
# FUNCTION:                    GUMBEL COPULA:
#  .rgumbelCopula               Generates fast Gumbel copula random variates
#  .dgumbelCopula               Computes Gumbel copula probability
#  .pgumbelCopula               Computes Gumbel copula probability
# FUNCTION:                    MIXED GUMBEL-SURVIVALGUMBEL-NORMAL COPULA:
#  .rgsgnormCopula              Generates G-SG-NORM copula random variates
#  .dgsgnormCopula              Computes G-SG-NORM copula probability
#  .gsgnormCopulaFit            Computes G-SG-NORM copula probability
# FUNCTION:                    NON-PARAMETRIC TAIL DEPENDECY ESTIMATOR:
#  .cfgTDE                      Estimates non-parametrically tail dependence
# FUNCTION:                    COPULA FIT WITH NIG MARGINALS:
#  .normDependencyFit           Estimates tail dependence with normal marginals
#  .nigDependencyFit            Estimates tail dependence with NIG marginals  
#  .ghtDependencyFit            Estimates tail dependence with GHT marginals   
################################################################################


################################################################################
# Gumbel Copula


.rgumbelCopula =
function(n, alpha = 2)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates fast Gumbel copula random variates
    
    # Arguments:
    
    # FUNCTION:
    
    # RVs:
    dim = 2
    theta <- runif(n, 0, pi)
    w <- rexp(n)
    b = 1/alpha
    a <- sin((1-b)*theta)*(sin(b*theta))^(b/(1-b))/(sin(theta))^(1/(1-b))
    fr = (a/w)^((1-b)/b)
    fr <- matrix(fr, nrow = n, ncol = dim)
    val <- matrix(runif(dim * n), nrow = n)
    s = -log(val)/fr
    ans = exp(-s^(1/alpha))
    
    control = list(alpha = alpha, copula = "archm", type = "gumbel")
    attr(ans, "control") <- unlist(control)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.dgumbelCopula = 
function(u = 0.5, v = u, alpha = 2, output = c("vector", "list"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Bivariate Gumbel Copula Density
    
    # FUNCTION:
    
    # Conveniance Wrapper:
    ans = darchmCopula(u, v, alpha, type = "4", output = output)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.pgumbelCopula = 
function(u = 0.5, v = u, alpha = 2, output = c("vector", "list"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Bivariate Gumbel Copula Probability
    
    # FUNCTION:
    
    # Conveniance Wrapper:
    ans = parchmCopula(u, v, alpha, type = "4", output = output)
    
    # Return Value:
    ans
}
 
    
################################################################################
# Mixed Gumbel-SurvivalGumbel-Normal Copula


.rgsgnormCopula = 
function(n = 1000, alpha = c(2, 2), rho = 0, gamma = c(0.5, 0.5))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes RVs from a mixed GSG copula
    
    # Example:
    #   .rgsgnormCopula(20)
    
    # FUNCTION:
        
    # Upper Gumbel = 1 , Lower Gumbel = 2, t = 3:
    n1 = round(gamma[1]*n)
    n2 = n - n1
    n1 = round(gamma[2]*n1)
    n2 = round(gamma[2]*n2)
    n3 = n - n1 - n2
    
    # Random Variates:
    r = rbind(
        if (n1 > 0) .rgumbelCopula(n1, alpha1),
        if (n2 > 0) 1-.rgumbelCopula(n2, alpha2),
        if (n3 > 0) rellipticalCopula(n3, rho, type = "norm") )
    index = sample(1:n)
    ans = r[index, ]
    N = c(n, n1, n2, n3)
    names(N) = c("n", "n1", "n2", "n3")
    attr(ans, "control")<-N
    
    # Return Value:
    ans
} 


# ------------------------------------------------------------------------------


.dgsgnormCopula = 
function(u = 0.5, v = u, alpha = c(2, 2), rho = 0, gamma = c(0.5, 0.5))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes mixed GSG copula density
    
    # Example:
    #   .dgsgnormCopula(grid2d()$x, grid2d()$y)
   
    # FUNCTION:
    
    # Settings:
    if (is.list(u)) {
        v = u[[2]]
        u = u[[1]]
    }
    if (is.matrix(u)) {
        v = u[, 1]
        u = u[, 2]
    }
    
    # Mix Gumbel + Survival Gumbel:
    dCopula1 = .dgumbelCopula(u, v, alpha[1], output = "list")
    dCopula2 = .dgumbelCopula(1-u, 1-v, alpha[2], output = "list")
    dCopula12 = dCopula1
    dCopula12$z = gamma[1]*dCopula1$z + (1-gamma[1])*dCopula2$z
    
    # Mix Gumbel/SurvivalGumbel + Student-t:
    dCopula3 = dellipticalCopula(u, v, rho, type = "norm", output = "list")
    dCopula123 = dCopula12
    dCopula123$z = gamma[2]*dCopula12$z + (1-gamma[2])*dCopula3$z
    ans = dCopula123
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.gsgnormCopulaFit =
function(u, v, trace = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits parameters for a mixed GSG copula
    
    # FUNCTION:
    
    # Settings:
    if (is.list(u)) {
        v = u[[2]]
        u = u[[1]]
    }
    if (is.matrix(u)) {
        v = u[, 1]
        u = u[, 2]
    }

    # Settings:
    u <<- u
    v <<- v
    .steps <<- 0
    .trace <<- trace
     
    # Estimate Copula:
    start = c(1.5, 1.5, 0, 1/3, 1/3)
    fun = function(x) {
        .steps <<- .steps + 1
        density = .dgsgnormCopula(u = u, v = v, 
            alpha = x[1:2], rho = x[3], gamma = x[4:5])$z
        density = density[!is.na(density)]
        f = -mean( log(density) )
        if (.trace) {
            cat("\n Optimization Step:         ", .steps)
            cat("\n Objective Function Value:  ", -f)
            cat("\n Parameter Estimates:       ",   
                round(c(x[1:3], x[5]*x[4], x[5]*(1-x[4]), 1-x[5]), 4), "\n")
        }
        f
    }

    # Fit:
    fit = nlminb(start = start, objective = fun, 
        lower = c(  1,   1, -0.999, 0, 0), 
        upper = c(Inf, Inf,  0.999, 1, 1))
        
    param = fit$par
    alpha1 = param[1]
    alpha2 = param[2]
    gamma1 = param[4]
    gamma2 = param[5]
    Upper = (gamma2*gamma1*(2 - 2^(1/alpha1)))[[1]]
    Lower = (gamma2*(1-gamma1)*(2 - 2^(1/alpha2)))[[1]]
    Param = c(param[1:3], param[5]*param[4], param[5]*(1-param[4]), 1-param[5])
    names(Param) = c("alpha1", "alpha2", "rho", "gumbel", "survival", "norm")
    Lambda = c(lower = Lower, upper = Upper)
      
    # Return Value:
    list(param = Param, lambda = Lambda, fitted = fit$par)
}


################################################################################
# Non-Parametric Tail Dependence Estimator


.cfgTDE = 
function(x, y)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Estimates non-parametrically tail dependency coefficient
    
    # FUNCTION:
    
    # Upper Tail:
    lambda = NULL
    n = length(x)
    for(i in 1:n){
        lambda = c(lambda,
            log(sqrt(log(1/x[i])*log(1/y[i]))/log(1/max(x[i],y[i])^2)))
    }
    upper = 2-2*exp(sum(lambda/n))
    
    # Lower Tail:
    x = 1-x
    y = 1-y
    lambda = NULL
    n = length(x)
    for(i in 1:n){
        lambda = c(lambda,
            log(sqrt(log(1/x[i])*log(1/y[i]))/log(1/max(x[i],y[i])^2)))
    }
    lower = 2-2*exp(sum(lambda/n))
    
    # Return Value:
    c(lower = lower, upper = upper)
}


################################################################################
# GSGNORM Parametric Tail Dependence Estimator


.normDependencyFit = 
function(x, doplot = TRUE, trace = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Estimates tail dependency coefficients with Normal marginals
    
    # Arguments:
    #   x - a multivariate 'timeSeries' object
    
    # FUNCTION:
    
    # Settings: 
    N = ncol(x)
    lowerLambda = upperLambda = 0*diag(N)
    assetsNames = colnames(x)
    P = NULL
    
    for (i in 1:(N-1)) {
        # First asset:
        r1 = as.vector(x[, i])
        fit1 = normFit(r1)
        estim1 = fit1$estimate
        p1 = pnorm(r1, estim1[1], estim1[2]) 
        Main1 = assetsNames[i]
        P = cbind(P, p1)
        for (j in (i+1):N) 
        {  
            # Second asset:
            r2 = as.vector(x[, j])
            fit2 = normFit(r2) 
            estim2 = fit2$estimate      
            p2 = pnorm(r2, estim2[1], estim2[2]) 
            Main2 = assetsNames[j]
            
            # Optional Plot:
            if (doplot) 
            {
                # Plot Distribution:
                MainR = paste("Distribution:", Main1, "-", Main2)
                plot(r1, r2, pch = 19, main = MainR)
                grid()
                
                # Plot Copula:
                MainP = paste("Copula:", Main1, "-", Main2)
                plot(p1, p2, pch = 19, main = MainP)
                grid()
            }
            
            # Fit GSG copula parameters:
            fit = .gsgnormCopulaFit(u = p1, v = p2, trace = FALSE)
            if (trace)
                cat(assetsNames[c(i,j)], round(fit$lambda, 3), "\n")  
            
                # Compose lambda Matrix:
            lowerLambda[i, j] = lowerLambda[j, i] = fit$lambda[1]
            upperLambda[i, j] = upperLambda[j, i] = fit$lambda[2]
        }
    }
    
    # Result:
    colnames(lowerLambda) = rownames(lowerLambda) = assetsNames
    colnames(upperLambda) = rownames(upperLambda) = assetsNames
    ans = list(lower = lowerLambda, upper = upperLambda)
     
    # Return Value:
    ans 
}


# ------------------------------------------------------------------------------


.nigDependencyFit = 
function(x, doplot = TRUE, trace = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Estimates tail dependency coefficients with NIG marginals
    
    # Arguments:
    #   x - a multivariate 'timeSeries' object
    
    # FUNCTION:
    
    # Settings:
    N = ncol(x)
    lowerLambda = upperLambda = 0*diag(N)
    assetsNames = colnames(x)
    P = NULL
    
    for (i in 1:(N-1)) {
        # First asset:
        r1 = as.vector(x[, i])
        fit1 = nigFit(r1, doplot = FALSE)
        estim1 = fit1@fit$estimate
        p1 = pnig(r1, estim1[1], estim1[2], estim1[3], estim1[4]) 
        Main1 = assetsNames[i]
        P = cbind(P, p1)
        for (j in (i+1):N) {  
            # Second asset:
            r2 = as.vector(x[, j])
            fit2 = nigFit(r2, doplot = FALSE) 
            estim2 = fit2@fit$estimate      
            p2 = pnig(r2, estim2[1], estim2[2], estim2[3], estim2[4]) 
            Main2 = assetsNames[j]
            # Optional Plot:
            if (doplot) {
                MainR = paste("Distribution:", Main1, "-", Main2)
                plot(r1, r2, pch = 19, main = MainR)
                grid()
                MainP = paste("Copula:", Main1, "-", Main2)
                plot(p1, p2, pch = 19, main = MainP)
                grid()
            }
            # Fit GSG copula parameters:
            fit = .gsgnormCopulaFit(u = p1, v = p2, trace = FALSE)
            if (trace)
                cat(assetsNames[c(i,j)], round(fit$lambda, 3), "\n")  
            # Compose lambda Matrix:
            lowerLambda[i, j] = lowerLambda[j, i] = fit$lambda[1]
            upperLambda[i, j] = upperLambda[j, i] = fit$lambda[2]
        }
    }
    
    # Result:
    colnames(lowerLambda) = rownames(lowerLambda) = assetsNames
    colnames(upperLambda) = rownames(upperLambda) = assetsNames
    ans = list(lower = lowerLambda, upper = upperLambda)
     
    # Return Value:
    ans 
}


# ------------------------------------------------------------------------------


.ghtDependencyFit = 
function(x, doplot = TRUE, trace = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Estimates tail dependency coefficients with GH Student-t marginals
    
    # Arguments:
    #   x - a multivariate 'timeSeries' object
    
    # FUNCTION:
    
    # Settings: 
    N = ncol(x)
    lowerLambda = upperLambda = 0*diag(N)
    assetsNames = colnames(x)
    P = NULL
    
    for (i in 1:(N-1)) {
        # First asset:
        r1 = as.vector(x[, i])
        fit1 = nigFit(r1, doplot = FALSE)
        estim1 = fit1@fit$estimate
        p1 = pnig(r1, estim1[1], estim1[2], estim1[3], estim1[4]) 
        Main1 = assetsNames[i]
        P = cbind(P, p1)
        for (j in (i+1):N) {  
            # Second asset:
            r2 = as.vector(x[, j])
            fit2 = nigFit(r2, doplot = FALSE) 
            estim2 = fit2@fit$estimate      
            p2 = pnig(r2, estim2[1], estim2[2], estim2[3], estim2[4]) 
            Main2 = assetsNames[j]
            # Optional Plot:
            if (doplot) {
                MainR = paste("Distribution:", Main1, "-", Main2)
                plot(r1, r2, pch = 19, main = MainR)
                grid()
                MainP = paste("Copula:", Main1, "-", Main2)
                plot(p1, p2, pch = 19, main = MainP)
                grid()
            }
            # Fit GSG copula parameters:
            fit = .gsgnormCopulaFit(u = p1, v = p2, trace = FALSE)
            if (trace)
                cat(assetsNames[c(i,j)], round(fit$lambda, 3), "\n")  
            # Compose lambda Matrix:
            lowerLambda[i, j] = lowerLambda[j, i] = fit$lambda[1]
            upperLambda[i, j] = upperLambda[j, i] = fit$lambda[2]
        }
    }
    
    # Result:
    colnames(lowerLambda) = rownames(lowerLambda) = assetsNames
    colnames(upperLambda) = rownames(upperLambda) = assetsNames
    ans = list(lower = lowerLambda, upper = upperLambda)
     
    # Return Value:
    ans 
}
  

################################################################################
# Examples


if (FALSE) 
{
    require(fCopulae)
    
    # Simulated data:
    x = .rnormCopula(1000, rho = 0.7)
    .gsgnormCopulaFit(x, trace = TRUE)
    
    # LPP Portfolio:
    require(fPortfolio)
    data(LPP2005REC)
    x = 100 * as.timeSeries(LPP2005REC)[, 1:6]
    head(x)
    
    # Tail Dependency Estimation:
    par(mfrow = c(2,2), cex = 0.7)   
    ans = .nigDependencyFit(x) 
    ans 
    
    #         Lower Upper
    # SBI SPI 0     0 
    # SBI SII 0.055 0 
    # SBI LMI 0.064 0.069 
    # SBI MPI 0     0 
    # SBI ALT 0     0 
    # SPI SII 0     0.064 
    # SPI LMI 0     0.072 
    # SPI MPI 0.352 0.214 
    # SPI ALT 0.273 0.048 
    # SII LMI 0.075 0 
    # SII MPI 0     0.164 
    # SII ALT 0     0.152 
    # LMI MPI 0     0 
    # LMI ALT 0     0 
    # MPI ALT 0.124 0.012 
    # 
    # $lower
    #            SBI       SPI        SII        LMI       MPI       ALT
    # SBI 0.00000000 0.0000000 0.05524575 0.06369211 0.0000000 0.0000000
    # SPI 0.00000000 0.0000000 0.00000000 0.00000000 0.3517273 0.2728653
    # SII 0.05524575 0.0000000 0.00000000 0.07541669 0.0000000 0.0000000
    # LMI 0.06369211 0.0000000 0.07541669 0.00000000 0.0000000 0.0000000
    # MPI 0.00000000 0.3517273 0.00000000 0.00000000 0.0000000 0.1236074
    # ALT 0.00000000 0.2728653 0.00000000 0.00000000 0.1236074 0.0000000
    # 
    # $upper
    #            SBI        SPI       SII        LMI        MPI        ALT
    # SBI 0.00000000 0.00000000 0.0000000 0.06935723 0.00000000 0.00000000
    # SPI 0.00000000 0.00000000 0.0638653 0.07169038 0.21421052 0.04785965
    # SII 0.00000000 0.06386530 0.0000000 0.00000000 0.16401986 0.15228270
    # LMI 0.06935723 0.07169038 0.0000000 0.00000000 0.00000000 0.00000000
    # MPI 0.00000000 0.21421052 0.1640199 0.00000000 0.00000000 0.01209013
    # ALT 0.00000000 0.04785965 0.1522827 0.00000000 0.01209013 0.00000000

    par(mfrow = c(1,1))
    .assetsStarPlot(ans$lower, main = "Lower Tail Relations")
    .assetsStarPlot(ans$upper, main = "Lower Tail Relations")
}


################################################################################

