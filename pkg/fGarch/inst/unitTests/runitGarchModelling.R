
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
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:               PARAMETER ESTIMATION: 
#  'fGARCH'                S4: fGARCH Class representation   
#  garchFit                Fits GARCH and APARCH processes
################################################################################


# garchFit(
    #   formula, 
    #   data, 
    #   init.rec = c("mci", "uev"), 
    #   delta = 2, 
    #   skew = 1, 
    #   shape = 4, 
    #   cond.dist = c("dnorm", "dsnorm", "dged", "dsged", "dstd", "dsstd"), 
    #   include.mean = TRUE, 
    #   include.delta = NULL, 
    #   include.skew = NULL, 
    #   include.shape = NULL, 
    #   leverage = NULL, 
    #   trace = TRUE, 
    #   algorithm = c("sqp", "nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm"), 
    #   control = list(), 
    #   title = NULL, 
    #   description = NULL, 
    #   ...)

    # *** OLD VERSION ***
    # .garchFit(
    #   formula.mean = ~arma(0, 0), 
    #   formula.var = ~garch(1, 1), 
    #   series = x, 
    #   init.rec = c("mci", "uev"), delta = 2, 
    #   skew = 1, 
    #   shape = 4,
    #   cond.dist = c("dnorm", "dsnorm", "dged", "dsged", "dstd", "dsstd"), 
    #   include.mean = TRUE, 
    #   include.delta = NULL, 
    #   include.skew = NULL,
    #   include.shape = NULL, 
    #   leverage = NULL, 
    #   trace = TRUE,  
    #   algorithm = c("sqp", "nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm"), 
    #   control = list(), 
    #   title = NULL, 
    #   description = NULL, 
    #   ...)
    
    
# ------------------------------------------------------------------------------


test.garchParser = 
function()
{       
    # Use Simulated Series - an Object of class 'ts' ...
    
    # Numeric Vector RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry") 
    # Simulate normal GARCH(1, 1) numeric Vector:
    model = list(omega = 1e-06, alpha = 0.1, beta = 0.8)
    N = 1000
    
    
    # UNIVARIATE:
    
    # A numeric Vector:
    x.vec = 100*garchSim(model, N, returnClass = "numeric")
    x.vec
    .garchArgsParser(x.vec ~ garch(1,1), data = as.data.frame(x.vec))
    coef(garchFit( ~ garch(1,1), data = x.vec))
   
    # An univariate timeSeries object with dummy dates:
    x.tS = dummyDailySeries(matrix(x.vec), units = "GARCH11")
    x.tS 
    .garchArgsParser(GARCH11 ~ garch(1,1), data = as.data.frame(x.tS)) 
    coef(garchFit(~ garch(1,1), data = x.tS))           

    # An univariate zoo object with dummy dates:
    x.zoo = zoo(x.vec, order.by = as.Date(rownames(x.tS)))
    .garchArgsParser(x.zoo ~ garch(1,1), data = as.data.frame(x.zoo))
    coef(garchFit(~ garch(1,1), data = x.zoo))   
    
    # An univariate "ts" object:
    x.ts = as.ts(x.vec)
    .garchArgsParser(x ~ garch(1,1), data = as.data.frame(x.ts))
    coef(garchFit(~ garch(1,1), data = x.ts))  
    
    
    # MULTIVARIATE:
    
    # A numeric matrix:
    X.mat = cbind(GARCH11 = x.vec, R = rnorm(N))
    X.mat
    .garchArgsParser(GARCH11 ~ garch(1,1), data = as.data.frame(X.mat))
    coef(garchFit(GARCH11 ~ garch(1,1), data = X.mat))
        
    # A multivariate timeSeries object with dummy dates:
    X.tS = dummyDailySeries(X.mat, units = c("GARCH11", "R"))
    .garchArgsParser(GARCH11 ~ garch(1,1), data = as.data.frame(X.tS)) 
    coef(garchFit(GARCH11 ~ garch(1,1), data = X.tS))
    
    # A multivariate zoo object without column names:
    X.zoo = zoo(X.mat, order.by = as.Date(rownames(x.tS)))
    .garchArgsParser(GARCH11 ~ garch(1,1), data = as.data.frame(X.zoo))
    coef(garchFit(GARCH11 ~ garch(1,1), data = X.zoo))

    # A multivariate "mts" object without column names:
    X.mts = as.ts(X.mat)
    .garchArgsParser(V1 ~ garch(1,1), data = as.data.frame(X.mts))
    coef(garchFit(GARCH11 ~ garch(1,1), data = X.mts))
 
       
    # MODELING THE PERCENTUAL SPI/SBI SPREAD FROM LPP BENCHMARK:
    
    X.tS = as.timeSeries(data(LPP2005REC))
    X.mat = as.matrix(x.tS)
    X.zoo = zoo(X.mat, order.by = as.Date(rownames(x.mat)))
    X.mts = ts(X.mat)
    coef(garchFit(100*(SPI - SBI) ~ garch(1,1), data = X.tS))
    coef(garchFit(100*(SPI - SBI) ~ garch(1,1), data = X.mat))
    coef(garchFit(100*(SPI - SBI) ~ garch(1,1), data = X.zoo))
    coef(garchFit(100*(SPI - SBI) ~ garch(1,1), data = X.mts))

    
    # MODELING HIGH/LOW SPREADS FROM MSFT PRICE SERIES:
    
    X.tS = as.timeSeries(msft.dat)
    coef(garchFit(Open ~ garch(1,1), data = returns(X.tS)))
    coef(garchFit(100*(High-Low) ~ garch(1,1), data = returns(X.tS)))
      
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------


test.garchFit = 
function()
{       
    # Use Simulated Series - an Object of class 'ts' ...
    
    # RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    
    # normal GARCH(1, 1)
    model = list(omega = 1e-06, alpha = 0.1, beta = 0.8)
    x = garchSim(model, 1000)
    # Default Settings ...
    fit = garchFit(x ~ garch(1,1), data = x)
    fit@fit$coef 
    
    # RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    
    # normal-GARCH(2, 1)
    model = list(omega = 1e-06, alpha = c(0.1, 0.2), beta = 0.6)
    x = garchSim(model, n = 1000)
    # Default Settings ...
    fit = garchFit(x ~ garch(2,1), data = x)
    fit@fit$coef
    
    # RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    
    # normal AR(1)-GARCH(1,1)
    model = list(omega = 1e-06, ar = -0.1, alpha = c(0.1, 0.2), beta = 0.6)
    x = garchSim(model, n = 1000)
    # Default Settings ...
    fit = garchFit(x ~ ar(1) + garch(1,1), data = x)
    fit@fit$coef
    
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------


test.aparch11 = 
function()
{       
    # Use Simulated Series - an Object of class 'ts' ...
    
    # RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    
    # Leveraged normal APARCH(1, 1)
    model = list(omega = 1e-06, alpha = 0.1, beta = 0.8, gamma = 0.1)
    x = garchSim(model, n = 1000)
    fit = garchFit(x ~ garch(1,1), data = x, leverage = TRUE)
    fit@fit$coef
    
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------


test.aparch11delta = 
function()
{       
    # RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    
    # Leveraged normal APARCH(1, 1) delta = 1.5
    model = list(omega = 1e-05, alpha = 0.1, beta = 0.8, gamma = 0.1, delta = 1.5)
    x = garchSim(model, n = 1000)
    var(x)    
    fit = garchFit(x ~ garch(1,1), data = x, leverage = TRUE,
        include.delta = TRUE, delta = 2)
    fit@fit$coef
    
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------


test.ar1aparch21 = 
function()
{       
    # RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    
    # Leveraged normal AR(1)-APARCH(2, 1) delta = 1
    model = list(omega = 1e-06, ar = -0.1, alpha = c(0.1, 0.2), beta = 0.6, 
        delta = 1)
    x = garchSim(model, n = 1000)
    taylor = teffectPlot(x)
    init.delta = mean(taylor$maxDelta)
    init.delta    
    
    ## fit = garchFit(x ~ ar(1) + garch(2,1), data = x, include.delta = TRUE, 
    ##     delta = init.delta)
    ## fit@fit$coef
    
    ## Error in solve.default(fit$hessian) : 
    ##  Lapack routine dgesv: system is exactly singular            ## CHECK !!!
    
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------
   

test.garchFit.norm = 
function()
{  
    # Conditional Densities:
    #   "dnorm", "dsnorm", "dged", "dsged", "dstd", "dsstd"
    
    # RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    
    # skewed normal GARCH(1, 1)
    model = list(omega = 1e-06, alpha = 0.1, beta = 0.8, skew = 0.9)
    x = garchSim(model, 1000, cond.dist = "rsnorm")
    fit = garchFit(~garch(1,1), data = x, include.skew = TRUE, 
        cond.dist = "dsnorm")
    fit@fit$coef
    
    # Skewed Normal GARCH(1, 1) - fixed skew ...
    fit = garchFit(~garch(1,1), data = x, skew = 0.9, include.skew = FALSE,
        cond.dist = "dsnorm")
    fit@fit$coef
    
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------


test.garchFit.ged = 
function()
{  
    # Conditional Densities:
    #   "dnorm", "dsnorm", "dged", "dsged", "dstd", "dsstd"
    
    # RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")    
    
    # GED-GARCH(1, 1)
    model = list(omega = 1e-06, alpha = 0.1, beta = 0.8, shape = 2)
    x = garchSim(model, 1000, cond.dist = "rged")
    fit = garchFit(~garch(1,1), data = x, include.shape = TRUE, 
        cond.dist = "dged")
    fit@fit$coef
    
    
    # RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")    
    
    # Skewed GED-GARCH(1, 1)
    model = list(omega = 1e-05, alpha = 0.1, beta = 0.8, shape = 4, skew = 0.9)
    x = garchSim(model, 1000, cond.dist = "rsged")
    fit = garchFit(~garch(1,1), data = x, include.shape = TRUE, 
        include.skew = TRUE, cond.dist = "dsged")
    fit@fit$coef
    
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------


test.garchFit.student = 
function()
{  
    # Conditional Densities:
    #   "dnorm", "dsnorm", "dged", "dsged", "dstd", "dsstd"
    
    # Algorithms:
    #   "sqp", "nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm"
    
    # RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")    
    
    # Student t-GARCH(1, 1)
    model = list(omega = 1e-06, alpha = 0.1, beta = 0.8, shape = 4)
    x = garchSim(model, 1000, cond.dist = "rstd")
    fit = garchFit(~garch(1,1), data = x, include.shape = TRUE, 
        cond.dist = "dstd")
    fit@fit$coef 
    
    # RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")    
    
    # Skewed Student t-GARCH(1, 1)
    model = list(omega = 1e-06, alpha = 0.1, beta = 0.8, shape = 4, skew = 0.9)
    x = garchSim(model, 1000, cond.dist = "rsstd")
    fit = garchFit(~garch(1,1), data = x, include.shape = TRUE, 
        include.skew = TRUE, cond.dist = "dsstd")
    fit@fit$coef
    
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------


test.garchFit.initialization = 
function()
{  
    # Load Data:
    data(dem2gbp)  
    
    # Data Frame to Numeric Vector:
    x = dem2gbp[, 1]
    print(head(x))
    print(class(x))
    
    # Modify Start Values - mci default:
    fit = garchFit( ~ garch(1,1), x, init.rec = "mci") # default
    fit@fit$coef
    
    # Modify Start Values - uev alternative:
    ## fit = garchFit( ~ garch(1,1), x, init.rec = "uev")
    ## fit@fit$coef
    
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------



test.garchFit.algorithms = 
function()
{  
    # Load Data:
    data(dem2gbp) 
     
    # Data Frame to Numeric Vector:
    x = dem2gbp[, 1]
    print(head(x))
    print(class(x))
    
    # Conditional Densities:
    #   "dnorm", "dsnorm", "dged", "dsged", "dstd", "dsstd"
    
    # Algorithms:
    #   "sqp", "nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm"
           
    # Modify Start Values - mci default:
    fit = garchFit( ~ garch(1,1), data = x, cond.dist = "dnorm", 
        algorithm = "nlminb")
    fit@fit$coef
    
    # Modify Start Values - mci default:
    fit = garchFit( ~ garch(1,1), data = x, cond.dist = "dsnorm", 
        algorithm = "nlminb")
    fit@fit$coef
    
    # Modify Start Values - mci default:
    fit = garchFit( ~ garch(1,1), data = x, cond.dist = "dged", 
        algorithm = "nlminb")
    fit@fit$coef
    
    # Modify Start Values - mci default:
    fit = garchFit( ~ garch(1,1), data = x, cond.dist = "dsged", 
        algorithm = "nlminb")
    fit@fit$coef
   
    # Modify Start Values - mci default:
    fit = garchFit( ~ garch(1,1), data = x, cond.dist = "dstd", 
        algorithm = "nlminb")
    fit@fit$coef
    
    # Modify Start Values - mci default:
    fit = garchFit( ~ garch(1,1), data = x, cond.dist = "dsstd", 
        algorithm = "nlminb", trace = FALSE)
    fit@fit$coef
    
    # Modify Start Values - mci default:
    fit = garchFit( ~ garch(1,1), data = x/sd(x), cond.dist = "dsstd", 
        trace = TRUE)
    fit@fit$coef
    
    fit = garchFit( ~ aparch(1,1), data = x/sd(x), cond.dist = "dsstd", 
        trace = TRUE)
    fit@fit$coef
    
    # Return Value:
    return()    
} 


################################################################################
    
