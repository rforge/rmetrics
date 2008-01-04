
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


test.formula-methods <- 
function()
{
    # Numeric Vector RVs:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry") 
    # Simulate normal GARCH(1, 1) numeric Vector:
    model = list(omega = 1e-06, alpha = 0.1, beta = 0.8)
    N = 250
    
    
    # UNIVARIATE DATA:
    
    # A numeric Vector:
    x.vec = 100*garchSim(model, N, returnClass = "numeric")
    x.vec
    x.tS = dummyDailySeries(matrix(x.vec), units = "GARCH11")
    x.tS 
    x.zoo = zoo(x.vec, order.by = as.Date(rownames(x.tS)))
    x.zoo
    x.ts = as.ts(x.vec)
    x.ts 
    
    
    # MULTIVARIATE DATA:
    
    # A numeric matrix:
    X.mat = cbind(GARCH11 = x.vec, R = rnorm(N))
    X.mat
    X.tS = dummyDailySeries(X.mat, units = c("GARCH11", "R"))
    X.tS
    X.zoo = zoo(X.mat, order.by = as.Date(rownames(x.tS)))
    X.zoo
    X.mts = as.ts(X.mat)
    X.mts
       
    # UNIVARIATE:
    
    # A numeric Vector:
    fit = garchFit(~ garch(1,1), data = x.vec, trace = FALSE)
    formula(fit)
    fit = garchFit(x.vec ~ garch(1,1), data = x.vec, trace = FALSE)
    formula(fit)
   
    # An univariate timeSeries object with dummy dates:
    fit = garchFit(~ garch(1,1), data = x.tS, trace = FALSE)
    formula(fit)
    fit = garchFit(x.tS ~ garch(1,1), data = x.tS, trace = FALSE)
    formula(fit)       

    # An univariate zoo object with dummy dates:
    fit = garchFit(~ garch(1,1), data = x.zoo, trace = FALSE)
    formula(fit)
    fit = garchFit(x.zoo ~ garch(1,1), data = x.zoo, trace = FALSE)
    formula(fit)    
    
    # An univariate "ts" object:
    fit = garchFit(~ garch(1,1), data = x.ts, trace = FALSE)
    formula(fit)
    fit = garchFit(x.ts ~ garch(1,1), data = x.ts, trace = FALSE)
    formula(fit)    
    
    
    # MULTIVARIATE:
    
    # A numeric matrix:
    fit = garchFit(GARCH11 ~ garch(1,1), data = X.mat, trace = FALSE)
    formula(fit)
    fit = garchFit(100*GARCH11 ~ garch(1,1), data = X.mat, trace = FALSE)
    formula(fit)    
    
    # A multivariate timeSeries object with dummy dates:
    fit = garchFit(GARCH11 ~ garch(1,1), data = X.tS, trace = FALSE)
    formula(fit)
    fit = garchFit(100*GARCH11 ~ garch(1,1), data = X.tS, trace = FALSE)
    formula(fit)  
    
    # A multivariate zoo object without column names:
    fit = garchFit(GARCH11 ~ garch(1,1), data = X.zoo, trace = FALSE)
    formula(fit)
    fit = garchFit(100*GARCH11 + R/100 ~ garch(1,1), data = X.zoo, trace = FALSE)
    formula(fit)  

    # A multivariate "mts" object without column names:
    fit = garchFit(GARCH11 ~ garch(1,1), data = X.mts, trace = FALSE)
    formula(fit)
    fit = garchFit(100*GARCH11 + R/100 ~ garch(1,1), data = X.mts, trace = FALSE)
    formula(fit)  
 
       
    # MODELING THE PERCENTUAL SPI/SBI SPREAD FROM LPP BENCHMARK:
    
    X.tS = as.timeSeries(data(LPP2005REC))
    head(X.tS)
    X.mat = as.matrix(X.tS)
    head(X.mat)
    X.zoo = zoo(X.mat, order.by = as.Date(rownames(X.mat)))
    head(X.zoo)
    X.mts = mts(X.mat)
    X.mts # head does not work for ts objects!
    
    fit = garchFit(100*(SPI - SBI) ~ garch(1,1), data = X.tS)
    formula(fit)
    fit = garchFit(100*(SPI - SBI) ~ garch(1,1), data = X.mat)
    formula(fit)
    fit = garchFit(100*(SPI - SBI) ~ garch(1,1), data = X.zoo)
    formula(fit)
    fit = garchFit(100*(SPI - SBI) ~ garch(1,1), data = X.mts)
    formula(fit)

    
    # MODELING HIGH/LOW SPREADS FROM MSFT PRICE SERIES:
    
    X.tS = as.timeSeries(msft.dat)
    fit = garchFit(Open ~ garch(1,1), data = returns(X.tS))
    formula(fit)
    fit = garchFit(100*(High-Low) ~ garch(1,1), data = returns(X.tS))
    formula(fit)
      
    # Return Value:
    return()    
} 


################################################################################
    
