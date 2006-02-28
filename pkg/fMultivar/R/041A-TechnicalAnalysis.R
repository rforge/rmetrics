
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
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 DESCRIPTION - UTILITY FUNCTIONS:
#  emaTA                     Exponential Moving Average
#  biasTA                    EMA-Bias
#  medpriceTA                Median Price                   
#  typicalpriceTA            Typical Price
#  wcloseTA                  Weighted Close Price
#  rocTA                     Rate of Change
#  oscTA                     EMA-Oscillator
# FUNCTION:                 OSCILLATOR INDICATORS:
#  momTA                     Momentum
#  macdTA                    MACD
#  cdsTA                     MACD Signal Line
#  cdoTA                     MACD Oscillator
#  vohlTA                    High/Low Volatility
#  vorTA                     Volatility Ratio
# FUNCTION:                 STOCHASTICS INDICATORS:
#  fpkTA                     Fast Percent %K
#  fpdTA                     Fast Percent %D
#  spdTA                     Slow Percent %D
#  apdTA                     Averaged Percent %D
#  wprTA                     Williams Percent %R
#  rsiTA                     Relative Strength Index
# FUNCTION:                 SPLUS LIKE MOVING AVERAGES:
#  SMA                       Computes Simple Moving Average           
#  EWMA                      Computes Exponentially Weighted  Moving Average
# FUNCTION:
#  .TA
################################################################################


# Notations / Data - Prices, Volume, OpenInterest:
#     O    Open         H    High
#     L    Low          C    Close
#     V    Volume       X    one of O H, L, C, V


# ------------------------------------------------------------------------------


emaTA = 
function(x, lambda, startup = 0)
{   # A function written by Diethelm Wuertz
    
    # Description:
    #   EXPONENTIAL MOVING AVERAGE:
    #   EMA: EMA(n) = lambda * X(n) + (1-lambda) * EMA(n-1)
    #       lambda = 2 / ( n+1 )
    
    # FUNCTION:
    
    if (lambda >= 1) lambda = 2/(lambda+1)
        if (startup == 0) startup = floor(2/lambda)
        if (lambda == 0){
        xema = rep (mean(x),length(x))}
        if (lambda > 0){
            xlam = x * lambda
            xlam[1] = mean(x[1:startup])
        xema = filter(xlam, filter = (1-lambda), method = "rec")}
    xema
}


# ------------------------------------------------------------------------------


biasTA = 
function(x, lag)
{   # A function written by Diethelm Wuertz

    # Description:
    #   BIAS: (X - EMA) / EMA

    # FUNCTION:
    
    xema = emaTA(x, lag)
    (x - xema)/xema
}


# ------------------------------------------------------------------------------


medpriceTA  = 
function(high, low) 
{   # A function written by Diethelm Wuertz

    # Return Value:
    (high + low) / 2 
}


# ------------------------------------------------------------------------------


typicalpriceTA = 
function(high, low, close) 
{   # A function written by Diethelm Wuertz

    # Return Value:
    (high + low + close) / 3 
}


# ------------------------------------------------------------------------------


wcloseTA = 
function(high, low, close) 
{   # A function written by Diethelm Wuertz

    # Return Value:
    (high + low + 2 * close) / 4 
}


# ------------------------------------------------------------------------------


rocTA = 
function(x, lag)
{   # A function written by Diethelm Wuertz

    # Description:  
    #   RATE OF CHANGE INDICATOR:
    #   ROC: (X(n) - X(n-k) ) / X(n)

    # FUNCTION:
    
    c(rep(0, times = lag), diff(x, lag = lag)) / x
}


# ******************************************************************************


oscTA = 
function(x, lag1, lag2)
{   # A function written by Diethelm Wuertz
    #
    # Description:
    #   EMA OSCILLATOR INDICATOR:
    #   OSC: (EMA_LONG - EMA_SHORT) / EMA_SHORT

    # FUNCTION:
    
    xema1 = emaTA(x, lag1)
    xema2 = emaTA(x, lag2)
    (xema1 - xema2) / xema2
}


# ------------------------------------------------------------------------------


momTA = 
function(x, lag)
{   # A function written by Diethelm Wuertz

    # Description:
    #   MOMENTUM INDICATOR:
    #   MOM: X(n) - X(n-lag)

    # FUNCTION:
    c(rep(0, times = lag), diff(x, lag = lag))
}


# ------------------------------------------------------------------------------


macdTA = 
function(x, lag1, lag2)
{   # A function written by Diethelm Wuertz

    # Description:
    #   MACD MA CONVERGENCE DIVERGENCE INDICATOR
    #   Fast MACD e.g. lag1=12, lag=26
    #   MCD: (EMA_SHORT - EMA_LONG)
    
    # FUNCTION:
    
    # Return Result:
    emaTA(x, lag1) - emaTA(x, lag2)
}


# ------------------------------------------------------------------------------


cdsTA = 
function(x, lag1, lag2, lag3)
{   # A function written by Diethelm Wuertz

    # Description:
    #   MACD SLOW SIGNAL LINE INDICATOR: e.g. lag3=9
    #   SIG: EMA(MCD)
    
    # FUNCTION:
    # Return Result:
    emaTA(macdTA(x, lag1, lag2), lag3)
}


# ------------------------------------------------------------------------------


cdoTA = 
function(x, lag1, lag2, lag3)
{   # A function written by Diethelm Wuertz

    # Description:
    #   MACD - MA CONVERGENCE/DIVERGENCE OSCILLATOR:
    #   CDO: MACD - SIG

    # FUNCTION:
    macdTA(x, lag1, lag2) - cdsTA(x, lag1, lag2, lag3)
}


# ------------------------------------------------------------------------------


vohlTA = 
function(high, low)
{   # A function written by Diethelm Wuertz

    # Description:
    #   HIGH LOW VOLATILITY:
    #   VOHL: high - low
    
    # FUNCTION:
    
    high - low
}


# ------------------------------------------------------------------------------


vorTA = 
function(high, low)
{   # A function written by Diethelm Wuertz

    # Description:
    #   VOLATILITY RATIO:
    #   VOR: (high-low)/low
    
    # FUNCTION:
    
    (high - low) / low
}


# ------------------------------------------------------------------------------


fpkTA = 
function(close, high, low, lag)
{   # A function written by Diethelm Wuertz

    # Description:
    #   FAST %K INDICATOR:
    
    # FUNCTION:
    
    minlag = function(x, lag){
       xm = x
       for (i in 1:lag){
         x1 = c(x[1],x[1:(length(x)-1)])
         xm = pmin(xm,x1)
         x = x1}
       xm}
    maxlag = function(x, lag){
       xm = x
       for (i in 1:lag){
         x1 = c(x[1],x[1:(length(x)-1)])
         xm = pmax(xm,x1)
         x = x1}
       xm}
    xmin = minlag(low, lag)
    xmax = maxlag(high, lag)
    (close - xmin ) / (xmax -xmin)
}


# ------------------------------------------------------------------------------


fpdTA = 
function(close, high, low, lag1, lag2)
{   # A function written by Diethelm Wuertz

    # Description:
    #   FAST %D INDICATOR:
    #   EMA OF FAST %K
    
    # FUNCTION:
    emaTA(fpkTA(close, high, low, lag1), lag2)
}


# ------------------------------------------------------------------------------


spdTA = 
function(close, high, low, lag1, lag2, lag3)
{   # A function written by Diethelm Wuertz

    # Description:
    #   SLOW %D INDICATOR:
    #   EMA OF FAST %D
    
    # FUNCTION:
    emaTA(fpdTA(close, high, low, lag1, lag2), lag3)
}


# ------------------------------------------------------------------------------


apdTA = 
function(close, high, low, lag1, lag2, lag3, lag4)
{   # A function written by Diethelm Wuertz

    # Description:
    #   AVERAGED %D INDICATOR:
    #   EMA OF SLOW %D
     
    # FUNCTION:
    emaTA(spdTA(close, high, low, lag1, lag2, lag3), lag4)
}


# ------------------------------------------------------------------------------


wprTA = 
function(close, high, low, lag)
{   # A function written by Diethelm Wuertz

    # Description:
    #   WILLIAMS %R INDICATOR:
    
    # FUNCTION:
        minlag = function(x, lag){
            xm = x
            for (i in 1:lag){
                x1 = c(x[1],x[1:(length(x)-1)])
                xm = pmin(xm,x1)
                x = x1}
                xm}
        maxlag = function(x, lag){
            xm = x
            for (i in 1:lag){
                x1 = c(x[1],x[1:(length(x)-1)])
                xm = pmax(xm,x1)
                x = x1}
                xm}
        xmin = minlag(low, lag)
        xmax = maxlag(high, lag)
    # Return Result:
    (close - xmin) / (xmax -xmin)
}


# ------------------------------------------------------------------------------


rsiTA = 
function(close, lag)
{   # A function written by Diethelm Wuertz

    # Description:
    #   RSI - RELATIVE STRENGTH INDEX INDICATOR:
        
    # FUNCTION:
    sumlag = function(x, lag){
        xs = x
        for (i in 1:lag){
            x1 = c(x[1],x[1:(length(x)-1)])
            xs = xs + x1
            x = x1}
        xs}
    close1 = c(close[1],close[1:(length(close)-1)])
    x = abs(close - close1)
    x[close<close1] = 0
    rsi = sumlag(x,lag)/sumlag (abs(close-close1),lag)
    rsi[1] = rsi[2]
     
    # Return Result:
    rsi
}


# ******************************************************************************


SMA = 
function(x, n = 5) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes a Simple Moving Average
    
    # FUNCTION:
    
    # Return Value:
    rollFun(x = x, n = n, FUN = mean) 
}


# ------------------------------------------------------------------------------


EWMA = 
function(x, lambda, startup = 0) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes an Exponentially Weighted Moving Average
    
    # FUNCTION:
    
    # EWMA:
    if (lambda >= 1) lambda <- 2/(lambda + 1)
    if (startup == 0) startup <- floor(2/lambda)
    if (lambda == 0) xema <- rep(mean(x), length(x))
    if (lambda > 0) {
        xlam <- x * lambda; xlam[1] <- mean(x[1:startup])
        xema <- filter(xlam, filter = (1 - lambda), method = "rec")}
        
    # Return Value:
    xema
}


################################################################################


.dailyTA = 
function(X, indicator = "ema", select = "Close", lag = 9)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute an indicator for technical analysis.
    
    # Arguments:
    #   X - a data.frame or timeSeries object with named
    #       columns, at least "Close", "Open", "High" and
    #       "Low". The order of the columns may be arbitrary.
        
    # FUNCTION:
    
    if (is.timeSeries(X)) {
        x = X@Data
    } else {
        stop("X must be a timeSeries object!")
    }
    
    if (indicator == "ema") {
        ans = emaTA(x = x[, select], lambda = lag)
    }
        
    if (indicator == "bias") {
        ans = biasTA(x = x[, select], lag = lag)
    }
    
    if (indicator == "medprice") {
        ans = medpriceTA(high = x[, "High"], low = x[, "Low"])
    }
        
    if (indicator == "typicalprice") {
        ans = typicalpriceTA(high = x[, "High"], low = x[, "Low"], 
            close = x[, "Close"])
    }
            
    if (indicator == "wclose") {
        ans = wcloseTA(high = x[, "High"], low = x[, "Low"], 
            close = x[, "Close"])
    }
            
    if (indicator == "roc") {
        ans = rocTA(x = x[, select], lag = lag)
    }
        
    if (indicator == "osc") {
        if (length(lag) < 2)
            stop("At least two lags must be specified!")
        ans = oscTA(x = x[, select], lag1 = lag[1], lag2 = lag[2])
    }
            
    if (indicator == "mom") {
        ans = momTA(x = x[, select], lag = lag)
    }
        
    if (indicator == "macd") {
        if (length(lag) < 2)
            stop("At least two lags must be specified!")
        ans = macdTA(x = x[, select], lag1 = lag[1], lag2 = lag[2])
    }
        
    if (indicator == "cds") {
        if (length(lag) < 3)
            stop("At least three lags must be specified!")
        ans = cdsTA(x = x[, select], lag1 = lag[1], lag2 = lag[2], 
            lag3 = lag[3])
    }
            
    if (indicator == "cdo") {
        if (length(lag) < 3)
            stop("At least three lags must be specified!")
        ans = cdoTA(x = x[, select], lag1 = lag[1], lag2 = lag[2], 
            lag3 = lag[3])
    }
            
    if (indicator == "vohl") {
        ans = vohlTA(high = x[, "High"], low = x[, "low"])
    }
        
    if (indicator == "vor") {
        ans = vorTA(high = x[, "High"], low = x[, "low"])
    }
        
    if (indicator == "fpk") {
        ans = fpkTA(close = x[, "Close"], high = x[, "High"], 
            low = x[, "Low"], lag = lag)
    }
        
    if (indicator == "fpd") {
        if (length(lag) < 2)
            stop("At least two lags must be specified!")
        ans = fpdTA(close = x[, "Close"], high = x[, "High"], 
            low = x[, "Low"], lag1 = lag[1], lag2 = lag[2])
    }
            
    if (indicator == "spd") {
        if (length(lag) < 3)
            stop("At least three lags must be specified!")
        ans = spdTA(close = x[, "Close"], high = x[, "High"], 
            low = x[, "Low"], lag1 = lag[1], lag2 = lag[2],
            lag3 = lag[3]) 
    }
        
    if (indicator == "apd") {
        if (length(lag) < 4)
            stop("At least four lags must be specified!")
        ans = apdTA(close = x[, "Close"], high = x[, "High"], 
            low = x[, "Low"], lag1 = lag[1], lag2 = lag[2],
            lag3 = lag[3], lag4 = lag[4])
    }
            
    if (indicator == "wpr") {
        ans = wprTA(close = x[, "Close"], high = x[, "High"], 
            low = x[, "Low"], lag = lag)
    }
            
    if (indicator == "rsi") {
        ans = rsiTA(x = x[, "Close"], lag = lag)
    }
        
    
    # Return Value:
    timeSeries(data = matrix(ans, ncol =1), charvec = X@positions, 
        units = indicator, format = "ISO", zone = "GMT", 
        FinCenter = "GMT")
            
}


################################################################################

