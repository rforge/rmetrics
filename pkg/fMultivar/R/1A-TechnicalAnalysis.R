
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
#  stochasticTA              Stochastics %K/%D, fast/slow
#  fpkTA                     Fast Percent %K
#  fpdTA                     Fast Percent %D
#  spdTA                     Slow Percent %D
#  apdTA                     Averaged Percent %D
#  wprTA                     Williams Percent %R
#  rsiTA                     Relative Strength Index
# FUNCTION:                 DESCRIPTION - MORE INDICATORS:
#  accelTA                   Acceleration
#  adiTA                     AD Indicator      
#  adoscillatorTA            AD Oscillator
#  bollingerTA               Bollinger Bands
#  chaikinoTA                Chaikin Oscillator
#  chaikinvTA                Chaikin Volatility
#  garmanklassTA             Garman-Klass Volatility
#  nviTA                     Negative Volume Index
#  obvTA                     On Balance Volume
#  pviTA                     Positive Volume Index
#  pvtrendTA                 Price-Volume Trend
#  williamsadTA              Williams AD
#  williamsrTA               Williams R%
# FUNCTION:                 SPLUS LIKE MOVING AVERAGES:
#  SMA                       Computes Simple Moving Average           
#  EWMA                      Computes Exponentially Weighted  Moving Average
# FUNCTION:
#  .dailyTA
################################################################################


# Notations / Data - Prices, Volume, OpenInterest:
#     O    Open         H    High
#     L    Low          C    Close
#     V    Volume       X    one of O H, L, C, V


# ------------------------------------------------------------------------------


emaTA = 
function(x, lambda = 0.1, startup = 0)
{   # A function written by Diethelm Wuertz
    
    # Description:
    #   Returns the Exponential Moving Average Indicator
    
    # Details:
    #   EXPONENTIAL MOVING AVERAGE:
    #   EMA: EMA(n) = lambda * X(n) + (1-lambda) * EMA(n-1)
    #       lambda = 2 / ( n+1 )
    
    # Example:
    #   head(emaTA(MSFT[, "Close"]))
    
    # FUNCTION:
    
    # Preprocessing:
    TS = is.timeSeries(x)
    y = as.vector(x)
    
    # EMA
    if (lambda >= 1) lambda = 2/(lambda+1)
    if (startup == 0) startup = floor(2/lambda)
    if (lambda == 0){
    ema = rep (mean(x),length(x))}
    if (lambda > 0){
        ylam = y * lambda
        ylam[1] = mean(y[1:startup])
    ema = filter(ylam, filter = (1-lambda), method = "rec")}
    ema =  as.vector(ema)
    
    # Convert to timeSeries object:
    if (TS) {
        ema = timeSeries(matrix(ema, ncol = 1), seriesPositions(x),
            units = paste(x@units, "EMA", sep = ""))
    }
        
    # Return Value:
    ema
}


# ------------------------------------------------------------------------------


biasTA = 
function(x, lag = 5)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Returns the Bias Indiacator
    
    # Example:
    #   head(biasTA(MSFT[, "Close"]))
    #   head(biasTA(rnorm(30)))
    
    # Details:
    #   BIAS: (X - EMA) / EMA

    # FUNCTION:
    
    # BIAS:
    xema = emaTA(x, lag)
    bias = (x - xema)/xema
    if (is.timeSeries(bias)) colnames(bias)<-"BIAS"
    
    # Return Value:
    bias
}


# ------------------------------------------------------------------------------


medpriceTA  = 
function(high, low) 
{   # A function written by Diethelm Wuertz

    # Description:
    #   Returns the Middle Price Indicator
    
    # Example:
    #   head(medpriceTA(MSFT[, "High"], MSFT[, "Low"]))
    #   head(medpriceTA(rnorm(30), rnorm(30)))
    
    # FUNCTION:
    
    # MEDPRICE:
    medprice = (high + low) / 2 
    if (is.timeSeries(medprice)) colnames(medprice)<-"MEDPRICE"
    
    # Return Value:
    medprice
}


# ------------------------------------------------------------------------------


typicalpriceTA = 
function(high, low, close) 
{   # A function written by Diethelm Wuertz

    # Description:
    #   Returns the Typical Price Indicator
    
    # Example:
    #   head(typicalpriceTA(MSFT[, "High"], MSFT[, "Low"], MSFT[, "Close"]))
    #   head(typicalpriceTA(rnorm(30), rnorm(30), rnorm(30)))
    
    # FUNCTION:
    
    # Typical Price
    typicalprice = (high + low + close) / 3 
    if (is.timeSeries(typicalprice)) colnames(typicalprice)<-"TYPICALPRICE"
    
    # Return Value:
    typicalprice
}


# ------------------------------------------------------------------------------


wcloseTA = 
function(high, low, close) 
{   # A function written by Diethelm Wuertz

    # Description:  
    #   Returns Weighted Close Indicator
    
    # Example:
    #   head(wcloseTA(MSFT[, "High"], MSFT[, "Low"], MSFT[, "Close"]))
    #   head(wcloseTA(rnorm(30), rnorm(30), rnorm(30)))
    
    # FUNCTION:
    
    # Weighted Close:
    wclose = (high + low + 2 * close) / 4 
    if (is.timeSeries(wclose)) colnames(wclose)<-"WCLOSE"
    
    # Return Value:
    wclose 
}


# ------------------------------------------------------------------------------


rocTA = 
function(x, lag = 5)
{   # A function written by Diethelm Wuertz

    # Description:  
    #   Returns rate of Change Indicator
    
    # Examples:
    #   head(rocTA(MSFT[, "Close"]))
    #   head(rocTA(rnorm(30)))
    
    # Details:
    #   RATE OF CHANGE INDICATOR:
    #   ROC: (X(n) - X(n-k) ) / X(n)

    # FUNCTION:
    
    # Rate of Change:
    roc = diff(x, lag = lag) 
    if (is.numeric(x)) roc = c(rep(NA, times = lag), roc)
    roc = roc/x
    if (is.timeSeries(roc)) colnames(roc)<-"ROC"
    
    # Return Value:
    roc
}


# ******************************************************************************


oscTA = 
function(x, lag = c(25, 65))
{   # A function written by Diethelm Wuertz
    #
    # Description:
    #   Returns EMA Oscillator Indicator
    
    # Examples:
    #   head(oscTA(MSFT[, "Close"]))
    #   head(oscTA(rnorm(30)))
    
    # Details:
    #   EMA OSCILLATOR INDICATOR:
    #   OSC: (EMA_LONG - EMA_SHORT) / EMA_SHORT

    # FUNCTION:
    
    # Oscillator:
    xema1 = emaTA(x, lag[1])
    xema2 = emaTA(x, lag[2])
    osc = (xema1 - xema2) / xema2
    if (is.timeSeries(osc)) colnames(osc)<-"OSC"

    # Return Value:
    osc
}


# ------------------------------------------------------------------------------


momTA = 
function(x, lag = 25)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Returns Momentum Indicator
    
    # Examples:
    #   head(momTA(MSFT[, "Close"]))
    #   head(momTA(rnorm(30)))
    
    # Details:
    #   MOMENTUM INDICATOR:
    #   MOM: X(n) - X(n-lag)

    # FUNCTION:
    
    # Momentum:
    mom = diff(x, lag = lag) 
    if (is.numeric(x)) mom = c(rep(NA, times = lag), mom)
    if (is.timeSeries(mom)) colnames(mom)<-"mom"
    
    # Return Value:
    mom
}


# ------------------------------------------------------------------------------


macdTA = 
function(x, lag1, lag2)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Returns MA Convergence-Divergence Indicator
    
    # Details
    #   MACD MA CONVERGENCE DIVERGENCE INDICATOR
    #   Fast MACD e.g. lag1=12, lag=26
    #   MCD: (EMA_SHORT - EMA_LONG)
    
    # FUNCTION:
    
    # MACD:
    x = as.vector(x)
    macd = emaTA(x, lag1) - emaTA(x, lag2)
    
    # Return Result:
    macd
}


# ------------------------------------------------------------------------------


cdsTA = 
function(x, lag = c(12, 26, 9))
{   # A function written by Diethelm Wuertz

    # Description:
    #   Returns MACD Slow Signal Line Indicator
    
    # Details:
    #   MACD SLOW SIGNAL LINE INDICATOR: e.g. lag3=9
    #   SIG: EMA(MCD)
    
    # FUNCTION:
    
    # CDS:
    x = as.vector(x)
    cds = emaTA(macdTA(x, lag[1:2]), lag[3])
    
    # Return Result:
    cds
}


# ------------------------------------------------------------------------------


cdoTA = 
function(x, lag = c(12, 26, 9))
{   # A function written by Diethelm Wuertz

    # Description:
    #   Returns MA Convergence-Divergence Oscillator Indicator
    
    # Details:
    #   MACD - MA CONVERGENCE DIVERGENCE OSCILLATOR:
    #   CDO: MACD - SIG

    # FUNCTION:
    
    # CDO:
    x = as.vector(x)
    cdo = macdTA(x, lag[1:2]) - cdsTA(x, lag)
    
    # Return Value:
    cdo
}


# ------------------------------------------------------------------------------


vohlTA = 
function(high, low)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Returns High Low Volatility Indicator
    
    # Details:
    #   HIGH LOW VOLATILITY:
    #   VOHL: high - low
    
    # FUNCTION:
    
    # VOHL:
    vohl = high - low
    
    # Return Value:
    vohl
}


# ------------------------------------------------------------------------------


vorTA = 
function(high, low)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Returns Volatility Ratio Indicator
    
    # Details:
    #   VOLATILITY RATIO:
    #   VOR: (high-low)/low
    
    # FUNCTION:
    
    # VOR:
    vor = (high - low) / low
    
    # Return Value:
    vor
}


# ------------------------------------------------------------------------------


stochasticTA = 
function (close, high, low, lag1, lag2, type = c("fast", "slow")) 
{   # A function written by Diethelm Wuertz

    # Description:
    #   Returns Stochastic Indicators
    
    # Example:
    #   stochasticTA(high, low, close, lag1 = 10, lag2 = 3, "fast") 
    #   stochasticTA(high, low, close, lag1 = 10, lag2 = 3, "slow")   

    # FUNCTION:
    
    # Settings:
    trim = FALSE
    na.rm = FALSE
    rollHigh = rollMax(high, lag1, trim = trim, na.rm = na.rm)
    rollLow = rollMin(low, lag1, trim = trim, na.rm = na.rm)
    
    # Fast:
    if (type[1] == "fast") {
        K = (close - rollLow)/(rollHigh - rollLow) * 100
        D = rollMean(K, lag2, trim = trim, na.rm = na.rm)
        K[1:lag1] = K[lag1]
        D[1:(lag1+lag2-1)] = D[lag1+lag2-1]
    }
    
    # Slow:
    if (type[1] == "slow") {
        K = (close - rollLow)/(rollHigh - rollLow) * 100
        D = rollMean(K, lag2, trim = trim, na.rm = na.rm)
        K = rollMean(K, lag2, trim = trim, na.rm = na.rm)
        D = rollMean(D, lag2, trim = trim, na.rm = na.rm)
        K[1:(lag1+lag2-1)] = K[lag1+lag2-1]
        D[1:(lag1+2*lag2-2)] = D[lag1+2*lag2-2]
    }
    
    # Indicator:
    stochastic = cbind(K, D)
    
    # Return Value:
    stochastic
}


# ------------------------------------------------------------------------------


fpkTA = 
function(close, high, low, lag)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Returns Fast %K Indicator
    
    # FUNCTION:
    
    # Minimum:
    minlag = function(x, lag) {
        xm = x
        for (i in 1:lag) {
            x1 = c(x[1],x[1:(length(x)-1)])
            xm = pmin(xm,x1)
            x = x1
        }
        xm
    }
    
    # Maximum:
    maxlag = function(x, lag) {
        xm = x
        for (i in 1:lag) {
            x1 = c(x[1],x[1:(length(x)-1)])
            xm = pmax(xm,x1)
            x = x1 
       }
       xm
    }
       
    # Result:
    xmin = minlag(low, lag)
    xmax = maxlag(high, lag)
    fpk = (close - xmin ) / (xmax -xmin)
    
    # Return Value:
    fpk
}


# ------------------------------------------------------------------------------


fpdTA = 
function(close, high, low, lag1, lag2)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Returns Fast %D Indicator
    
    # Details:
    #   FAST %D INDICATOR: EMA OF FAST %K
    
    # FUNCTION:
    
    # FPD:
    fpd = emaTA(fpkTA(close, high, low, lag1), lag2)
    
    # Return Value:
    fpd 
}


# ------------------------------------------------------------------------------


spdTA = 
function(close, high, low, lag1, lag2, lag3)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Return Slow %D Indicator
    
    # Details:
    #   SLOW %D INDICATOR:
    #   EMA OF FAST %D
    
    # FUNCTION:
    
    # SPD:
    spd = emaTA(fpdTA(close, high, low, lag1, lag2), lag3)
    
    # Return Value:
    spd 
}


# ------------------------------------------------------------------------------


apdTA = 
function(close, high, low, lag1, lag2, lag3, lag4)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Returns A veraged %D Indicator
    
    # Details:
    #   AVERAGED %D INDICATOR: EMA OF SLOW %D
     
    # FUNCTION:
    
    # APD:
    apd = emaTA(spdTA(close, high, low, lag1, lag2, lag3), lag4)
    
    # Return Value:
    apd 
}


# ------------------------------------------------------------------------------


wprTA = 
function(close, high, low, lag)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Returns Williams %R Indicator
    
    # FUNCTION:
    
    # %R:
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
    wpr = (close - xmin) / (xmax -xmin)   
        
    # Return Result:
    wpr
}


################################################################################


rsiTA = 
function(close, lag)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Returns Relative Strength Index Indicator
        
    # FUNCTION:
    
    # RSI:
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


################################################################################
# MORE INDICATORS:


accelTA = 
function(x, n = 12) 
{   # A function written by Diethelm Wuertz

    # Description:
    #
    
    # FUNCTION:
    
    # Indicator:
    x = as.vector(x)
    accel = diff( x[(n+1):length(x)]-x[1:(length(x)-n)] )  
    accel = c(rep(NA, n+1), accel)
    
    # Return Value:
    accel 
}   


# ------------------------------------------------------------------------------
    
    
adiTA = 
function(high, low, close, volume) 
{   # A function written by Diethelm Wuertz

    # Description:
    #
    
    # FUNCTION:
    
    # Indicator:
    adi = cumsum((2 * close - high - low) / (high - low) * volume) 
    
    # Return Value:
    adi 
}
    

# ------------------------------------------------------------------------------

    
adoscillatorTA = 
function(open, high, low, close) 
{   # A function written by Diethelm Wuertz

    # Description:
    #
    
    # FUNCTION:
    
    # Indicator:
    adoscillator = (high - open + close - low) / (high - low) * 50 
    
    # Return Value:
    adoscillator 
}
    

# ------------------------------------------------------------------------------

    
bollingerTA = 
function(x, n = 20, n.sd = 2) 
{   # A function written by Diethelm Wuertz

    # Description:
    #
    
    # FUNCTION:
    
    # Indicator:
    mean = c(rep(NA, n-1), SMA(x = x, n = n))
    std = c(rep(NA, n-1), n.sd*sqrt(rollVar(x = x, n = n)))
    bollinger = as.matrix(cbind(upper = mean+std, price = x, lower = mean-std))
    rownames(bollinger) = as.character(1:length(x)) 
    
    # Return Value:
    bollinger 
}
    

# ------------------------------------------------------------------------------


chaikinoTA = 
function(high, low, close, volume, n.long = 10, n.short = 3, 
start = "average", na.rm = NULL) 
{   # A function written by Diethelm Wuertz

    # Description:
    #
    
    # FUNCTION:
    
    # Indicator:
    adi = TA.adi(high, low, close, volume)
    chaikino = EWMA(adi, n.short, start = start, na.rm = na.rm) - 
        EWMA(adi, n.long, start=start, na.rm = na.rm) 
    
    # Return Value:
    chaikino 
}
    

# ------------------------------------------------------------------------------

    
chaikinvTA = 
function(high, low, n.range = 10, n.change = 10, start = "average") 
{   # A function written by Diethelm Wuertz

    # Description:
    #
    
    # FUNCTION:
    
    # Indicator:
    rt = EWMA(high-low, n.range, start = start)
    chaikinv = (rt[-(1:n.change)]/rt[1:(length(rt)-n.change)]-1)*100
    chaikinv = c(rep(NA, n), chaikinv)
    
    # Return Value:
    chaikinv 
}   
    

# ------------------------------------------------------------------------------

        
garmanklassTA = 
function(open, high, low, close) 
{   # A function written by Diethelm Wuertz

    # Description:
    #
    
    # FUNCTION:
    
    # Indicator:
    prices = log(cbind(open, high, low, close))
    n = nrow(prices); alpha = 0.12; f = 0.192
    u = high-open; d = low-open; cc = close - open
    oc = (prices[2:n, 1] - prices[1:(n - 1), 4])^2
    garmanklass = 0.511*(u-d)^2 - 0.019*(cc*(u+d) - 2*u*d) - 0.383*cc^2
    garmanklass = sqrt(((1 - alpha)*garmanklass[2:n])/(1-f) + (alpha*oc)/f)
    garmanklass = c(NA, garmanklass)
    
    # Return Value:
    garmanklass 
}
    
    
# ------------------------------------------------------------------------------


nviTA = 
function(close, volume) 
{   # A function written by Diethelm Wuertz

    # Description:
    #
    
    # FUNCTION:
    
    # Indicator:
    ind = rep(0, length(close)-1)
    ind[diff(volume) < 0] = 1
    ch = c(0, TA.roc(close, n = 1)/100) 
    nvi = cumsum(ch * c(0, ind)) 
    
    # Return Value:
    nvi 
}
    
    

# ------------------------------------------------------------------------------


obvTA = 
function(close, volume) 
{   # A function written by Diethelm Wuertz

    # Description:
    #
    
    # FUNCTION:
    
    # Indicator:
    obv = cumsum(volume * c(0, sign(diff(close))))
    
    # Return Value:
    obv 
}
    


# ------------------------------------------------------------------------------

    
pviTA = 
function(close, volume) 
{   # A function written by Diethelm Wuertz

    # Description:
    #
    
    # FUNCTION:
    
    # Indicator:
    ind = rep(0, length(close)-1)
    ind[diff(volume) > 0] = 1
    ch = c(0, TA.roc(close, n = 1)/100)
    pvi = cumsum(ch * c(0, ind))
    
    # Return Value:
    pvi 
}
    

# ------------------------------------------------------------------------------


pvtrendTA = 
function(close, volume) 
{   # A function written by Diethelm Wuertz

    # Description:
    #
    
    # FUNCTION:
    
    # Indicator:
    m = length(close)   
    ch = cumsum( volume * c(0, (close[2:m]/close[1:(m-1)]-1)*100)) 
    
    # Return Value:
    ch 
}
     

# ------------------------------------------------------------------------------

    
williamsadTA = 
function(high, low, close) 
{   # A function written by Diethelm Wuertz

    # Description:
    #
    
    # FUNCTION:
    
    # Indicator:
    ind = c(0, sign(diff(close)))
    williamsad = vector("numeric", length(close))
    ind.pos = (ind == 1)
    ind.neg = (ind == -1)
    williamsad[ind.pos] = (close - low)[ind.pos]
    williamsad[ind.neg] =  - (high - close)[ind.neg]
    williamsad = cumsum(williamsad) 
    names(williamsad) = as.character(1:length(x))
    
    # Return Value:
    williamsad 
}
    


# ------------------------------------------------------------------------------

    
williamsrTA = 
function(high, low, close, n = 20) 
{   # A function written by Diethelm Wuertz

    # Description:
    #
    
    # FUNCTION:
    
    # Indicator:
    hh = rollMax(high, n, trim = FALSE)
    ll = rollMin(low, n, trim = FALSE)
    williamsr = (hh-close)/(hh-ll)*100 
    names(williamsr) = as.character(1:length(x))
    
    # Return Value:
    williamsr 
}           


################################################################################


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
        ans = oscTA(x = x[, select], lag = lag[1:2])
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
        ans = vohlTA(high = x[, "High"], low = x[, "Low"])
    }
        
    if (indicator == "vor") {
        ans = vorTA(high = x[, "High"], low = x[, "Low"])
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
        ans = rsiTA(close = x[, "Close"], lag = lag)
    }
         
    # Return Value:
    timeSeries(data = matrix(ans, ncol = 1), charvec = X@positions, 
        units = indicator, format = "ISO", zone = "GMT", 
        FinCenter = "GMT")         
}


################################################################################

