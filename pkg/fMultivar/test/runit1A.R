
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
#   1999 - 2006, Diethelm Wuertz, GPL
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
# FUNCTION:                 DESCRIPTION:
#  .dailyTA                  Computes an indicator for technical analysis
# FUNCTION:                 DESCRIPTION:
#  .tradeSignals             Computes trade signals from trading positions
#  .tradeLengths             Computes trade length from trading signals
#  .hitRate                  Computes hit rates from returns and positions
################################################################################


### Uncomplete - Under Development ###


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(TechnicalAnalysis); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.technicalAnalysis =
function()
{
    # UNIT TEST:
    
    # Data:
    URL = "http://www.itp.phys.ethz.ch/econophysics/R/data/organisations/YAHOO/data/MSFT.CSV"
    download.file(URL, "MSFT.CSV")
    X = readSeries("MSFT.CSV")
    print(X)
    
    x = close = X[, "Close"]
    high   = X[, "High"]
    low    = X[, "Low"]
    open   = X[, "Open"]
    volume = X[, "Volume"]
    
    TA = emaTA(x, lambda = 0.1, startup = 0)
    dim(TA)
    head(TA)

    TA = biasTA(x, lag = 5)
    dim(TA)
    head(TA)
    
    TA = rocTA(x, lag = 5)
    dim(TA)
    head(TA)
    
    TA = oscTA(x, lag1 = 25, lag2 = 65)
    dim(TA)
    head(TA)
    
    # TA = momTA(x, lag = 5)
    # dim(TA)
    # head(TA)
    
    TA = macdTA(x, lag1 = 12, lag2 = 26)
    dim(TA)
    head(TA)
    
    TA = cdsTA(x, lag1 = 12, lag2 = 26, lag3 = 9)
    dim(TA)
    head(TA)
    
    TA = cdoTA(x, lag1 = 12, lag2 = 26, lag3 = 9)
    dim(TA)
    head(TA)
    
    TA = vohlTA(high, low)
    dim(TA)
    head(TA)
    
    TA = vorTA(high, low)
    dim(TA)
    head(TA)
    
    # TA = stochasticTA(close, high, low, lag1, lag2, type = c("fast", "slow")) 
    # dim(TA)
    # head(TA)
    
    # TA = fpkTA(close, high, low, lag = 9) ERROR
    dim(TA)
    head(TA)
    
    # TA = fpdTA(close, high, low, lag1, lag2)
    # dim(TA)
    # head(TA)
    
    # TA = spdTA(close, high, low, lag1, lag2, lag3)
    # dim(TA)
    # head(TA)
    
    # TA = apdTA(close, high, low, lag1, lag2, lag3, lag4)
    # dim(TA)
    # head(TA)
    
    # TA = wprTA(close, high, low, lag)
    # dim(TA)
    # head(TA)
    
    # TA = rsiTA(close, lag)
    # dim(TA)
    # head(TA)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.splusLike.MA =
function()
{    
    #  SMA                       Computes Simple Moving Average           
    #  EWMA                      Computes Exponentially Weighted  Moving Average

    # UNIT TEST:
    
    TA = SMA(x, n = 5)
    dim(TA) # !!!
    head(TA)
    
    TA = EWMA(x, 25)
    dim(TA)  
    head(TA)
    
    TA = EWMA(x, 2/(25+1))
    dim(TA)  
    head(TA)
    
    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------


test.dailyTA =
function()
{    
    #  .dailyTA                  Computes an indicator for technical analysis
    
    # UNIT TEST:
    
    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------


test.trading =
function()
{    

    #  .tradeSignals             Computes trade signals from trading positions
    #  .tradeLengths             Computes trade length from trading signals
    #  .hitRate                  Computes hit rates from returns and positions 
    
    # UNIT TEST:
    
    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult = runTestFile("C:/Rmetrics/SVN/trunk/fMultivar/test/runit1A.R")
    printTextProtocol(testResult)
}
   

################################################################################
    
