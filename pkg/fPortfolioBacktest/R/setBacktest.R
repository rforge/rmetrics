
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
# FUNCTION:                    		 				WINDOWS SLOT:
#	setWindowsFun<-								Sets the rolling windows function
#  setWindowsParams<-							Sets the additional parameters to windows function
#	setWindowsHorizon<-							Sets the horizon of the rolling window
# FUNCTION:                     						STRATEGY SLOT:
#  setStrategyFun<-         							Sets the portfolio strategy function	
#  setStrategyParams<-							Sets the additional parameters to strategy function
# FUNCTION:                     						SMOOTHER SLOT:
#  setSmootherFun<-								Sets the smoothing function
#  setSmootherParams<-							Sets the additional parameters to smoother function
#  setSmootherLambda<-							Sets the lambda for EMA smoothing
#  setSmootherDoubleSmoothing<-			Sets the double ema setting, logical
#  setSmootherInitialWeights<- 				Sets the initial weights of the portfolio
#  setSmootherSkip<-								Sets the number of months to skip starting

################################################################################


"setWindowsFun<-" =
    function(backtest, value)
{
    backtest@windows$windows = value
    backtest
}

# ------------------------------------------------------------------------------

"setWindowsParams<-" =
    function(backtest, value)
{
    backtest@windows$params = value
    backtest
}

# ------------------------------------------------------------------------------

"setWindowsHorizon<-" =
    function(backtest, value)
{
    backtest@windows$params$horizon = value
    backtest
}

# ------------------------------------------------------------------------------


"setStrategyFun<-" =
    function(backtest, value)
{
    backtest@strategy$strategy = value
    backtest
}

# ------------------------------------------------------------------------------

"setStrategyParams<-" =
    function(backtest, value)
{
    backtest@strategy$params = value
    backtest
}

# ------------------------------------------------------------------------------

"setSmootherFun<-" =
    function(backtest, value)
{
    backtest@smoother$smoother = value
    backtest
}

# ------------------------------------------------------------------------------


"setSmootherParams<-" =
    function(backtest, value)
{
    backtest@smoother$params = value
    backtest
}

# ------------------------------------------------------------------------------

"setSmootherLambda<-" =
    function(backtest, value)
{
    backtest@smoother$params$lambda = value
    backtest
}

# ------------------------------------------------------------------------------


"setSmootherDoubleSmoothing<-" =
    function(backtest, value)   
{
    backtest@smoother$params$doubleSmoothing = value
    backtest
}

# ------------------------------------------------------------------------------


"setSmootherInitialWeights<-" =
    function(backtest, value)
{
    backtest@smoother$params$initialWeights = value
    backtest
}

# ------------------------------------------------------------------------------

"setSmootherSkip<-" = 
	function(backtest, value)
{
    backtest@smoother$params$skip = value
    backtest
}
   
################################################################################
   