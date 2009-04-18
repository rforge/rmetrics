
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
#   1999 - 2009, Rmetrics Association, Zurich
#   1999 - 2009, Diethelm Wuertz <wuertz@itp.phys.ethz.ch>  
#   www.rmetrics.org
# for code accessed (or partly included) from other R-ports 
#   and other sources see R's copyright and license files


################################################################################
# FUNCTION:                    DESCRIPTION:
#  setWindowsFun<-              Sets rolling windows function
#  setWindowsParams<-           Sets additional parameters to windows function
#   setWindowsHorizon<-         Sets horizon of the rolling window
# FUNCTION:                    DESCRIPTION:         
#  setStrategyFun<-             Sets portfolio strategy function    
#  setStrategyParams<-          Sets additional parameters to strategy function
# FUNCTION:                    DESCRIPTION:
#  setSmootherFun<-             Sets smoothing function
#  setSmootherParams<-          Sets additional parameters to smoother function
#  setSmootherLambda<-          Sets lambda for EMA smoothing
#  setSmootherDoubleSmoothing<- Sets double ema setting, logical
#  setSmootherInitialWeights<-  Sets initial weights of the portfolio
#  setSmootherSkip<-            Sets number of months to skip starting
################################################################################


"setWindowsFun<-" =
function(backtest, value)
{
    # A function implemented by William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    backtest@windows$windows = value
    
    # Return Value:
    backtest
}


# ------------------------------------------------------------------------------


"setWindowsParams<-" =
function(backtest, value)
{
    # A function implemented by William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    backtest@windows$params = value
    
    # Return Value:
    backtest
}


# ------------------------------------------------------------------------------


"setWindowsHorizon<-" =
function(backtest, value)
{
    # A function implemented by William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    backtest@windows$params$horizon = value
    
    # Return Value:
    backtest
}


# ------------------------------------------------------------------------------


"setStrategyFun<-" =
function(backtest, value)
{
    # A function implemented by William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    backtest@strategy$strategy = value
    
    # Return Value:
    backtest
}

# ------------------------------------------------------------------------------


"setStrategyParams<-" =
function(backtest, value)
{
    # A function implemented by William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    backtest@strategy$params = value
    
    # Return Value:
    backtest
}


# ------------------------------------------------------------------------------


"setSmootherFun<-" =
function(backtest, value)
{
    # A function implemented by William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    backtest@smoother$smoother = value
    
    # Return Value:
    backtest
}


# ------------------------------------------------------------------------------


"setSmootherParams<-" =
function(backtest, value)
{
    # A function implemented by William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    backtest@smoother$params = value
    
    # Return Value:
    backtest
}


# ------------------------------------------------------------------------------


"setSmootherLambda<-" =
function(backtest, value)
{
    # A function implemented by William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    backtest@smoother$params$lambda = value
    
    # Return Value:
    backtest
}


# ------------------------------------------------------------------------------


"setSmootherDoubleSmoothing<-" =
function(backtest, value)   
{
    # A function implemented by William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    backtest@smoother$params$doubleSmoothing = value
    
    # Return Value:
    backtest
}


# ------------------------------------------------------------------------------


"setSmootherInitialWeights<-" =
function(backtest, value)
{
    # A function implemented by William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    backtest@smoother$params$initialWeights = value
    
    # Return Value:
    backtest
}


# ------------------------------------------------------------------------------


"setSmootherSkip<-" = 
function(backtest, value)
{
    # A function implemented by William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    backtest@smoother$params$skip = value
    
    # Return Value:
    backtest
}
   

################################################################################
  
 