
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
#  Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#  info@rmetrics.org
#  www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#  see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#  see Rmetrics's copyright file


################################################################################
# FUNCTION:             REGRESSION MODELLING DESCRIPTION:
#  regFit                Wrapper Function for Regression Models
################################################################################


test.lmFit <- 
    function()
{   
    # Simulate Artificial LM:
    x = regSim(model = "LM3", n = 50)
    
    # Fit Parameters:
    lmfit = regFit(Y ~ X1 + X2 + X3, data = x, use = "lm") 
    lm = lm(Y ~ X1 + X2 + X3, data = x) 
    
    # Terms:
    terms(lmfit@fit)
    terms(lm)
 
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.rlmFit <- 
    function()
{   
    # Simulate Artificial LM:
    x = regSim(model = "LM3", n = 50)
    
    # Fit Parameters:
    rlmfit = regFit(Y ~ X1 + X2 + X3, data = x, use = "rlm") 
    rlm = rlm(Y ~ X1 + X2 + X3, data = x) 
    
    # Terms:
    terms(rlmfit@fit)
    terms(rlm)
      
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.glmFit <- 
    function()
{   
    # Simulate Artificial LM:
    x = regSim(model = "LM3", n = 50)
    
    # Fit Parameters:
    glmfit = regFit(Y ~ X1 + X2 + X3, data = x, use = "glm") 
    glm = glm(Y ~ X1 + X2 + X3, data = x) 
    
    # Terms:
    terms(glmfit@fit)
    terms(glm)
     
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.gamFit <- 
    function()
{   
    # Simulate Artificial LM:
    x = regSim(model = "LM3", n = 50)
    
    # Fit Parameters:
    gamfit = regFit(Y ~ s(X1) + s(X2) + X3, data = x, use = "gam") 
    gam = gam(Y ~ X1 + X2 + X3, data = x) 
    
    # Terms:
    terms(gamfit@fit)
    terms(gam)
      
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.pprFit <- 
    function()
{   
    # Simulate Artificial LM:
    x = regSim(model = "LM3", n = 50)
    
    # Fit Parameters:
    pprfit = regFit(Y ~ X1 + X2 + X3, data = x, use = "ppr") 
    ppr = ppr(Y ~ X1 + X2 + X3, data = x, nterms = 2) 
    
    # Terms:
    terms(pprfit@fit)
    terms(ppr)
       
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.nnetFit <- 
    function()
{   
    # Simulate Artificial LM:
    x = regSim(model = "LM3", n = 50)
    
    # Fit Parameters:
    nnetfit = regFit(Y ~ X1 + X2 + X3, data = x, use = "nnet") 
    nnet = nnet(Y ~ X1 + X2 + X3, data = x, trace = FALSE, 
        size = 2, linout = TRUE) 
    
    # Terms:
    terms(nnetfit@fit)
    terms(nnet)
       
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.polymarsFit <- 
    function()
{   
    # Simulate Artificial LM:
    x = regSim(model = "LM3", n = 50)
    
    # Fit Parameters:
    polymarsfit = regFit(Y ~ X1 + X2 + X3, data = x, use = "polymars") 
    polymars = .polymars(Y ~ X1 + X2 + X3, data = x)
    
    # Terms:
    terms(polymarsfit@fit)
    terms(polymars)
    
    # Return Value:
    return()
}


################################################################################

