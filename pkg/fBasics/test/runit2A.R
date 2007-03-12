
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
# FUNCTION:             SYMMETRIC STABLE DISTRIBUTION:
#  dsymstb               Returns density for symmetric stable DF
#  psymstb               Returns probabilities for symmetric stable DF
#  qsymstb               Returns quantiles for symmetric stable DF
#  rsymstb               Returns random variates for symmetric stable DF
# FUNCTIONS:            STABLE DISTRIBUTION:
#  stableMode            Computes stable mode
#  dstable               Returns density for stable DF
#  pstable               Returns probabilities for stable DF
#  qstable               Returns quantiles for stable DF
#  rstable               Returns random variates for stable DF
# FUNCTION:             STABLE SLIDERS:
#  symstbSlider          Displays symmetric stable distribution function
#  stableSlider          Displays stable distribution function
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(StableDistribution, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.symstb = 
function()
{ 
    # rsymstb, alpha=1.8
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("symstb", alpha = 1.8)
    print(test)
    checkTrue(mean(test) == 1)
    
    # rsymstb, alpha=1.2
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("symstb", alpha = 1.2)
    print(test)
    checkTrue(mean(test[2:3]) == 1)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.stableS0 = 
function()
{   
    # stable - Parameterization S0:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("stable", alpha = 1.8, beta = 0.3)
    print(test)
    checkTrue(mean(test[1:2]) == 1)
    
    # stable - Parameterization S0:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("stable", alpha = 1.2, beta = -0.3)
    print(test)
    checkTrue(mean(test[1:2]) == 1)
    
    # stable - Parameterization S0:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("stable", alpha = 0.6, beta = 0)
    print(test)
    checkTrue(mean(test[1:2]) == 1)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.stableS1 = 
function()
{   
    # stable - Parameterization S1:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("stable", alpha = 1.8, beta = 0.3, pm = 1)
    print(test)
    checkTrue(mean(test[1:2]) == 1)
    
    # stable - Parameterization S1:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("stable", alpha = 1.2, beta = -0.3, pm = 1)
    print(test)
    checkTrue(mean(test[1:2]) == 1)
    
    # stable - Parameterization S1:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("stable", alpha = 0.6, beta = 0, pm = 1)
    print(test)
    checkTrue(mean(test[1:2]) == 1)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.stableS2 = 
function()
{   
    # stable - Parameterization S2:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("stable", alpha = 1.8, beta = 0.3, pm = 2)
    print(test)
    checkTrue(mean(test[1:2]) == 1)
    
    # stable - Parameterization S2:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("stable", alpha = 1.2, beta = -0.3, pm = 2)
    print(test)
    checkTrue(mean(test[1:2]) == 1)
    
    # stable - Parameterization S2:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("stable", alpha = 0.6, beta = 0, pm = 2)
    print(test)
    checkTrue(mean(test[1:2]) == 1)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.symstbSlider = 
function()
{
    # Try:
    symstbSlider()
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.stableSlider = 
function()
{
    # Try:
    stableSlider()
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fBasics/test/runit2A.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}

       
################################################################################

