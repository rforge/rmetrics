
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
# FUNCTION:                ADF TESTS:
#  adfTest                  ADF unit root test using Banarjee's test statistics
#  unitrootTest             ADF unit root test using McKinnon's test statistics
################################################################################


pvalue = 
function(object) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Return Value: 
    UseMethod("pvalue") 
}


# ------------------------------------------------------------------------------


pvalue.fHTEST = 
function(object) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # p Value:
    pValue = object@test$p.value 
    pNames = names(pValue)
    for (i in 1:length(pValue)) {
        if (is.null(pNames[i]) || pNames[i] == "") pNames[i] = "p.value"
    }
    names(pValue)<-pNames
    
    # Return Value:
    pValue        
}


################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(UnitrootDistribution, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.adfTest = 
function()
{  
    # A time series which contains no unit-root:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    x = rnorm(1000)  
    
    # A time series which contains a unit-root:
    y = cumsum(c(0, x))
    
    # Test x:
    A = adfTest(x)
    print(A)
    checkTrue(pvalue(A) < 0.05)
    
    # Test x:
    B = adfTest(y)
    print(B)
    checkTrue(pvalue(B) > 0.15)
   
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
 
    
test.unitrootTest = 
function()
{     
    # A time series which contains no unit-root:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    x = rnorm(1000)  
    
    # A time series which contains a unit-root:
    y = cumsum(c(0, x))
    
    # Test x:
    A = unitrootTest(x)
    print(A)
    checkTrue(pvalue(A)[1] < 0.05)
    checkTrue(pvalue(A)[2] < 0.05)
    
    # Test x:
    B = unitrootTest(y)    
    print(B)
    checkTrue(pvalue(B)[1] > 0.15)
    checkTrue(pvalue(B)[2] > 0.15)
   
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fSeries/test/runit2A.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################
    
