
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
# FUNCTION:             DISTRIBUTIONAL TESTS:
#  ks2Test               Two sample Kolmogorov-Smirnov test
# FUNCTION:             LOCATION TESTS:
#  .locationTest         Location test suite
#  tTest                 Unpaired t test for differences in mean
#  kw2Test               Kruskal-Wallis test for differences in locations  
# FUNCTION:             VARIANCE TESTS:
#  .varianceTest         Variance test suite
#  varfTest              F test for differences in variances
#  bartlett2Test         Bartlett's test for differences in variances
#  fligner2Test          Fligner-Killeen test for differences in variances
# FUNCTION:             SCALE TESTS:
#  .scaleTest            Scale test suite
#  ansariTest            Ansari-Bradley test for differences in scale
#  moodTest              Mood test for differences in scale
# FUNCTION:             CORRELATION TESTS:
#  .correlationTest      Correlation test suite
#  pearsonTest           Pearson product moment correlation coefficient
#  kendallTest           Kendall's tau correlation test
#  spearmanTest          Spearman's rho correlation test
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(TwoSampleTests); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
  

test.distributionTest = 
function()
{
    # Data:
    set.seed(1953)
    X = rnorm(100)
    Y = rt(50, df = 3)
    
    # Two Sample Kolmogorov-Smirnov Test:
    TEST = ks2Test(X, Y)
    print(TEST)
    checkIdentical(as.character(class(TEST)), "fHTEST")
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
  

test.locationTests = 
function()
{
    # Data:
    set.seed(1953)
    X = rnorm(100)
    Y = rt(50, df = 3)    
 
    # Location t-Test:
    TEST = tTest(X, Y)
    print(TEST)
    checkIdentical(as.character(class(TEST)), "fHTEST")
    
    # Location kw2-Test:
    TEST = kw2Test(X, Y)
    print(TEST)
    checkIdentical(as.character(class(TEST)), "fHTEST")
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
  

test.varianceTests = 
function()
{
    # Data:
    set.seed(1953)
    X = rnorm(100)
    Y = rt(50, df = 3)              
        
    # Variance F-Test:
    TEST = varfTest(X, Y)
    print(TEST)
    checkIdentical(as.character(class(TEST)), "fHTEST")
    
    # Variance Bartlett-Test:
    TEST = bartlett2Test(X, Y)
    print(TEST)
    checkIdentical(as.character(class(TEST)), "fHTEST")
    
    # Variance Fligner-Test:
    TEST = fligner2Test(X, Y)
    print(TEST)
    checkIdentical(as.character(class(TEST)), "fHTEST")
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
  

test.scaleTests = 
function()
{
    # Data:
    set.seed(1953)
    X = rnorm(100)
    Y = rt(50, df = 3)           
        
    # Scale Ansari-Test:
    TEST = varfTest(X, Y)
    print(TEST)
    checkIdentical(as.character(class(TEST)), "fHTEST")
    
    # Scale Mood-Test:
    TEST = moodTest(X, Y)
    print(TEST)
    checkIdentical(as.character(class(TEST)), "fHTEST")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.correlationTests = 
function()
{ 
    # Data:
    set.seed(1953)
    X = rnorm(100)
    Y = rt(100, df = 3)
    
    # Correlation Pearson-Test:
    TEST = pearsonTest(X, Y)
    print(TEST)
    checkIdentical(as.character(class(TEST)), "fHTEST")
    
    # Correlation Kendall-Test:
    TEST = kendallTest(X, Y)
    print(TEST)
    checkIdentical(as.character(class(TEST)), "fHTEST")
    
    # Correlation Spearman-Test:
    TEST = spearmanTest(X, Y)
    print(TEST)
    checkIdentical(as.character(class(TEST)), "fHTEST")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------
 

if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fBasics/test/runit5C.R")
    printTextProtocol(testData = testResult, fileName = "")
}
   
    
################################################################################

