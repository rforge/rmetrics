
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
# FUNCTION:              SKEW NORMAL DISTRIBUTION:
#  dsnorm                 Density for the skew normal Distribution
#  psnorm                 Probability function for the skew NORM
#  qsnorm                 Quantile function for the skew NORM
#  rsnorm                 Random Number Generator for the skew NORM
#  .snormSlider           Displays Normal Distribution and RVS
# FUNCTION:              VARIANCE-1 STUDENT-T DISTRIBUTION:
#  dstd                   Density for the Student-t Distribution
#  pstd                   Probability function for the Student-t Distribution
#  qstd                   Quantile function for the Student-t Distribution
#  rstd                   Random Number Generator for the Student-t
# FUNCTION:              SKEW VARIANCE-1 STUDENT-T DISTRIBUTION:
#  dsstd                  Density for the skewed Student-t Distribution
#  psstd                  Probability function for the skewed STD
#  qsstd                  Quantile function for the skewed STD
#  rsstd                  Random Number Generator for the skewed STD
#  .stdSlider             Displays Variance-1 Student-t Distribution and RVS
# FUNCTION:              GED DISTRIBUTION:
#  dged                   Density for the Generalized Error Distribution
#  pged                   Probability function for the GED
#  qged                   Quantile function for the GED
#  rged                   Random Number Generator for the GED
# FUNCTION:              SKEW GED DISTRIBUTION:
#  dsged                  Density for the skewed GED
#  psged                  Probability function for the skewed GED
#  qsged                  Quantile function for the skewed GED
#  rsged                  Random Number Generator for the skewed GED
# FUNCTION:              GED DISTRIBUTION SLIDER:
#  .sgedSlider            Displays Generalized Error Distribution and RVS
# FUNCTION:              PARAMETER ESTIMATION:
#  normFit                Fit the parameters for a Normal distribution
#  snormFit               Fit the parameters for a skew Normal distribution
#  gedFit                 Fit the parameters for a GED distribution
#  sgedFit                Fit the parameters for a skew GED distribution
#  stdFit                 Fit the parameters for a Sudent-t distribution
#  sstdFit                Fit the parameters for a skew Sudent-t distribution
# FUNCTION:              MOMENTS:
#  absMoments             Compute absolute moments of a symmetric distribution
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(GarchDistributions, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.normDistribution = 
function()
{   
    # Normal Distribution:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("norm",  mean = 0, sd = 1, robust = TRUE)
    print(test)
    ### checkTrue(mean(test) == 1)                                        #CHECK
    
    # Skew Normal Distribution:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("snorm", mean = 0, sd = 1, xi = 1.5, robust = TRUE) 
    print(test)
    ### checkTrue(mean(test) == 1)                                       # CHECK
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.snormSlider = 
function()
{   
    # Try Distribution:
    .snormSlider(type = "dist")
   
    # Try Random Variates:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    .snormSlider(type = "rand")
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.sstdDistribution = 
function()
{ 
    # Standardized Student-t Distribution:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("std",  mean = 0, sd = 1, nu = 5, robust = TRUE) 
    print(test)
    ### checkTrue(mean(test) == 1)                                       # CHECK
    
    # Skew Standardized Student-t Distribution:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("sstd", mean = 0, sd = 1, nu = 5, xi = 1.5, robust = TRUE) 
    print(test)
    ### checkTrue(mean(test) == 1)                                       # CHECK

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.sstdSlider = 
function()
{   
    # Try Distribution:
    .sstdSlider(type = "dist")                              
   
    # Try Random Variates:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    .sstdSlider(type = "rand")
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.sgedDistribution = 
function()
{       
    # Generalized Error Distribution:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("ged",  mean = 0, sd = 1, nu = 2, robust = TRUE) 
    print(test)
    ### checkTrue(mean(test) == 1)                                       # CHECK
       
    # Skew Generalized Error Distribution:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(1953, kind = "Marsaglia-Multicarry")
    test = .distCheck("sged", mean = 0, sd = 1, nu = 2, xi = 0.8, robust = TRUE) 
    print(test)
    ### checkTrue(mean(test) == 1)                                       # CHECK
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.sgedSlider = 
function()
{   
    # Try Distribution:
    .sgedSlider(type = "dist")
   
    # Try Random Variates:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    .sgedSlider(type = "rand")
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.normFit = 
function()
{  
    # Parameter Estimation:
    #  normFit - Fit the parameters for a Normal distribution
    
    # Normal Distribution:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    x = rnorm(n = 1000, mean = 0, sd = 1)
    fit = normFit(x)
    print(fit)
    target = round(as.vector(fit$estimate), 4)
    print(target)
    current = c(0.0114, 0.9904)
    checkEqualsNumeric(target, current)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.snormFit = 
function()
{  
    # Parameter Estimation:
    #  snormFit - Fit the parameters for a skew Normal distribution
    
    # Skew Normal Distribution:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    x = rsnorm(n = 1000, mean = 0, sd = 1, xi = 1.5)
    fit = snormFit(x)
    print(fit)
    target = round(as.vector(fit$estimate), 4)
    print(target)
    current = c(-0.0017, 1.0259, 1.5632)
    checkEqualsNumeric(target, current)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.gedFit = 
function()
{      
    # Parameter Estimation:
    #  gedFit - Fit the parameters for a GED distribution
    
    # Generalized Error Distribution:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    x = rged(1000, mean = 0, nu = 2)
    fit = gedFit(x)
    print(fit)
    target = round(as.vector(fit$estimate), 4)
    print(target)
    current = c(0.0270, 1.0295, 2.4100)
    checkEqualsNumeric(target, current)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.sgedFit = 
function()
{      
    # Fit the parameters for a skew GED distribution
    #  sgedFit - Fit the parameters for a skew GED distribution
    
    # Skew Generalized Error Distribution:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    x = rsged(1000, mean = 0, sd = 1, nu = 2, xi = 1.5)
    fit = sgedFit(x)
    print(fit)
    target = round(as.vector(fit$estimate), 4)
    print(target)
    current = c(0.0128, 1.0128, 2.3499, 1.5328)
    checkEqualsNumeric(target, current)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.stdFit = 
function()
{         
    # Fit the parameters for a Student-t distribution
    # stdFit - Fit the parameters for a Sudent-t distribution
    
    # Standardized Student-t Distribution:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    x = rstd(n = 2500, mean = 0, sd = 1, nu = 5)
    fit = stdFit(x)
    print(fit)
    target = round(as.vector(fit$estimate), 4)
    print(target)
    current = c(0.0321, 1.0074, 4.9768)
    checkEqualsNumeric(target, current)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.sstdFit = 
function()
{         
    # Fit the parameters for a skew Sudent-t distribution
    # sstdFit - Fit the parameters for a Sudent-t distribution
    
    # Skew Standardized Student-t Distribution:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    x = rsstd(n = 2500, mean = 0, sd = 1, nu = 5, xi = 1.5)
    fit = sstdFit(x)
    print(fit)
    target = round(as.vector(fit$estimate), 4)
    print(target)
    current = c(-0.0185, 1.0159, 4.9880, 1.4675)
    checkEqualsNumeric(target, current)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.absMoments = 
function()
{  
    #  Compute absolute moments of a symmetric distribution
    
    # Function Call:
    #   absMoments(n, density = c("dnorm", "dged", "dstd"), ...) 
    
    # Absolute Moments - Normal Distribution:
    ans = absMoments(1:4, "dnorm")
    print(ans)
    
    # Absolute Moments - Skew Student-t Distribution:
    ans = absMoments(1:4, "dstd", nu = 20)
    print(ans)
    
    # Absolute Moments - GED Distribution:
    ans = absMoments(1:4, "dged", nu = 2)
    print(ans)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fSeries/tests/runit4A.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################
    
