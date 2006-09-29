#
# Examples from the forthcoming Monograph:
#   Rmetrics - Financial Engineering and Computational Finance
#   written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 4.4
#   Vector ARMA Models and Cointegration
#
# List of Examples, Exercises and Code Snippets:
#    
#   Example: Phillips-Ouliaris Test, Non-Cointegrated Case
#   Example: Phillips-Ouliaris Test: Cointegrated Case
#
#   *** This list is not yet complete ***
#
# Author:
#   (C) 2002-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################
    

### Example: Phillips-Ouliaris Test,  Non-Cointegrated Case

    # Create Series:
    x = ts(diffinv(matrix(rnorm(2000), 1000, 2)))
    
    # Test:
    tspoTest(x)
    ###
    

# ------------------------------------------------------------------------------
    
    
### Example: Phillips-Ouliaris Test: Cointegrated Case

    # Create Series:
    x = diffinv(rnorm(1000))
    y = 2.0 - 3.0*x + rnorm(x, sd = 5)
    z = ts(cbind(x, y))
    
    # Test:
    tspoTest(z)
    ###
    
    
################################################################################

