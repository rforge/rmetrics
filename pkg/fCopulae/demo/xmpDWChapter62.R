#
# Examples from the Monograph:
#   "Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 6.2
#   Elliptical Copulae
#
# List of Examples, Exercises and Code Snippets:
#
#   6.2.1 Example: Elliptically Contoured Distributions
#   6.2.2 Example: Generate Normal Copula
#   6.2.3 Example: Generate Student-t Copula
#       * Code Snipptet:  
#
#   *** This list is not yet complete ***
#
# Author:
#   (C) 2002-2004, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################


### Load Library:

    # Load:
    require(fCopulae)
    ###
    
    
# ------------------------------------------------------------------------------
# 6.2.1 Example: Elliptically Contoured Distributions

    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Generate the 2D Grid and Fix Contour Levels:
    x = grid2d(seq(-4, 4, length = 101))
    Levels = c(0.005, 0.01, 0.02, 0.05, 0.1, 0.2)
    ###

    # Bivariate Densities for 4 Different Distributions:
    Type = c("norm", "t", "logistic", "laplace")
    Title = paste(c("Normal", "Student-t", "Logistic",
        "Laplace"), "Copula")
    for (i in 1:4 ) {
        D = delliptical2d(x, rho = 0.5, type = Type[i],
            output = "list")
        contour(D, levels = Levels, xlab = "x", ylab = "y",
            main = Title[i], cex = 0.5)
    }
    ###
    
    
# ------------------------------------------------------------------------------
# 6.2.2 Example: Generate Normal Copula

    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Generate the 2D Grid:
    N = 25; x = (0:N)/N
    uv = grid2d(x)
    u = uv$x; v = uv$y
    ###

    # Compute the normal Copula:
    C.norm = pellipticalCopula(u, v, rho = 3/4, type = "norm",
        output = "list")
    ###

    # Create a Perspective Plot:
    persp(C.norm, theta = -40, phi = 30, ticktype = "detailed",
        col = "steelblue", main = "Normal Copula", cex = 0.5)
    ###
    
    # Create a Contour Plot:
    contour(C.norm, nlevels = 10, main = "Normal Copula")
    ###
    
    
# ------------------------------------------------------------------------------
# 6.2.3 Example: Generate Student-t Copula

    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Generate the 2D Grid:
    N = 25; x = (0:N)/N; uv = grid2d(x); u = uv$x; v = uv$y
    ###
    
    # Compute the Copula:
    C.t = pellipticalCopula(u, v, rho = 3/4, type = "t",
        output = "list")
    ###
    
    # Create a Perspective Plot:
    persp(C.t, theta = -40, phi = 30, ticktype = "detailed",
        col = "steelblue", main = "Student-t Copula", cex = 0.5)
    ###
    
    # Create a Contour Plot:
    contour(C.t, nlevels = 10, main = "Student-t Copula")
    ###
 
################################################################################

   