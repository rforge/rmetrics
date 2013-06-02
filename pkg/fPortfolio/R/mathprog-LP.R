
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


###############################################################################
# FUNCTION:                    DESCRIPTION:
#  glpkLP                       Convenience wrapper for GLPK solver
#  glpkControl                  GLPK control parameter list
# FUNCTION:                    DESCRIPTION:
#  symphonyLP                   Convenience wrapper for SYMPHONY solver
#  symphonyControl              SYMPHONY control parameter list
# EXAMPLES:                    DESCRIPTION:
#  .demoLP                      Internal SYMPHONY Demo Function
###############################################################################


glpkLP <- 
function(obj, mat, dir, rhs, bounds=NULL, types=NULL, max=FALSE)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Convenience wrapper for GLPK solver
    
    # FUNCTION:
    
    # Load:
    require(Rglpk)
    
    # Solve:
    optim <- Rglpk::Rglpk_solve_LP(
        obj = obj, 
        mat = mat, 
        dir = dir, 
        rhs = rhs, 
        bounds = bounds, 
        types = types, 
        max = max)   
        
    # Version:
    package <- packageDescription(pkg="Rglpk")
    version <- paste(package$Package, package$Version, package$Date)

    # Return Value:
    value <- list(
        opt = optim,
        solution = optim$solution, 
        objective = optim$optimum[[1]],
        status = optim$status[[1]],
        message = "Not available",
        solver = "glpkLP",
        version = version)
    class(value) <- c("solver", "list")
    value
}


# -----------------------------------------------------------------------------


glpkControl <- 
    function(trace=FALSE) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns control parameter list
    
    # FUNCTION:
    
    # Return Value:
    list(solver="glpk", trace=trace)
}


###############################################################################


symphonyLP <- 
function(obj, mat, dir, rhs, bounds=NULL, types=NULL, max=FALSE)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Convenience wrapper for SYMPHONY solver
    
    # FUNCTION:
    
    # Load:
    require(Rsymphony)
    
    # Solve:
    optim <- Rsymphony::Rsymphony_solve_LP(
        obj = obj, 
        mat = mat, 
        dir = dir, 
        rhs = rhs, 
        bounds = bounds, 
        types = types, 
        max = max)
        
    # Version:
    package <- packageDescription(pkg="Rsymphony")
    version <- paste(package$Package, package$Version, package$Date)
    
    # Return Value:
    value <- list(
        opt = optim,
        solution = optim$solution, 
        objective = optim$objval[[1]],
        status = optim$status[[1]],
        message = names(optim$status),
        solver = "symphonyLP",
        version = version)
    class(value) <- c("solver", "list")
    value
}


# -----------------------------------------------------------------------------


symphonyControl <- 
    function(trace=FALSE) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns control parameter list
    
    # FUNCTION:
    
    # Return Value:
    list(solver="symphony", trace=trace)
}


###############################################################################


.demoLP <- 
function()
{  
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #    Solver Examples
    
    # FUNCTION:
    
    # Settings:
    obj <- c(10, 6, 4)
    mat <- matrix(c(1, 10, 2, 1, 4, 2, 1, 5, 6), nrow = 3)
    dir <- c("<=", "<=", "<=")
    rhs <- c(100, 600, 300)
    bounds <- NULL
    types <- NULL
    max <- TRUE
    
    # Solve:
    glpk <- glpkLP(obj, mat, dir, rhs, bounds, types, max)
    print(glpk)
    symphony <- symphonyLP(obj, mat, dir, rhs, bounds, types, max)
    print(symphony)
    
    # Return Value:
    invisible()
}


###############################################################################

