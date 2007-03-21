
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
#   1999 - 2007, Rmetrics Foundation, GPL
#   Contact: Diethelm Wuertz <wuertz@phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:               CONSTRAINTS:
#  setConstraints          Transforms constraint strings into a list value
#  getConstraints          Transforms a constraint list value into strings
################################################################################
   
    
setConstraints =
function(data, spec = portfolioSpec(), constraints = NULL)
{   # A function implemented by Rmetrics

    # Description:
    #   Transforms constraint strings into a list value
    
    # Arguments:
    #   assetReturns - a numerich vector of asset returns
    #   targetReturn - a nueric value specifying the target freturn
    #   constrainedStrings - a character vector with the following entries:
    #       minW[index] = numeric      maxW[index] = numeric  
    #       minsumW[index] = numeric   maxsumW[index] = numeric
    #       where index is an integer vector or integer value specifying
    #       one or more of the assets, numbered by index = 1, ..., N.
    
    # FUNCTION:
     
    # Get Statistics:
    mu = data$statistics$mu
    Sigma = data$statistics$Sigma

    N = length(mu)
    # Target Return:
    targetReturn = spec@portfolio$targetReturn 
    weights = spec@portfolio$weights
    if(is.null(targetReturn) & is.null(weights)) {
        weights = rep(1/N, N)
        warning("Equal Weights Portfolio in use")
    }
    if(is.null(targetReturn)) {
        targetReturn = (weights %*% Sigma %*% weights)[1, 1]
    }

    # Compose Matrix A:
    A = matrix(c(rep(1, times = N), mu), byrow = TRUE, ncol = N) 
    A = rbind(A, diag(1, N), diag(-1, N))
    colnames(A) = paste("A", 1:N, sep = "")
    rownames(A) = c("Budget", "Return", paste("minW", 1:N, sep = ""), 
        paste("maxW", 1:N, sep = ""))

    # Compose vector b0:
    minW = rep(0, N)
    maxW = rep(1, N)
    b0 = matrix(c(1, targetReturn, minW, -maxW), ncol = 1)
    colnames(b0) = "b0"
    if (!is.null(constraints)) {
        nC = length(constraints)
        what = substr(constraints, 1, 4)
        for (i in 1:nC) {       
            if (what[i] == "minW" | what[i] == "maxW") {
                eval(parse(text = constraints[i]))
            }
        }  
        b0 = matrix(c(1, targetReturn, minW, -maxW), ncol = 1)     
        what = substr(constraints, 1, 7)
        for (i in 1:nC) {       
            if (what[i] == "minsumW")  {
                minsumW = rep(0, times = N)
                eval(parse(text = constraints[i]))
                A = rbind(A, minsumW = sign(minsumW))          
                b = strsplit(constraints[i], "=")[[1]][2]
                b0 = rbind(b0, as.numeric(b))
            }
        }
        for (i in 1:nC) {       
            if (what[i] == "maxsumW")  {
                maxsumW = rep(0, times = N)
                eval(parse(text = constraints[i]))
                A = rbind(A, maxsumW = -sign(maxsumW))          
                b = strsplit(constraints[i], "=")[[1]][2]
                b0 = rbind(b0, -as.numeric(b))
            }
        }
    }


    rownames(b0) = rownames(A)
    ans = cbind(A = A, b = b0)
    colnames(ans) = c(colnames(A), "Exposure")
    class(ans) = c("constraintsMatrix", "matrix")
 
    # Return Value:
    ans
}


################################################################################


getConstraints =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getConstraints")
}


# ------------------------------------------------------------------------------


getConstraints.constraintsMatrix = 
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Transforms a constraint list value into strings
    
    # Arguments:
    #   object - the "constraintMatrix", a list with two named elements, 
    #   the constrainded Matrix A and the constrained vector b, satisfying
    #   A * w >= b, wher b is the exosure.
    
    # Value:
    #   A one column matrix with constraint strings.
    
    # FUNCTION:
    
    # Extract Matrix A and Vector b0:
    M = dim(object)[2]
    A = object[, -M]
    b0 = t(t(object[, M]))
    
    # Matrix Dimension:
    L = dim(A)[1]
    N = dim(A)[2] 
    
    # Start with Box Constraints:
    const1 = paste("minW[", 1:N, "] = ", b0[3:(2+N),], sep = "")
    const2 = paste("maxW[", 1:N, "] = ", -b0[(3+N):(2+2*N),], sep = "")
    constraints = matrix(c(const1, const2), ncol = 1)
    
    # Add Sector Constraints:
    if((3+2*N) <= L) {
        for (i in (3+2*N):L) {
            index = paste ((1:N)[abs(A[i, ]) != 0], collapse = ",")
            addConstraintString = paste(rownames(A)[i], "[c(", index, ")] = ", 
                abs(b0[i]), sep = "")
            constraints = rbind(constraints, addConstraintString)
        }
    }
    
    # Add Names:
    colnames(constraints) = "Constraints"
    rownames(constraints) = 1:(L-2)
    
    # Return Value:
    constraints
}


################################################################################

