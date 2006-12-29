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
# You should have received A copy of the GNU Library General 
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
# FUNCTION:            AUGMENTED DICKEY FULLER DATA TABLES:
# .ansariTable          Finite sample p values for the Dickey-Fuller test
# .ansariPlot           Plots
# .dansari              Returns densities for the ADF Test given quantiles
# .pansari              Returns probabilities for the ADF Test given quantiles
# .qansari              Returns quantiles for the ADF Test given probabilities
################################################################################


################################################################################
# Note: x=-3:0; y=0:3; z=outer(x,y,"*"); rownames(z)=x; colnames(z)=y; z


.ansariTable =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:     
    #   Tables critical values for augmented Dickey-Fuller test.
    
    # Examples:
    #   .ansariTable()

    # FUNCTION:
      
    # Tables:
    N = c(5, 10, 50, 100, 500, 1000, 5000, 10000)
    p = c(0.01, 0.05, 0.10, 0.90, 0.95, 0.99)
    
    q = matrix(0, nrow = length(N), ncol = length(p))
    colnames(q) = p
    rownames(q) = N
    
    for (i in 1:length(N)) {
        for(j in 1:length(p)) {
            q[i,j] = .qansariw(p[j], N[i], N[i])
            print(c(i,j, q[i,j]))
        }
    }
 
    # Add Control:
    ans = q
    attr(ans, "control") <-
        c(table = "ansari")
        
    # Return Value:
    ans
} 


# ------------------------------------------------------------------------------


.ansariPlot =
function()
{   # A function implemented by Diethelm Wuertz

    # Match Arguments:
    trend = match.arg(trend)
    statistic = match.arg(statistic)
    
    # Load Table:
    Y = .ansariTable()
    X = cbind(expand.grid(x = Y$x, y = Y$y), z = as.vector(Y$z))
    x = X[, 1] # N
    y = X[, 3] # q-Stat
    z = X[, 2] # p-Value
    
    # Interpolate:
    gridPoints = 51
    ans = linearInterp(x, y, z, 
        xo = seq(min(x)-5, max(x)+5, length = gridPoints), 
        yo = seq(min(y), max(y), length = gridPoints)) 

    # Plot:
    persp(ans, theta = 40, xlab = "N", ylab = "q", zlab = "p-Value",
        main = paste(trend, "Ansari"))
    
    # Return Value:
    invisible(NULL)
}


# ------------------------------------------------------------------------------


.dansariw = 
function(x = NULL, m, n = m)
{   # A function Implemented by Diethelm Wuertz
    
    # Description:
    
    # Arguments:
    #   x - if x is null, then all available density-values are
    #       returned, the names of the vector belong to the
    #       allowed x values.
    #   m, n - lengths of x and y series.
    
    # Example:
    #   .dansariw(m = 3, n = 4)
    
    # FUNCTION:
    
    astart = 0
    L1 = 1 + floor(m *n/2)
    A1 = A2 = A3 = rep(-99, times = L1)
    IFAULT = 0
    lower = floor((m+1)^2/4)
    upper = lower + floor(m*n/2)
    Q = lower:upper
    
    # Density:
    result = .Fortran("asgscale", as.integer(m), as.integer(n), 
        as.double(astart), as.double(A1), as.integer(L1), as.double(A2), 
        as.double(A3), as.integer(IFAULT), PACKAGE = "fEcofin")
        
    # Result:
    ans = result[[4]]/choose(m+n, n)
    if (is.null(x)) {
        names(ans) = as.character(Q)
    } else {
        x = as.integer(x)
        d = rep(0, times = length(x))   
        for (i in 1:length(x)) {
            if (x[i] >= upper) {
                d[i] = 1
            } else {
                if (x[i] >= lower) d[i] = ans[x[i]+1-lower]
            }
        } 
        ans = d
        names(ans) = as.character(x)
    }
    
    # Return Value:
    ans

}


# ------------------------------------------------------------------------------


.pansariw =
function(q = NULL, m, n = m)
{   # A function Implemented by Diethelm Wuertz
    
    # Arguments:
    #   q - if q is null, then all available p-values are returned, 
    #       the names of the vector belong to the allowed q values
    #   m, n - lengths of x and y series.
    
    # Example:
    #   .pansariw(m = 3, n = 4)
    
    # Note:
    #   There exists an undocumented C function in R:
    #   .pansari = function(q, m, n) {
    #       .C("pansari", as.integer(length(q)), p = as.double(q),
    #           as.integer(m), as.integer(n), PACKAGE = "stats")$p }
    
    # FUNCTION:
    
    # Settings:
    astart = 0
    L1 = 1 + floor(m *n/2)
    A1 = A2 = A3 = rep(-99, times = L1)
    IFAULT = 0
    lower = floor((m+1)^2/4)
    upper = lower + floor(m*n/2)
    Q = lower:upper
    
    # p-values:
    result = .Fortran("wprob", as.integer(m), as.integer(n), 
        as.double(astart), as.double(A1), as.integer(L1), 
        as.double(A2), as.double(A3), as.integer(IFAULT), 
        PACKAGE = "fEcofin")
        
    # Result:
    ans = result[[4]]
    if (is.null(q)) {
        names(ans) = as.character(Q)
    } else {
        q = as.integer(q)
        p = rep(0, times = length(q))   
        for (i in 1:length(q)) {
            if (q[i] >= upper) {
                p[i] = 1
            } else {
                if (q[i] >= lower) p[i] = ans[q[i]+1-lower]
            }
        } 
        ans = p
        names(ans) = as.character(q)
    }
    
    # Return Value:
    ans
}
    

# ------------------------------------------------------------------------------


.qansariw =
function(p, m, n = m)
{   # A function Implemented by Diethelm Wuertz
    
    # Arguments:
    #   p - if p is null, then all available quantiles are returned, 
    #       the names of the vector belong to the allowed p values
    
    # Example:
    #   .qansariw(.pansariw(m = 3, n = 4), m = 3, n = 4)
    #   .qansariw((0:10)/10,  m = 3, n = 4)
    
    # Note:
    #   There exists an undocumented C function in R:
    #   .qansari = function(p, m, n) {
    #       .C("qansari", as.integer(length(p)), q = as.double(p),
    #           as.integer(m), as.integer(n), PACKAGE = "stats")$q }
    #   m, n - lengths of x and y series.
    
    # FUNCTION:
    
    # Settings:
    P = .pansariw(q = NULL, m = m, n = n)
    q = 0 * p 
    
    # Quantiles:
    for ( i in 1:length(p) ) {
        index = sign(P-p[i])
        q[i] = as.integer(names(index[index >= 0])[1])
    }
    ans = q
    
    # Return Value:
    ans 
}


################################################################################

