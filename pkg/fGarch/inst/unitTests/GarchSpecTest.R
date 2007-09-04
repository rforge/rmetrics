
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
# FUNCTION:               SPECIFICATION: 
#  'garchSpec'             S4: garchSpec Class representation 
#  garchSpec               S4: Creates a 'garchSpec' object from scratch
#  show.garchSpec          S4: Print method for an object of class 'garchSpec'
################################################################################


test.garchSpec = 
function()
{   
    # Internal print Function:
    Print = function(x) {
        cat("RUnit Test:\n ")
        print(x@call)
        print(x)
        cat("\n")
    }
    
    # ARCH(1) - use default omega and default alpha[1]
    Print(garchSpec(model = list())) 
    
    # ARCH(1) - use default omega and specify alpha
    Print(garchSpec(model = list(alpha = 0.2))) 
    
    # ARCH(1) - specify omega and alpha
    Print(garchSpec(model = list(omega = 3.0e-6, alpha = 0.3)))
    
    # AR(1)-ARCH(1) - use default omega/alpha and specify alpha[1]
    Print(garchSpec(model = list(ar = 0.5))) 
    
    # AR([1,5])-ARCH(1) - use default omega, specify alpha and subset ar[.]
    Print(garchSpec(model = list(ar = c(0.5,0,0,0,0.1), alpha = 0.25)))
    
    # ARMA(1,2)-ARCH(1) - use default omega/alpha and specify ar[1]/ma[2]
    Print(garchSpec(model = list(ar = 0.5, ma = c(0.3, -0.3))))
    
    # ARMA(2,2)-ARCH(1) use default omega/alpha and specify ar[2]/ma[2]
    Print(garchSpec(model = list(ar = c(0.5, -0.5), ma = c(0.3,-0.3))))
    
    # ARCH(2) - use default omega and specify alpha[2]
    Print(garchSpec(model = list(alpha = c(0.12, 0.04))))
    
    # GARCH(1,1) - use just defaults
    Print(garchSpec())
    
    # GARCH(1,1) - use default omega and specify alpha/beta
    Print(garchSpec(model = list(alpha = 0.2, beta = 0.7)))
    
    # GARCH(1,1) - specify omega/alpha/beta
    Print(garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta = 0.8)))
    
    # GARCH(1,2) - use default omega and specify alpha[1]/beta[2]
    Print(garchSpec(model = list(alpha = 0.1, beta = c(0.4, 0.4))))
    
    # GARCH(2,1) - use default omega and specify alpha[2]/beta[1]
    Print(garchSpec(model = list(alpha = c(0.12, 0.04), beta = 0.08)))
    
    # rsnorm-ARCH(1) - use defaults with skew Normal
    Print(garchSpec(model = list(dist = 2), cond.dist = "rsnorm"))
    
    # rged-ARCH(1) using default omega and alpha[1]
    Print(garchSpec(model = list(dist = 4), cond.dist = "rged"))
    
    # rsged-ARCH(1) using default omega and alpha[1]
    Print(garchSpec(model = list(dist = c(4, 2)), cond.dist = "rsged"))
    
    # rstd-ARCH(1) using default omega and alpha[1]
    Print(garchSpec(model = list(dist = 4), cond.dist = "rstd"))
    
    # rsstd-ARCH(1) using default omega and alpha[1]
    Print(garchSpec(model = list(dist = c(4, 2)), cond.dist = "rsstd"))
    
    # TS-GARCH(1,1)
    Print(garchSpec(model = list(delta = 1)))
    
    # AR(1)-t-APARCH(2, 1)
    Print(garchSpec(model = list(mu = 1.0e-4, ar = 0.5, omega = 1.0e-6, 
        alpha = c(0.10, 0.05), gamma = c(0, 0), beta = 0.8, delta = 1.8, 
        dist = c(nu = 4, xi = 0.5)), cond.dist = "rsstd"))
        
    # Return Value:
    return() 
}


################################################################################

