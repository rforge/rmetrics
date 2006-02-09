
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description.  See the
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
# FUNCTION:       DESCRIPTION:               
#   kummerM        Computes Confluent Hypergeometric Function of the 1st Kind
#   kummerU        Computes Confluent Hypergeometric Function of the 2nd Kind
#   whittakerM     Computes Whittaker's M Function
#   whittakerW     Computes Whittaker's M Function
#   hermiteH       Computes the Hermite Polynomial
################################################################################


################################################################################
# DESCRIPTION:
#  This is a collection and description of functions which compute 
#  the Confluent Hypergeometric and Related Functions. The functions 
#  use the TOMS707 Algorithm by MARK NARDIN, W. F. PERGER and 
#  ATUL BHALLA Michigan Technological University, Copyright 1989.    
#  A numerical evaluator for the confluent hypergeometric function for 
#  complex arguments with large magnitudes using a direct summation of 
#  the Kummer series. The method used allows an accuracy of up to thirteen      
#  decimal places through the use of large real arrays and a single final 
#  division. LNCHF is a variable which selects how the result should be 
#  represented.  A '0' will return the value in standard exponential 
#  form.  A '1' will return the LOG of the result. IP is an integer     
#  variable that specifies how many array positions are desired (usually 
#  10 is sufficient). Setting IP=0 causes the program to estimate the 
#  number of array positions.                                                                 
#  The confluent hypergeometric function is the solution to  
#  the differential equation:                                
# 
#         zf"(z) + (a-z)f'(z) - bf(z) = 0    
#
#  These functions are required to valuate Asian Options based on
#  the theory of Exponential Brownian Motion.    
################################################################################ 


kummerM =  
function(x, a, b, lnchf = 0, ip = 0) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculate the Confluent Hypergeometric Function of the First  
    #   Kind for complex argument "x" and complex indexes "a" and "b"
    
    # Arguments:
    
    # FUNCTION:
    
    # Test for complex arguments:
    if (!is.complex(x)) x = complex(real = x, imag = 0*x)
    if (!is.complex(a)) a = complex(real = a, imag = 0)
    if (!is.complex(b)) b = complex(real = b, imag = 0)    
    
    # Calculate KummerM:
    chm = rep(complex(real = 0, imag = 0), length = length(x))
    value = .Fortran("chfm",
        as.double(Re(x)),
        as.double(Im(x)),
        as.double(Re(a)),
        as.double(Im(a)),
        as.double(Re(b)),
        as.double(Im(b)),
        as.double(Re(chm)),
        as.double(Im(chm)),
        as.integer(length(x)),
        as.integer(lnchf),
        as.integer(ip),
        PACKAGE = "fOptions")
    result = complex(real = value[[7]], imag = value[[8]]) 
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


kummerU =  
function(x, a, b, ip = 0) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculate the Confluent Hypergeometric Function of the Second 
    #   Kind for complex argument "x" and complex indexes "a" and "b"
  
    # Arguments:
    
    # FUNCTION:
    
    # Todo ...
    lnchf = 0
    
    # Test for complex arguments:
    if (!is.complex(x)) x = complex(real = x, imag = 0*x)
    if (!is.complex(a)) a = complex(real = a, imag = 0)
    if (!is.complex(b)) b = complex(real = b, imag = 0)
    
    # Calculate KummerU:
    # From KummerM:
    # Uses the formula ...
    #   pi/sin(pi*b) [ M(a,b,z) / (Gamma(1+a-b)*Gamma(b)) - 
    #        x^(1-b) * M(1+a-b,2-b,z) / (Gamma(a)*Gamma(2-b)) ]
    ans = ( pi/sin(pi*b) ) * (
        kummerM(x, a = a, b = b, lnchf = lnchf, ip=ip) /
            ( cgamma(1+a-b)*cgamma(b) ) - (x^(1-b)) * 
        kummerM(x, a = (1+a-b), b=2-b, lnchf = lnchf, ip = ip) / 
            ( cgamma(a)*cgamma(2-b) ) )

    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------


whittakerM = 
function(x, kappa, mu, ip = 0) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Whittaker's M Function
    
   	# Arguments:
    
    # FUNCTION:
    
    # Test for complex arguments:
    if (!is.complex(x)) x = complex(real = x, imag = 0*x)
    if (!is.complex(kappa)) kappa = complex(real = kappa, imag = 0)
    if (!is.complex(mu)) mu = complex(real = mu, imag = 0)
    
    # Calculate:
    ans = exp(-x/2) * x^(1/2+mu) * kummerM(x, 1/2+mu-kappa, 1+2*mu, ip = ip)
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


whittakerW = 
function(x, kappa, mu, ip = 0) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Whittaker's M Function
    
    # Arguments:
    
    # FUNCTION:
    
    # Test for complex arguments:
    if (!is.complex(x)) x = complex(real = x, imag = 0*x)
    if (!is.complex(kappa)) kappa = complex(real = kappa, imag = 0)
    if (!is.complex(mu)) mu = complex(real = mu, imag = 0)
    
    # Calculate:
    ans = exp(-x/2) * x^(1/2+mu) * kummerU(x, 1/2+mu-kappa, 1+2*mu, ip = ip)
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


hermiteH = 
function(x, n, ip = 0)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the Hermite Polynomial 
    
    # Arguments:
    #	n - the index of the Hermite polynomial.
    
    # FUNCTION:
    
    # Result:
    ans = 2^n * Re ( kummerU(x^2, -n/2, 1/2, ip = ip) )
    
    # Return Value:
    ans
    
}


################################################################################

