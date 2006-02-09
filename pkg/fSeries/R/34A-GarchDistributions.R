
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
# PART I: Skew Normal Distribution
# PART II: Skew Variance-1 Student-t Distribution
# PART III: Skew Generlaized Error Distribution
# PART IV: Parameter Estimation


################################################################################
# PART I:
# FUNCTION:             NORMAL Distribution - part of R's base Package:
#  * dnorm                R: Density for the Normal Distribution
#  * pnorm                R: Probability function for the Normal Distribution
#  * qnorm                R: Quantile function for the Normal Distribution
#  * rnorm                R: Random Number Generator for the Normal Distribution  
# FUNCTION:             SKEW-DISTRIBUTIONS DESCRIPTION:
#  dsnorm                 Density for the skew normal Distribution
#  psnorm                 Probability function for the skew NORM
#  qsnorm                 Quantile function for the skew NORM
#  rsnorm                 Random Number Generator for the skew NORM
################################################################################


################################################################################
# PART II:
# FUNCTION:             VARIANCE-1 STUDENT-T DISTRIBUTION:
#  dstd                   Density for the Student-t Distribution
#  pstd                   Probability function for the Student-t Distribution
#  qstd                   Quantile function for the Student-t Distribution
#  rstd                   Random Number Generator for the Student-t
# FUNCTION:             DESCRIPTION:
#  dsstd                  Density for the skewed Student-t Distribution
#  psstd                  Probability function for the skewed STD
#  qsstd                  Quantile function for the skewed STD
#  rsstd                  Random Number Generator for the skewed STD
################################################################################


################################################################################
# PART III:
# FUNCTION:             GED DISTRIBUTION:
#  dged                   Density for the Generalized Error Distribution
#  pged                   Probability function for the GED
#  qged                   Quantile function for the GED
#  rged                   Random Number Generator for the GED
# FUNCTION:             DESCRIPTION:
#  dsged                  Density for the skewed GED
#  psged                  Probability function for the skewed GED
#  qsged                  Quantile function for the skewed GED
#  rsged                  Random Number Generator for the skewed GED
################################################################################


################################################################################
# PART IV:
# FUNCTION:              PARAMETER ESTIMATION:
#  normFit            	  Fit the parameters for a Normal distribution
#  snormFit           	  Fit the parameters for a skew Normal distribution
#  gedFit             	  Fit the parameters for a GED distribution
#  sgedFit            	  Fit the parameters for a skew GED distribution
#  stdFit             	  Fit the parameters for a Sudent-t distribution
#  sstdFit            	  Fit the parameters for a skew Sudent-t distribution
################################################################################


################################################################################
# PART I: NORMAL DISTRIBUTION


.dsnorm = 
function(x, xi) 
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Compute the density function of the "normalized" skew 
    #   normal distribution
    
    # FUNCTION:

    # Standardize:
    m1 = 2/sqrt(2*pi)
    mu = m1 * (xi - 1/xi)
    sigma = sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
    z = x*sigma + mu  
    # Compute:
    Xi = xi^sign(z)
    g = 2 / (xi + 1/xi) 
    Density = g * dnorm(x = z/Xi)  
    # Return Value:
    Density * sigma 
}


# ------------------------------------------------------------------------------
        

dsnorm =
function(x, mean = 0, sd = 1, xi = 1.5)
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Compute the density function of the skew normal distribution
    
    # Arguments:
    #   x - a numeric vector of quantiles.
    #   mean, sd, xi - location parameter, scale parameter, and 
    #       skewness parameter.
    
    # FUNCTION:
    
    # Shift and Scale:
    result = .dsnorm(x = (x-mean)/sd, xi = xi) / sd
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


.psnorm =
function(q, xi) 
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Standardize:
      m1 = 2/sqrt(2*pi)
      mu = m1 * (xi - 1/xi)
      sigma = sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
      z = q*sigma + mu
    # Compute:  
      Xi = xi^sign(z)
      g = 2  / (xi + 1/xi)  
      Probability = H(z) - sign(z) * g * Xi * pnorm(q = -abs(z)/Xi)
    # Return Value:
      Probability 
}
    
      
# ------------------------------------------------------------------------------
  

psnorm =
function(q, mean = 0, sd = 1, xi = 1.5)
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Compute the distribution function of the 
    #   skew normal distribution
    
    # Arguments:
    #   q - a numeric vector of quantiles.
    #   mean, sd, xi - location parameter, scale parameter, and 
    #       skewness parameter.
    
    # FUNCTION:
    
    # Shift and Scale:
    result = .psnorm(q = (q-mean)/sd, xi = xi)
          
    # Return Value:
    result
}


# ------------------------------------------------------------------------------    


.qsnorm =
function(p, xi) 
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Standardize:
      m1 = 2/sqrt(2*pi)
      mu = m1 * (xi - 1/xi)
      sigma = sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
    # Compute:  
      g = 2  / (xi + 1/xi)
      sig = sign(p-1/2) 
      Xi = xi^sig         
      p = (H(p-1/2)-sig*p) / (g*Xi)
      Quantile = (-sig*qnorm(p = p, sd = Xi) - mu ) / sigma
    # Return Value:
      Quantile 
}
    
 
# ------------------------------------------------------------------------------

   
qsnorm =
function(p, mean = 0, sd = 1, xi = 1.5)
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Compute the quantile function of the 
    #   skew normal distribution
    
    # Arguments:
    #   p - a numeric vector of probabilities.
    #   mean, sd, xi - location parameter, scale parameter, and 
    #       skewness parameter.
    
    # FUNCTION:
    
    # Shift and Scale:
    result = .qsnorm(p = p, xi = xi) * sd + mean
    
    # Return Value:
    result
}

    
# ------------------------------------------------------------------------------


.rsnorm =
function(n, xi) 
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Generate Random Deviates:
      weight = xi / (xi + 1/xi)
      z = runif(n, -weight, 1-weight)
      Xi = xi^sign(z)
      Random = -abs(rnorm(n))/Xi * sign(z)  
    # Scale:
      m1 = 2/sqrt(2*pi)
      mu = m1 * (xi - 1/xi)
      sigma = sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
      Random = (Random - mu ) / sigma   
    # Return value:
      Random 
}
 

# ------------------------------------------------------------------------------
       

rsnorm =
function(n, mean = 0, sd = 1, xi = 1.5)
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Generate random deviates from the 
    #   skew normal distribution
    
    # Arguments:
    #   n - an integer value giving the number of observation.
    #   mean, sd, xi - location parameter, scale parameter, and 
    #       skewness parameter.
    
    # FUNCTION:
    
    # Shift and Scale:
    result = .rsnorm(n = n, xi = xi) * sd + mean
    
    # Return Value:
    result
}



################################################################################
# PART II:
# FUNCTION:             VARIANCE-1 STUDENT-T DISTRIBUTION:
#  dstd                   Density for the Student-t Distribution
#  pstd                   Probability function for the Student-t Distribution
#  qstd                   Quantile function for the Student-t Distribution
#  rstd                   Random Number Generator for the Student-t
# FUNCTION:             DESCRIPTION:
#  dsstd                  Density for the skewed Student-t Distribution
#  psstd                  Probability function for the skewed STD
#  qsstd                  Quantile function for the skewed STD
#  rsstd                  Random Number Generator for the skewed STD
################################################################################


dstd =
function(x, mean = 0, sd = 1, nu = 5)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute the density for the  
    #   Student-t distribution.
    
    # FUNCTION:
    
    # Compute Density:
    s = sqrt(nu/(nu-2))
    z = (x - mean) / sd
    # result = .Internal(dnt(x = z*s, df = nu, ncp = 0, log = FALSE)) / (sd/s)
    result = dt(x = z*s, df = nu) * s / sd
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


pstd =
function (q, mean = 0, sd = 1, nu = 5)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute the probability for the  
    #   Student-t distribution.
    
    # FUNCTION:
    
    # Compute Probability:
    s = sqrt(nu/(nu-2))
    z = (q - mean) / sd
    # result = .Internal(pnt(q = z*s, df = nu, ncp = 0, lower.tail = TRUE, 
    #   log.p = FALSE))
    result = pt(q = z*s, df = nu)
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


qstd =
function (p, mean = 0, sd = 1, nu = 5)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute the quantiles for the  
    #   Student-t distribution.
    
    # FUNCTION:
    
    # Compute Quantiles:
    s = sqrt(nu/(nu-2))
    # x = .Internal(qt(p = p, df = nu, lower.tail = TRUE, log.p = FALSE)) / s
    # result = x*sd + mean
    result = qt(p = p, df = nu) * sd / s + mean
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rstd =
function(n, mean = 0, sd = 1, nu = 5)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generate random deviates from the  
    #   Student-t distribution.
    
    # FUNCTION:
    
    # Generate Random Deviates:
    s = sqrt(nu/(nu-2))
    # result = .Internal(rt(n = n, df = nu)) * sd / s + mean
    result = rt(n = n, df = nu) * sd / s + mean 
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


.dsstd =
function(x, nu, xi) 
{	# A function implemented by Diethelm Wuertz 

	# Description:
	# 	Internal Function
	
	# FUNCTION:
	
	# For SPlus compatibility:
	if (!exists("beta"))
		beta <<- function (a, b) exp( lgamma(a) + lgamma(b) -lgamma(a+b) )
	
	# Standardize:	
	m1 = 2 * sqrt(nu-2) / (nu-1) / beta(1/2, nu/2)
	mu = m1*(xi-1/xi)
	sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
	z = x*sigma + mu  
	
	# Compute:
	Xi = xi^sign(z)
	g = 2 / (xi + 1/xi)	
	Density = g * dstd(x = z/Xi, nu = nu)  
	
	# Return Value:
	Density * sigma 
}


# ------------------------------------------------------------------------------

	  
dsstd =
function(x, mean = 0, sd = 1, nu = 5, xi = 1.5)
{	# A function implemented by Diethelm Wuertz 

	# Description:
	# 	Compute the density function of the 
	#	skewed Student-t distribution
	
	# FUNCTION:
	
    # Shift and Scale:
	result = .dsstd(x = (x-mean)/sd, nu = nu, xi = xi) / sd
	
	# Return Value:
	result
}


# ------------------------------------------------------------------------------


.psstd =
function(q, nu, xi) 
{	# A function implemented by Diethelm Wuertz 

	# Description:
	# 	Internal Function
	
	# FUNCTION:
	
	# For SPlus compatibility:
	if (!exists("beta"))
		beta <<- function (a, b) exp( lgamma(a) + lgamma(b) -lgamma(a+b) )
		
	# Standardize:
	m1 = 2 * sqrt(nu-2) / (nu-1) / beta(1/2, nu/2)
	mu = m1*(xi-1/xi)
	sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
	z = q*sigma + mu
	
	# Compute:	
	Xi = xi^sign(z)
	g = 2 / (xi + 1/xi)	
	Probability = H(z) - sign(z) * g * Xi * pstd(q = -abs(z)/Xi, nu = nu)
	
	# Return Value:
	Probability 
}


# ------------------------------------------------------------------------------

	  
psstd =
function(q, mean = 0, sd = 1, nu = 5, xi = 1.5)
{	# A function implemented by Diethelm Wuertz 

	# Description:
	# 	Compute the distribution function of the 
	#	skewed Student-t distribution
	
	# FUNCTION:
		  
	# Shift and Scale:
	result = .psstd(q = (q-mean)/sd, nu = nu, xi = xi)
		  
	# Return Value:
	result
}


# ------------------------------------------------------------------------------	


.qsstd =
function(p, nu, xi) 
{	# A function implemented by Diethelm Wuertz 

	# Description:
	# 	Internal Function
	
	# FUNCTION:
	
	# For SPlus compatibility:
	if (!exists("beta"))
		beta <<- function (a, b) exp( lgamma(a) + lgamma(b) -lgamma(a+b) )
		
	# Standardize:
	m1 = 2 * sqrt(nu-2) / (nu-1) / beta(1/2, nu/2)
	mu = m1*(xi-1/xi)
	sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
	
	# Compute:	
	g = 2  / (xi + 1/xi)
	sig = sign(p-1/2) 
	Xi = xi^sig		  
	p = (H(p-1/2)-sig*p) / (g*Xi)
	Quantile = (-sig*qstd(p = p, sd = Xi, nu = nu) - mu ) / sigma
	
	# Return Value:
	Quantile 
}


# ------------------------------------------------------------------------------

	
qsstd =
function(p, mean = 0, sd = 1, nu = 5, xi = 1.5)
{	# A function implemented by Diethelm Wuertz 

	# Description:
	# 	Compute the quantile function of the 
	#	skewed Student-t distribution
	
	# FUNCTION:
		
	# Shift and Scale:
	result = .qsstd(p = p, nu = nu, xi = xi) * sd + mean
	
	# Return Value:
	result
}

	
# ------------------------------------------------------------------------------


.rsstd =
function(n, nu, xi) 
{	# A function implemented by Diethelm Wuertz 

	# Description:
	# 	Internal Function
	
	# FUNCTION:
	
	# For SPlus compatibility:
	if (!exists("beta"))
		beta <<- function (a, b) exp( lgamma(a) + lgamma(b) -lgamma(a+b) )
		
	# Generate Random Deviates:
	weight = xi / (xi + 1/xi)
	z = runif(n, -weight, 1-weight)
	Xi = xi^sign(z)
	Random = -abs(rstd(n, nu = nu))/Xi * sign(z)	
	
	# Scale:
	m1 = 2 * sqrt(nu-2) / (nu-1) / beta(1/2, nu/2)
	mu = m1*(xi-1/xi)
	sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
	Random = (Random - mu ) / sigma	
	
	# Return value:
	Random 
}


# ------------------------------------------------------------------------------
	

rsstd =
function(n, mean = 0, sd = 1, nu = 5, xi = 1.5)
{	# A function implemented by Diethelm Wuertz 

	# Description:
	# 	Generate random deviates from the 
	#	skewed Student-t distribution
	
	# FUNCTION:
		
	# Shift and Scale:
	result = .rsstd(n = n, nu = nu, xi = xi) * sd + mean
	
	# Return Value:
	result
}


################################################################################
# PART III:
# FUNCTION:             GED Distribution:
#  dged                   Density for the Generalized Error Distribution
#  pged                   Probability function for the GED
#  qged                   Quantile function for the GED
#  rged                   Random Number Generator for the GED
# FUNCTION:             DESCRIPTION:
#  dsged                  Density for the skewed GED
#  psged                  Probability function for the skewed GED
#  qsged                  Quantile function for the skewed GED
#  rsged                  Random Number Generator for the skewed GED
################################################################################


dged =
function(x, mean = 0, sd = 1, nu = 2)
{   # A function imlemented by Diethelm Wuertz

    # Description:
    #   Compute the density for the 
    #   generalized error distribution.
    
    # FUNCTION:
    
    # Compute Density:
    z = (x - mean ) / sd
    lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
    g  = nu / ( lambda * (2^(1+1/nu)) * gamma(1/nu) )
    result = g * exp (-0.5*(abs(z/lambda))^nu) / sd
    
    # Return Value
    result
}

        
# ------------------------------------------------------------------------------


pged = 
function(q, mean = 0, sd = 1, nu = 2)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Compute the probability for the  
    #   generalized error distribution.
    
    # FUNCTION:
        
    # Compute Probability:
    q = (q - mean ) / sd
    lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
    g  = nu / ( lambda * (2^(1+1/nu)) * gamma(1/nu) )
    h = 2^(1/nu) * lambda * g * gamma(1/nu) / nu
    s = 0.5 * ( abs(q) / lambda )^nu
    result = 0.5 + sign(q) * h * pgamma(s, 1/nu)
    
    # Return Value:
    result
}
        

# ------------------------------------------------------------------------------


qged =
function(p, mean = 0, sd = 1, nu = 2)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Compute the quantiles for the  
    #   generalized error distribution.
    
    # FUNCTION:
    
    # Compute Quantiles:
    lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
    q = lambda * (2*qgamma((abs(2*p-1)), 1/nu))^(1/nu)
    result = q*sign(2*p-1) * sd + mean
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------

    
rged = 
function(n, mean = 0, sd = 1, nu = 2)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generate GED random deviates. The function uses the 
    #   method based on the transformation of a Gamma random 
    #   variable.
    
    # FUNCTION:
    
    # Generate Random Deviates:
    lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
    # print(lambda)
    r = rgamma(n, 1/nu)
    z =  lambda * (2*r)^(1/nu) * sign(runif(n)-1/2)
    result = z * sd + mean

    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


.dsged = 
function(x, nu, xi) 
{	# A function implemented by Diethelm Wuertz 

	# Description:
	# 	Internal Function
	
	# FUNCTION:
	
	# Standardize:
	lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
	g  = nu / ( lambda * (2^(1+1/nu)) * gamma(1/nu) )
	m1 = 2^(1/nu) * lambda * gamma(2/nu) / gamma(1/nu)
	mu = m1*(xi-1/xi)
	sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
	z = x*sigma + mu  
	
	# Compute:
	Xi = xi^sign(z)
	g = 2  / (xi + 1/xi)	
	Density = g * dged(x = z/Xi, nu=nu)  
	
	# Return Value:
	Density * sigma 
}

	  
dsged =
function(x, mean = 0, sd = 1, nu = 2, xi = 1.5)
{	# A function implemented by Diethelm Wuertz 

	# Description:
	# 	Compute the density function of the 
	#	skewed generalized error distribution
	
	# FUNCTION:
		
    # Shift and Scale:
	result = .dsged(x = (x-mean)/sd, nu = nu, xi = xi) / sd
	
	# Return Value:
	result
}


# ------------------------------------------------------------------------------


.psged =
 function(q, nu, xi) 
{	# A function implemented by Diethelm Wuertz 

	# Description:
	# 	Internal Function
	
	# FUNCTION:
	
	# Standardize:
	lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
	g  = nu / ( lambda * (2^(1+1/nu)) * gamma(1/nu) )
	m1 = 2^(1/nu) * lambda * gamma(2/nu) / gamma(1/nu)
	mu = m1*(xi-1/xi)
	sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
	z = q*sigma + mu
	
	# Compute:	
	Xi = xi^sign(z)
	g = 2  / (xi + 1/xi)	
	Probability = H(z) - sign(z) * g * Xi * pged(q = -abs(z)/Xi, nu=nu)
	
	# Return Value:
	Probability 
}

	  
psged =
function(q, mean = 0, sd = 1, nu = 2, xi = 1.5)
{	# A function implemented by Diethelm Wuertz 

	# Description:
	# 	Compute the distribution function of the 
	#	skewed generalized error distribution
	
	# FUNCTION:
			  
	# Shift and Scale:
	result = .psged(q = (q-mean)/sd, nu = nu, xi = xi)
		  
	# Return Value:
	result
}


# ------------------------------------------------------------------------------	


.qsged =
function(p, nu, xi) 
{	# A function implemented by Diethelm Wuertz 

	# Description:
	# 	Internal Function
	
	# FUNCTION:
	
	# Standardize:
	lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
	g  = nu / ( lambda * (2^(1+1/nu)) * gamma(1/nu) )
	m1 = 2^(1/nu) * lambda * gamma(2/nu) / gamma(1/nu)
	mu = m1*(xi-1/xi)
	sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
	
	# Compute:	
	g = 2  / (xi + 1/xi)
	sig = sign(p-1/2) 
	Xi = xi^sig		  
	p = (H(p-1/2)-sig*p) / (g*Xi)
	Quantile = (-sig*qged(p=p, sd=Xi, nu=nu) - mu ) / sigma
	
	# Return Value:
	Quantile 
}

	  	
qsged =
function(p, mean = 0, sd = 1, nu = 2, xi = 1.5)
{	# A function implemented by Diethelm Wuertz 

	# Description:
	# 	Compute the quantile function of the 
	#	skewed generalized error distribution
	
	# FUNCTION:
		
	# Shift and Scale:
	result = .qsged(p = p, nu = nu, xi = xi) * sd + mean
	
	# Return Value:
	result
}

	
# ------------------------------------------------------------------------------
	

.rsged =
function(n, nu, xi) 
{	# A function implemented by Diethelm Wuertz 

	# Description:
	# 	Internal Function
	
	# FUNCTION:
	
	# Generate Random Deviates:
	weight = xi / (xi + 1/xi)
	z = runif(n, -weight, 1-weight)
	Xi = xi^sign(z)
	Random = -abs(rged(n, nu=nu))/Xi * sign(z)	
	
	# Scale:
	lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
	g  = nu / ( lambda * (2^(1+1/nu)) * gamma(1/nu) )
	m1 = 2^(1/nu) * lambda * gamma(2/nu) / gamma(1/nu)
	mu = m1*(xi-1/xi)
	sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
	Random = (Random - mu ) / sigma	
	
	# Return value:
	Random 
}


# ------------------------------------------------------------------------------


rsged =
function(n, mean = 0, sd = 1, nu = 2, xi = 1.5)
{	# A function implemented by Diethelm Wuertz 

	# Description:
	# 	Generate random deviates from the 
	#	skewed generalized error distribution
	
	# FUNCTION:
		
	# Shift and Scale:
	result = .rsged(n = n, nu = nu, xi = xi) * sd + mean
	
	# Return Value:
	result
}


################################################################################
# PART IV:
# FUNCTION:              PARAMETER ESTIMATION:
#  normFit            	  Fit the parameters for a Normal distribution
#  snormFit           	  Fit the parameters for a skew Normal distribution
#  gedFit             	  Fit the parameters for a GED distribution
#  sgedFit            	  Fit the parameters for a skew GED distribution
#  stdFit             	  Fit the parameters for a Sudent-t distribution
#  sstdFit            	  Fit the parameters for a skew Sudent-t distribution
################################################################################


normFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

	# Description:
	#	Fit the parameters for a Normal distribution
	
	# FUNCTION:
    
    # For S-Plus compatibility:
	if (!exists("nlm")) 
		nlm = function (f, p, ...) nlminb(start = p, objective = f, ...) 
		
	# Start Value:
    p = c(mean = mean(x), sd = sqrt(var(x)))

    # Log-likelihood Function:
    loglik = function(x, y = x){ 
        f = -sum(log(dnorm(y, x[1], x[2])))
        f }
        
    # Minimization:
    fit = nlm(f = loglik, p = p, y = x, ...)
    
    # Return Value:
    fit
}   


# ------------------------------------------------------------------------------


snormFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
	#	Fit the parameters for a skew Normal distribution
	
	# FUNCTION:
	
	# For S-Plus compatibility:
	if (!exists("nlm")) 
		nlm = function (f, p, ...) nlminb(start = p, objective = f, ...) 
		
	# Start Value:
    p = c(mean = mean(x), sd = sqrt(var(x)), xi = 1)

    # Log-likelihood Function:
    loglik = function(x, y = x){ 
        f = -sum(log(dsnorm(y, x[1], x[2], x[3])))
        f }
        
    # Minimization:
    fit = nlm(f = loglik, p = p, y = x, ...)
    
    # Return Value:
    fit
}   


# ------------------------------------------------------------------------------


gedFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #	Fit the parameters for a GED distribution
	
	# FUNCTION:
	
	# For S-Plus compatibility:
	if (!exists("nlm")) 
		nlm = function (f, p, ...) nlminb(start = p, objective = f, ...) 
		
	# Start Value:
    p = c(mean = mean(x), sd = sqrt(var(x)), nu = 2)

    # Log-likelihood Function:
    loglik = function(x, y = x){ 
        f = -sum(log(dged(y, x[1], x[2], x[3])))
        f }
        
    # Minimization:
    fit = nlm(f = loglik, p = p, y = x, ...)
    
    # Return Value:
    fit
}   


# ------------------------------------------------------------------------------


sgedFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #	Fit the parameters for a skew GED distribution
	
	# FUNCTION:
	
	# For S-Plus compatibility:
	if (!exists("nlm")) 
		nlm = function (f, p, ...) nlminb(start = p, objective = f, ...) 
		
	# Start Value:
    p = c(mean = mean(x), sd = sqrt(var(x)), nu = 2, xi = 1)

    # Log-likelihood Function:
    loglik = function(x, y = x){ 
        f = -sum(log(dsged(y, x[1], x[2], x[3], x[4])))
        f }
        
    # Minimization:
    fit = nlm(f = loglik, p = p, y = x, ...)
    
    # Return Value:
    fit
}   


# ------------------------------------------------------------------------------


stdFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #	Fit the parameters for a Sudent-t distribution
    #	with unit variance
	
	# FUNCTION:
	
	# For S-Plus compatibility:
	if (!exists("nlm")) 
		nlm = function (f, p, ...) nlminb(start = p, objective = f, ...) 
		
	# Start Value:
    p = c(mean = mean(x), sd = sqrt(var(x)), nu = 4)

    # Log-likelihood Function:
    loglik = function(x, y = x){ 
        f = -sum(log(dstd(y, x[1], x[2], x[3])))
        f }
        
    # Minimization:
    fit = nlm(f = loglik, p = p, y = x, ...)
    
    # Return Value:
    fit
}   


# ------------------------------------------------------------------------------


sstdFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #	Fit the parameters for a skew Sudent-t distribution
    #	with unit variance
	
	# FUNCTION:
	
	# For S-Plus compatibility:
	if (!exists("nlm")) 
		nlm = function (f, p, ...) nlminb(start = p, objective = f, ...) 
	
	# Start Value:
    p = c(mean = mean(x), sd = sqrt(var(x)), nu = 4, xi = 1)

    # Log-likelihood Function:
    loglik = function(x, y = x){ 
        f = -sum(log(dsstd(y, x[1], x[2], x[3], x[4])))
        f }
        
    # Minimization:
    fit = nlm(f = loglik, p = p, y = x, ...)
    
    # Return Value:
    fit
}   


################################################################################

