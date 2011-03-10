
## This R package is free software; you can redistribute it and/or
## modify it under the terms of the GNU Library General Public
## License as published by the Free Software Foundation; either
## version 2 of the License, or (at your option) any later version.
##
## This R package is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU Library General Public License for more details.
##
## You should have received a copy of the GNU Library General
## Public License along with this library; if not, write to the
## Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
## MA  02111-1307  USA


################################################################################
## FUNCTIONS:		 DESCRIPTION:
##  dstable		  Returns density for stable DF
##  pstable		  Returns probabilities for stable DF
##  qstable		  Returns quantiles for stable DF
##  rstable		  Returns random variates for stable DF
## UTILITY FUNCTION	 DESCRIPTION:
##  .integrate2  	  Integrates internal functions for *stable
################################################################################


##==============================================================================

### MM TODO:

## 0) All d, p, q, q -- have identical parts
##  a) 'parametrization' (pm) check
##  b) checking, alpha, beta,
##  c) subdivisions etc {all but rstable}
## ---	to do: "Fix" these in dstable(), then copy/paste to others

##==============================================================================

##' @title omega() according to Lambert & Lindsey (1999), p.412
##' @param gamma
##' @param alpha
##' @return
.om <- function(gamma,alpha)
    if(alpha == 1) (2/pi)*log(gamma) else tan(pi*alpha/2)

dstable <- function(x, alpha, beta,
		    gamma = 1, delta = 0, pm = 0, log = FALSE,
		    tol = 16*.Machine$double.eps, subdivisions = 1000)
{
    ## Original implemented by Diethelm Wuertz;
    ## Changes for efficiency and accuracy by Martin Maechler

    ## Description:
    ##	 Returns density for stable DF

    ## Details:
    ##	 The function uses the approach of J.P. Nolan for general
    ##	 stable distributions. Nolan derived expressions in form
    ##	 of integrals based on the charcteristic function for
    ##	 standardized stable random variables. These integrals
    ##	 can be numerically evaluated.

    ## Arguments:
    ##	 alpha = index of stability, in the range (0,2]
    ##	 beta  = skewness, in the range [-1, 1]
    ##	 gamma = scale, in the range (0, infinity)
    ##	 delta = location, in the range (-infinity, +infinity)
    ##	 param = type of parmeterization

    ## Notes:
    ##	 For R and SPlus compatibility use integrate()[[1]] instead of
    ##	     integrate()$value and integrate()$integral.
    ##	 optimize() works in both R and SPlus.

    ## Parameter Check:
    stopifnot( 0 < alpha, alpha <= 2, length(alpha) == 1,
	      -1 <= beta, beta	<= 1, length(beta) == 1,
	      length(pm) == 1, pm %in% 0:2,
              tol > 0, subdivisions > 0)

    ## Parameterizations:
    if (pm == 1) {
	delta <- delta + beta*gamma * .om(gamma,alpha)
    } else if (pm == 2) {
	delta <- delta - alpha^(-1/alpha)*gamma*stableMode(alpha, beta)
	gamma <- alpha^(-1/alpha) * gamma
    } ## else pm == 0

    ## Shift and Scale:
    x <- (x - delta) / gamma
    ans <-
	## Special Cases:
	if (alpha == 2) {
	    dnorm(x, mean = 0, sd = sqrt(2))
	} else if (alpha == 1 && beta == 0) {
	    dcauchy(x)
	} else {

	    ## General Case
	    if (alpha != 1) { ## 0 < alpha < 2	&  |beta| <= 1	from above
		tanpa2 <- tan(pi*alpha/2)
		zeta <- -beta * tanpa2
		theta0 <- atan(beta * tanpa2) / alpha

		## Loop over all x values:
		unlist(lapply(x, function(z) {
		    z.m.varz <- abs(z - zeta)
		    ## Modified D.W. and M.M. {was  if (z == zeta)}
		    if (z.m.varz <= 1e-5 * abs(z)) {
			gamma(1+1/alpha)*cos(theta0) / (pi*(1+zeta^2)^(1/(2*alpha)))
		    } else {
			## (z < zeta) <==> (-z > -zeta) <==> -z-(-zeta) > 0
			.fct1(z.m.varz, alpha=alpha, theta0 = sign(z - zeta)* theta0,
			      tol=tol, subdivisions=subdivisions)
		    }
		}))
	    }
	    ## Special Case alpha == 1	and  -1 <= beta <= 1 (but not = 0) :
	    else { ## (alpha == 1)  and	 0 < |beta| <= 1  from above
		## Loop over all x values:
		unlist(lapply(x, function(z) {
		    if (z >= 0) {
			.fct2( z , beta, tol=tol, subdivisions=subdivisions)
		    } else {
			.fct2(-z, -beta, tol=tol, subdivisions=subdivisions)
		    }
		}))
	    }
	}

    ans <- ans/gamma
    ## Return:
    if (log) log(ans) else ans
}

## ------------------------------------------------------------------------------

.fct1 <- function(x.m.zeta, alpha, theta0, tol, subdivisions,
                  verbose = getOption("dstable.debug", default=FALSE))
{
    ## -- Integrand for dstable() --

    ## constants (independent of integrand (th)):
    ## zeta <- -beta * tan(pi*alpha/2)
    ## theta0 <- (1/alpha) * atan( beta * tan(pi*alpha/2))
    ## x.m.zeta <- xarg - zeta
    a_1 <- alpha - 1
    ## aa1 <- alpha/a_1
    cat0 <- cos(at0 <- alpha*theta0)
    ## g0 <- x.m.zeta^aa1

    ## g <- (cat0 * cos(th))^(1/a_1) * sin(at0+alpha*th)^-aa1 * cos(at0+ a_1*th)
    ## much better when alpha ~= 1 :
    ## g <- g0 * g
    g <- function(th) (cat0 * cos(th) * (x.m.zeta/sin(at0+alpha*th))^alpha)^(1/a_1)  * cos(at0+ a_1*th)

    ## Function to Integrate:
    g1 <- function(th) ## and (xarg, alpha, beta) => (zeta,at0,g0,.....)
    {
        ## g1 = g(.) exp(-g(.))
        v <- g(th)
        ## making sure that we don't get   Inf * 0  =>  NaN
	r <- v * exp(-v)
	if(any(L <- v > 1000)) ## actually, v > -(.Machine$double.min.exp * log(2)) == 708.396...
	    r[L] <- 0
	r
    }

    c2 <- ( alpha / (pi*abs(a_1)*x.m.zeta) )
    ## Result = c2 * \int_{-t0}^{pi/2} g1(u) du
    ## where however, g1(.) may look to be (almost) zero almost everywhere and just have a small peak
    ## ==> Split the integral into two parts of two intervals  (t0, t_max) + (t_max, pi/2)

    ##  However, this may still be bad, e.g., for dstable(71.61531, alpha=1.001, beta=0.6),
    ##  or  dstable(1.205, 0.75, -0.5)
    ##   the 2nd integral is "completely wrong" (basically zero, instead of ..e-5)
    ## FIXME --- Lindsey uses "Romberg" integration -- maybe we must split even more

    ## We know that the maximum of g1(.) is = exp(-1) = 0.3679  "at" g(.) == 1
    ## find that by uniroot :
    ur <- uniroot(function(th) g(th) - 1, lower = -theta0, upper = pi/2, tol = tol)
    theta2 <- ur$root

    ## now, because the peak may be extreme, we find two more intermediate values
    ## NB: Max = 0.3679 ==> 1e-4 is a very small fraction of that
    ## to the left:
    th1 <- uniroot(function(th) g1(th) - 1e-4, lower = -theta0, upper = theta2,
		   tol = tol)$root
    if((do4 <- g1(pi/2) < 1e-4))
        ## to the right:
        th3 <- uniroot(function(th) g1(th) - 1e-4, lower = theta2, upper = pi/2,
                       tol = tol)$root

    Int <- function(a,b)
        .integrate2(g1, lower = a, upper = b,
                    subdivisions=subdivisions, rel.tol= tol, abs.tol= tol)

    r1 <- Int(-theta0, th1)
    r2 <- Int(         th1, theta2)
    if(do4) {
        r3 <- Int(           theta2, th3)
        r4 <- Int(                   th3, pi/2)
    } else {
        r3 <- Int(           theta2, pi/2)
        r4 <- 0
    }
    if(verbose)
        cat(sprintf(".fct1(%12.9g,..): c2*(r1+r2+r3+r4) = %12g*(%12g + %12g+ %12g + %12g) = %g\n",
                    x.m.zeta, c2, r1,r2,r3,r4, c2*(r1+r2+r3+r4)))
    c2*(r1+r2+r3+r4)
}


## ------------------------------------------------------------------------------

##' is only used when alpha == 1 (!)
.fct2 <- function(xarg, beta, tol, subdivisions)
{
    ## Integration:

    i2b <- 1/(2*beta)
    p2b <- pi*i2b # = pi/(2 beta)
    pi2 <- pi/2
    g. <- exp(-p2b*xarg)

    ## Function to Integrate; x is a non-sorted vector!
    g2 <- function(x) { ## and (xarg, beta)
	g <- p2b+ x # == g'/beta where g' := pi/2 + beta*x
	v <- g / (p2b*cos(x)) * exp(g*tan(x))
	g <- g. * v
	gval <- g * exp(-g)
	if(any(ina <- is.na(gval))) gval[ina] <- 0 ## replace NA at pi/2
	gval
    }

    theta2 <- optimize(g2, lower = -pi2, upper = pi2,
		       maximum = TRUE, tol = tol)$maximum
    r1 <- .integrate2(g2, lower = -pi2, upper = theta2,
		     subdivisions = subdivisions,
		     rel.tol = tol, abs.tol = tol)
    r2 <- .integrate2(g2, lower = theta2, upper = pi2,
		     subdivisions = subdivisions,
		     rel.tol = tol, abs.tol = tol)
    abs(i2b)*(r1 + r2)
}

### ------------------------------------------------------------------------------


pstable <- function(q, alpha, beta, gamma = 1, delta = 0, pm = 0,
		    tol = 16*.Machine$double.eps, subdivisions = 1000)
{
    ## A function implemented by Diethelm Wuertz

    ## Description:
    ##	 Returns probability for stable DF

    x <- q
    ## Parameter Check:
    stopifnot( 0 < alpha, alpha <= 2, length(alpha) == 1,
	      -1 <= beta, beta	<= 1, length(beta) == 1,
	      length(pm) == 1, pm %in% 0:2,
              tol > 0, subdivisions > 0)

    ## Parameterizations:
    if (pm == 1) {
	delta <- delta + beta*gamma * .om(gamma,alpha)
    } else if (pm == 2) {
	delta <- delta - alpha^(-1/alpha)*gamma*stableMode(alpha, beta)
	gamma <- alpha^(-1/alpha) * gamma
    } ## else pm == 0

    ## Shift and Scale:
    x <- (x - delta) / gamma

    ## Return directly
    ## ------  first, special cases:
    if (alpha == 2) {
	pnorm(x, mean = 0, sd = sqrt(2))
    } else if (alpha == 1 && beta == 0) {
	pcauchy(x)
    } else {

	## General Case
	if (alpha != 1) { ## 0 < alpha < 2	&  |beta| <= 1	from above
	    tanpa2 <- tan(pi*alpha/2)
	    zeta <- -beta * tanpa2
	    theta0 <- atan(beta * tanpa2) / alpha

	    ## Loop over all x values:
	    unlist(lapply(x, function(z) {
		z.m.varz <- abs(z - zeta)

		if (z.m.varz < 2 * .Machine$double.eps)
		    ## FIXME? same problem as dstable
		    (1/2- theta0/pi)
		else if (z > zeta)
		    .FCT1(z.m.varz, alpha=alpha, theta0= theta0,
			  tol = tol, subdivisions = subdivisions)
		else ## (z < zeta)
		    1 - .FCT1(z.m.varz, alpha=alpha, theta0= -theta0,
			      tol = tol, subdivisions = subdivisions)
	    }))
	}
	## Special Case alpha == 1	and  -1 <= beta <= 1 (but not = 0) :
	else { ## (alpha == 1)	and	 0 < |beta| <= 1  from above
	    ## Loop over all x values:
	    unlist(lapply(x, function(z) {
		if (beta >= 0) {
		    .FCT2(xarg = z, beta = beta,
			  tol = tol, subdivisions = subdivisions)
		} else {
		    1 - .FCT2(xarg = -z, beta = -beta,
			      tol = tol, subdivisions = subdivisions)
		}
	    }))
	}
    }
}

## ------------------------------------------------------------------------------

.FCT1 <- function(x.m.zeta, alpha, theta0, tol, subdivisions)
{
    a_1 <- alpha - 1
    aa1 <- alpha/a_1
    cat0 <- cos(at0 <- alpha*theta0)
    g0 <- x.m.zeta^aa1

    ## Function to integrate:
    G1 <- function(x) { ## (xarg, alpha, beta)
	v <- (cat0*cos(x))^(1/a_1) * sin(at0+ alpha*x)^-aa1 * cos(at0+a_1*x)
	exp(-(g0 * v))
    }

    theta2 <- optimize(G1, lower = -theta0, upper = pi/2,
		       maximum = TRUE, tol = tol)$maximum
    c1 <- if(alpha < 1) 1/2 - theta0/pi else 1
    c3 <- sign(1-alpha)/pi
    r1 <- .integrate2(G1, lower = -theta0,
		      upper = theta2, subdivisions = subdivisions,
		      rel.tol = tol, abs.tol = tol)
    r2 <- .integrate2(G1, lower = theta2,
		      upper = pi/2, subdivisions = subdivisions,
		      rel.tol = tol, abs.tol = tol)
    c1 + c3*(r1+r2)
}

## ------------------------------------------------------------------------------

.FCT2 <- function(xarg, beta, tol, subdivisions)
{
    i2b <- 1/(2*beta)
    p2b <- pi*i2b # = pi/(2 beta)
    pi2 <- pi/2
    g. <- exp(-p2b*xarg)

    ## Function to Integrate; x is a non-sorted vector!
    G2 <- function(x) { ## and (xarg, beta)
	g <- p2b+ x # == g'/beta where g' := pi/2 + beta*x
	v <- g / (p2b*cos(x)) * exp(g*tan(x))
	g <- g. * v
	gval <- exp(-g)
	if(any(ina <- is.na(gval))) gval[ina] <- 0 ## replace NA at pi/2
	gval
    }

    ## Integration:
    theta2 <- optimize(G2, lower = -pi2, upper = pi2,
		       maximum = TRUE, tol = tol)$maximum
    r1 <- .integrate2(G2, lower = -pi2,
		     upper = theta2, subdivisions = subdivisions,
		     rel.tol = tol, abs.tol = tol)
    r2 <- .integrate2(G2, lower = theta2,
		     upper = pi2, subdivisions = subdivisions,
		     rel.tol = tol, abs.tol = tol)
    (r1+r2)/pi
}

### ------------------------------------------------------------------------------


qstable <- function(p, alpha, beta, gamma = 1, delta = 0, pm = 0,
                    tol = .Machine$double.eps^0.25, maxiter = 1000,
                    integ.tol = 1e-7, subdivisions = 200)
{
    ## A function implemented by Diethelm Wuertz

    ## Description:
    ##	 Returns quantiles for stable DF

    ## Parameter Check:
    stopifnot( 0 < alpha, alpha <= 2, length(alpha) == 1,
	      -1 <= beta, beta	<= 1, length(beta) == 1,
	      length(pm) == 1, pm %in% 0:2,
              tol > 0, subdivisions > 0)

    ## Parameterizations:
    if (pm == 1) {
	delta <- delta + beta*gamma * .om(gamma,alpha)
    } else if (pm == 2) {
	delta <- delta - alpha^(-1/alpha)*gamma*stableMode(alpha, beta)
	gamma <- alpha^(-1/alpha) * gamma
    } ## else pm == 0

    result <-
	## Special Cases:
	if (alpha == 2)
	    qnorm(p, mean = 0, sd = sqrt(2))
	else if (alpha == 1 && beta == 0)
	    qcauchy(p)
	else if (abs(alpha-1) < 1) { ## -------------- 0 < alpha < 2 ---------------
            .froot <- function(x, p) {
                pstable(q = x, alpha=alpha, beta=beta, pm = 0,
                        tol=integ.tol, subdivisions=subdivisions) - p
            }
            ## Calculate:
            unlist(lapply(p, function(pp) {
                if (beta < 0) {
                    xmin <- -(1-pp)/pp
                    ## xmax = pp/(1-pp)
                    if (pp < 0.5) {
                        xmax <- qnorm(pp, mean = 0, sd = sqrt(2))
                    } else {
                        xmax <- qcauchy(pp)
                    }
                }
                if (beta > 0 ) {
                    ## xmin = -(1-pp)/pp
                    if (pp < 0.5) {
                        xmin <- qcauchy(pp)
                    } else {
                        xmin <- qnorm(pp, mean = 0, sd = sqrt(2))
                    }
                    xmax <- pp/(1-pp)
                }
                if (beta == 0 ) {
                    ## xmin = -(1-pp)/pp
                    if (pp < 0.5) {
                        xmin <- qcauchy(pp)
                    } else {
                        xmin <- qnorm(pp, mean = 0, sd = sqrt(2))
                    }
                    ## xmax = pp/(1-pp)
                    if (pp < 0.5) {
                        xmax <- qnorm(pp, mean = 0, sd = sqrt(2))
                    } else {
                        xmax <- qcauchy(pp)
                    }
                }
                root <- NA
                counter <- 0
                while (is.na(root)) {
                    root <- .unirootNA(.froot, interval = c(xmin, xmax), p = pp,
                                       tol=tol, maxiter=maxiter)
                    counter <- counter + 1
                    xmin <- xmin-2^counter
                    xmax <- xmax+2^counter
                }
                root
            }))
        }

    ## Result:
    result * gamma + delta
}

## ------------------------------------------------------------------------------


rstable <- function(n, alpha, beta, gamma = 1, delta = 0, pm = 0)
{
    ## Description:
    ##	 Returns random variates for stable DF

    ## slightly amended along  nacopula::rstable1

    ## Parameter Check:
    stopifnot( 0 < alpha, alpha <= 2, length(alpha) == 1,
	      -1 <= beta, beta	<= 1, length(beta) == 1,
	      length(pm) == 1, pm %in% 0:2)

    ## Parameterizations:
    if (pm == 1) {
	delta <- delta + beta*gamma * .om(gamma,alpha)
    } else if (pm == 2) {
	delta <- delta - alpha^(-1/alpha)*gamma*stableMode(alpha, beta)
	gamma <- alpha^(-1/alpha) * gamma
    } ## else pm == 0

    ## Calculate uniform and exponential distributed random numbers:
    theta <- pi * (runif(n)-1/2)
    w <- -log(runif(n))

    result <-
        ## If alpha is equal 1 then:
        if (alpha == 1 & beta == 0) {
            rcauchy(n)
            ## Otherwise, if alpha is different from 1:
        } else {
            ## FIXME: learn from nacopula::rstable1R()
	    b.tan.pa <- beta*tan(pi*alpha/2)
	    theta0 <- atan(b.tan.pa) / alpha ## == \theta_0
            c <- (1+b.tan.pa^2)^(1/(2*alpha))
	    a.tht <- alpha*(theta+theta0)
            r <- ( c*sin(a.tht)/
                  (cos(theta))^(1/alpha) ) *
                      (cos(theta-a.tht)/w)^((1-alpha)/alpha)
            ## Use Parametrization 0:
            r - b.tan.pa
        }

    ## Result:
    result * gamma + delta
}


## ------------------------------------------------------------------------------


##' Numerically Integrate -- basically the same as R's	integrate()
##' --------------------- main difference: no errors, but warnings
.integrate2 <- function(f, lower, upper, subdivisions, rel.tol, abs.tol, ...)
{
    ## Originally implemented by Diethelm Wuertz -- without *any* warnings

    if (class(version) != "Sversion") {
	## R:
	f <- match.fun(f)
	ff <- function(x) f(x, ...)
	wk <- .External("call_dqags", ff,
			rho = environment(), as.double(lower),
			as.double(upper), as.double(abs.tol),
			as.double(rel.tol), limit = as.integer(subdivisions),
			PACKAGE = "base")[c("value","ierr")]
	iErr <- wk[["ierr"]]
	if(iErr == 6) stop("the input is invalid")
	if(iErr > 0)
            ## NB:  "roundoff error ..." happens many times
	    warning(switch(iErr + 1, "OK",
			   "maximum number of subdivisions reached",
			   "roundoff error was detected",
			   "extremely bad integrand behaviour",
			   "roundoff error is detected in the extrapolation table",
			   "the integral is probably divergent"))
	wk[[1]]

    } else {
	## SPlus:
	integrate(f, lower, upper, subdivisions, rel.tol, abs.tol, ...)[[1]]
    }
}


################################################################################

