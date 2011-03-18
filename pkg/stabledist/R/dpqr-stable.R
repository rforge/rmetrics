## Part of R package 'stabledist' (part of the Rmetrics project).

## The stabledist R package is free software; you can redistribute it and/or
## modify it under the terms of the GNU Library General Public
## License as published by the Free Software Foundation; either
## version 2 of the License, or (at your option) any later version.
##
## This R package is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU Library General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  http://www.r-project.org/Licenses/


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

    ## Note: S+ compatibility no longer considered (explicitly)

    ## Parameter Check:
    ## NB: (gamma, delta) can be *vector*s (vectorized along x)
    stopifnot( 0 < alpha, alpha <= 2, length(alpha) == 1,
	      -1 <= beta, beta	<= 1, length(beta) == 1,
	      0 <= gamma, length(pm) == 1, pm %in% 0:2,
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
		unlist(lapply(x, function(z)
                              .fct1(z, zeta, alpha=alpha, theta0 = theta0,
                                    tol=tol, subdivisions=subdivisions)))
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

.large.exp.arg <- -(.Machine$double.min.exp * log(2)) ## == 708.396...
##' @title  x*exp(-x)  numerically stably, with correct limit 0 for x --> Inf
##' @param x  numeric
##' @return x*exp(x)
##' @author Martin Maechler
x.exp.m.x <- function(x) {
    r <- x*exp(-x)
    if(any(lrg <- x > .large.exp.arg))
	r[lrg] <- 0
    if(any(nax <- is.na(x)))
	r[nax] <- NA_real_
    r
}

.fct1 <- function(x, zeta, alpha, theta0, tol, subdivisions,
                  verbose = getOption("dstable.debug", default=FALSE))
{
    ## -- Integrand for dstable() --

    x.m.zet <- abs(x - zeta)
    f.zeta <- function()
        gamma(1+1/alpha)*cos(theta0) / (pi*(1+zeta^2)^(1/(2*alpha)))

    ## Modified: originally was  if (z == zeta),
    ## then (D.W.)   if (x.m.zet < 2 * .Machine$double.eps)
    ## then (M.M.)   if (x.m.zet <= 1e-5 * abs(x))
    if(x.m.zet < 1e-10)
        return(f.zeta())
    ## the real check should be about the feasibility of g() below, or its integration

    if(x < zeta) theta0 <- -theta0

    ## constants ( independent of integrand g1(th) = g*exp(-g) ):
    ## zeta <- -beta * tan(pi*alpha/2)
    ## theta0 <- (1/alpha) * atan( beta * tan(pi*alpha/2))
    ## x.m.zet <- abs(x - zeta)
    a_1 <- alpha - 1
    cat0 <- cos(at0 <- alpha*theta0)

    g <- function(th) (cat0 * cos(th) * (x.m.zet/sin(at0+alpha*th))^alpha)^(1/a_1)  * cos(at0+ a_1*th)
    ## Function to Integrate:
    g1 <- function(th) {
	## g1 :=  g(.) exp(-g(.))
	x.exp.m.x( g(th) )
    }

    c2 <- ( alpha / (pi*abs(a_1)*x.m.zet) )
    ## Result = c2 * \int_{-t0}^{pi/2} g1(u) du
    ## where however, g1(.) may look to be (almost) zero almost everywhere and just have a small peak
    ## ==> Split the integral into two parts of two intervals  (t0, t_max) + (t_max, pi/2)

    ##  However, this may still be bad, e.g., for dstable(71.61531, alpha=1.001, beta=0.6),
    ##  or  dstable(1.205, 0.75, -0.5)
    ##   the 2nd integral is "completely wrong" (basically zero, instead of ..e-5)
    ## FIXME --- Lindsey uses "Romberg" integration -- maybe we must split even more

    ## NB: g() is monotone, typically from 0 to +Inf
    ##     alpha >= 1  <==>  g() is falling, ie. from Inf --> 0;  otherwise growing from 0 to +Inf
    th0.. <- function(th0, eps) -th0+ eps* abs(th0)
    pi2.. <- function(eps) pi/2 * (1 - eps)

    g. <- if(alpha >= 1) g(th0..(theta0, 1e-6)) else g(pi2..(1e-6))
    if(is.na(g.) || identical(g., 0))# g() is not usable: it's 0 *very close* besides +Inf
	return(f.zeta())

    if((alpha >= 1 && g( pi/2  ) > .large.exp.arg) ||
       (alpha  < 1 && g(-theta0) > .large.exp.arg))
	## g() is numerically too large -->  g() * exp(-g()) is 0 everywhere
	return(0)

    if(verbose)
	cat(sprintf(".fct1(%11g,%10g,..): c2*sum(r[1..4])= %11g*", x,zeta, c2))

    Int <- function(a,b)
	.integrate2(g1, lower = a, upper = b,
                    subdivisions=subdivisions, rel.tol= tol, abs.tol= tol)

    ## We know that the maximum of g1(.) is = exp(-1) = 0.3679  "at" g(.) == 1
    ## find that by uniroot :
    ## g(.) == 1  <==>  log(g(.)) == 0   --- the latter is better conditioned,
    ##                                       e.g., for (x = -1, alpha = 0.95, beta = 0.6)
    ## the former is better for  dstable(-122717558, alpha = 1.8, beta = 0.3, pm = 1)
    ## However, it can be that the maximum is at the boundary,  and
    ## g(.) > 1 everywhere or  g(.) < 1  everywhere  {in that case we could revert to optimize..}
    if((alpha >= 1 && g(pi/2) > 1) ||
       (alpha <  1 && g(pi/2) < 1))
        g1.th2 <- g1( theta2 <- pi2..(1e-6) )
    else if((alpha <  1 && g(-theta0) > 1) ||
            (alpha >= 1 && g(-theta0) < 1))
        g1.th2 <- g1( theta2 <- th0..(theta0, 1e-6) )
    else {
        ur1 <- uniroot(function(th) g(th) - 1,
                       lower = -theta0, upper = pi/2, tol = .Machine$double.eps)
        ur2 <- uniroot(function(th) log(g(th)),
                       lower = -theta0, upper = pi/2, tol = .Machine$double.eps)
	g.1 <- x.exp.m.x(ur1$f.root+1)
	g.2 <- x.exp.m.x(exp(ur2$f.root))
        if(g.1 >= g.2) {
            theta2 <- ur1$root
            g1.th2 <- g.1 ## == g1(theta2)
        } else {
            theta2 <- ur2$root
            g1.th2 <- g.2
        }
    }
    ## now, because g1()'s peak (at th = theta2) may be extreme, we find two more intermediate values
    ## NB: Theoretically: Max = 0.3679 = g1(theta2)  ==> 1e-4 is a very small fraction of that
    ## to the left:
    eps <- 1e-4
    if((do1 <- g1.th2 > eps && g1(-theta0) < eps))
	th1 <- uniroot(function(th) g1(th) - eps, lower = -theta0, upper = theta2,
		       tol = tol)$root
    if((do4 <- g1.th2 > eps && g1(pi/2) < eps))
	## to the right:
	th3 <- uniroot(function(th) g1(th) - eps, lower = theta2, upper = pi/2,
		       tol = tol)$root

    if(do1) {
        r1 <- Int(-theta0, th1)
        r2 <- Int(         th1, theta2)
    } else {
        r1 <- 0
        r2 <- Int(-theta0,      theta2)
    }
    if(do4) {
        r3 <- Int(              theta2, th3)
        r4 <- Int(                      th3, pi/2)
    } else {
        r3 <- Int(              theta2,      pi/2)
        r4 <- 0
    }
    if(verbose)
	cat(sprintf("(%9.4g + %9.4g + %9.4g + %9.4g)= %g\n",
		    r1,r2,r3,r4, c2*(r1+r2+r3+r4)))
    c2*(r1+r2+r3+r4)
}


## ------------------------------------------------------------------------------

##' is only used when alpha == 1 (!)
.fct2 <- function(x, beta, tol, subdivisions)
{
    ## Integration:

    i2b <- 1/(2*beta)
    p2b <- pi*i2b # = pi/(2 beta)
    g. <- exp(-p2b*x)
    if(g. == 0 || g. == Inf) return(0)

    g <- function(th) { ## and (p2b, g.) {from (x, beta)}
	g <- p2b+ th # == g'/beta where g' := pi/2 + beta*th
	g. * g / (p2b*cos(th)) * exp(g*tan(th))
    }
    ## Function to Integrate; th is a non-sorted vector!
    g2 <- function(th) {
	## g2 = g(.) exp(-g(.))
	x.exp.m.x( g(th) )
    }

    pi2 <- pi/2
    ## p2 <- (1 - 1e-6)*pi2
    ## g. <- if(alpha >= 1) g(-p2) else g(p2)
    ## if(is.na(g.) || identical(g., 0))# g() is not usable
    ##     return(f.zeta()) --- but we have no f.zeta() here!
    ## We know that the maximum of g2(.) is = exp(-1) = 0.3679  "at" g(.) == 1
    ## find that by uniroot :
    ur <- uniroot(function(th) g(th) - 1, lower = -pi2, upper = pi2, tol = tol)
    theta2 <- ur$root

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
    ## NB: (gamma, delta) can be *vector*s (vectorized along x)
    stopifnot( 0 < alpha, alpha <= 2, length(alpha) == 1,
	      -1 <= beta, beta	<= 1, length(beta) == 1,
	      0 <= gamma, length(pm) == 1, pm %in% 0:2,
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
		z.m.zet <- abs(z - zeta)

		if (z.m.zet < 2 * .Machine$double.eps)
		    ## FIXME? same problem as dstable
		    (1/2- theta0/pi)
		else if (z > zeta)
		    .FCT1(z.m.zet, alpha=alpha, theta0= theta0,
			  tol = tol, subdivisions = subdivisions)
		else ## (z < zeta)
		    1 - .FCT1(z.m.zet, alpha=alpha, theta0= -theta0,
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
}## {pstable}

## ------------------------------------------------------------------------------

##' Auxiliary for pstable()  (for alpha != 1)
.FCT1 <- function(x.m.zet, alpha, theta0, tol, subdivisions)
{
    ## identically as in .fct1() for dstable():
    a_1 <- alpha - 1
    cat0 <- cos(at0 <- alpha*theta0)
    ## Nolan(1997) shows that   g() is montone
    g <- function(th) (cat0 * cos(th) * (x.m.zet/sin(at0+alpha*th))^alpha)^(1/a_1)  * cos(at0+ a_1*th)

    ## Function to integrate:
    G1 <- function(th) { ## (xarg, alpha, beta)
	v <- g(th)
	exp(-v)
    }

    ## as g() is montone,  G1() is too -- so the maximum is at the boundary

    ## theta2 <- optimize(G1, lower = -theta0, upper = pi/2,
    ##     	       maximum = TRUE, tol = tol)$maximum
    c1 <- if(alpha < 1) 1/2 - theta0/pi else 1
    c3 <- sign(1-alpha)/pi
    ## r1 <- .integrate2(G1, lower = -theta0,
    ##     	      upper = theta2, subdivisions = subdivisions,
    ##     	      rel.tol = tol, abs.tol = tol)
    ## r2 <- .integrate2(G1, lower = theta2,
    ##     	      upper = pi/2, subdivisions = subdivisions,
    ##     	      rel.tol = tol, abs.tol = tol)
    ## c1 + c3*(r1+r2)
    r <- .integrate2(G1, lower = -theta0, upper = pi/2, subdivisions = subdivisions,
                     rel.tol = tol, abs.tol = tol)
    c1 + c3* r
}

## ------------------------------------------------------------------------------

##' Auxiliary for pstable()  only used when alpha == 1 :
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
    ## NB: (gamma, delta) can be *vector*s (vectorized along x)
    stopifnot( 0 < alpha, alpha <= 2, length(alpha) == 1,
	      -1 <= beta, beta	<= 1, length(beta) == 1,
	      0 <= gamma, length(pm) == 1, pm %in% 0:2,
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
		    xmax <-
			if (pp < 0.5)
			    qnorm(pp, mean = 0, sd = sqrt(2))
			else
			    qcauchy(pp)
		}
		else if (beta > 0 ) {
		    xmin <-
			if (pp < 0.5)
			    qcauchy(pp)
			else
			    qnorm(pp, mean = 0, sd = sqrt(2))
		    xmax <- pp/(1-pp)
		}
		else { ## (beta == 0)
		    xmin <-
			if (pp < 0.5)
			    qcauchy(pp)
			else
			    qnorm(pp, mean = 0, sd = sqrt(2))
		    xmax <-
			if (pp < 0.5)
			    qnorm(pp, mean = 0, sd = sqrt(2))
			else
			    qcauchy(pp)
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
    ## NB: (gamma, delta) can be *vector*s (vectorized along x)
    stopifnot( 0 < alpha, alpha <= 2, length(alpha) == 1,
	      -1 <= beta, beta	<= 1, length(beta) == 1,
	      0 <= gamma, length(pm) == 1, pm %in% 0:2)

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

