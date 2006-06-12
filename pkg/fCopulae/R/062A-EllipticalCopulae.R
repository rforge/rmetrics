
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
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                  UTILITY FUNCTIONS:
#  .ellipticalParam           Sets Default parameters for an elliptical copula
#  .ellipticalRange           Returns the range of valid rho values
#  .ellipticalCheck           Checks if rho is in the valid range
# FUNCTION:                  ELLIPTICAL GENERATOR AND RELATED FUNCTIONS:
#  gfunc                      Generator function for elliptical distributions
#  gfuncSlider                Slider for generator, density and probability
#  .pelliptical               Univariate elliptical distribution probability
#  .delliptical               Univariate elliptical distribution density
#  .qelliptical               Univariate elliptical distribution quantiles
#  .qlogistic                 Fast tabulated logistic quantile function
#  .qlogisticData             Table generator for logistic quantiles
# FUNCTION:                  ELLIPTICAL COPULAE RANDOM DEVIATES:
#  rellipticalCopula          Generates elliptical copula variates
#  rellipticalSlider          Interactive plots of random variates
#  .rnormCopula               Generates normal copula random variate
#  .pnormCopula               Computes normal copula probability
#  .dnormCopula               Computes normal copula density
#  .rcauchyCopula             Generates Cauchy copula random variate
#  .pcauchyCopula             Computes Cauchy copula probability
#  .dcauchyCopula             Computes Cauchy copula density
#  .rtCopula                  Generates Student-t copula random variate
#  .ptCopula                  Computes Student-t copula probability
#  .dtCopula                  Computes Student-t copula density
# FUNCTION:                  ELLIPTICAL COPULAE PROBABILITY:
#  pellipticalCopula          Computes elliptical copula probability
#  pellipticalSlider          Interactive plots of probability
#  .pellipticalCopulaGrid     Fast equidistant grid version
#  .pellipticalCopulaDiag     Fast diagonal cross section version
#  .pellipticalPerspSlider    Interactive perspective plots of probability
#  .pellipticalContourSlider  Interactive contour plots of probability
# FUNCTION:                  ELLIPTICAL COPULAE DENSITY:
#  dellipticalCopula          Computes elliptical copula density 
#  dellipticalSlider          Interactive plots of density
#  .dellipticalCopulaGrid     Fast grid version for elliptical copula density
#  .dellipticalCopula.RUnit   R Unit test for elliptical copula density
#  .dellipticalPerspSlider    Interactive perspective plots of density
#  .dellipticalContourSlider  Interactive contour plots of density
# FUNCTION:                  ELLIPTICAL DEPENDENCE MASURES:
#  ellipticalTau              Computes Kendall's tau for elliptical copulae
#  ellipticalRho              Computes Spearman's rho for elliptical copulae
# FUNCTION:                  ELLIPTICAL TAIL COEFFICIENT:
#  ellipticalTailCoeff        Computes tail dependence for elliptical copulae
#  ellipticalTailPlot         Plots tail dependence function
# FUNCTION:                  ELLIPTICAL COPULAE PARAMETER FITTING:
#  ellipticalCopulaSim        Simulates bivariate elliptical copula
#  ellipticalCopulaFit        Fits the paramter of an elliptical copula
################################################################################


################################################################################
# UTILITY FUNCTIONS:


.ellipticalParam =
function(type = c("norm", "cauchy", "t", "logistic", "laplace", "kotz", 
"epower"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Sets default parameters for elliptical copulae
    
    # Arguments:
    #   type -  a character string denoting the type of distribution.
    #       This may be either 
    #       "norm" for the normal distribution, or
    #       "cauchy" for the Cauchy distribution, or
    #       "t" for the Student-t distribution, or
    #       "logistic" for the logistic distribution, or
    #       "laplace" for the distribution, or
    #       "kotz" for the original Kotz distribution, or 
    #       "epower" for the exponential power distribution
    
    # Value:
    #   returns a list with two elements, 'param' sets the parameters
    #       which may be a vector, 'range' the range with minimum and
    #       maximum values for each of the parameters. 
    
    # Example:
    #   .ellipticalParam("norm"); .ellipticalParam("t")
    
    # FUNCTION:
    
    # Settings:
    type = match.arg(type)
    
    # Parameter Values:
    #       "norm", "cauchy",  "t", "logistic", "laplace", "kotz", "epower"
    lower  = c( -1,      -1,   -1,         -1,         -1,     -1,       -1)
    upper  = c( +1,      +1,   +1,         +1,         +1,     +1,       +1)
    rho    = c(3/4,     3/4,  3/4,        3/4,        3/4,    3/4,      3/4)
    param1 = c( NA,      NA, nu=4,         NA,         NA,    r=1,      r=1)
    param2 = c( NA,      NA,   NA,         NA,         NA,     NA,      s=1)
    
    # Parameter List:
    ans = list(type = type)
    if (type == "norm") {
        ans$param = c(rho = rho[1])
    }
    if (type == "cauchy") {
        ans$param = c(rho = rho[1])
    }
    if (type == "t") {
        ans$param = c(rho = rho[1], nu = param1[3])
    }
    if (type == "logistic") {
        ans$param = c(rho = rho[1])
    }
    if (type == "laplace") {
        ans$param = c(rho = rho[1])
    }
    if (type == "kotz") {
        ans$param = c(rho = rho[1], r = param1[5])
    }
    if (type == "epower") {
        ans$param = c(rho = rho[1], r = param1[5], s = param2[6])
    }
    ans$range = c(lower = lower[1], upper = upper[1]) 
    
    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------ 
   
  
.ellipticalRange = 
function(type = c("norm", "cauchy", "t", "logistic", "laplace", "kotz", 
"epower"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the range of valid alpha values
    
    #   type -  a character string denoting the type of distribution.
    #       This may be either 
    #       "norm" for the normal distribution, or
    #       "cauchy" for the Cauchy distribution, or
    #       "t" for the Student-t distribution, or
    #       "logistic" for the logistic distribution, or
    #       "laplace" for the distribution, or
    #       "kotz" for the original Kotz distribution, or 
    #       "epower" for the exponential power distribution
    
    # Example:
    #   .ellipticalRange("norm"); .ellipticalRange("t")
    
    # FUNCTION:
    
    # Type:
    type = match.arg(type)
    
    # Range:
    ans = .ellipticalParam(type)$range
    
    # ReturnVa lue:
    ans
}


# ------------------------------------------------------------------------------


.ellipticalCheck =
function(rho = 0.75, param = NULL, type = c("norm", "cauchy", "t", 
"logistic", "laplace", "kotz", "epower"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Checks if alpha is in the valid range
    
    # Arguments:
    #   rho - correlation coefficient
    #   param - currently not used
    #   type -  a character string denoting the type of distribution.
    #       This may be either 
    #       "norm" for the normal distribution, or
    #       "cauchy" for the Cauchy distribution, or
    #       "t" for the Student-t distribution, or
    #       "logistic" for the logistic distribution, or
    #       "laplace" for the distribution, or
    #       "kotz" for the original Kotz distribution, or 
    #       "epower" for the exponential power distribution
    
    # Example:
    #   .ellipticalCheck(0.5, NULL, "norm") 
    #   .ellipticalCheck(1.5, NULL, "t")
    
    # FUNCTION:
    
    # Type:
    type = match.arg(type)

    # Range:
    range = as.vector(.ellipticalRange(type))
    if (rho < range[1] | rho > range[2]) {
        print(c(rho = rho))
        print(c(range = range))
        stop("rho is out of range")
    }
    
    # Return Value:
    invisible()
}


################################################################################
# GENERATOR:


gfunc = 
function(x, param = NULL, type = c("norm", "cauchy", "t", "logistic", 
"laplace", "kotz", "epower"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generator function for elliptical distributions
    
    # Arguments:
    #   x -  a numeric vector
    #   param - NULL, a numeric value, or a numeric vector adding.
    #       additional parameters to the generator function.
    #   type -  a character string denoting the type of distribution.
    #       This may be either 
    #       "norm" for the normal distribution, or
    #       "cauchy" for the Cauchy distribution, or
    #       "t" for the Student-t distribution, or
    #       "logistic" for the logistic distribution, or
    #       "laplace" for the distribution, or
    #       "kotz" for the original Kotz distribution, or 
    #       "epower" for the exponential power distribution
    
    # Value:
    #   Returns a numeric vector "g(x)" for the generator computed at
    #   the x values taken from the input vector. If x is missing,
    #   the normalizing constant "lambda" will be returned.
    
    # FUNCTION:
    
    # Type:
    type = match.arg(type)
    
    # Handle Missing x:
    if (missing(x)) {
        x = NA
        output = "lambda"
    } else {
        output = "g"
    }
    
    # Get Type:
    type = type[1]
    
    # Get Parameters:   
    # if (is.null(param)) param = .ellipticalParam$param
    
    # Create Generator:
    if (type == "norm") {
        g = exp(-x/2)
        lambda = 1 / (2*pi)
        param = NULL
    }
    if (type == "cauchy") {
        g = ( 1 + x )^ (-3/2 )
        lambda = 1 / (2*pi)
        param = NULL
    }
    if (type == "t") {
        if (is.null(param)) {
            nu = 4
        } else {
            nu = param[[1]]
        }
        g = ( 1 + x/nu )^ ( -(nu+2)/2 )
        lambda = 1/(2*pi)
        param = c(nu = nu)
    }
    if (type == "logistic"){
        g = exp(-x/2)/(1+exp(-x/2))^2
        # lambda:
        # integrate(function(x) { exp(-x)/(1+exp(-x))^2}, 0, Inf, 
        #   subdivision = 10000, rel.tol = .Machine$double.eps^0.8)
        # 0.5 with absolute error < 2.0e-13
        lambda = 1 / pi 
        param = NULL
    }
    if (type == "laplace") { # or "double exponential"
        # epower - with r = 1, s = 1
        # g = exp(-r*(x/2)^s)
        # lambda = s * r^(1/s) / ( 2 * pi * gamma(1/s) )
        g = exp(-sqrt(x))
        lambda = 1/(2*pi)
        param = NULL
    }
    if (type == "kotz") {
        # epower - with s = 1
        if (is.null(param)) {
            r = 1
        } else {
            r = param
        }
        g = exp(-r*(x/2))
        lambda = r/(2*pi)
        param = c(r = r)
    }
    if (type == "epower") {
        if (is.null(param)) {
            r = 1 
            s = 1
        } else {
            r = param[[1]]
            s = param[[2]]
        }
        g = exp(-r*(x/2)^s)
        lambda = s * r^(1/s) / ( 2 * pi * gamma(1/s) )
        param = c(r = r, s = s)
    }
    
    # Output:
    output = output[1]
    if (output == "g") {
        ans = g
    } else if (output == "lambda") {
        ans = lambda
    }
    
    # Add Control:
    if (output == "g") {
        attr(ans, "control") = c(type = type, lambda = as.character(lambda))
    } else if (output == "lambda") {
        if (is.null(param)) {
            attr(ans, "control") = unlist(list(type = type))
        } else {
            attr(ans, "control") = unlist(list(type = type, param = param))
        }
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


gfuncSlider = 
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Slider for generator function, density and probability
    
    # FUNCTION:
    
    # Graphic Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        Copula = as.integer(.sliderMenu(no = 1))
        type = c("norm", "cauchy", "t", "logistic", "laplace", "kotz", 
            "epower")
        type = type[Copula]
        Type = c("Normal", "Cauchy", "Student-t", "Logistic", "Laplace",
            "Kotz", "Exponential Power")
        Type = Type[Copula]
        N = .sliderMenu(no = 2)
        nu = .sliderMenu(no = 3)
        r = .sliderMenu(no = 4)
        s = .sliderMenu(no = 5)
        rho = .sliderMenu(no = 6)
        L = 6.5
      
        # Parameters:
        param = NULL
        if (Copula == 3) param = nu
        if (Copula == 6) param = r
        if (Copula == 7) param = c(r, s)
        prefactor = gfunc(param = param, type = type)[[1]]
        Lambda = as.character(round(prefactor, digits = 3)) 
        Nu = R = S = NA
        if (Copula == 3) Nu = as.character(round(nu, digits = 1))
        if (Copula >= 6) R = as.character(round(r, digits = 1))
        if (Copula == 7) S = as.character(round(s, digits = 1))
        delta = 10/N
                  
        # Bivariate Density:
        x = y = seq(-4, 4, length = 101)
        D = delliptical2d(grid2d(x), rho = rho, param = param, 
            type = type, output = "list")
                        
        # Plot 1:
        Limit = ""
        if (Copula == 3 & nu == 1) Limit = "| [Cauchy]" 
        if (Copula == 6 & r == 1) Limit = "| [Normal]" 
        if (Copula == 7 & s == 1) Limit = "| [Kotz]"
        if (Copula == 7 & r == 1 & s == 1) Limit = "| [Normal]" 
        lambda = gfunc(param = param, type = type)
        x = seq(0, L, length = N)
        y = gfunc(x, param = param, type = type)
        y.norm = gfunc(x, type = "norm")    
        plot(x, y, type = "l", ylab = "g", ylim = c(0, 1))
        abline(h = 0, lty = 3, col = "grey")
        lines(x, y.norm, lty = 3, col = "red")
        title(main = paste("Generator:", Type, Limit, "\nPre-Factor:", Lambda))
        mtext("Dotted Curve: Normal Generator", side = 4, col = "grey", 
            cex = 0.7)
        
        # Plot 2 - Density:
        x = seq(-L, L, length = N)
        y = .delliptical(x, param = param, type = type)
        y.norm = .delliptical(x, type = "norm")
        plot(x, y, type = "l", ylab = "Density", ylim = c(0, 0.65))
        abline(h = 0, lty = 3, col = "grey")
        abline(v = 0, lty = 3, col = "grey")
        lines(x, y.norm, lty = 3, col = "red")
        Y = 2*integrate(.delliptical, 0, Inf, param = param, type = type)[[1]]
        Y = as.character(round(Y, 2))
        .velliptical = function(x, param, type) x^2*.delliptical(x, param, type)
        V = 2*integrate(.delliptical, 0, Inf, param = param, type = type)[[1]]
        V = as.character(round(V, 2))
        mtext(paste("Normalization Test:", Y, " |  Variance Test:", V), 
            side = 4, col = "grey", cex = 0.7)
        if (type == "t") {
            title(main = paste(Type, "Density\n nu =", Nu))
        } else if (type == "kotz") {
            title(main = paste(Type, "Density\n r =", R))
        } else if (type == "epower") {
            title(main = paste(Type, "Density\n r =", R, "s =", S))
        } else {
            title(main = paste(Type, "Density\n "))
        }
        
        # Plot 3 - Probability:
        x = seq(-L, L, length = N)
        y = .pelliptical(x, param = param, type = type)
        y.norm = .pelliptical(x, type = "norm")
        plot(x, y, type = "l", ylab = "Probability", ylim = c(0, 1))
        abline(h = 0, lty = 3, col = "grey")
        abline(h = 1, lty = 3, col = "grey")
        abline(h = 0.5, lty = 3, col = "grey")
        lines(x, y.norm, lty = 3, col = "red")
        p95 = .qelliptical(0.95, param = param, type = type)
        P95 = as.character(round(p95, digits = 2))
        abline(v = p95, lty = 3)
        abline(v = -p95, lty = 3)
        q95 = .pelliptical(p95, param = param, type = type)
        points(+p95, q95, pch = 19, cex = 0.5)
        points(-p95, 1-q95, pch = 19, cex = 0.5)
        mtext("Dots: Probability(Quantile(0.95)) Test", side = 4, 
            col = "grey", cex = 0.7)
        Title = paste(Type, "Probability\n 95% =", P95)
        title(main = Title)
                
        # Plot 4 - Bivariate Density:
        contour(D, levels = c(0.001, 0.01, 0.025, 0.05, 0.1), 
            xlab = "x", ylab = "y")
        title(main = paste("Bivariate Density\nrho = ", as.character(rho)))
        grid()
               
        # Reset Frame:
        par(mfrow = c(2, 2), cex = 0.7)
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
        names       = c("Copula",   "N", "3: nu",  "6|7: r",  "7: s", "rho"),
        minima      = c(       1,    50,       1,       0.1,     0.1, -0.95),
        maxima      = c(       7,  2000,      10,        10,      10,  0.95),
        resolutions = c(       1,    50,     0.1,       0.1,     0.1,  0.05),
        starts      = c(       1,   100,       4,         1,       1,  0.00)) 
}
  

# ------------------------------------------------------------------------------


.pelliptical = 
function(q, param = NULL, type = c("norm", "cauchy", "t", "logistic", 
"laplace", "kotz", "epower"), alternative = TRUE, subdivisions = 100)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Probability function for univariate elliptical distributions
    
    # Arguments:
    #   x -  a numeric vector
    #   param - NULL, a numeric value, or a numeric vector adding.
    #       additional parameters to the generator function.
    #   type -  a character string denoting the type of distribution.
    #       This may be either 
    #       "norm" for the normal distribution, or
    #       "cauchy" for the Cauchy distribution, or
    #       "t" for the Student-t distribution, or
    #       "logistic" for the logistic distribution, or
    #       "laplace" for the distribution, or
    #       "kotz" for the original Kotz distribution, or 
    #       "epower" for the exponential power distribution.
    
    # Details:
    #   The probability is computed by integration using the generator
    #       function. If an alternative faster algorithm is available,
    #       this one is used by default.
    
    # FUNCTION:
    
    # Type:
    type = match.arg(type)
    
    # Alternative Available?
    if (type == "logistic") alternative = FALSE
    if (type == "laplace") alternative = FALSE
    if (type == "kotz") alternative = FALSE
    if (type == "epower") alternative = FALSE 
    
    # Original Function:
    # Fq1 = function (x, Q, param, type) { 
    #    acos(abs(Q)/sqrt(x)) * gfunc(x, param, type) } 
    # Transformed Function: u = exp(-x+Q^2)
    Fq2 = function (x, Q, param, type) { 
        Q^2 * acos(sqrt(x))/x^2 * gfunc(Q^2/x, param, type) } 
    # Add Default Parameters:
    if (is.null(param)) {
        if (type == "t") param = c(nu = 4)
        if (type == "kotz") param = c(r = 1)
        if (type == "epower") param = c(r = 1, s = 1)
    }
        
    # Probability:
    ans = NULL
    if (alternative) {
        ans = NA
        if (type[1] == "norm") ans = pnorm(q)
        if (type[1] == "cauchy") ans = pt(q, df = 1) # pcauchy(q)
        if (type[1] == "t") ans = pt(q, df = param[[1]])
        if (type[1] == "kotz") ans = dnorm(x, sd = 1/sqrt(param[[1]]))
    } else {
        lambda = gfunc(param = param, type = type)[[1]]
        ans = NULL
        for ( Q in q ) {
            # More Precise Adaptive Rule:
            # p = lambda * integrate(Fq1, lower = Q^2, upper = Inf, Q = Q,
            # param = param, type = type, subdivisions = subdivisions)[[1]]
            p = lambda*integrate(Fq2, lower = .Machine$double.eps^0.5, 
                upper = 1, Q = Q, param = param, type = type, 
                stop.on.error = FALSE, subdivisions = subdivisions)[[1]]
            if (Q > 0) p = 1 - p
            if (abs(Q) < .Machine$double.eps^0.5) p = 0.5
            ans = c(ans, p)
        }
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.pelliptical.RUnit = 
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   RUnit test

    # TEST:
    
    # Probability:
    q = (-1000:1000)/2000
    S = NULL
    
    s = Sys.time()
    .pelliptical(q = q, param = NULL, type = "norm")
    S = c(S, as.integer(Sys.time() - s))
     
    s = Sys.time()
    .pelliptical(q = q, param = NULL, type = "cauchy") 
    S = c(S, as.integer(Sys.time() - s))
    
    s = Sys.time()
    .pelliptical(q = q, param = 2, type = "t") 
    S = c(S, as.integer(Sys.time() - s))
    
    s = Sys.time()
    .pelliptical(q = q, param = NULL, type = "logistic") 
    S = c(S, as.integer(Sys.time() - s))
    
    s = Sys.time()
    .pelliptical(q = q, param = NULL, type = "laplace")
    S = c(S, as.integer(Sys.time() - s))
    
    s = Sys.time()
    .pelliptical(q = q, param = c(r = 1), type = "kotz")  
    S = c(S, as.integer(Sys.time() - s))
    
    s = Sys.time()
    .pelliptical(q = q, param = c(r = 1, s = 1), type = "epower")
    S = c(S, as.integer(Sys.time() - s))
    
    S 
}


# ------------------------------------------------------------------------------


.delliptical = 
function(x, param = NULL, type = c("norm", "cauchy", "t", "logistic", 
"laplace", "kotz", "epower"), alternative = TRUE, subdivisions = 100)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Density function for univariate elliptical distributions
    
    # Arguments:
    #   x -  a numeric vector
    #   param - NULL, a numeric value, or a numeric vector adding.
    #       additional parameters to the generator function.
    #   type -  a character string denoting the type of distribution.
    #       This may be either 
    #       "norm" for the normal distribution, or
    #       "cauchy" for the Cauchy distribution, or
    #       "t" for the Student-t distribution, or
    #       "logistic" for the logistic distribution, or
    #       "laplace" for the distribution, or
    #       "kotz" for the original Kotz distribution, or 
    #       "epower" for the exponential power distribution.
    #   alternative -  a logical flag. Should alternatively used a 
    #       faster algorithm if available? By default TRUE. 
    
    # Details:
    #   The density is computed by integration using the generator
    #       function. If an alternative faster algorithm is available,
    #       this one is used by default.
       
    # FUNCTION:
    
    # Type:
    type = match.arg(type)
    
    # Alternative Available?
    if (type == "logistic") alternative = FALSE
    if (type == "laplace") alternative = FALSE
    if (type == "kotz") alternative = FALSE
    if (type == "epower") alternative = FALSE 
    
    # Original Function:
    # fq1 = function (x, Q, param, type) { 
    #    gfunc(x, param, type) / ( sqrt(x - Q^2) ) }
    # Transformed Function: log(x)^2 = x - Q^2
    fq2 = function (x, Q, param, type) { 
        2 * gfunc(log(x)^2+Q^2, param, type) / x  }
     
    
    # Add Default Parameters:
    if (is.null(param)) {
        if (type == "t") param = c(nu = 4)
        if (type == "kotz") param = c(r = 1)
        if (type == "epower") param = c(r = 1, s = 1)
    }
    
    # Normalizing constant lambda:
    lambda = gfunc(param = param, type = type)[[1]]
    
    # Density:
    ans = NULL
    if (alternative) {
        ans = NA
        if (type[1] == "norm") ans = dnorm(x)
        if (type[1] == "cauchy") ans = dt(x, df = 1) # dcauchy(x)
        if (type[1] == "t") ans = dt(x, df = param[[1]])
        if (type[1] == "kotz") ans = dnorm(x, sd = 1/sqrt(param[[1]]))
    } else {
        lambda = gfunc(param = param, type = type)[[1]]
        ans = NULL
        for ( Q in x ) {
            # More Precise Adaptive Rule:
            # p = lambda*integrate(fq1, lower = Q^2, upper = Inf, Q = Q,
            #     param = param, type = type)[[1]]
            p = lambda*integrate(fq2, lower = 0, upper = 1, Q = Q, param = 
                param, type = type, stop.on.error = FALSE,
                subdivisions = subdivisions)[[1]]
            ans = c(ans, p)
        } 
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.delliptical.RUnit = 
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   RUnit test

    # TEST:
    
    # Probability:
    N = 100
    x = (-1999:1999)/N
    d = .delliptical(x = x, param = NULL, type = "norm")
        sum(d)/N
    d = .delliptical(x = x, param = NULL, type = "cauchy")
        sum(d)/N
    d = .delliptical(x = x, param = NULL, type = "t")
        sum(d)/N
    d = .delliptical(x = x, param = NULL, type = "logistic")
        sum(d)/N
    d = .delliptical(x = x, param = NULL, type = "laplace")
        sum(d)/N
    d = .delliptical(x = x, param = NULL, type = "kotz")
        sum(d)/N
    d = .delliptical(x = x, param = NULL, type = "epower")
        sum(d)/N
        
    # Non-default Parameters:
    d = .delliptical(x = (-100:100)/10, param = 1, type = "kotz")
        sum(d)/N
    d = .delliptical(x = (-100:100)/10, param = 1/2, type = "kotz")
        sum(d)/N
}


# ------------------------------------------------------------------------------


.qelliptical = 
function(p, param = NULL, type = c("norm", "cauchy", "t", "logistic", 
"laplace", "kotz", "epower"), alternative = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Quantile function for univariate elliptical distributions
    
    # Arguments:
    #   x -  a numeric vector
    #   param - NULL, a numeric value, or a numeric vector adding.
    #       additional parameters to the generator function.
    #   type -  a character string denoting the type of distribution.
    #       This may be either 
    #       "norm" for the normal distribution, or
    #       "cauchy" for the Cauchy distribution, or
    #       "t" for the Student-t distribution, or
    #       "logistic" for the logistic distribution, or
    #       "laplace" for the distribution, or
    #       "kotz" for the original Kotz distribution, or 
    #       "epower" for the exponential power distribution.
    #   alternative - a logical flag. Should be an alternative
    #       faster algorithm used and not the standard algorithm?
    
    # Details:
    #   The probability is computed by integration using the generator
    #       function. If an alternative faster algorithm is available,
    #       this one is used by default.
    
    # FUNCTION:
    
    # Type:
    type = match.arg(type)
    
    # Alternative Available?
    if (type == "laplace") alternative = FALSE
    if (type == "kotz") alternative = FALSE
    if (type == "epower") alternative = FALSE 
        
    # Add Default Parameters:
    if (is.null(param)) {
        if (type == "t") param = c(nu = 4)
        if (type == "kotz") param = c(r = 1)
        if (type == "epower") param = c(r = 1, s = 1)
    }
        
    # Probability:
    ans = NULL
    if (alternative) {
        ans = NA
        if (type[1] == "norm") ans = qnorm(p)
        if (type[1] == "cauchy") ans = qcauchy(p)
        if (type[1] == "t") ans = qt(p, df = param[[1]])
        if (type[1] == "logistic") ans = .qlogistic(p)
        if (type[1] == "kotz") ans = dnorm(x, sd = 1/sqrt(param[[1]]))
    } else {  
        froot <<- function(x, p, param, type) {
            .pelliptical(q = x, param = param, type = type) - p }
        ans = NULL
        for (pp in p) {
            if (pp < .Machine$double.eps) {
                ans = c(ans, -Inf)
            } else if (pp > 1-.Machine$double.eps) {
                ans = c(ans, Inf)
            } else { 
                lower = -1
                upper = +1
                counter = 0
                iteration = NA
                while (is.na(iteration)) {
                    iteration = .unirootNA(f = froot, interval = c(lower, 
                        upper), param = param, type = type, p = pp)
                    counter = counter + 1
                    lower = lower - 2^counter
                    upper = upper + 2^counter
                }
                ans = c(ans, iteration)
            }
        }
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.qelliptical.RUnit = 
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   RUnit test

    # TEST:
    
    # Probability:
    p = (0:10)/10
    .qelliptical(p = p, param = NULL, type = "norm") 
    .qelliptical(p = p, param = NULL, type = "cauchy") 
    .qelliptical(p = p, param = 2, type = "t") 
    .qelliptical(p = p, param = NULL, type = "logistic") 
    .qelliptical(p = p, param = NULL, type = "laplace")
    .qelliptical(p = p, param = c(r = 1), type = "kotz")  
    .qelliptical(p = p, param = c(r = 1, s = 1), type = "epower") 
}


# ------------------------------------------------------------------------------


.qlogisticData = 
function (dump = FALSE ) 
{   # A function implemented by Diethelm Wuertz
    
    # FUNCTION:

    # Range:
    p = seq(0.001, 0.500, by = 0.001)
    
    # Quantiles by Integration:
    froot = function(x, p) { .pelliptical(x, type = "logistic") - p }
    X = NULL
    for (P in p) {
        lower = -1
        upper = +1
        counter = 0
        iteration = NA
        while (is.na(iteration)) {
            iteration = .unirootNA(f = froot, interval = c(lower, upper), p = P)
            counter = counter + 1
            lower = lower - 2^counter
            upper = upper + 2^counter
        }
        X = c(X, iteration)
    }
    Y = pelliptical(X, type = "logistic")
    qlogisticTable = data.frame(cbind(X = X, Y = Y))
    
    # Dump:
    if (dump) dump("qlogisticTable", "qlogisticTable.R")
    
    # Return Value:
    invisible(qlogisticTable)
}


# ------------------------------------------------------------------------------


.qlogistic = 
function(p)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fast Quantile function for the logistic distribution
    
    # FUNCTION:
    
    # Table:
    data(qlogisticTable)
    data = qlogisticTable
    
    # Quantiles:
    P = (sign(p-1/2)+1)/2 - sign(p-1/2)*p
    ans = sign(0.5-p) * approx(x = data[, 2], y = data[, 1], xout = P)$y
    
    # p Boundary:
    index = which(p < 0.001 & p > 0)
    if (length(index) > 0) {
        ans[index] = 
            .qelliptical(p[index], type = "logistic", alternative = FALSE)
    }
    index = which(p > 1-0.001 & p < 1)
    if (length(index) > 0) {
        ans[index] = 
            .qelliptical(p[index], type = "logistic", alternative = FALSE)
    }
    ans[p == 0.5] = 0
    ans[p == 0] = -Inf
    ans[p == 1] = Inf
    
    # Return Value:
    ans
}



################################################################################
# NORMAL, CAUCHY AND STUDENT-T COPULAE:


.rnormCopula =
function(n, rho = 0.75)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates normal copula random variate
    
    # Example:
    #   UV = rnormCopula(n = 10000); plot(UV[,1], UV[,2], cex = 0.25)

    # FUNCTION:
    
    # Use: X = rnorm2d(n, rho) or alternatively:
    X = rnorm2d(n = n, rho = rho)
    
    # Generate
    Z <- NULL
    for(i in (1:n)) Z <- rbind(Z, pnorm(X [i,]))
    
    # Return Value:
    Z
}


# ------------------------------------------------------------------------------


.pnormCopula = 
function(u = 0.5, v = u, rho = 0.75, output = c("vector", "list") )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes normal copula probability
    
    # Arguments:
    #   see function 'pellipticalCopula'
    
    # FUNCTION:
    
    # Type:
    output = match.arg(output)
    
    # Settings:
    type = "norm"
    if (is.list(u)) {
        v = u[[2]]
        u = u[[1]]
    }
    if (is.matrix(u)) {
        v = u[, 2]
        u = u[, 1]
    }
    
    # Copula Probability:
    C.uv = pnorm2d(qnorm(u), qnorm(v), rho = rho) 
    names(C.uv) = NULL
    
    # Simulates Max function:
    C.uv = (C.uv + abs(C.uv))/2       
    
    # On Boundary:
    C.uv[is.na(C.uv)] = 0      
    C.uv[which(u == 0)] = 0
    C.uv[which(u == 1)] = v[which(u == 1)]
    C.uv[which(v == 0)] = 0
    C.uv[which(v == 1)] = u[which(v == 1)]
    C.uv[which(u*v == 1)] = 1
    C.uv[which(u+v == 0)] = 0
    
    # Result:
    attr(C.uv, "control") <- c(rho = rho)
    
    # As List ?
    if (output == "list") {
        N = sqrt(length(u))
        x = u[1:N]
        y = matrix(v, ncol = N)[1, ]
        C.uv = list(x = x, y = y,  z = matrix(C.uv, ncol = N))
    }
    
    # Return Value:
    C.uv
}


# ------------------------------------------------------------------------------


.dnormCopula = 
function(u = 0.5, v = u, rho = 0.75, output = c("vector", "list") )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes normal copula density
    
    # Arguments:
    #   see function 'dellipticalCopula'
    
    # FUNCTION:
    
    # Type:
    output = match.arg(output)
    
    # Settings:
    type = "norm"
    if (is.list(u)) {
        v = u[[2]]
        u = u[[1]]
    }
    if (is.matrix(u)) {
        v = u[, 2]
        u = u[, 1]
    }
    
    # Copula Density:
    x = qnorm(u)
    y = qnorm(v)
    c.uv = dnorm2d(x, y, rho)/(dnorm(x) * dnorm(y))
    names(c.uv) = NULL
    
    # Result:
    attr(c.uv, "control") <- c(rho = rho)
    
    # As List ?
    if (output == "list") {
        N = sqrt(length(u))
        x = u[1:N]
        y = matrix(v, ncol = N)[1, ]
        c.uv = list(x = x, y = y,  z = matrix(c.uv, ncol = N))
    }
    
    # Return Value:
    c.uv
}


# ******************************************************************************


.rtCopula =
function(n, rho = 0.75, nu = 4)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates Student-t copula random variate
    
    # Example:
    #   UV = rtCopula(n = 10000); plot(UV[,1], UV[,2], cex = 0.25)

    # FUNCTION:
    
    # Use: X = .rnorm2d(n, rho) or alternatively:
    X = rt2d(n = n, rho = rho, nu = nu)
    
    # Generate
    Z = NULL
    for (i in (1:n)) Z = rbind(Z, pt(X [i,], df = nu))
    
    # Return Value:
    Z
}


# ------------------------------------------------------------------------------


.ptCopula = 
function(u = 0.5, v = u, rho = 0.75, nu = 4, output = c("vector", "list") )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Student-t copula probability
    
    # Arguments:
    #   see function 'pellipticalCopula'
    
    # FUNCTION:
    
    # Match Arguments:
    output = match.arg(output)
    
    # Settings:
    type = "t"
    if (is.list(u)) {
        v = u[[2]]
        u = u[[1]]
    }
    if (is.matrix(u)) {
        v = u[, 2]
        u = u[, 1]
    }
    
    # Copula Probability:
    C.uv = pt2d(qt(u, df = nu), qt(v, df = nu), rho = rho, nu = nu) 
    names(C.uv) = NULL
    
    # Simulates Max function:
    C.uv = (C.uv + abs(C.uv))/2       
    
    # On Boundary:
    C.uv[is.na(C.uv)] = 0      
    C.uv[which(u == 0)] = 0
    C.uv[which(u == 1)] = v[which(u == 1)]
    C.uv[which(v == 0)] = 0
    C.uv[which(v == 1)] = u[which(v == 1)]
    C.uv[which(u*v == 1)] = 1
    C.uv[which(u+v == 0)] = 0
    
    # Result:
    attr(C.uv, "control") <- c(rho = rho, nu = nu)
    
    # As List ?
    if (output == "list") {
        N = sqrt(length(u))
        x = u[1:N]
        y = matrix(v, ncol = N)[1, ]
        C.uv = list(x = x, y = y,  z = matrix(C.uv, ncol = N))
    }
    
    # Return Value:
    C.uv
}


# ------------------------------------------------------------------------------


.dtCopula = 
function(u = 0.5, v = u, rho = 0.75, nu = 4, output = c("vector", "list") )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Student-t copula density
    
    # Arguments:
    #   see function 'dellipticalCopula'
    
    # FUNCTION:
    
    # Match Arguments:
    output = match.arg(output)
    
    # Settings:
    type = "t"
    if (is.list(u)) {
        v = u[[2]]
        u = u[[1]]
    }
    if (is.matrix(u)) {
        v = u[, 2]
        u = u[, 1]
    }
    
    # Copula Probability:
    x = qt(u, df = nu)
    y = qt(v, df = nu)
    c.uv = dt2d(x, y, rho, nu)/(dt(x, nu) * dt(y, nu))
    names(c.uv) = NULL
    
    # Result:
    attr(c.uv, "control") <- c(rho = rho, nu = nu)
    
    # As List ?
    if (output == "list") {
        N = sqrt(length(u))
        x = u[1:N]
        y = matrix(v, ncol = N)[1, ]
        c.uv = list(x = x, y = y,  z = matrix(c.uv, ncol = N))
    }
    
    # Return Value:
    c.uv
}


################################################################################
# COPULA RANDOM DEVIATES:


rellipticalCopula =
function(n, rho = 0.75, param = NULL, type = c("norm", "cauchy", "t"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes extreme value copula probability
    
    # Arguments:
    #   n - number of deviates to be generated.
    #   rho - a numeric value setting the coorelation strength, ranging
    #       between minus one and one.
    #   nu - the number of degrees of freedom, only required for 
    #       Student-t copulae.
    #   type - the type of the elliptical copula. Either "norm" or
    #       "t" denoting the normal or Student-t copula, respectively.
    #   output - a character string specifying how the output should
    #       be formatted. By default a vector of the same length as 
    #       'u' and 'v'. If specified as "list" then 'u' and 'v' are
    #       expected to span a two-dimensional grid as outputted by the 
    #       function 'grid2d' and the function returns a list with
    #       elements '$x', 'y', and 'z' which can be directly used 
    #       for example by 2D plotting functions. 
    
    # Value:
    #   returns a vector or list of probabilities depending on the
    #   value of the "output" variable.
    
    # Example:
    #   Diagonal Value: pnormCopula((0:10)/10)
    #   persp(pnormCopula(u = grid2d(), output = "list"))
    
    # FUNCTION:
    
    # Settings:
    type = match.arg(type)
    
    # Parameters:
    if (type == "t") {
        if(is.null(param)) {
            param = c(nu = 4)
        } else {
            param = c(nu = param)
        }
        names(param) = "nu"
    }
    
    # Copula:
    if (type == "norm") 
        ans = .rnormCopula(n = n, rho = rho)
    if (type == "cauchy") 
        ans = .rcauchyCopula(n = n, rho = rho)
    if (type == "t") 
        ans = .rtCopula(n = n, rho = rho, nu = param)
    
    # Add Control Attribute:
    control = list(rho = rho, param = param, type = type)
    attr(ans, "control")<-unlist(control)
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


rellipticalSlider =
function(B = 100)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Displays interactively perspective plots of random variates
    
    #FUNCTION:
    
    # Graphic Frame:
    par(mfrow = c(1, 1), cex = 0.7)
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        Copula = .sliderMenu(no = 1)
        N = .sliderMenu(no = 2)
        rho = .sliderMenu(no = 3)
        nu = .sliderMenu(no = 4)
        seed = .sliderMenu(no = 5)
        size = .sliderMenu(no = 6)
        col = .sliderMenu(no = 7)
        Names = c("- Normal", "- Cauchy", "- Student t") 
        Type = c("norm", "cauchy", "t")
        eps = 1.0e-6
        if (rho == +1) rho = rho - eps
        if (rho == -1) rho = rho + eps
        
        # Tau and Rho:
        Tau = ellipticalTau(rho)
        Rho = ellipticalRho(rho)
        
        # Plot: 
        Title = paste("Elliptical Copula No:", as.character(Copula), 
            Names[Copula], "\nrho =", as.character(rho), "|")
        if (Copula == 2) Title = paste(Title, "nu =", as.character(nu), "|")   
        Title = paste(Title,       
            "Kendall = ", as.character(round(Tau, digits = 3)), "|",
            "Spearman = ", as.character(round(Rho, digits = 3)) )       
        set.seed(seed)
        R = rellipticalCopula(n = N, rho = rho, param = nu, type = Type[Copula])
        plot(x = R[, 1], y = R[, 2], xlim = c(0, 1), ylim = c(0, 1), 
            xlab = "u", ylab = "v", pch = 19, col = col, cex = size)
        title(main = Title)
        
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    plot.names = c("Plot - size", "... color")
    .sliderMenu(refresh.code,
        names       = c("Copula",   "N", "rho", "t: nu", "seed", plot.names),
        minima      = c(       1,  1000,    -1,       1,   1000,     0,   1),
        maxima      = c(       3, 10000,    +1,       B,   9999,     1,  16),
        resolutions = c(       1,   500,  0.01,       1,      1,   0.1,   1),
        starts      = c(       1,  1000,     0,       4,   4711,   0.5,   1)) 
}


# ------------------------------------------------------------------------------


.rcauchyCopula =
function(n, rho = 0.75)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates Student-t copula random variate
    
    # Example:
    #   UV = rtCopula(n = 10000); plot(UV[,1], UV[,2], cex = 0.25)

    # FUNCTION:
    
    # Cauchy Deviates:
    Z = .rtCopula(n = n, rho = rho, nu = 1)
    
    # Return Value:
    Z
}


# ------------------------------------------------------------------------------


.pcauchyCopula = 
function(u = 0.5, v = u, rho = 0.75, output = c("vector", "list") )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Student-t copula probability
    
    # Arguments:
    #   see function 'pellipticalCopula'
    
    # FUNCTION:
    
    # Cauchy Probability:
    C.uv = .ptCopula(u = u, v = v, rho = rho, nu = 1, output = output)
    attr(C.uv, "control") <- c(rho = rho)
    
    # Return Value:
    C.uv
}


# ------------------------------------------------------------------------------


.dcauchyCopula = 
function(u = 0.5, v = u, rho = 0.75, nu = 4, output = c("vector", "list") )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Student-t copula density
    
    # Arguments:
    #   see function 'dellipticalCopula'
    
    # FUNCTION:
    
    # Cauchy Density:
    c.uv = .dtCopula(u = u, v = v, rho = rho, nu = 1, output = output)
    attr(c.uv, "control") <- c(rho = rho)
    
    # Return Value:
    c.uv
}


################################################################################
# ELLIPTICAL COPULAE PROBABILIY:


pellipticalCopula =
function(u = 0.5, v = u, rho = 0.75, param = NULL, type = c("norm", "cauchy", 
"t", "logistic", "laplace", "kotz", "epower"), output = c("vector", "list"), 
border = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes extreme value copula probability
    
    # Arguments:
    #   u, v - two numeric values or vectors of the same length at
    #       which the copula will be computed. If 'u' is a list then the
    #       the '$x' and '$y' elements will be used as 'u' and 'v'.
    #       If 'u' is a two column matrix then the first column will
    #       be used as 'u' and the the second as 'v'.
    #   rho - a numeric value setting the coorelation strength, ranging
    #       between minus one and one.
    #   param - distributional parameters, the number of degrees of 
    #       freedom for the Student-t copulae.
    #   type - the type of the elliptical copula. Either "norm" or
    #       "t" denoting the normal or Student-t copula, respectively.
    #   output - a character string specifying how the output should
    #       be formatted. By default a vector of the same length as 
    #       'u' and 'v'. If specified as "list" then 'u' and 'v' are
    #       expected to span a two-dimensional grid as outputted by the 
    #       function 'grid2d' and the function returns a list with
    #       elements '$x', 'y', and 'z' which can be directly used 
    #       for example by 2D plotting functions. 
    
    # Value:
    #   returns a vector or list of probabilities depending on the
    #   value of the "output" variable.
    
    # FUNCTION:

    # Match Arguments:
    type = match.arg(type)
    output = match.arg(output)
    
    # Settings:
    subdivisions = 100
    if (is.list(u)) {
        v = u[[2]]
        u = u[[1]]
    }
    if (is.matrix(u)) {
        v = u[, 2]
        u = u[, 1]
    }
    if (length(u) == 1 & u[1] > 1) {
        return(.pellipticalCopulaGrid(N = u, rho, param, type, border = border))
    }
    
    # Parameters:
    if (type == "t") if (is.null(param)) param = c(nu = 4)
    if (type == "kotz") if (is.null(param)) param = c(r = 1)
    if (type == "epower") if (is.null(param)) param = c(r = 1, s = 1)
    
    # Specical Copulae:
    if (type == "norm") {
        if (rho == -1) {
            ans = pfrechetCopula(u = u, v = v, type = "m", output = output)
            return(ans)
        } else if (rho == +1) {
            ans = pfrechetCopula(u = u, v = v, type = "w", output = output)
            return(ans)
        } else {
            ans = .pnormCopula(u = u, v = v, rho = rho, output = output)
            return(ans)
        }
    } else if (type == "cauchy") {
        ans = .pcauchyCopula(u = u, v = v, rho = rho, output = output)
        return(ans)
    } else if (type == "t") {
        if (is.null(param)) param = 4
        ans = .ptCopula(u = u, v = v, rho = rho, nu = param, output = output)
        return(ans)
    }
    
    # The remaining Copulae - Compute Density on Regular Grid:
    N = subdivisions
    x = (0:N)/N
    c.uv = .dellipticalCopulaGrid(N = N, rho, param, type, border = TRUE)
    c.uv$z[is.na(c.uv$z)] = 0      
    
    # Integrate to get Probability:
    C.uv = 0*c.uv$z
    for (i in 1:(N+1)) {
        D = matrix(rep(0, times = (N+1)^2), ncol = N+1)
        for (j in 1:i) {    
            D[1:i, j] = 1
            C.uv[i,j] = C.uv[j,i] = sum(D*c.uv$z)
        }
    }
    C.uv = C.uv/N^2
    
    # Take care about the Boundary on the Unit Square:
    C.uv[1, ] = C.uv[, 1] = 0
    C.uv[N+1, ] = C.uv[, N+1] = c.uv$x   
    
    # Interpolate for the desired Values on the grid:
    U0 = trunc(u*N)
    V0 = trunc(v*N)
    P = (u - U0/N)
    Q = (v - V0/N)
    U0 = U0 + 1
    U1 = U0 + 1
    V0 = V0 + 1
    V1 = V0 + 1
    C.vec = rep(NA, times = length(u))
    for ( i in 1:length(u) ) {
        p = P[i]
        q = Q[i]
        if (p == 0 & q == 0) {
            C.vec[i] = C.uv[U0[i], V0[i]] 
        } else if (p == 0 & q > 0) {
            C.vec[i] = (1-q)*C.uv[U0[i], V0[i]] + q*C.uv[U0[i], V1[i]]
        } else if (p > 0 & q == 0) {
            C.vec[i] = (1-p)*C.uv[U0[i], V0[i]] + p*C.uv[U1[i], V0[i]] 
        } else {
            C.vec[i] = (1-p)*(1-q)*C.uv[U0[i], V0[i]] + 
                p*(1-q)*C.uv[U1[i], V0[i]] + (1-p)*q*C.uv[U0[i], V1[i]] + 
                p*q*C.uv[U1[i], V1[i]] 
        }
    }
    C.uv = round(C.vec, digits = 3)
    attr(C.uv, "control") <- c(rho = rho)
    
    # As List ?
    if (output == "list") {
        N = sqrt(length(u))
        x = u[1:N]
        y = matrix(v, ncol = N)[1, ]
        C.uv = list(x = x, y = y,  z = matrix(C.uv, ncol = N))
    }
    
    # Return Value:
    C.uv 
}


# ------------------------------------------------------------------------------


pellipticalSlider =
function(type = c("persp", "contour"), B = 20)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Displays interactively plots of probability
    
    # Description:
    #   Displays interactively plots of probability
    
    # Arguments:
    #   type - a character string specifying the plot type.
    #       Either a perspective plot which is the default or
    #       a contour plot with an underlying image plot will
    #       be created.
    #   B - the maximum slider menu value when the boundary
    #       value is infinite. By default this is set to 10.
    
    # FUNCTION:
    
    # Settings:
    type = type[1]
    
    # Plot:
    if (type == "persp")
        .pellipticalPerspSlider(B = B)
    if (type == "contour")
        .pellipticalContourSlider(B = B)
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.pellipticalCopulaGrid =
function(N, rho = 0.75, param = NULL, type = c("norm", "cauchy",
"t", "logistic", "laplace", "kotz", "epower"), border = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes elliptical copula probability on a 2d grid
    
    # Arguments:
    #   see function pellipticalCopula()
    
    # FUNCTION:
    
    # Settings:
    U = (0:N)/N
    V = (1:(N-1))/N
    
    # Compute Density on Regular Grid:
    c.uv = .dellipticalCopulaGrid(N, rho, param, type, border = TRUE)
    c.uv$z[is.na(c.uv$z)] = 0      
    
    # Integrate to get Probability:
    if (TRUE) {
        C.uv = 0*c.uv$z
        for (i in 1:(N+1)) {
            for (j in 1:i) {    
                C.uv[i,j] = C.uv[j,i] = sum(c.uv$z[1:i, 1:j])
            }
        }
        C.uv = C.uv/N^2
    }
        
    if (FALSE) {
        # This is much slower !
        IJ = grid2d(1:(N+1))
        X = cbind(IJ$x, IJ$y)
        fun = function(X, C) sum(C[1:X[1], 1:X[2]])
        C.uv = apply(X, MARGIN=1, FUN = fun, C = c.uv$z)
        C.uv = matrix(C.uv, byrow = TRUE, ncol = N+1) / N^2
    } 
    
    # Probability - Take care about the Boundary on the Unit Square:
    C.uv[1, ] = C.uv[, 1] = 0
    C.uv[N+1, ] = C.uv[, N+1] = c.uv$x   
    names(C.uv) = NULL
    attr(C.uv, "control") <- c(rho = rho)
    C.uv = list(x = U, y = U, z = matrix(C.uv, ncol = length(U)))
    if (!border) {
        C.uv$z = C.uv$z[-1, ]
        C.uv$z = C.uv$z[-N, ]
        C.uv$z = C.uv$z[, -1]
        C.uv$z = C.uv$z[, -N]
        C.uv$x = C.uv$y = V
    }

    # Return Value:
    C.uv 
}


# ------------------------------------------------------------------------------


.pellipticalCopulaDiag =
function(N, rho = 0.75, param = NULL, type = c("norm", "cauchy",
"t", "logistic", "laplace", "kotz", "epower"), border = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes elliptical diagonal cross section copula probability 
    
    # Arguments:
    #   see function pellipticalCopula()
    
    # FUNCTION:
    
    # Settings:
    U = (0:N)/N
    V = (1:(N-1))/N
    
    # Compute Density on Regular Grid:
    c.uv = .dellipticalCopulaGrid(N, rho, param, type[1], border = TRUE)
    c.uv$z[is.na(c.uv$z)] = 0      
    
    # Integrate to get Probability:
    C.uu = 0*U
    for (i in 1:(N+1)) {
        C.uu[i] = sum(c.uv$z[1:i, 1:i])
    }
    C.uu = C.uu/N^2
    names(C.uu) = NULL
    attr(C.uu, "control") <- c(rho = rho)
    
    if (border) {
        C.uu = list(x = U, y = C.uu)
    } else {
        C.uu = list(x = V, y = C.uu[c(-1,-(N+1))])
    }

    # Return Value:
    C.uu
}


# ------------------------------------------------------------------------------


.pellipticalPerspSlider =
function(B = 20)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Displays interactively perspective plots of probability
    
    # FUNCTION:
    
    # Graphic Frame:
    par(mfrow = c(1, 1), cex = 0.7)
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        Copula = .sliderMenu(no = 1)
        N = .sliderMenu(no = 2)
        rho = .sliderMenu(no = 3)
        nu = .sliderMenu(no = 4)
        s = .sliderMenu(no = 5)
        theta = .sliderMenu(no = 6)
        phi = .sliderMenu(no = 7)
        r = 1
        
        # Title:
        Names = 
            c("- Normal", "- Student t", "- Logistic", "- Exponential Power")   
        if (nu == 1) Names[2] = "- Student-t [Cauchy]"  
        if (s == 0.5) Names[4] = "- Exponential Power [Laplace]" 
        if (s == 1) Names[4] = "- Exponential Power [Kotz|Normal]"
        Title = paste("Elliptical Copula No:", as.character(Copula), 
            Names[Copula], "\nrho = ", as.character(rho)) 
        if (Copula == 2) Title = paste(Title, "nu =", as.character(nu))
        if (Copula == 4) Title = paste(Title, "s =", as.character(s))
        
        # Plot: 
        Type = c("norm", "t", "logistic", "epower")
        param = NULL
        if (Copula == 2) param = nu
        if (Copula == 4) param = c(r, s)
        P = .pellipticalCopulaGrid(N = N, rho = rho, param = param, 
            type = Type[Copula], border = TRUE)
        persp(P, theta = theta, phi = phi, col = "steelblue", shade = 0.5,
            ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
            zlab = "C(u, v)", xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1) )
        title(main = Title)
        Tau = as.character(round(2*asin(rho)/pi, 2))
        mTitle = paste("Tau", Tau)
        mtext(mTitle, side = 4, col = "grey", cex = 0.7)
        mTitle = paste("1: Normal | 2: Student-t [Cauchy] | 3: Logistic |", 
            "4: Exponential Power [Laplace|Kotz]")
        mtext(mTitle, side = 1, line = 3, col = "grey", cex = 0.7)
                           
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    plot.names = c("Plot - theta", "... phi")
    .sliderMenu(refresh.code,
        names = c("Copula",  "N", "rho", "2: nu", "4: s", plot.names),
        minima      = c( 1,   10, -0.95,       1,    0.1,  -180,    0),
        maxima      = c( 4,  100,  0.95,       B,      5,   180,  360),
        resolutions = c( 1,   10,  0.05,     0.1,    0.1,     1,    1),
        starts      = c( 1,   20,  0.50,       4,      1,   -40,   30)) 
}


# ------------------------------------------------------------------------------


.pellipticalContourSlider =
function(B = 20)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Displays interactively perspective plots of probability
    
    #FUNCTION:
    
    # Graphic Frame:
    par(mfrow = c(1, 1), cex = 0.7)
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        Copula = .sliderMenu(no = 1)
        N = .sliderMenu(no = 2)
        rho = .sliderMenu(no = 3)
        nu = .sliderMenu(no = 4)
        s = .sliderMenu(no = 5)
        nlev = .sliderMenu(no = 6)
        ncol = .sliderMenu(no = 7)
        r = 1
        
        # Title:
        Names = 
            c("- Normal", "- Student t", "- Logistic", "- Exponential Power")   
        if (nu == 1) Names[2] = "- Student-t [Cauchy]"  
        if (s == 0.5) Names[4] = "- Exponential Power [Laplace]" 
        if (s == 1) Names[4] = "- Exponential Power [Kotz|Normal]"
        Title = paste("Elliptical Copula No:", as.character(Copula), 
            Names[Copula], "\nrho = ", as.character(rho)) 
        if (Copula == 2) Title = paste(Title, "nu =", as.character(nu))
        if (Copula == 4) Title = paste(Title, "s =", as.character(s))
        
        # Plot: 
        Type = c("norm", "t", "logistic", "epower")
        param = NULL
        if (Copula == 2) param = nu
        if (Copula == 4) param = c(r, s)
        P = .pellipticalCopulaGrid(N = N, rho = rho, param = param, 
            type = Type[Copula], border = FALSE)
        image(P, col = heat.colors(ncol), ylab = "v")
        mtext("u", side = 1, line = 2, cex = 0.7)
        contour(P, nlevels = nlev, add = TRUE)
        title(main = Title)
        Tau = as.character(round(2*asin(rho)/pi, 2))
        mTitle = paste("Tau", Tau)
        mtext(mTitle, side = 4, col = "grey", cex = 0.7)
        mTitle = paste("1: Normal | 2: Student-t [Cauchy] | 3: Logistic |", 
            "4: Exponential Power [Laplace|Kotz]")
        mtext(mTitle, side = 1, line = 3, col = "grey", cex = 0.7)
        
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    plot.names = c("Plot - levels", "... colors")
    .sliderMenu(refresh.code,
        names = c("Copula", "N", "rho", "2: nu", "4: s", plot.names),
        minima      = c( 1,  10, -0.95,       1,    0.1,     5,   12),
        maxima      = c( 4, 100,  0.95,       B,      5,   100,  256),
        resolutions = c( 1,  10,  0.05,     0.1,    0.1,     5,    4),
        starts      = c( 1,  20,  0.50,       4,      1,    10,   32)) 
}


################################################################################
# ELLIPTICAL COPULA DENSITY:


dellipticalCopula = 
function(u = 0.5, v = u, rho = 0.75, param = NULL, type = c("norm", "cauchy",
"t", "logistic", "laplace", "kotz", "epower"), output = c("vector", "list"), 
border = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes extreme value copula density
    
    # Arguments:
    #   u, v - two numeric values or vectors of the same length at
    #       which the copula will be computed. If 'u' is a list then the
    #       the '$x' and '$y' elements will be used as 'u' and 'v'.
    #       If 'u' is a two column matrix then the first column will
    #       be used as 'u' and the the second as 'v'.
    #   rho - a numeric value setting the coorelation strength, ranging
    #       between minus one and one.
    #   param - additional distributional parameters.
    #   type - the type of the elliptical copula. Either "norm" or
    #       "t" denoting the normal or Student-t copula, respectively.
    #   output - a character string specifying how the output should
    #       be formatted. By default a vector of the same length as 
    #       'u' and 'v'. If specified as "list" then 'u' and 'v' are
    #       expected to span a two-dimensional grid as outputted by the 
    #       function 'grid2d' and the function returns a list with
    #       elements '$x', 'y', and 'z' which can be directly used 
    #       for example by 2D plotting functions. 
    
    # Value:
    #   returns a vector or list of probabilities depending on the
    #   value of the "output" variable.
    
    # Example:
    #   Diagonal Value: pnormCopula((0:10)/10)
    #   persp(pnormCopula(u = grid2d(), output = "list"))
    
    # FUNCTION:
    
    # Use Grid Version?
    if (is.numeric(u)) {
        if (length(u) == 1 & u[1] > 1) {
            ans = .dellipticalCopulaGrid(N = u, rho = rho, param = param, 
                type = type, border = border)
            return(ans)
        }
    }

    # Match Arguments:
    type = match.arg(type)
    output = match.arg(output)
    
    # Settings:
    if (is.list(u)) {
        v = u[[2]]
        u = u[[1]]
    }
    if (is.matrix(u)) {
        v = u[, 2]
        u = u[, 1]
    }
    if (length(u) == 1 & u[1] > 1) {
        return(.pellipticalCopulaGrid(N = u, rho, param, type, border = border))
    }
    
    # Parameters:
    if (type == "t") if (is.null(param)) param = c(nu = 4)
    if (type == "kotz") if (is.null(param)) param = c(r = 1)
    if (type == "epower") if (is.null(param)) param = c(r = 1, s = 1)
    
    # Density:  
    x = .qelliptical(u, param = param, type = type)
    y = .qelliptical(v, param = param, type = type)
    c.uv = delliptical2d(x, y, rho = rho, param = param, type = type) / (
        .delliptical(x, param = param, type = type) * 
        .delliptical(y, param = param, type = type) )
    if (rho == 0 & type == "norm") c.uv[!is.na(c.uv)] = 1
    names(c.uv) = NULL
    attr(c.uv, "control") <- c(rho = rho)
    if (output == "list") {
        N = sqrt(length(u))
        x = u[1:N]
        y = matrix(v, ncol = N)[1, ]
        c.uv = list(x = x, y = y, z = matrix(c.uv, ncol = N))
    }
    
    # Return Value:
    c.uv
}


# ------------------------------------------------------------------------------


dellipticalSlider =
function(type = c("persp", "contour"), B = 20)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Displays interactively plots of density
    
    # Description:
    #   Displays interactively plots of density
    
    # Arguments:
    #   type - a character string specifying the plot type.
    #       Either a perspective plot which is the default or
    #       a contour plot with an underlying image plot will
    #       be created.
    #   B - the maximum slider menu value when the boundary
    #       value is infinite. By default this is set to 10.
    
    # FUNCTION:
    
    # Settings:
    type = type[1]
    
    # Plot:
    if (type == "persp")
        .dellipticalPerspSlider(B = B)
    if (type == "contour")
        .dellipticalContourSlider(B = B)
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.dellipticalCopulaGrid =
function(N, rho = 0.75, param = NULL, type = c("norm", "cauchy",
"t", "laplace", "kotz", "epower"), border = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes extreme value copula density
    
    # Arguments:
    #   N - the number of grid points is (N+1)*(N+1)
    #   rho - a numeric value setting the coorelation strength, ranging
    #       between minus one and one.
    #   param - additional distributional parameters.
    #   type - the type of the elliptical copula. Either "norm" or
    #       "t" denoting the normal or Student-t copula, respectively.
    
    # Value:
    #   returns a vector or list of probabilities depending on the
    #   value of the "output" variable.
    
    # Note:
    #   Made for the Sliders.
    
    # FUNCTION:
    
    # Settings:
    type = type[1]
    U = (0:N)/N
    V = (1:(N-1))/N

    # Reduce to Grid - speeds up the computation:
    M = N%/%2 + 1
    X = .qelliptical(U[1:M], param = param, type = type)
    if (N%%2 == 0) {
        X = c(X, rev(-X)[-1]) 
    } else {
        X = c(X, rev(-X)) 
    }
    NX = length(X)
    x = rep(X, times = NX)
    y = rep(X, each = NX)
    D = .delliptical(X, param = param, type = type)
    DX = rep(D, times = NX)
    DY = rep(D, each = NX)

    # Density:  
    c.uv = delliptical2d(x, y, rho = rho, param = param, type = type) / (DX*DY)
    if (rho == 0 & type == "norm") c.uv[!is.na(c.uv)] = 1
    c.uv[is.na(c.uv)] = 0
    names(c.uv) = NULL
    attr(c.uv, "control") <- c(rho = rho)
    c.uv = list(x = U, y = U, z = matrix(c.uv, ncol = N+1))
    if (!border) {
        c.uv$z = c.uv$z[-1, ]
        c.uv$z = c.uv$z[-N, ]
        c.uv$z = c.uv$z[, -1]
        c.uv$z = c.uv$z[, -N]
        c.uv = list(x = V, y = V, z = matrix(c.uv$z, ncol = N-1))
    }
    
    # Return Value:
    c.uv
}


# ------------------------------------------------------------------------------


.dellipticalCopulaDensityNorm.RUnit = 
function()
{
    require(adapt)
    
    # Normal Distribution:
    Mean = mean(.dellipticalCopulaGrid(N = 100, rho = 0.5, param = NULL, 
        type = "norm", border = FALSE)$z)   
    print(Mean)
    Density = function(z, rho) 
        dellipticalCopula(z[1], z[2], rho = rho)
    Integrated = adapt(2, lower = c(0, 0), upper = c(1, 1), functn = Density, 
        rho = 0.5, eps = 0.0001)$value
    print(Integrated)
        
    # Student-T Distribution:
    Mean = mean(.dellipticalCopulaGrid(N = 100, rho = 0.5, param = 4, 
        type = "t", border = FALSE)$z)  
    print(Mean)
    Density = function(z, rho) 
        dellipticalCopula(z[1], z[2], rho = rho, param = 4, type = "t")
    Integrated = adapt(2, lower = c(0, 0), upper = c(1, 1), functn = Density, 
        rho = 0.5, eps = 0.0001)$value
    print(Integrated)
    
    # Logistic Distribution:
    Mean = mean(.dellipticalCopulaGrid(N = 100, rho = 0.5, param = NULL, 
        type = "logistic", border = FALSE)$z)   
    print(Mean)
    Density = function(z, rho) 
        dellipticalCopula(z[1], z[2], rho = rho, param = NULL, type = "logistic")
    Integrated = adapt(2, lower = c(0, 0), upper = c(1, 1), functn = Density, 
        rho = 0.5, eps = 0.0001)$value
    print(Integrated)
    
    # Exponential Power Distribution:
    Mean = mean(.dellipticalCopulaGrid(N = 100, rho = 0.5, 
        param = c(r = 1, s = 1), type = "epower", border = FALSE)$z)  
    print(Mean)
    Density = function(z, rho) 
        dellipticalCopula(z[1], z[2], rho = rho, param = NULL, type = "logistic")
    Integrated = adapt(2, lower = c(0, 0), upper = c(1, 1), functn = Density, 
        rho = 0.5, eps = 0.0001)$value
    print(Integrated)
    
}


# ------------------------------------------------------------------------------


.dellipticalCopula.RUnit = 
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   RUnit Test for elliptical copulae
    
    # FUNCTION:
    
    # Settings:
    N = 10
    uv = grid2d((1:(N-1))/N)
    
    # Normal Copula:
    c.uv = dellipticalCopula(uv, rho = 0.75, type = "norm", output = "list")
    persp(c.uv, theta = -40, phi = 30, ticktype = "detailed", col = "steelblue",
        main = "Normal Copula | rho = 0.75", cex = 0.5)
    
    # Cauchy Copula:
    c.uv = dellipticalCopula(uv, rho = 0.75, type = "cauchy", output = "list")
    persp(c.uv, theta = -40, phi = 30, ticktype = "detailed", col = "steelblue",
        main = "Cauchy Copula | rho = 0.75", cex = 0.5)
    
    # Student-t Copula:
    c.uv = dellipticalCopula(uv, rho = 0.75, type = "t", output = "list")
    persp(c.uv, theta = -40, phi = 30, ticktype = "detailed", col = "steelblue",
        main = "Student-t Copula | rho = 0.75", cex = 0.5)
    
    # Logistic Copula:
    c.uv = dellipticalCopula(uv, rho = 0.75, type = "logistic", output = "list")
    persp(c.uv, theta = -40, phi = 30, ticktype = "detailed", col = "steelblue",
        main = "Logistic Copula | rho = 0.75", cex = 0.5)
    c.uv = .dellipticalCopulaGrid(N, rho = 0.75, 
        type = "logistic", border = FALSE)
    persp(c.uv, theta = -40, phi = 30, ticktype = "detailed", col = "steelblue",
        main = "Logistic Copula | rho = 0.75", cex = 0.5)
    
    # Laplace Copula:
    c.uv = dellipticalCopula(uv, rho = 0.75, type = "laplace", output = "list")
    persp(c.uv, theta = -40, phi = 30, ticktype = "detailed", col = "steelblue",
        main = "Laplace Copula | rho = 0.75", cex = 0.5)
    
    # Original Kotz Copula:
    c.uv = dellipticalCopula(uv, rho = 0.75, type = "kotz", output = "list")
    persp(c.uv, theta = -40, phi = 30, ticktype = "detailed", col = "steelblue",
        main = "Kotz Copula | rho = 0.75", cex = 0.5)
    
    # Exponential Power Copula:
    c.uv = dellipticalCopula(uv, rho = 0.75, type = "epower", output = "list")
    persp(c.uv, theta = -40, phi = 30, ticktype = "detailed", col = "steelblue",
        main = "Exponential Power Copula | rho = 0.75", cex = 0.5)
    c.uv = .dellipticalCopulaGrid(N, rho = 0.75, param = c(2, 2), 
        type = "epower", border = FALSE)
    persp(c.uv, theta = -40, phi = 30, ticktype = "detailed", col = "steelblue",
        main = "Exponential Power Copula | rho = 0.75", cex = 0.5)
        
    # Return value:
    invisible()
}


# ------------------------------------------------------------------------------


.dellipticalPerspSlider =
function(B = 20)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Displays interactively perspective plots of density
    
    # FUNCTION:
    
    # Graphic Frame:
    par(mfrow = c(1, 1), cex = 0.7)
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        Copula = .sliderMenu(no = 1)
        N = .sliderMenu(no = 2)
        rho = .sliderMenu(no = 3)
        nu = .sliderMenu(no = 4)
        s = .sliderMenu(no = 5)
        theta = .sliderMenu(no = 6)
        phi = .sliderMenu(no = 7)
        r = 1
        
        # Title:
        Names = 
            c("- Normal", "- Student t", "- Logistic", "- Exponential Power")   
        if (nu == 1) Names[2] = "- Student-t [Cauchy]"  
        if (s == 0.5) Names[4] = "- Exponential Power [Laplace]" 
        if (s == 1) Names[4] = "- Exponential Power [Kotz|Normal]"
        Title = paste("Elliptical Copula Density No:", as.character(Copula), 
            Names[Copula], "\nrho = ", as.character(rho)) 
        if (Copula == 2) Title = paste(Title, "nu =", as.character(nu))
        if (Copula == 4) Title = paste(Title, "s =", as.character(s))
        
        # Plot: 
        uv = grid2d(x = (1:(N-1))/N)
        Type = c("norm", "t", "logistic", "epower")
        param = NULL
        if (Copula == 2) param = nu
        if (Copula == 4) param = c(r, s)
        D = .dellipticalCopulaGrid(N, rho = rho, param = param, 
            type = Type[Copula], border = FALSE)
        Integrated = as.character(round(mean(D$z),2))
        Var = var(as.vector(D$z), na.rm = TRUE)
        if (Var < 1.0e-6) {
            # A flat perspective plot fails, if zlim is not specified!
            Mean = round(1.5*mean(as.vector(D$z), na.rm = TRUE), 2)
            persp(D, theta = theta, phi = phi, col = "steelblue", shade = 0.5,
                ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
                zlim = c(0, Mean), zlab = "C(u,v)" )
        } else {
            persp(D, theta = theta, phi = phi, col = "steelblue", shade = 0.5,
                ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
                zlab = "C(u,v)" )
        }
        title(main = Title)
        Tau = as.character(round(2*asin(rho)/pi, 2))
        mTitle = paste("Mean: ", Integrated, " |  Tau", Tau)
        mtext(mTitle, side = 4, col = "grey", cex = 0.7)
        mTitle = paste("1: Normal | 2: Student-t [Cauchy] | 3: Logistic |", 
            "4: Exponential Power [Laplace|Kotz]")
        mtext(mTitle, side = 1, col = "grey", cex = 0.7)
                           
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    plot.names = c("Plot - theta", "... phi")
    .sliderMenu(refresh.code,
        names = c("Copula", "N", "rho", "3: nu", "4: s", plot.names),
        minima      = c( 1,  10, -0.95,       1,   0.1,  -180,    0),
        maxima      = c( 4, 100,  0.95,       B,     5,   180,  360),
        resolutions = c( 1,  10,  0.05,     0.1,   0.1,     1,    1),
        starts      = c( 1,  20,  0.50,       4,     1,   -40,   30)) 
}


# ------------------------------------------------------------------------------


.dellipticalContourSlider =
function(B = 20)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Displays interactively perspective plots of density
    
    #FUNCTION:
    
    # Graphic Frame:
    par(mfrow = c(1, 1), cex = 0.7)
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        Copula = .sliderMenu(no = 1)
        N = .sliderMenu(no = 2)
        rho = .sliderMenu(no = 3)
        nu = .sliderMenu(no = 4)
        s = .sliderMenu(no = 5)
        nlev = .sliderMenu(no = 6)
        ncol = .sliderMenu(no = 7)
        if (rho == 0 & Copula == 1) return(invisible())
        r = 1
        
        # Title:
        Names = 
            c("- Normal", "- Student t", "- Logistic", "- Exponential Power")   
        if (nu == 1) Names[2] = "- Student-t [Cauchy]"  
        if (s == 0.5) Names[4] = "- Exponential Power [Laplace]" 
        if (s == 1) Names[4] = "- Exponential Power [Kotz|Normal]"
        Title = paste("Elliptical Copula Density No:", as.character(Copula), 
            Names[Copula], "\nrho = ", as.character(rho)) 
        if (Copula == 2) Title = paste(Title, "nu =", as.character(nu))
        if (Copula == 4) Title = paste(Title, "s =", as.character(s))
        
        # Plot: 
        uv = grid2d(x = (0:N)/N)
        Type = c("norm", "t", "logistic", "laplace", "kotz", "epower")
        param = NULL
        if (Copula == 2) param = nu
        if (Copula == 5) param = r
        if (Copula == 6) param = c(r, s)
        D = .dellipticalCopulaGrid(N, rho = rho, param = param, 
            type = Type[Copula], border = FALSE)
        Integrated = as.character(round(mean(D$z),2))
        image(D, col = heat.colors(ncol), ylab = "v",
            xlim = c(0,1), ylim = c(0,1) )
        mtext("u", side = 1, line = 2, cex = 0.7)
        contour(D, nlevels = nlev, add = TRUE)
        title(main = Title)
        Tau = as.character(round(2*asin(rho)/pi, 2))
        mTitle = paste("Mean: ", Integrated, " |  Tau", Tau)
        mtext(mTitle, side = 4, col = "grey", cex = 0.7)
        mTitle = paste("1: Normal | 2: Student-t [Cauchy] | 3: Logistic |", 
            "4: Exponential Power [Laplace|Kotz]")
        mtext(mTitle, side = 1, line =  3, col = "grey", cex = 0.7)
        
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    plot.names = c("Plot - levels", "... colors")
    .sliderMenu(refresh.code,
        names = c("Copula", "N", "rho", "2: nu", "4: s", plot.names),
        minima      = c( 1,  10, -0.95,       1,    0.1,     5,   12),
        maxima      = c( 4, 100,  0.95,       B,      5,   100,  256),
        resolutions = c( 1,  10,  0.05,     0.1,    0.1,     5,    4),
        starts      = c( 1,  20,  0.50,       4,      1,    10,   32)) 
}


################################################################################
# COPULA DEPENDENCE MEASURES:


ellipticalTau =
function(rho)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Kendall's tau for elliptical copulae
    
    # Arguments:
    #   rho - a numeric value setting the coorelation strength, ranging
    #       between minus one and one.

    # FUNCTION:
    
    # Compute Kendall's Tau:
    ans = 2 * asin(rho) / pi
    attr(ans, "control") = c(rho = rho)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


ellipticalRho =
function(rho, param = NULL, type = c("norm", "cauchy", "t", "logistic", 
"laplace", "kotz", "epower"), subdivisions = 500)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Spearman's rho for elliptical copulae
    
    # Arguments:
    #   rho - a numeric value setting the coorelation strength, ranging
    #       between minus one and one.

    # FUNCTION:
    
    # Settings:
    Type = c("Normal Copula", "Cauchy Copula", "Student-t Copula", 
        "Logistic Copula", "Laplace Copula", "Kotz Copula", 
        "Exponential Power Copula")
    names(Type) = c("norm", "cauchy", "t", "logistic", "laplace", 
        "kotz", "epower")
    type = type[1]
    Type = Type[type]
    
    # Compute Spearman's Rho:
    ans.norm = round(6 * asin(rho/2) / pi, 2)
    
    # Spearman's Rho:
    N = subdivisions
    Pi = pfrechetCopula(u = grid2d((1:(N-1))/N), type = "pi", output = "list")
    D = .dellipticalCopulaGrid(N = N, rho = rho, param = param, 
        type = type, border = FALSE)
    ans = round(12*mean(Pi$z*D$z)-3, 2)
    names(ans) = NULL
    
    # Add Control Attribute:
    control = c(rho = rho, param = param, type = type, tau = 2*asin(rho)/pi)
    attr(ans, "control")<-unlist(control)
    
    # Return Value:
    ans
}



# ------------------------------------------------------------------------------


ellipticalTailCoeff =
function(rho, param = NULL, type = c("norm", "cauchy", "t", "logistic",
"laplace", "kotz", "epower"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes tail dependence for elliptical copulae
    
    # Arguments:
    #   rho - a numeric value setting the coorelation strength, ranging
    #       between minus one and one.

    # FUNCTION:
    
    # Match Arguments:
    type = match.arg(type)
    
    # Check:
    stopifnot(length(rho) == 1)
    
    # Compute Tail Dependence:
    lambda = 0
    if (type == "cauchy") {
        nu = 1 
        arg = sqrt(nu+1) * sqrt(1-rho) / sqrt(1+rho)
        lambda = 2 * (1 - pt(arg, df = nu+1))
    }
    if (type == "t") {
        nu = param
        if (is.null(nu)) nu = 4
        arg = sqrt(nu+1) * sqrt(1-rho) / sqrt(1+rho)
        lambda = 2 * (1 - pt(arg, df = nu+1))
    }
    if (type == "kotz" & is.null(param)) {
        param = c(r = 1)
    }
    if (type == "epower" & is.null(param)) {
        param = c(r = 1, s = 1)
    }
    
    # Result:
    ans = lambda
    attr(ans, "control") = c(rho = rho, type = type)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


ellipticalTailPlot =
function(param = NULL, type = c("norm", "cauchy", "t", "logistic", 
"laplace", "kotz", "epower"), tail = c("Upper", "Lower"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plots tail dependence for elliptical copulae
    
    # Arguments:
    #   rho - a numeric value setting the coorelation strength, ranging
    #       between minus one and one.

    # FUNCTION:
    
    # Match Arguments:
    type = match.arg(type)
    tail = match.arg(tail)
    
    # Settings:
    Title = c("Normal", "Cauchy", "Student-t", "Logistic", "Laplace",
        "Kotz", "Exponential Power")
    Title = paste(Title, "Copula")
    names(Title) = c("norm", "cauchy", "t", "logistic", "laplace", 
        "kotz", "epower")
    Title = Title[type]
    tail = tail[1]
    N = 1000; Points = 20 # don't change these values!
    u = (0:N)/N
    SHOW = N+1
    
    # Parameters:
    if (type == "t" & is.null(param)) {
        param = c(nu = 4)
    }
    if (type == "kotz" & is.null(param)) {
        param = c(r = 1)
    }
    if (type == "epower" & is.null(param)) {
        param = c(r = 1, s = 1)
    }
    
    # Plot Frame:
    if (type == "t") 
        Title = paste(Title, "| nu =", as.character(param))
    if (type == "t") 
        Title = paste(Title, "| r =", as.character(param))
    if (type == "epower") 
        Title = paste(Title, "| r =", as.character(param[1]), 
            " s =", as.character(param[2]))
    plot(c(0,1), c(0,1), type = "n", main = Title, xlab = "u", 
        ylab = paste(tail, "Tail Dependence"))
        
    # Cauchy Tail dependence:
    if (type == "cauchy") {
        type = "t"
        param = c(nu = 1)
    }
    
    # Iterate rho:
    Rho = c(-0.99, seq(-0.9, 0.9, by = 0.3), 0.99)
    for (rho in Rho) {
        
        # Compute Tail Coefficient:
        lambda = ellipticalTailCoeff(rho = rho, param = param, type = type)
        
        # Compute Copula Cross Section C(u,u)"
        if (type == "norm") 
            C.uu = pellipticalCopula(u, rho = rho, type = type)
        if (type == "t") 
            C.uu = .ptCopula(u = u, v = u, rho = rho, nu = param)
        if (type == "logistic" | type == "laplace" | type == "kotz" | 
            type == "epower")
            C.uu = .pellipticalCopulaDiag(N, rho = rho, param = param, 
                type = type)$y
        
        # Compute Copula Tail dependence lambda:
        if (tail == "Upper") {
            lambdaTail = (1-2*u+C.uu)/(1-u)
        } else if (tail == "Lower") {
            lambdaTail = C.uu/u
        }
        
        # Define Plot Elements:
        if (abs(rho) < 0.05) {
            color = "black"
            linetype = 1
        } else if (abs(rho) > 0.95) {
            color = "blue" 
            linetype = 1
        } else {
            color = "black"
            linetype = 3
        }
        
        # Normal Tail Dependence:
        if (type == "norm") { 
            lines(u, lambdaTail, type = "l", lty = linetype, col = color) 
        }
        
        # Cauchy and Student-t Tail Dependence:
        if (type == "t") {
            if (tail == "Upper") 
                lines(u[u < 0.99], lambdaTail[u<0.99], lty = linetype, 
                    col = color)
            if (tail == "Lower") 
                lines(u[u > 0.01], lambdaTail[u>0.01], lty = linetype, 
                    col = color)
        }
        
        # Logistic Tail dependence:
        if (type == "logistic" | type == "laplace" | type == "kotz") {
            if (tail == "Lower") {
                SHOW = which.min(lambdaTail[-1])
                lines(u[SHOW:(N+1)], lambdaL[SHOW:(N+1)], type = "l", 
                    lty = linetype, col = color)
            }       
            if (tail == "Upper") {
                SHOW = which.min(lambdaTail[-(N+1)])
                lines(u[1:SHOW], lambdaTail[1:SHOW], type = "l", 
                    lty = linetype, col = color)    
            }
        }
        
        # Add rho Labels
        text(x = 0.5, y = lambdaTail[floor(N/2)]+0.05, col = "red", cex = 0.7,
            labels = as.character(round(rho, 2)))
            
        # Add Points to Curves: 
        if (tail == "Upper") {
            M = min(SHOW, N)
            Index = seq(1, M, by = Points)
            X = 1
        } else if (tail == "Lower") {
            M = max(51, SHOW)
            Index = rev(seq(N+1, M, by = -Points))
            X = 0
        }
        points(u[Index], lambdaTail[Index], pch = 19, cex = 0.7)
        
        # Add Tail Coefficient:
        points(x = X, y = lambda[1], pch = 19, col = "red")
        
    }
    points(1, 0, pch = 19, col = "red")
    abline(h = 0, lty = 3, col = "grey")
    abline(v = X, lty = 3, col = "grey")
    
    # Return Value:
    invisible()
}


################################################################################


ellipticalCopulaSim = 
function (n, rho = 0.75, param = NULL, type = c("norm", "cauchy", "t")) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Simulates bivariate elliptical Copula
    
    # Match Arguments:
    type = match.arg(type)
      
    # "norm" Random Deviates:  
    if (type == "norm") {
        ans = .rnormCopula(n = n, rho = rho)
    }
    
    # "cauchy" Random Deviates:
    if (type == "cauchy") {
        ans = .rcauchyCopula(n = n, rho = rho)
    }
    
    # "t" Random Deviates:
    if (type == "t") {
        if (is.null(param)) {
            param = c(nu = 4)
        } else {
            param = c(nu = param[1])
        }
        ans = .rtCopula(n = n, rho = rho, nu = param)
    }
     
    # "logistic" Random Deviates:
    # NOT YET IMPLEMENTED ...
    
    # "laplace" Random Deviates:
    # NOT YET IMPLEMENTED ...

    
    # "kotz" Random Deviates:
    # NOT YET IMPLEMENTED ...

    # "epower" Random Deviates:
    # NOT YET IMPLEMENTED ...
 
    # Control:
    control = list(rho = rho, param = param, type = type)
    attr(ans, "control") = unlist(control)
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------

    
ellipticalCopulaFit =
function(u, v = NULL, type = c("norm", "cauchy", "t"), ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Fits the paramter of an elliptical copula
    
    # Note:
    #   The upper limit for nu is 100
    
    # FUNCTION:
    
    # Match Arguments:
    type = match.arg(type)
    
    # Settings:
    U = u
    V = v
    if (is.list(u)) {
        u = u[[1]]
        v = u[[2]]
    }
    if (is.matrix(u)) {
        U = u[, 1]
        V = u[, 2]
    }
    U <<- u
    V <<- v

    # Estimate Rho from Kendall's tau for all types of Copula:
    tau = cor(x = U, y = V, method = "kendall") #[1, 2]
    Rho = rho = sin((pi*tau/2))
     
    # Estimate "norm" Copula:
    if (type == "norm") {
        fun = function(x) {
            -mean( log(.dnormCopula(u = U, v = V, rho = x)) )
        }
        fit = nlminb(start = rho, objective = fun, lower = -1, upper = 1, ...)
    }
    
    # Estimate "cauchy" Copula:
    if (type == "cauchy") {
        fun = function(x) {
            -mean( log(.dcauchyCopula(u = U, v = V, rho = x)) ) 
        }
        fit = nlminb(start = rho, objective = fun, lower = -1, upper = 1, ...)
    }
    
    # Estimate "t" Copula:
    if (type == "t") {
        fun = function(x) {
            -mean( log(.dtCopula(u = U, v = V, rho = x[1], nu = x[2])) ) 
        }
        fit = nlminb(start = c(rho = rho, nu = 4), objective = fun, 
             lower = c(-1, 1), upper = c(1, Inf), ...)
        fit$Nu = 4
    }
    
    # Estimate "logistic" Copula:
    if (type == "logistic") {
        # NOT YET IMPLEMENTED ...
        fun = function(x) {
            -mean( log(dellipticalCopula(u = U, v = V, ...)) ) 
        }
        fit = nlminb(start = c(), objective = fun, 
             lower = c(rho = -1, NA), upper = c(rho = 1, NA), ...)
    }
    
    # Estimate "laplace" Copula:
    if (type == "laplace") {
        # NOT YET IMPLEMENTED ...
        fun = function(x) {
            -mean( log(dellipticalCopula(u = U, v = V, ...)) ) 
        }
        fit = nlminb(start = c(), objective = fun, 
             lower = c(rho = -1, NA), upper = c(rho = 1, NA), ...)
    }
    
    # Estimate "kotz" Copula:
    if (type == "kotz") {
        # NOT YET IMPLEMENTED ...
        fun = function(x) {
            -mean( log(dellipticalCopula(u = U, v = V, ...)) ) 
        }
        fit = nlminb(start = c(), objective = fun, 
             lower = c(rho = -1, NA), upper = c(rho = 1, NA), ...)
    }
    
    # Estimate "epower" Copula:
    if (type == "epower") {
        # NOT YET IMPLEMENTED ...
        fun = function(x) {
            -mean( log(dellipticalCopula(u = U, v = V, ...)) ) 
        }
        fit = nlminb(start = c(), objective = fun, 
             lower = c(rho = -1, NA), upper = c(rho = 1, NA), ...)
    }
    
    # Keep Start Value:
    # fit$Rho = Rho
    
    # Return Value:
    fit
}


################################################################################

