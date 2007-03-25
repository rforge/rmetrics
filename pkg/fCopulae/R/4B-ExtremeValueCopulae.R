
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
#                       X     NOT YET IMPEMENTED
# FUNCTION:                  EXTREME VALUE COPULAE PARAMETER:
#  .evParam                   Sets parameters for an extreme value copula
#  .evRange             X
#  .evCheck             X
# FUNCTION:                  EXTREME VALUE COPULAE GENERATOR FUNCTION:
#  Afunc                      Computes Dependence function
#  AfuncSlider                Displays interactively dependence function
#  .AfuncFirstDer              Computes Derivative of dependence function
#  .AfuncSecondDer             Computes 2nd Derivative of dependence function
# FUNCTION                   KENDALL'S TAU AND SPEARMAN'S RHO:
#  evTau                X      Returns Kendall's tau for extreme value copulae
#  evRho                X      Returns Spearman's rho for extreme value copulae
# FUNCTION:                  EXTREME VALUE COPULAE TAIL DEPENDENCE:
#  evTailCoeff          X     Computes tail dependence for extreme value copulae
#  evTailPlot           X     Plots extreme value tail dependence function
# FUNCTION:                  EXTREME VALUE COPULAE RANDOM VARIATES:
#  revCopula            X     Generates extreme value copula random variates 
#  revSlider            X
# FUNCTION:                  EXTREME VALUE COPULAE PROBABILIY:
#  pevCopula                  Computes extreme value copula probability
#  .pev1Copula                 EV copula probability via dependence function
#  .pev2Copula                 EV copula probability direct computation
#  .pevContourSlider           Interactive contour plots of EV probability
#  .pevPerspSlider             Interactive perspective plots of EV probability
# FUNCTION:                  EXTREME VALUE COPULAE DENSITY:
#  devCopula                  Computes extreme value copula density
#  .dev1Copula                 EV copula density via dependence function
#  .dev2Copula                 EV copula density direct computation
#  .devContourSlider           Interactive contour plots of EV density
#  .devPerspSlider             Interactive perspective plots of EV density
# FUNCTION:                  EXTREME VALUE COPULA PARAMETER FITTING:
#  evCopulaSim          X      Simulates bivariate extreme value copula
#  evCopulaFit          X      Fits the paramter of an extreme value copula
################################################################################


################################################################################
# EXTREME VALUE COPULAE:

    
.evParam =
function(type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Sets default parameters for extreme value copulae
    
    # Arguments:
    #   type - a character string naming the copula. By default the
    #       "gumbel" copula will be chosen.
    
    # Value:
    #   returns a list with two elements, 'param' sets the parameters
    #       which may be a vector, 'range' the range with minimum and
    #       maximum values for each of the parameters. For the "pi" and
    #       "m" copula NULL will be returned.
    
    # FUNCTION:
    
    # Settings:
    type = match.arg(type)
    ans = list(copula = type)
    
    # Select:
    if ( type == "gumbel" ) {
        ans$param = c(delta = 2)
        ans$range = c(1, Inf) }
    if ( type == "galambos" ) {
        ans$param = c(delta = 2)
        ans$range = c(0, Inf) }
    if ( type == "husler.reiss" ) {
        ans$param = c(delta = 2) 
        ans$range = c(0, Inf) }
    if ( type == "tawn" ) {
        ans$param = c(alpha = 2, beta = 1/2, r = 2)
        ans$range = c(0, 1, 0, 1, 1, Inf) }
    if ( type == "bb5" ) {
        ans$param = c(delta = 2, theta = 2)
        ans$range = c(0, Inf, 0, Inf) }
        
    # Some more, yet untested and undocumented:
    if ( type == "gumbelII" ) {
        ans$param = c(alpha = 2)
        ans$range = NULL }
    if ( type == "marshall.olkin" ) {
        ans$param = c(alpha1 = 2, alpha2 = 2)
        ans$range = NULL }
    if ( type == "pi" ) {
        ans$param = NULL
        ans$range = NULL }
    if ( type == "m" ) {
        ans$param = NULL
        ans$range = NULL }
            
    # Return Value: 
    ans
}


    
################################################################################
# DEPENDENCE FUNCTION:


Afunc = 
function(x, param = NULL, type = c("gumbel", "galambos", 
"husler.reiss", "tawn", "bb5"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes dependence function for extreme value copulae
    
    # Arguments:
    #   x - a numeric vector, with values ranging between 
    #       zero and one
    #   param - numeric parameter vector, if set to NULL then
    #       default values are taken
    #   type - character string naming the type of copula,
    #       by default "gumbel"
    
    # Details:
    #   Extreme Value Copulae can be represented in the form
    #
    #      C(u,v) = exp { log(uv)*A[log(u)/log(uv)] }
    #
    #   where A:[0,1] -> [1/2,1] is a convex function
    #   such that max(x,1-x) < A(x) < 1 for all x in [0,1].
    
    # Notes:
    #   Copulae included also in EVANESCE:
    #       gumbel, galambos, husler.reiss, tawn, bb5 
    #   Additionally - not yet tested and documented
    #       gumbelII, marshall.olkin, pi[Cperp], m[Cplus]
    
    # References:
    #   Bouye E. (2000), Copulas for Finance: A Reading Guide and
    #       Some Applications, (see the Table on page 49).
    #   Insightful Corp, EVANESCE Implementation in S-PLUS 
    #       FinMetrics Module.
   
    # FUNCTION:
    
    # Missing x:
    if (missing(x)) x = (0:10)/10
    
    # Type:
    type = type[1]
    if (is.null(param)) param = .evParam(type)$param
    names(param) = names(.evParam(type)$param)
    
    # Compute Dependence Function:
    if (type == "gumbel") {
        # 1 <= alpha < Inf
        alpha = param[1]    
        if (alpha == 1) A = rep(1, times = length(x)) else 
        A = 
       (x^alpha + (1-x)^alpha)^(1/alpha) 
    }    
    if (type == "galambos") {
        # 0 <= alpha < Inf
        alpha = param[1]   
        A = 1 - (x^(-alpha) + (1-x)^(-alpha))^(-1/alpha) 
    }   
    if (type == "husler.reiss") {
        # 0 <= alpha <= Inf
        alpha = param[1]    
        A =   
        x * pnorm(1/alpha + 0.5*alpha*log(x/(1-x))) + 
        (1-x) * pnorm(1/alpha - 0.5*alpha*log(x/(1-x))) 
    }    
    if (type == "tawn") {
        # 0 <= alpha <=1
        # 0 <= beta <= 1
        # 1 <= r < Inf
        alpha = param[1]
        beta = param[2]
        r = param[3]   
        if (alpha == 0 | beta == 0 | r == 1) A = rep(1, times = length(x)) else 
        A = 
        1 - beta +(beta-alpha)*x + ( (alpha*x)^r + (beta*(1-x))^r )^(1/r) 
    }    
    if (type == "bb5") {
        # 0 < delta < Inf
        # 1 <= theta Inf
        delta = param[1]
        theta = param[2]    
        if (theta == 1) return(Afunc(x, param, "galambos")) else
        A = 
        ( x^theta + (1-x)^theta - 
        ( x^(-delta*theta) + (1-x)^(-delta*theta) )^(-1/delta))^(1/theta) 
    }   
    
    # Some more, yet untested and undocumented:
    if (type == "gumbelII") {
        # 0 <= alpha < Inf
        alpha = param[1]
        A = alpha*x^2 - alpha*x + 1
    }    
    if (type == "marshall.olkin") {
        alpha1 = param[1]
        alpha2 = param[2]
        A = NULL
        for (i in 1:length(x)) A = c(A, max(1-alpha1*x[i], 1-alpha2*(1-x[i]))) 
    }
    if (type == "pi" || type == "Cperp") {
        # No parameters
        A = rep(1, times = length(x))
    }   
    if (type == "m" || type == "Cplus") {
        # No parameters
        A = NULL
        for (i in 1:length(x)) A = c(A, max(x[i], 1-x[i])) 
    }  
     
    # Result:
    attr(A, "control") <- unlist(list(param = param, type = type))
    
    # Return Value:
    A   
}
       

# ------------------------------------------------------------------------------


.AfuncFirstDer = 
function(x, param = NULL, type = c("gumbel", "galambos", "husler.reiss", 
"tawn", "bb5"), eps = 1.0e-6 )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   #   Computes derivaive of dependence function
    
    # Arguments:
    #   x - a numeric vector, with values ranging between 
    #       zero and one
    #   param - numeric parameter vector, if set to NULL then
    #       default values are taken
    #   type - character string naming the type of copula,
    #       by default "gumbel"
    
    # Details:
    #   Extreme Value Copulae can be represented in the form
    #
    #      C(u,v) = exp { log(uv)*A[log(u)/log(uv)] }
    #
    #   where A:[0,1] -> [1/2,1] is a convex function
    #   such that max(x,1-x) < A(x) < 1 for all x in [0,1].
    
    # Notes:
    #   Copulae included also in EVANESCE:
    #       gumbel, galambos, husler.reiss, tawn, bb5 
    #   Additionally - not yet tested and documented
    #       gumbelII, marshall.olkin, pi[Cperp], m[Cplus]
    
    # References:
    #   Bouye E. (2000), Copulas for Finance: A Reading Guide and
    #       Some Applications, (see the Table on page 49).
    #   Insightful Corp, EVANESCE Implementation in S-PLUS 
    #       FinMetrics Module.
    
    # FUNCTION:
    
    # Missing x:
    if (missing(x)) x = (0:10)/10
    
    # Type:
    type = type[1]
    if (is.null(param)) param = .evParam(type)$param
    names(param) = names(.evParam(type)$param)
    
    # Settings for Maple Output:
    Pi = pi 
    ln = function(x) { log(x) }
    erf = function (x) { 2*pnorm(sqrt(2)*x)-1 }
    
    # Compute Derivative:   
    if (type == "gumbel") {
        # alpha >= 1
        alpha = param[1]        
        # Maple Generated Output:
        if (alpha == 1) A1 = rep(0, times = length(x)) else {
        A1 = 
        (x^alpha+(1-x)^alpha)^(1/alpha)/alpha*(x^alpha*alpha/x-(1-x)^alpha*
        alpha/(1-x))/(x^alpha+(1-x)^alpha)
        A1[x < eps] = -1
        A1[x > 1-eps] = 1 }
    }
    if (type == "galambos") {
        # 0 <= alpha < Inf
        alpha = param[1]    
        # Maple Generated Output:
        if (alpha == 0) A1 = rep(1, times = length(x)) else {
        A1 = 
        (x^(-alpha)+(1-x)^(-alpha))^(-1/alpha)/alpha*(-x^(-alpha)*alpha/x+(
        1-x)^(-alpha)*alpha/(1-x))/(x^(-alpha)+(1-x)^(-alpha))
        A1[x < eps] = -1
        A1[x > 1-eps] = 1 }
    }   
    if (type == "husler.reiss") {
        # 0 <= alpha <= Inf
        alpha = param[1]    
        # Maple Generated Output:
        if (alpha == 0) A1 = rep(1, times = length(x)) else {
        A1 = 
        .5*erf(1/2*(1/alpha+.5*alpha*ln(x/(1-x)))*2^(1/2))+.2500000000/Pi^(
        1/2)*exp(-1/2*(1/alpha+.5*alpha*ln(x/(1-x)))^2)*alpha*(1/(1-x)+x/(1
        -x)^2)*(1-x)*2^(1/2)-.5*erf(1/2*(1/alpha-.5*alpha*ln(x/(1-x)))*2^(1
        /2))-.2500000000*(1-x)^2/Pi^(1/2)*exp(-1/2*(1/alpha-.5*alpha*ln(x/(
        1-x)))^2)*alpha*(1/(1-x)+x/(1-x)^2)/x*2^(1/2)
        A1[x < eps] = -1
        A1[x > 1-eps] = 1 }
    }    
    if (type == "tawn") {
        # 0 <= alpha < Inf
        # beta <= 1
        # 1 <= r < Inf
        alpha = param[1]
        beta = param[2]
        r = param[3]    
        # Maple Generated Output:
        if (alpha == 0 | beta == 0 | r == 1) A1 = rep(0, length(x)) else {
        A1 = 
        beta-alpha+((alpha*x)^r+(beta*(1-x))^r)^(1/r)/r*((alpha*x)^r*r/x-(
        beta*(1-x))^r*r/(1-x))/((alpha*x)^r+(beta*(1-x))^r)
        A1[x < eps] = -alpha
        A1[x > 1-eps] = beta }
    }    
    if (type == "bb5") {
        # 0 < delta < Inf 
        # 1 <= theta < Inf
        delta = param[1]
        theta = param[2]    
        # Maple Generated Output:
        if (theta == 1) return(.AfuncFirstDer(x, param, "galambos")) else
        A1 = 
        (x^theta+(1-x)^theta-(x^(-delta*theta)+(1-x)^(-delta*theta))^(-1/
        delta))^(1/theta)/theta*(x^theta*theta/x-(1-x)^theta*theta/(1-x)+(x
        ^(-delta*theta)+(1-x)^(-delta*theta))^(-1/delta)/delta*(-x^(-delta*
        theta)*delta*theta/x+(1-x)^(-delta*theta)*delta*theta/(1-x))/(x^(-
        delta*theta)+(1-x)^(-delta*theta)))/(x^theta+(1-x)^theta-(x^(-delta
        *theta)+(1-x)^(-delta*theta))^(-1/delta))
        A1[x < eps] = -1
        A1[x > 1-eps] = 1
    } 
    
    # Some more, yet untested and undocumented:   
    if (type == "gumbelII") {
        # 0 <= alpha < Inf
        alpha = param[1]
        A1 = 2*alpha*x-alpha
    }   
    if (type == "marshall.olkin") {
        alpha1 = param[1]
        alpha2 = param[2]
        A1 = NULL
        for (i in 1:length(x)) {
            if (x[i] < 0) A1 = c(A1, -alpha1)
            if (x[i] > 0) A1 = c(A1, alpha2)
            if (x[i] == 0) A1 = c(A1, NA) }
    }    
    if (type == "pi" || type == "Cperp") {
        A1 = rep(0, times = length(x)) 
    }
    if (type == "m" || type == "Cplus") {
        A1 = sign(x-1/2) 
    }
    
    # Result:
    attr(A1, "control") <- unlist(list(param = param, type = type))
    
    # Return Value:
    A1  
}


# ------------------------------------------------------------------------------


.AfuncSecondDer = 
function(x, param = NULL, type = c("gumbel", "galambos", "husler.reiss", 
"tawn", "bb5"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes 2nd derivative of dependence function
    
    # Arguments:
    #   x - a numeric vector, with values ranging between 
    #       zero and one
    #   param - numeric parameter vector, if set to NULL then
    #       default values are taken
    #   type - character string naming the type of copula,
    #       by default "gumbel"
    
    # Details:
    #   Extreme Value Copulae can be represented in the form
    #
    #      C(u,v) = exp { log(uv)*A[log(u)/log(uv)] }
    #
    #   where A:[0,1] -> [1/2,1] is a convex function
    #   such that max(x,1-x) < A(x) < 1 for all x in [0,1].
    
    # Note:
    #   The five Copulae considered in EVANESCE are:
    #       gumbel, galambos, husler.reis, tawn, bb5
    #   Furthermore, added are:
    #       pi|Cperp, gumbelII, marshall.olkin, m|Cplus
    
    # References:
    #   Bouye E. (2000), Copulas for Finance: A Reading Guide and
    #       Some Applications, (see the Table on page 49).
    #   Insightful Corp, EVANESCE Implementation in S-PLUS 
    #       FinMetrics Module.
    
    # FUNCTION:
    
    # Missing x:
    if (missing(x)) x = (0:10)/10
    
    # Type:
    type = type[1]
    if (is.null(param)) param = .evParam(type)$param
    names(param) = names(.evParam(type)$param)
    
    # Settings for Maple Output:
    Pi = pi 
    ln = function(x) { log(x) }
    erf = function (x) { 2*pnorm(sqrt(2)*x)-1 }
    
    # Compute 2nd Derivative:    
    if (type == "gumbel") {
        # alpha >= 1
        alpha = param[1]        
        # Maple Generated Output:
        if (alpha == 1) A2 = rep(0, times = length(x)) else
        A2 = 
        (x^alpha+(1-x)^alpha)^(1/alpha)/alpha^2*(x^alpha*alpha/x-(1-x)^
        alpha*alpha/(1-x))^2/(x^alpha+(1-x)^alpha)^2+(x^alpha+(1-x)^alpha)^
        (1/alpha)/alpha*(x^alpha*alpha^2/x^2-x^alpha*alpha/x^2+(1-x)^alpha*
        alpha^2/(1-x)^2-(1-x)^alpha*alpha/(1-x)^2)/(x^alpha+(1-x)^alpha)-(x
        ^alpha+(1-x)^alpha)^(1/alpha)/alpha*(x^alpha*alpha/x-(1-x)^alpha*
        alpha/(1-x))^2/(x^alpha+(1-x)^alpha)^2
    }   
    if (type == "galambos") {
        # 0 <= alpha < Inf
        alpha = param[1]    
        # Maple Generated Output:
        if (alpha == 0) A2 = rep(0, times = length(x)) else 
        if (alpha == 1) A2 = rep(2, times = length(x)) else
        A2 = 
        -(x^(-alpha)+(1-x)^(-alpha))^(-1/alpha)/alpha^2*(-x^(-alpha)*alpha/
        x+(1-x)^(-alpha)*alpha/(1-x))^2/(x^(-alpha)+(1-x)^(-alpha))^2+(x^(-
        alpha)+(1-x)^(-alpha))^(-1/alpha)/alpha*(x^(-alpha)*alpha^2/x^2+x^(
        -alpha)*alpha/x^2+(1-x)^(-alpha)*alpha^2/(1-x)^2+(1-x)^(-alpha)*
        alpha/(1-x)^2)/(x^(-alpha)+(1-x)^(-alpha))-(x^(-alpha)+(1-x)^(-
        alpha))^(-1/alpha)/alpha*(-x^(-alpha)*alpha/x+(1-x)^(-alpha)*alpha/
        (1-x))^2/(x^(-alpha)+(1-x)^(-alpha))^2
    }   
    if (type == "husler.reiss") {
        # 0 <= alpha <= Inf
        alpha = param[1]    
        # Maple Generated Output:
        if (alpha == 0) A2 = rep(0, times = length(x)) else
        A2 = 
        .2500000000/Pi^(1/2)*exp(-1/2*(1/alpha+.5*alpha*ln(x/(1-x)))^2)*
        alpha*(1/(1-x)+x/(1-x)^2)/x*(1-x)*2^(1/2)-.1250000000/Pi^(1/2)*(1/
        alpha+.5*alpha*ln(x/(1-x)))*alpha^2*(1/(1-x)+x/(1-x)^2)^2/x*(1-x)^2*
        exp(-1/2*(1/alpha+.5*alpha*ln(x/(1-x)))^2)*2^(1/2)+.2500000000/Pi^(
        1/2)*exp(-1/2*(1/alpha+.5*alpha*ln(x/(1-x)))^2)*alpha*(2/(1-x)^2+2
        *x/(1-x)^3)*(1-x)*2^(1/2)-.2500000000/Pi^(1/2)*exp(-1/2*(1/alpha+.5
        *alpha*ln(x/(1-x)))^2)*alpha*(1/(1-x)+x/(1-x)^2)*2^(1/2)+.75000000/
        Pi^(1/2)*exp(-1/2*(1/alpha-.5*alpha*ln(x/(1-x)))^2)*alpha*(1/(1-x
        )+x/(1-x)^2)/x*(1-x)*2^(1/2)-.1250000000*(1-x)^3/Pi^(1/2)*(1/alpha-
        .5*alpha*ln(x/(1-x)))*alpha^2*(1/(1-x)+x/(1-x)^2)^2/x^2*exp(-1/2*(1
        /alpha-.5*alpha*ln(x/(1-x)))^2)*2^(1/2)-.2500000000*(1-x)^2/Pi^(1/2
        )*exp(-1/2*(1/alpha-.5*alpha*ln(x/(1-x)))^2)*alpha*(2/(1-x)^2+2*x/(
        1-x)^3)/x*2^(1/2)+.2500000000*(1-x)^2/Pi^(1/2)*exp(-1/2*(1/alpha-.5
        *alpha*ln(x/(1-x)))^2)*alpha*(1/(1-x)+x/(1-x)^2)/x^2*2^(1/2)
    }  
    if (type == "tawn") {
        # 0 <= alpha, beta <= 1, 1 <= r < Inf
        alpha = param[1]
        beta = param[2]
        r = param[3]    
        # Maple Generated Output:
        if (alpha == 0 | beta == 0 | r == 1) A2 = rep(0, length(x)) else 
        A2 = 
        ((alpha*x)^r+(beta*(1-x))^r)^(1/r)/r^2*((alpha*x)^r*r/x-(beta*(1-x)
        )^r*r/(1-x))^2/((alpha*x)^r+(beta*(1-x))^r)^2+((alpha*x)^r+(beta*(1
        -x))^r)^(1/r)/r*((alpha*x)^r*r^2/x^2-(alpha*x)^r*r/x^2+(beta*(1-x))
        ^r*r^2/(1-x)^2-(beta*(1-x))^r*r/(1-x)^2)/((alpha*x)^r+(beta*(1-x))^
        r)-((alpha*x)^r+(beta*(1-x))^r)^(1/r)/r*((alpha*x)^r*r/x-(beta*(1-x
        ))^r*r/(1-x))^2/((alpha*x)^r+(beta*(1-x))^r)^2 
        # A2[x<1e-12] = 0
        # A2[x>1-1e-12] = 0 
    }
    if (type == "bb5") {
        # delta > 0, theta >= 1
        delta = param[1]
        theta = param[2]    
        # Maple Generated Output:
        if (theta == 1) return(.AfuncSecondDer(x, param, "galambos")) else
        A2 = 
        (x^theta+(1-x)^theta-(x^(-delta*theta)+(1-x)^(-delta*theta))^(-1/
        delta))^(1/theta)/theta^2*(x^theta*theta/x-(1-x)^theta*theta/(1-x)+
        (x^(-delta*theta)+(1-x)^(-delta*theta))^(-1/delta)/delta*(-x^(-
        delta*theta)*delta*theta/x+(1-x)^(-delta*theta)*delta*theta/(1-x))/
        (x^(-delta*theta)+(1-x)^(-delta*theta)))^2/(x^theta+(1-x)^theta-(x^
        (-delta*theta)+(1-x)^(-delta*theta))^(-1/delta))^2+(x^theta+(1-x)^
        theta-(x^(-delta*theta)+(1-x)^(-delta*theta))^(-1/delta))^(1/theta)/
        theta*(x^theta*theta^2/x^2-x^theta*theta/x^2+(1-x)^theta*theta^2/(
        1-x)^2-(1-x)^theta*theta/(1-x)^2-(x^(-delta*theta)+(1-x)^(-delta*
        theta))^(-1/delta)/delta^2*(-x^(-delta*theta)*delta*theta/x+(1-x)^(
        -delta*theta)*delta*theta/(1-x))^2/(x^(-delta*theta)+(1-x)^(-delta*
        theta))^2+(x^(-delta*theta)+(1-x)^(-delta*theta))^(-1/delta)/delta*
        (x^(-delta*theta)*delta^2*theta^2/x^2+x^(-delta*theta)*delta*theta/
        x^2+(1-x)^(-delta*theta)*delta^2*theta^2/(1-x)^2+(1-x)^(-delta*
        theta)*delta*theta/(1-x)^2)/(x^(-delta*theta)+(1-x)^(-delta*theta))
        -(x^(-delta*theta)+(1-x)^(-delta*theta))^(-1/delta)/delta*(-x^(-
        delta*theta)*delta*theta/x+(1-x)^(-delta*theta)*delta*theta/(1-x))^
        2/(x^(-delta*theta)+(1-x)^(-delta*theta))^2)/(x^theta+(1-x)^theta-(
        x^(-delta*theta)+(1-x)^(-delta*theta))^(-1/delta))-(x^theta+(1-x)^
        theta-(x^(-delta*theta)+(1-x)^(-delta*theta))^(-1/delta))^(1/theta)/
        theta*(x^theta*theta/x-(1-x)^theta*theta/(1-x)+(x^(-delta*theta)+(
        1-x)^(-delta*theta))^(-1/delta)/delta*(-x^(-delta*theta)*delta*
        theta/x+(1-x)^(-delta*theta)*delta*theta/(1-x))/(x^(-delta*theta)+(
        1-x)^(-delta*theta)))^2/(x^theta+(1-x)^theta-(x^(-delta*theta)+(1-x
        )^(-delta*theta))^(-1/delta))^2
    } 
    
    # Some more, yet untested and undocumented:  
    if (type == "gumbelII") {
        alpha = param[1]
        A2 = rep(2*alpha, times = length(x))
    }   
    if (type == "marshall.olkin") {
        alpha1 = param[1]
        alpha2 = param[2]
        A2 = rep(0, times = length(x))
    }    
        if (type == "pi" || type == "Cperp") {
        A2 = rep(0, times = length(x))
    }
    if (type == "m" || type == "Cplus") {
        A2 = rep(0, times = length(x)) 
    }
    
    # Result:
    attr(A2, "control") <- unlist(list(param = param, type = type))
    
    # Return Value:
    A2  
}


# ------------------------------------------------------------------------------


AfuncSlider =
function()
{   # A function implemented by Diethelm Wuertz
       
    # Description:
    #   Displays interactively the dependence function
    
    # Graphic Frame:
    par(mfrow = c(2,2), cex = 0.7)
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        Type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5")
        Copula = .sliderMenu(no = 1)
        N = .sliderMenu(no = 2)
        if (Copula <= 3) 
            param = c(delta = .sliderMenu(no = Copula + 2))
        if (Copula == 4) 
            param = c(alpha = .sliderMenu(no = 6), 
                beta = .sliderMenu(no = 7), r = .sliderMenu(no = 8))
        if (Copula == 5)   
            param = c(delta = .sliderMenu(no = 9), theta = .sliderMenu(no = 10))  
        
        # Title:
        type = Type[Copula]
        subTitle = paste(paste(names(param) , "="), param, collapse = " | " )
        Title = paste(" ", type, "\n", subTitle) 
        
        # Plot A:   
        plot(x = (0:N)/N, Afunc(x = (0:N)/N, param = param, type = type), 
            ylim = c(0.5, 1), type = "l", xlab = "x", ylab = "A", main = Title)
        lines(c(0.0, 1.0), c(1.0, 1.0), col = "steelblue", lty = 3)
        lines(c(0.0, 0.5), c(1.0, 0.5), col = "steelblue", lty = 3)
        lines(c(0.5, 1.0), c(0.5, 1.0), col = "steelblue", lty = 3)    
        points(x = c(0, 1), Afunc(x = c(0, 1), param = param, type = type), 
            col = "red")
        # Plot A':           
        plot(x = (0:N)/N, .AfuncFirstDer(x = (0:N)/N, param = param, type = type), 
            type = "l", xlab = "x", ylab = "A'", main = Title) 
        points(x = c(0, 1),
            .AfuncFirstDer(x = c(0, 1), param = param, type = type), col = "red")
        # Plot A'':         
        plot(x = (0:N)/N, .AfuncSecondDer(x = (0:N)/N, param = param, type = type), 
            type = "l", xlab = "x", ylab = "A''", main = Title) 
        points(x = c(0, 1),
            .AfuncSecondDer(x = c(0, 1), param = param, type = type), col = "red")
                           
        # Reset Frame:
        par(mfrow = c(2, 2), cex = 0.7)
    }
  
    # Open Slider Menu:
    C = c("Gumbel: delta", "Galambos: delta", "Husler-Reis: delta",
          "Tawn: alpha", "... beta", "... r", "BB5: delta", "... theta")
    .sliderMenu(refresh.code,
        names =       c("Copula", "N", C), #gal hr  tawn               bb5
        minima =      c(1,   100,  1.0, 0.00, 0.00, 0.00, 0.00, 1.0, 0.0, 1.0),
        maxima =      c(5, 10000, 10.0, 10.0, 10.0, 1.00, 1.00, 10., 10., 10.),
        resolutions = c(1,   100, 0.05, 0.05, 0.05, 0.01, 0.01, 0.1, 0.1, 0.1),
        starts =      c(1,  5000, 1.00, 0.00, 0.00, 0.00, 0.00, 1.0, 0.0, 1.0))
}


################################################################################
# KENDALL'S TAU AND SPEARMAN'S RHO:


evTau = 
function(param = NULL, type = 1:22, lower = 1.0e-10)
{
    NA
}


# ------------------------------------------------------------------------------


evRho = 
function(param = NULL, type = 1:22, method = c("integrate2d", "adapt"), 
error = 1.0e-5)
{
    NA
}


################################################################################
# EXTREME VALUE COPULAE TAIL DEPENDENCE:


evTailCoeff = 
function(param = NULL, type = 1:22)
{
    NA
}


# ------------------------------------------------------------------------------


evTailPlot = 
function(param = NULL, type = 1:22, tail = c("Upper", "Lower"))
{
    NA
}


################################################################################
# EXTREME VALUE COPULAE RANDOM VARIATES:


revCopula = 
function(n, param = NULL, type = c("gumbel", "galambos", "husler.reiss", 
"tawn", "bb5"))
{
    NA
}


# ------------------------------------------------------------------------------


revSlider =
function(B = 10)
{
    NA
}


################################################################################
# EXTREME VALUE COPULAE PROBABILITY:


pevCopula = 
function(u = 0.5, v = u, param = NULL, 
type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5"),
output = c("vector", "list"), alternative = FALSE )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes extreme value copula probability
    
    # Arguments:
    #   u, v - two numeric values or vectors of the same length at
    #       which the copula will be computed. If 'u' is a list then the
    #       the '$x' and '$y' elements will be used as 'u' and 'v'.
    #       If 'u' is a two column matrix then the first column will
    #       be used as 'u' and the the second as 'v'.
    #   param - a numeric value or vector of named parameters as 
    #       required by the copula specified by the variable 'type'.
    #       If set to NULL, then the parameters will be taken as
    #       specified by the function '.evParam'.
    #   type - the type of the maximum extreme value copula. A character
    #       string selected from: "gumbel", "galambos", "husler.reiss", 
    #       "tawn", or "bb5".
    #   output - a character string specifying how the output should
    #       be formatted. By default a vector of the same length as 
    #       'u' and 'v'. If specified as "list" then 'u' and 'v' are
    #       expected to span a two-dimensional grid as outputted by the 
    #       function 'grid2d' and the function returns a list with
    #       elements '$x', 'y', and 'z' which can be directly used 
    #       for example by 2D plotting functions.
    #   alternative - Should the probability be computed alternatively
    #       in a direct way from the probability formula or by default 
    #       via the dependency function?  
    
    # Value:
    #   returns a vector or list of probabilities depending on the
    #   value of the "output" variable.
    
    # Example:
    #   Diagonal Value: pevCopula((0:10)/10)
    #   persp(pevCopula(u=grid2d(), output="list"), theta=-40, phi=30, xlab="x")
    
    # FUNCTION:
    
    # Select Type:
    type = match.arg(type)
    
    # Compute Copula:
    if (alternative) {
        ans = .pev2Copula(u, v, param, type, output)
    } else {
        ans = .pev1Copula(u, v, param, type, output)
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


pevSlider =
function(type = c("persp", "contour"), B = 10)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Displays interactively plots of probability
    
    # Arguments:
    #   type - a character string specifying the plot type.
    #       Either a perspective plot which is the default or
    #       a contour plot with an underlying image plot will
    #       be created.
    #   B - the maximum slider menu value when the boundary
    #       value is infinite. By default this is set to 10.
    
    # Match Arguments:
    type = match.arg(type)
    
    # Plot:
    if (type == "persp")
        .pevPerspSlider(B = B)
    if (type == "contour")
        .pevContourSlider(B = B)
        
    # Return Value:
    invisible()
}
    

# ------------------------------------------------------------------------------


.pev1Copula = 
function(u = 0.5, v = u, param = NULL, 
type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5"),
output = c("vector", "list") )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes extreme value copula probability via dependency function
    
    # FUNCTION:
    
    # Match Arguments:
    type = match.arg(type)
    output = match.arg(output)
    
    # Settings:
    if (is.null(param)) {
        param = .evParam(type)$param
    }
    if (is.list(u)) {
        v = u$y
        u = u$x
    }
    if (is.matrix(u)) {
        v = u[, 2]
        u = u[, 1]
    }
      
    # Settings:
    log.u = log(u)
    log.v = log(v)
    x = log.u/(log.u+log.v)
    
    # Copula Probability:
    A = Afunc(x, param = param, type = type)
    C = exp((log.u+log.v) * A)
    names(C) = NULL
    
    # Simulates Max function:
    C = (C + abs(C))/2       
    
    # On Boundary:
    C[is.na(C)] = 0      
    C[which(u == 0)] = 0
    C[which(u == 1)] = v[which(u == 1)]
    C[which(v == 0)] = 0
    C[which(v == 1)] = u[which(v == 1)]
    C[which(u*v == 1)] = 1
    C[which(u+v == 0)] = 0
    
    # Result:
    attr(C, "control") <- unlist(list(param = param, type = type))
    
    # As List ?
    if (output == "list") {
        N = sqrt(length(u))
        x = u[1:N]
        y = matrix(v, ncol = N)[1, ]
        C = list(x = x, y = y,  z = matrix(C, ncol = N))
    }
    
    # Return Value:
    C 
}


# ------------------------------------------------------------------------------


.pev2Copula = 
function(u = 0.5, v = u, param = NULL,
type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5"),
output = c("vector", "list") )
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes extreme value copula probability directly
  
    # FUNCTION:
    
    # Match Arguments:
    type = match.arg(type)
    output = match.arg(output)
    
    # Settings:
    if (is.null(param)) {
        param = .evParam(type)$param
    }
    if (is.list(u)) {
        v = u$y
        u = u$x
    }
    if (is.matrix(u)) {
        v = u[, 2]
        u = u[, 1]
    }
       
    # Compute Probability:
    if (type == "gumbel") {
        alpha = param[1]
        C = exp(-((-log(u))^alpha + (-log(v))^alpha)^(1/alpha)) 
    }
    if (type == "galambos") {
        alpha = param[1]
        u.tilde = -log(u)
        v.tilde = -log(v)
        C = u*v*exp(((u.tilde)^(-alpha) + 
            (v.tilde)^(-alpha))^(-1/alpha)) 
    }
    if (type == "husler.reiss") {
        alpha = param[1]
        u.tilde = -log(u)
        v.tilde = -log(v)
        C = exp(-
            u.tilde * pnorm(1/alpha + 0.5*alpha*log(u.tilde/v.tilde)) -
            v.tilde * pnorm(1/alpha + 0.5*alpha*log(v.tilde/u.tilde)) ) 
    }
    if (type == "tawn") {
        b = param[1]
        a = param[2]
        r = param[3]
        log.uv = log(u*v)
        t = log(u)/log.uv
        A = 1-b+(b-a)*t+(a^r*t^r+b^r*(1-t)^r)^(1/r)
        C = exp(log.uv*A)
    }
    if (type == "bb5") {
        delta = param[1]
        theta = param[2]
        u.tilde = -log(u)
        v.tilde = -log(v)
        C = exp(-(  u.tilde^theta + v.tilde^theta - 
            ( u.tilde^(-theta*delta) + 
              v.tilde^(-theta*delta) )^(-1/delta))^(1/theta)) 
    }
    
    # Some more, yet untested and undocumented:
    if (type == "gumbelII") {
        alpha = param[1]
        C = u*v*exp(alpha*log(u)*log(v)/(log(u)+log(v))) 
    }
    if (type == "marshall.olkin") {
        a = param[1]
        b = param[2]
        C = apply(cbind(v*u^(1-a), u*v^(1-b)), 1, min) 
    }
    if (type == "pi" || type == "Cperp") {
        C = u*v
    }
    if (type == "m" || type == "Cplus") {
        C = apply(cbind(u, v), 1, min) 
    }
    
    # Simulates Max function:
    C = (C + abs(C))/2       
    
    # On Boundary:
    C[is.na(C)] = 0      
    C[which(u == 0)] = 0
    C[which(u == 1)] = v[which(u == 1)]
    C[which(v == 0)] = 0
    C[which(v == 1)] = u[which(v == 1)]
    C[which(u*v == 1)] = 1
    C[which(u+v == 0)] = 0
    
    # Result:
    attr(C, "control") <- unlist(list(param = param, type = type))
    
    # As List ?
    if (output == "list") {
        N = sqrt(length(u))
        x = u[1:N]
        y = matrix(v, ncol = N)[1, ]
        C = list(x = x, y = y,  z = matrix(C, ncol = N))
    }
    
    # Return Value:
    C 
}


# ------------------------------------------------------------------------------


.pevContourSlider =
function(B = 10)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays interactively contour plots of probability
    
    #FUNCTION:
    
    # Graphic Frame:
    par(mfrow = c(1, 1), cex = 0.7)
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        Type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5")
        Copula = .sliderMenu(no = 1)
        N = .sliderMenu(no = 2)
        if (Copula <= 3) 
            param = c(delta = .sliderMenu(no = Copula + 2))
        if (Copula == 4) 
            param = c(alpha = .sliderMenu(no = 6), 
                beta = .sliderMenu(no = 7), r = .sliderMenu(no = 8))
        if (Copula == 5)   
            param = c(delta = .sliderMenu(no = 9), theta = .sliderMenu(no = 10)) 
        nlev = .sliderMenu(no = 11)
        ncol = .sliderMenu(no = 12) 
        
        # Title:
        type = Type[Copula]
        subTitle = paste(paste(names(param) , "="), param, collapse = " | " )
        Title = paste(" ", type, "\n", subTitle) 
        
        # Plot:   
        uv = grid2d(x = (0:N)/N)
        D = .pev1Copula(u = uv, type = type, param = param, output = "list")
        image(D, col = heat.colors(ncol) )
        contour(D, nlevels = nlev, add = TRUE)
        title(main = Title)
                           
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    C = c("1 Gumbel: delta", "2 Galambos: delta", "3 Husler-Reis: delta",
          "4 Tawn: alpha", "... beta", "... r", "5 BB5: delta", "... theta", 
          "Plot - levels", "... colors")
    .sliderMenu(refresh.code,
        names = c("Copula","N", C), #gal   hr   tawn          bb5    nlev  ncol
        minima =      c(1,  10,   1,   0,   0,   0,   0,  1,  0,  1,   5,   12),
        maxima =      c(5, 100,   B,   B,   B,   1,   1,  B,  B,  B, 100,  256),
        resolutions = c(1,   1, .05, .05, .05, .01, .01, .1, .1, .1,   5,    1),
        starts =      c(1,  25,   2,   1,   1,  .5,  .5,  2,  1,  2,  10,   12))
}


# ------------------------------------------------------------------------------


.pevPerspSlider =
function(B = 10)
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
        Type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5")
        Copula = .sliderMenu(no = 1)
        N = .sliderMenu(no = 2)
        if (Copula <= 3) 
            param = c(delta = .sliderMenu(no = Copula + 2))
        if (Copula == 4) 
            param = c(alpha = .sliderMenu(no = 6), 
                beta = .sliderMenu(no = 7), r = .sliderMenu(no = 8))
        if (Copula == 5)   
            param = c(delta = .sliderMenu(no = 9), theta = .sliderMenu(no = 10)) 
        theta = .sliderMenu(no = 11)
        phi = .sliderMenu(no = 12) 
        
        # Title:
        type = Type[Copula]
        subTitle = paste(paste(names(param) , "="), param, collapse = " | " )
        Title = paste(" ", type, "\n", subTitle) 
        
        # Plot:   
        uv = grid2d(x = (0:N)/N)
        D =  .pev1Copula(u = uv, type = type, param = param, output = "list")
        #D2 = .pev2Copula(u = uv, type = type, param = param, output = "list")
        persp(D, theta = theta, phi = phi, col = "steelblue", shade = 0.5,
            ticktype = "detailed", cex = 0.5)
        title(main = Title)
                           
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    C = c("1 Gumbel: delta", "2 Galambos: delta", "3 Husler-Reis: delta",
          "4 Tawn: alpha", "... beta", "... r", "5 BB5: delta", "... theta", 
          "Plot - theta", "... phi")
    .sliderMenu(refresh.code,
        names = c("Copula", "N", C), #gal  hr  tawn          bb5    theta  phi
        minima =      c(1,  10,   1,   0,   0,   0,   0,  1,  0,  1, -180,   0),
        maxima =      c(5, 100,   B,   B,   B,   1,   1,  B,  B,  B,  180, 360),
        resolutions = c(1,   1, .05, .05, .05, .01, .01, .1, .1, .1,    1,   1),
        starts =      c(1,  25,   2,   1,   1,  .5,  .5,  2,  1,  2,  -40,  30))
}


################################################################################
# EXTREME VALUE COPULAE DENSITY:


devCopula =
function(u = 0.5, v = u, param = NULL, 
type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5"),
output = c("vector", "list"), alternative = FALSE )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes extreme value copula density from dependence function
    
    # Arguments:
    #   u, v - two numeric values or vectors of the same length at
    #       which the copula will be computed. If 'u' is a list then the
    #       the '$x' and '$y' elements will be used as 'u' and 'v'.
    #       If 'u' is a two column matrix then the first column will
    #       be used as 'u' and the the second as 'v'.
    #   param - a numeric value or vector of named parameters as 
    #       required by the copula specified by the variable 'type'.
    #       If set to NULL, then the parameters will be taken as
    #       specified by the function '.evParam'.
    #   type - the type of the maximum extreme value copula. A character
    #       string selected from: "gumbel", "galambos", "husler.reiss", 
    #       "tawn", or "bb5".
    #   output - a character string specifying how the output should
    #       be formatted. By default a vector of the same length as 
    #       'u' and 'v'. If specified as "list" then 'u' and 'v' are
    #       expected to span a two-dimensional grid as outputted by the 
    #       function 'grid2d' and the function returns a list with
    #       elements '$x', 'y', and 'z' which can be directly used 
    #       for example by 2D plotting functions.
    #   alternative - Should the density be computed alternatively
    #       in a direct way from the probability formula or by default 
    #       via the dependency function?  
    
    # Value:
    #   returns a vector or list of density values depending on the
    #   value of the "output" variable.
    
    # Example:
    #   Diagonal Value: devCopula((0:10)/10)
    #   persp(devCopula(u=grid2d(), output="list"), theta=-40, phi=30, xlab="x")
    
    # FUNCTION:
    
    # Match Arguments:
    type = match.arg(type)
    output = match.arg(output)
    
    # Copula Density:
    if (alternative) {
        ans = .dev2Copula(u, v, param, type, output)
    } else {
        ans = .dev1Copula(u, v, param, type, output)
    }
    
    # Return Value:
    ans
}
       
    
# ------------------------------------------------------------------------------


devSlider =
function(type = c("persp", "contour"), B = 10)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Displays interactively plots of probability
    
    # Arguments:
    #   type - a character string specifying the plot type.
    #       Either a perspective plot which is the default or
    #       a contour plot with an underlying image plot will
    #       be created.
    #   B - the maximum slider menu value when the boundary
    #       value is infinite. By default this is set to 10.
    
    # Match Arguments:
    type = match.arg(type)
    
    # Plot:
    if (type == "persp")
        .devPerspSlider(B = B)
    if (type == "contour")
        .devContourSlider(B = B)
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.dev1Copula =
function(u = 0.5, v = u, param = NULL, 
type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5"),
output = c("vector", "list") )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes extreme value copula density from dependence function
    
    # Example:
    #   Diagonal Value: devCopula((0:10)/10)
    #   persp(devCopula(u=grid2d(), output="list"), theta=-40, phi=30, xlab="x")
    
    # FUNCTION:
    
    # Match Arguments:
    type = match.arg(type)
    output = match.arg(output)
    
    # Settings:
    if (is.null(param)) {
        param = .evParam(type)$param
    }
    if (is.list(u)) {
        v = u$y
        u = u$x
    }
    if (is.matrix(u)) {
        v = u[, 2]
        u = u[, 1]
    }
    
    # Settings for Maple Output:
    Pi = pi 
    ln = function(x) { log(x) }
    erf = function (x) { 2*pnorm(sqrt(2)*x)-1 }
       
    # Further Settings:
    log.u = log(u)
    log.v = log(v)
    x = log.u/(log.u+log.v)
    y = log.v/(log.u+log.v)
    
    # Copula Probability:
    A = Afunc(x, param = param, type = type)
    A1 = .AfuncFirstDer(x, param = param, type = type)
    A2 = .AfuncSecondDer(x, param = param, type = type)
    
    # Prefactor:
    P = pevCopula(u, v, param = param, type = type) / (u*v)
    c.uv = P * (( -x*y/(log.u+log.v))*A2 + (A+y*A1)*(A-x*A1) )
    c.uv[which(u*v == 0 | u*v == 1)] = 0
    
    # Result:
    attr(c.uv, "control") <- unlist(list(param = param, type = type))
    
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


# ------------------------------------------------------------------------------


.dev2Copula = 
function(u = 0.5, v = u, param = NULL, 
type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5"),  
output = c("vector", "list") ) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes extreme value copula density directly
    
    # Details:
    #   List - 9 Types:
    #   pi[Cperp], gumbel, gumbelII, galambos, husler.reiss,
    #   tawn, bb5, marshall.olkin, m[Cplus]
    
    # References:
    #   Carmona, Evanesce
    
    # FUNCTION:
    
    # Match Arguments:
    type = match.arg(type)
    output = match.arg(output)
    
    # Settings:
    if (is.null(param)) {
        param = .evParam(type)$param
    }
    if (is.list(u)) {
        v = u$y
        u = u$x
    }
    if (is.matrix(u)) {
        v = u[, 2]
        u = u[, 1]
    }
    
    # Settings:
    if (is.null(param)) param = .evParam[[type]]
    Pi = pi 
    ln = function(x) { log(x) }
    erf = function (x) { 2*pnorm(sqrt(2)*x)-1 }
       
    # Compute Probability:
    if (type == "gumbel") {
        alpha = param[1]
        # Maple Generated Output:
        c.uv =
        -((-ln(u))^alpha+(-ln(v))^alpha)^(1/alpha)*(-ln(v))^alpha/v/ln(v)/(
        (-ln(u))^alpha+(-ln(v))^alpha)^2*(-ln(u))^alpha/u/ln(u)*exp(-((-ln(
        u))^alpha+(-ln(v))^alpha)^(1/alpha))+((-ln(u))^alpha+(-ln(v))^alpha
        )^(1/alpha)*(-ln(u))^alpha/u/ln(u)/((-ln(u))^alpha+(-ln(v))^alpha)^
        2*exp(-((-ln(u))^alpha+(-ln(v))^alpha)^(1/alpha))*(-ln(v))^alpha*
        alpha/v/ln(v)+(((-ln(u))^alpha+(-ln(v))^alpha)^(1/alpha))^2*(-ln(u)
        )^alpha/u/ln(u)/((-ln(u))^alpha+(-ln(v))^alpha)^2*(-ln(v))^alpha/v/
        ln(v)*exp(-((-ln(u))^alpha+(-ln(v))^alpha)^(1/alpha))
    }
    if (type == "galambos") {
        alpha = param[1]
        # Maple Generated Output:
        c.uv = 
        exp(((-ln(u))^(-alpha)+(-ln(v))^(-alpha))^(-1/alpha))+((-ln(u))^(-
        alpha)+(-ln(v))^(-alpha))^(-1/alpha)*(-ln(v))^(-alpha)/ln(v)/((-ln(
        u))^(-alpha)+(-ln(v))^(-alpha))*exp(((-ln(u))^(-alpha)+(-ln(v))^(-
        alpha))^(-1/alpha))+((-ln(u))^(-alpha)+(-ln(v))^(-alpha))^(-1/alpha
        )*(-ln(u))^(-alpha)/ln(u)/((-ln(u))^(-alpha)+(-ln(v))^(-alpha))*exp(
        ((-ln(u))^(-alpha)+(-ln(v))^(-alpha))^(-1/alpha))+((-ln(u))^(-
        alpha)+(-ln(v))^(-alpha))^(-1/alpha)*(-ln(v))^(-alpha)/ln(v)/((-ln(
        u))^(-alpha)+(-ln(v))^(-alpha))^2*(-ln(u))^(-alpha)/ln(u)*exp(((-ln
        (u))^(-alpha)+(-ln(v))^(-alpha))^(-1/alpha))+((-ln(u))^(-alpha)+(-
        ln(v))^(-alpha))^(-1/alpha)*(-ln(u))^(-alpha)/ln(u)/((-ln(u))^(-
        alpha)+(-ln(v))^(-alpha))^2*exp(((-ln(u))^(-alpha)+(-ln(v))^(-alpha
        ))^(-1/alpha))*(-ln(v))^(-alpha)*alpha/ln(v)+(((-ln(u))^(-alpha)+(-
        ln(v))^(-alpha))^(-1/alpha))^2*(-ln(u))^(-alpha)/ln(u)/((-ln(u))^(-
        alpha)+(-ln(v))^(-alpha))^2*(-ln(v))^(-alpha)/ln(v)*exp(((-ln(u))^(
        -alpha)+(-ln(v))^(-alpha))^(-1/alpha))
    }
    if (type == "husler.reiss") {
        # Maple Generated Output:
        c.uv =
        (-.2500000000/u/Pi^(1/2)*exp(-1/2*(1/alpha+.5*alpha*ln(ln(u)/ln(v))
        )^2)*alpha/v/ln(v)*2^(1/2)+.1250000000/Pi^(1/2)*(1/alpha+.5*alpha*
        ln(ln(u)/ln(v)))*alpha^2/v/ln(v)*exp(-1/2*(1/alpha+.5*alpha*ln(ln(u
        )/ln(v)))^2)/u*2^(1/2)-.2500000000/v/Pi^(1/2)*exp(-1/2*(1/alpha+.5*
        alpha*ln(ln(v)/ln(u)))^2)*alpha/u/ln(u)*2^(1/2)+.1250000000/Pi^(1/2
        )*(1/alpha+.5*alpha*ln(ln(v)/ln(u)))*alpha^2/v*exp(-1/2*(1/alpha+.5
        *alpha*ln(ln(v)/ln(u)))^2)/u/ln(u)*2^(1/2))*exp(.5*ln(u)*(erf(1/2*(
        1/alpha+.5*alpha*ln(ln(u)/ln(v)))*2^(1/2))+1)+.5*ln(v)*(erf(1/2*(1/
        alpha+.5*alpha*ln(ln(v)/ln(u)))*2^(1/2))+1))+(.5/u*(erf(1/2*(1/
        alpha+.5*alpha*ln(ln(u)/ln(v)))*2^(1/2))+1)+.2500000000/Pi^(1/2)*
        exp(-1/2*(1/alpha+.5*alpha*ln(ln(u)/ln(v)))^2)*alpha/u*2^(1/2)-.25*
        ln(v)/Pi^(1/2)*exp(-1/2*(1/alpha+.5*alpha*ln(ln(v)/ln(u)))^2)*
        alpha/u/ln(u)*2^(1/2))*(-.2500000000*ln(u)/Pi^(1/2)*exp(-1/2*(1/
        alpha+.5*alpha*ln(ln(u)/ln(v)))^2)*alpha/v/ln(v)*2^(1/2)+.5/v*(erf(
        1/2*(1/alpha+.5*alpha*ln(ln(v)/ln(u)))*2^(1/2))+1)+.2500000000/Pi^(
        1/2)*exp(-1/2*(1/alpha+.5*alpha*ln(ln(v)/ln(u)))^2)*alpha/v*2^(1/2)
        )*exp(.5*ln(u)*(erf(1/2*(1/alpha+.5*alpha*ln(ln(u)/ln(v)))*2^(1/2))
        +1)+.5*ln(v)*(erf(1/2*(1/alpha+.5*alpha*ln(ln(v)/ln(u)))*2^(1/2))+1
        ))
    }
    if (type == "tawn") {
        # 0 <= alpha, beta <= 1, 1 <= r < Inf
        b = param[1]
        a = param[2]
        r = param[3]    
        # Maple Generated Output:
        c.uv =  
        (-(b-a)/u/ln(u*v)^2/v+2*(b-a)*ln(u)/ln(u*v)^3/u/v+(a^r*(ln(u)/ln(u*
        v))^r+b^r*(1-ln(u)/ln(u*v))^r)^(1/r)/r^2*(-a^r*(ln(u)/ln(u*v))^r*r/
        ln(u*v)/v+b^r*(1-ln(u)/ln(u*v))^r*r*ln(u)/ln(u*v)^2/v/(1-ln(u)/ln(u
        *v)))/(a^r*(ln(u)/ln(u*v))^r+b^r*(1-ln(u)/ln(u*v))^r)^2*(a^r*(ln(u)
        /ln(u*v))^r*r*(1/u/ln(u*v)-ln(u)/ln(u*v)^2/u)/ln(u)*ln(u*v)+b^r*(1-
        ln(u)/ln(u*v))^r*r*(-1/u/ln(u*v)+ln(u)/ln(u*v)^2/u)/(1-ln(u)/ln(u*v
        )))+(a^r*(ln(u)/ln(u*v))^r+b^r*(1-ln(u)/ln(u*v))^r)^(1/r)/r*(-a^r*(
        ln(u)/ln(u*v))^r*r^2/v*(1/u/ln(u*v)-ln(u)/ln(u*v)^2/u)/ln(u)+a^r*(
        ln(u)/ln(u*v))^r*r*(-1/u/ln(u*v)^2/v+2*ln(u)/ln(u*v)^3/u/v)/ln(u)*
        ln(u*v)+a^r*(ln(u)/ln(u*v))^r*r*(1/u/ln(u*v)-ln(u)/ln(u*v)^2/u)/ln(
        u)/v+b^r*(1-ln(u)/ln(u*v))^r*r^2*ln(u)/ln(u*v)^2/v/(1-ln(u)/ln(u*v)
        )^2*(-1/u/ln(u*v)+ln(u)/ln(u*v)^2/u)+b^r*(1-ln(u)/ln(u*v))^r*r*(1/u
        /ln(u*v)^2/v-2*ln(u)/ln(u*v)^3/u/v)/(1-ln(u)/ln(u*v))-b^r*(1-ln(u)/
        ln(u*v))^r*r*(-1/u/ln(u*v)+ln(u)/ln(u*v)^2/u)/(1-ln(u)/ln(u*v))^2*
        ln(u)/ln(u*v)^2/v)/(a^r*(ln(u)/ln(u*v))^r+b^r*(1-ln(u)/ln(u*v))^r)-
        (a^r*(ln(u)/ln(u*v))^r+b^r*(1-ln(u)/ln(u*v))^r)^(1/r)/r*(a^r*(ln(u)
        /ln(u*v))^r*r*(1/u/ln(u*v)-ln(u)/ln(u*v)^2/u)/ln(u)*ln(u*v)+b^r*(1-
        ln(u)/ln(u*v))^r*r*(-1/u/ln(u*v)+ln(u)/ln(u*v)^2/u)/(1-ln(u)/ln(u*v
        )))/(a^r*(ln(u)/ln(u*v))^r+b^r*(1-ln(u)/ln(u*v))^r)^2*(-a^r*(ln(u)/
        ln(u*v))^r*r/ln(u*v)/v+b^r*(1-ln(u)/ln(u*v))^r*r*ln(u)/ln(u*v)^2/v/
        (1-ln(u)/ln(u*v))))*exp(ln(u*v)-b+(b-a)*ln(u)/ln(u*v)+(a^r*(ln(u)/
        ln(u*v))^r+b^r*(1-ln(u)/ln(u*v))^r)^(1/r))+(1/u+(b-a)/u/ln(u*v)-(b-
        a)*ln(u)/ln(u*v)^2/u+(a^r*(ln(u)/ln(u*v))^r+b^r*(1-ln(u)/ln(u*v))^r
        )^(1/r)/r*(a^r*(ln(u)/ln(u*v))^r*r*(1/u/ln(u*v)-ln(u)/ln(u*v)^2/u)/
        ln(u)*ln(u*v)+b^r*(1-ln(u)/ln(u*v))^r*r*(-1/u/ln(u*v)+ln(u)/ln(u*v)
        ^2/u)/(1-ln(u)/ln(u*v)))/(a^r*(ln(u)/ln(u*v))^r+b^r*(1-ln(u)/ln(u*v
        ))^r))*(1/v-(b-a)*ln(u)/ln(u*v)^2/v+(a^r*(ln(u)/ln(u*v))^r+b^r*(1-
        ln(u)/ln(u*v))^r)^(1/r)/r*(-a^r*(ln(u)/ln(u*v))^r*r/ln(u*v)/v+b^r*(
        1-ln(u)/ln(u*v))^r*r*ln(u)/ln(u*v)^2/v/(1-ln(u)/ln(u*v)))/(a^r*(ln(
        u)/ln(u*v))^r+b^r*(1-ln(u)/ln(u*v))^r))*exp(ln(u*v)-b+(b-a)*ln(u)/
        ln(u*v)+(a^r*(ln(u)/ln(u*v))^r+b^r*(1-ln(u)/ln(u*v))^r)^(1/r))
    }
    if (type == "bb5") {
        # delta > 0, theta >= 1
        delta = param[1]
        theta = param[2]  
        # Maple Generated Output:
        c.uv = 
        -((-ln(u))^theta+(-ln(v))^theta-((-ln(u))^(-theta*delta)+(-ln(v))^(
        -theta*delta))^(-1/delta))^(1/theta)/theta^2*((-ln(v))^theta*theta/
        v/ln(v)-((-ln(u))^(-theta*delta)+(-ln(v))^(-theta*delta))^(-1/delta
        )*(-ln(v))^(-theta*delta)*theta/v/ln(v)/((-ln(u))^(-theta*delta)+(-
        ln(v))^(-theta*delta)))/((-ln(u))^theta+(-ln(v))^theta-((-ln(u))^(-
        theta*delta)+(-ln(v))^(-theta*delta))^(-1/delta))^2*((-ln(u))^theta
        *theta/u/ln(u)-((-ln(u))^(-theta*delta)+(-ln(v))^(-theta*delta))^(-
        1/delta)*(-ln(u))^(-theta*delta)*theta/u/ln(u)/((-ln(u))^(-theta*
        delta)+(-ln(v))^(-theta*delta)))*exp(-((-ln(u))^theta+(-ln(v))^
        theta-((-ln(u))^(-theta*delta)+(-ln(v))^(-theta*delta))^(-1/delta))
        ^(1/theta))-((-ln(u))^theta+(-ln(v))^theta-((-ln(u))^(-theta*delta)
        +(-ln(v))^(-theta*delta))^(-1/delta))^(1/theta)/theta*(-((-ln(u))^(
        -theta*delta)+(-ln(v))^(-theta*delta))^(-1/delta)*(-ln(v))^(-theta*
        delta)*theta^2/v/ln(v)/((-ln(u))^(-theta*delta)+(-ln(v))^(-theta*
        delta))^2*(-ln(u))^(-theta*delta)/u/ln(u)-((-ln(u))^(-theta*delta)+
        (-ln(v))^(-theta*delta))^(-1/delta)*(-ln(u))^(-theta*delta)*theta^2
        /u/ln(u)/((-ln(u))^(-theta*delta)+(-ln(v))^(-theta*delta))^2*(-ln(v
        ))^(-theta*delta)*delta/v/ln(v))/((-ln(u))^theta+(-ln(v))^theta-((-
        ln(u))^(-theta*delta)+(-ln(v))^(-theta*delta))^(-1/delta))*exp(-((-
        ln(u))^theta+(-ln(v))^theta-((-ln(u))^(-theta*delta)+(-ln(v))^(-
        theta*delta))^(-1/delta))^(1/theta))+((-ln(u))^theta+(-ln(v))^theta
        -((-ln(u))^(-theta*delta)+(-ln(v))^(-theta*delta))^(-1/delta))^(1/
        theta)/theta*((-ln(u))^theta*theta/u/ln(u)-((-ln(u))^(-theta*delta)
        +(-ln(v))^(-theta*delta))^(-1/delta)*(-ln(u))^(-theta*delta)*theta/
        u/ln(u)/((-ln(u))^(-theta*delta)+(-ln(v))^(-theta*delta)))/((-ln(u)
        )^theta+(-ln(v))^theta-((-ln(u))^(-theta*delta)+(-ln(v))^(-theta*
        delta))^(-1/delta))^2*exp(-((-ln(u))^theta+(-ln(v))^theta-((-ln(u))
        ^(-theta*delta)+(-ln(v))^(-theta*delta))^(-1/delta))^(1/theta))*((-
        ln(v))^theta*theta/v/ln(v)-((-ln(u))^(-theta*delta)+(-ln(v))^(-
        theta*delta))^(-1/delta)*(-ln(v))^(-theta*delta)*theta/v/ln(v)/((-
        ln(u))^(-theta*delta)+(-ln(v))^(-theta*delta)))+(((-ln(u))^theta+(-
        ln(v))^theta-((-ln(u))^(-theta*delta)+(-ln(v))^(-theta*delta))^(-1/
        delta))^(1/theta))^2/theta^2*((-ln(u))^theta*theta/u/ln(u)-((-ln(u)
        )^(-theta*delta)+(-ln(v))^(-theta*delta))^(-1/delta)*(-ln(u))^(-
        theta*delta)*theta/u/ln(u)/((-ln(u))^(-theta*delta)+(-ln(v))^(-
        theta*delta)))/((-ln(u))^theta+(-ln(v))^theta-((-ln(u))^(-theta*
        delta)+(-ln(v))^(-theta*delta))^(-1/delta))^2*((-ln(v))^theta*theta
        /v/ln(v)-((-ln(u))^(-theta*delta)+(-ln(v))^(-theta*delta))^(-1/
        delta)*(-ln(v))^(-theta*delta)*theta/v/ln(v)/((-ln(u))^(-theta*
        delta)+(-ln(v))^(-theta*delta)))*exp(-((-ln(u))^theta+(-ln(v))^
        theta-((-ln(u))^(-theta*delta)+(-ln(v))^(-theta*delta))^(-1/delta))
        ^(1/theta))
    }
    
    # Result:
    attr(c.uv, "control") <- unlist(list(param = param, type = type))
    
    # As List ?
    if (output[1] == "list") {
        N = sqrt(length(u))
        x = u[1:N]
        y = matrix(v, ncol = N)[1, ]
        c.uv = list(x = x, y = y,  z = matrix(c.uv, ncol = N))
    }
    
    # Return Value:
    c.uv
}


# ------------------------------------------------------------------------------


.devContourSlider =
function(B = 10)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays interactively contour plots of density
    
    # FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        Type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5")
        Copula = .sliderMenu(no = 1)
        N = .sliderMenu(no = 2)
        if (Copula <= 3) 
            param = c(delta = .sliderMenu(no = Copula + 2))
        if (Copula == 4) 
            param = c(alpha = .sliderMenu(no = 6), 
                beta = .sliderMenu(no = 7), r = .sliderMenu(no = 8))
        if (Copula == 5)   
            param = c(delta = .sliderMenu(no = 9), theta = .sliderMenu(no = 10)) 
        nlev = .sliderMenu(no = 11)
        ncol = .sliderMenu(no = 12) 
        
        # Title:
        type = Type[Copula]
        subTitle = paste(paste(names(param) , "="), param, collapse = " | " )
        Title = paste(" ", type, "\n", subTitle) 
        
        # Plot:   
        n = N/2 
        F = (2*1.0e-2)^(1/n)
        x = 0.5*F^(1:n)
        x = c(rev(x), 0.5, 1-x)
        uv = grid2d(x = (1:(N-1))/N)
        D = .dev1Copula(u = uv, type = type, param = param, output = "list")
        image(D, col = heat.colors(ncol) )
        contour(D, nlevels = nlev, add = TRUE)
        title(main = Title)
                           
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    C = c("1 Gumbel: delta", "2 Galambos: delta", "3 Husler-Reis: delta",
          "4 Tawn: alpha", "... beta", "... r", "5 BB5: delta", "... theta", 
          "Plot - levels", "... colors")
    .sliderMenu(refresh.code,
        names = c("Copula","N", C), #gal   hr   tawn          bb5    nlev  ncol
        minima =      c(1,  10,   1,   0,   0,   0,   0,  1,  0,  1,   5,   12),
        maxima =      c(5, 100,   B,   B,   B,   1,   1,  B,  B,  B, 100,  256),
        resolutions = c(1,   1, .05, .05, .05, .01, .01, .1, .1, .1,   5,    1),
        starts =      c(1,  25,   2,   1,   1,  .5,  .5,  2,  1,  2,  10,   12))
}


# ------------------------------------------------------------------------------


.devPerspSlider =
function(B = 10)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays interactively contour plots of density
    
    #FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        Type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5")
        Copula = .sliderMenu(no = 1)
        N = .sliderMenu(no = 2)
        if (Copula <= 3) 
            param = c(delta = .sliderMenu(no = Copula + 2))
        if (Copula == 4) 
            param = c(alpha = .sliderMenu(no = 6), 
                beta = .sliderMenu(no = 7), r = .sliderMenu(no = 8))
        if (Copula == 5)   
            param = c(delta = .sliderMenu(no = 9), theta = .sliderMenu(no = 10)) 
        theta = .sliderMenu(no = 11)
        phi = .sliderMenu(no = 12) 
        
        # Title:
        type = Type[Copula]
        subTitle = paste(paste(names(param) , "="), param, collapse = " | " )
        Title = paste(" ", type, "\n", subTitle) 
        
        # Plot:   
        n = N/2 
        F = (2*1.0e-2)^(1/n)
        x = 0.5*F^(1:n)
        x = c(rev(x), 0.5, 1-x)    
        uv = grid2d(x = x)
        D = .dev1Copula(u = uv, type = type, param = param, output = "list")
        persp(D, theta = theta, phi = phi, col = "steelblue", shade = 0.5, 
            ticktype = "detailed", cex = 0.5)
        title(main = Title)
                           
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    C = c("1 Gumbel: delta", "2 Galambos: delta", "3 Husler-Reis: delta",
          "4 Tawn: alpha", "... beta", "... r", "5 BB5: delta", "... theta", 
          "Plot - theta", "... phi")
    .sliderMenu(refresh.code,
        names = c("Copula", "N", C), #gal  hr  tawn          bb5    theta  phi
        minima =      c(1,  10,   1,   0,   0,   0,   0,  1,  0,  1, -180,   0),
        maxima =      c(5, 100,   B,   B,   B,   1,   1,  B,  B,  B,  180, 360),
        resolutions = c(1,   1, .05, .05, .05, .01, .01, .1, .1, .1,    1,   1),
        starts =      c(1,  25,   2,   1,   1,  .5,  .5,  2,  1,  2,  -40,  30))
}


################################################################################
# ETREME VALUE COPULAE SIMULATION AND PARAMETER FITTING:


evCopulaSim = 
function(n, param = NULL, 
type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Simulates bivariate extreme value Copula
    
    return("not yet implemented")
    # revCopula() is missing ...
    
    # Match Arguments:
    type = match.arg(type)
      
    # Settings:
    if (is.null(param)) param = .evParam(type)$param
    
    # Random Variates:
    ans = revCopula(n = n, param = parm, type = type) 

    # Control:
    control = list(param = param, copula = "ev", type = type)
    attr(ans, "control")<-unlist(control)
      
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------

    
evCopulaFit =
function(u, v = NULL, 
type = c("gumbel", "galambos", "husler.reiss", "tawn", "bb5"), ...)
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

    # Start Values:
    param = .evParam(type)$param
     
    # Estimate Copula:
    fun = function(x, type) {
        -mean( log(evCopula(u = U, v = V, param = x, type = type)) )
    }
    range = .evRange(type)

    # fit = nlminb(start = alpha, objective = fun, 
    #     lower = range[1], upper = range[2],  type = type, ...)
    fit = NA
    
    # Return Value:
    fit
}


################################################################################

