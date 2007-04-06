
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
# FUNCTION:                  ARCHIMEDEAN COPULAE RANDOM VARIATES:
#  rarchmCopula               Generates Archimedean copula random variates 
#  rarchmSlider               Displays interactively archimedean probability
#  .rNo1Copula                Generates rv's for copulae No 1
#  .rNo2Copula                Generates rv's for copulae No 2
# FUNCTION:                  ARCHIMEDEAN COPULAE PROBABILITY:
#  parchmCopula               Computes Archimedean copula probability 
#  parchmSlider               Displays interactively archimedean probability 
#  .parchm1Copula              Utility Function
#  .parchm2Copula              Utility Function
#  .parchmPerspSlider          Utility Function
#  .parchmContourSlider        Utility Function
# FUNCTION:                  ARCHIMEDEAN COPULAE DENSITY:
#  darchmCopula               Computes Archimedean copula density 
#  darchmSlider                Displays interactively archimedean density 
#  .darchm1Copula              Utility Function
#  .darchm2Copula              Utility Function
#  .darchmPerspSlider          Utility Function
#  .darchmContourSlider        Utility Function
################################################################################


################################################################################
#  rarchmCopula               Generates Archimedean copula random variates 
#  .r1Copula                  Generates rv's for copulae No 1
#  .r2Copula                  Generates rv's for copulae No 2
 

rarchmCopula =
function(n, alpha = NULL, type = paste(1:22))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates Archimedean copula random variate
    
    # FUNCTION:
    
    # Type:
    type = match.arg(type)
    Type = as.integer(type)
    
    # Alpha:
    if (is.null(alpha)) alpha = .archmParam(type)$param
    
    # Check alpha:
    check = .archmCheck(alpha, type)
    
    if (Type == 1) {
        # Use faster Algorithm:
        ans = .rNo1Copula(n, alpha)
    } else {
        # Generate rv's for the remaining Copulae:
        X = runif(n)
        Y = runif(n)
        t = .invK(Y, alpha, type)
        U = .invPhi(X*.Phi(t, alpha, type), alpha, type)
        V = .invPhi((1-X)*.Phi(t, alpha, type), alpha, type)
        ans = cbind(U, V)
        # Add Control Attribute:
        colnames(ans) = NULL
    }
    
    # Add Control List:
    control = list(alpha = alpha[[1]], copula = "archm", type = type)
    attr(ans, "control")<-unlist(control)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


rarchmSlider =
function(B = 10)
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
        #           1       5        10        15        20
        Counter = c(1,2,3,2,4,2,5,2,5,5,6,2,7,2,2,7,4,8,7,7,2,5)
        Copula = as.integer(.sliderMenu(no = 1))
        No = Counter[Copula]
        N = .sliderMenu(no = 2)
        alpha = .sliderMenu(no = No+2)
                     
        # There is no known Copula for the following bounds:
        eps = 1.0e-6
        if (Copula == 11) if (alpha == 0.5) alpha = 0.5 - eps
        if (Copula == 13) if (alpha == 0.0) alpha = eps
        
        # Title:
        Names = c(
            "- Clayton", "", 
            "- Ali-Mikhail-Hag", 
            "- Gumbel-Hougard", 
            "- Frank",
            "- Joe-Frank", "", "", 
            "- Gumbel-Barnett", "", "", "", "", "", 
            "- Genest-Ghoudi", "", "", "", "", "", "", "")      
        Title = paste("Archimedean Copula No:", as.character(Copula), 
            Names[Copula], "\nalpha = ", as.character(alpha)) 
        
        # Plot: 
        R = rarchmCopula(n = N, alpha = alpha, type = as.character(Copula))
        plot(R, xlab = "U", ylab = "V", pch = 19, col = "steelblue")
        grid()
        title(main = Title)
                           
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    C2 = "2-4-6-8-12-14-15-21"
    C = c("1", C2, "3", "5-17", "7-9-10-22", "11", "13-16-19-20","18")                                           
    L = c( -1,  1,  -1,     -B,           0,    0,            0,   2 )
    U = c(  B,  B,   1,      B,           1,  0.5,            B,   B )
    A = c(0.5,  2, 0.5,      1,         0.5,  0.2,            1,   3 ) 
    V = rep(0.1, 8)
    .sliderMenu(refresh.code,
        names       = c("Copula",  "N", C),
        minima      = c(       1,  100, L),
        maxima      = c(      22, 1000, U),
        resolutions = c(       1,  100, V),
        starts      = c(       1,  100, A)) 
}


# ------------------------------------------------------------------------------


.rNo1Copula =
function(n, alpha = NULL, alternative = FALSE, doplot = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generates rv's for copula No 1
    
    # Default Parameter:
    if (is.null(alpha)) alpha = .archmParam(1)$param
    
    # Clayton Random Variate:
    if (alternative) {
        # Source: aas04.pdf
        X = rgamma(n, 1/alpha)
        V1 = runif(n)
        U = (1-log(V1)/X)^(-1/alpha)
        V2 = runif(n)
        V = (1-log(V2)/X)^(-1/alpha)
        ans = cbind(U, V)
    } else {
        # Source: armstrong03.pdf
        U = runif(n)
        W = runif(n)
        # W = C(V|U) =>
        V = ( W^(-alpha/(alpha+1)) * U^(-alpha) - U^(-alpha) + 1 )^(-1/alpha)
        ans = cbind(U, V)
    }   
    
    # Optional Plot:
    if (doplot) {
        plot(U, V, cex = 0.25, main = "Copula No. 1")
    }
    
    # Add Attribute:
    colnames(ans) = NULL
    control = list(alpha = alpha[[1]], copula = "archm", type = "1")
    attr(ans, "control")<-unlist(control)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.rNo2Copula = 
function(n, alpha = NULL, doplot = FALSE)
{   # A function implemented by Diethelm Wuertz

    # HERE IS SOMETHING WRONG !!!!
    
    # Description:
    #   Generates rv's for copula No 2
    
    # Source: armstrong03.pdf
    
    # Default Parameter:
    if (is.null(alpha)) alpha = .archmParam(2)$param
    
    # Random Variates:
    U = runif(n)
    W = runif(n)
    
    # W = C(V|U) =>
    V = 1 - ( (1-U)^alpha * (W^(alpha/(1-alpha)) - 1 ) + 1 ) ^ (1/alpha)
    ans = cbind(U, V)
    
    # Optional Plot:
    if (doplot) {
        plot(U, V, cex = 0.25, main = "Copula No. 2")
    }
    
    # Add Attribute:
    colnames(ans) = NULL
    control = list(alpha = alpha[[1]], copula = "archm", type = "2")
    attr(ans, "control")<-unlist(control)
    
    # Return Value:
    ans
}


################################################################################
# FUNCTION:                  ARCHIMEDEAN COPULAE PROBABILITY:
#  parchmCopula               Computes Archimedean copula probability 
#  parchmSlider               Displays interactively archimedean probability 
#  .parchm1Copula              Utility Function
#  .parchm2Copula              Utility Function
#  .parchmPerspSlider          Utility Function
#  .parchmContourSlider        Utility Function


parchmCopula = 
function(u = 0.5, v = u, alpha = NULL, type = paste(1:22),
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
    #   alpha - a numeric value or vector of named parameters as 
    #       required by the copula specified by the variable 'type'.
    #       If set to NULL, then the parameters will be taken as
    #       specified by the function 'eparchParam'.
    #   type - the type of the Archimedean copula. An integer or character
    #       string selected from: "1", ..., "22".
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
    #   Diagonal Value: parchmCopula((0:10)/10)
    #   persp(parchmCopula(u = grid2d(), output = "list"))
    
    # FUNCTION:
    
    # Copula:
    if (alternative) {
        ans = .parchm2Copula(u, v, alpha, type, output)
    } else {
        ans = .parchm1Copula(u, v, alpha, type, output)
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


parchmSlider =
function(type = c("persp", "contour"), B = 10)
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
    
    # Plot:
    if (type[1] == "persp")
        .parchmPerspSlider(B = B)
    if (type[1] == "contour")
        .parchmContourSlider(B = B)
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.parchm1Copula =
function(u = 0.5, v = u, alpha = NULL, type = paste(1:22), 
output = c("vector", "list") )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute Maximum Extreme Value Copulae
    
    # Arguments:
    #   see function: parchmCopula
    
    # Example:
    #   Diagonal Value: .parchm1Copula((0:10)/10)
    #   persp(.parchm1Copula(u = grid2d(), output = "list"))
    
    # FUNCTION:
    
    # Match Arguments:
    output = match.arg(output)
    
    # Type:
    type = match.arg(type)
    Type = as.integer(type)
    
    # Settings:
    if (is.null(alpha)) {
        alpha = .archmParam(type)$param
    }
    if (is.list(u)) {
        v = u[[2]]
        u = u[[1]]
    }
    if (is.matrix(u)) {
        v = u[,1]
        u = u[,2]
    }
 
    # Copula:
    if (alpha == 0 & Type == 1) {
        C.uv = pfrechetCopula(u, v, type = "pi")
    } else if (alpha == 1 & Type == 3) {
        C.uv = pfrechetCopula(u, v, type = "psp") 
    } else if (alpha == 0 & Type == 7) {
        C.uv = pfrechetCopula(u, v, type = "w")
    } else if (alpha == 0 & Type == 9) {
        C.uv = pfrechetCopula(u, v, type = "pi")
    } else if (alpha == 0 & Type == 10) {
        C.uv = pfrechetCopula(u, v, type = "pi")
    } else if (alpha == 0 & Type == 11) {
        C.uv = pfrechetCopula(u, v, type = "pi")
    } else if (alpha == 1 & Type == 13) {
        C.uv = pfrechetCopula(u, v, type = "pi")
    } else if (alpha == 0 & Type == 19) {
        C.uv = pfrechetCopula(u, v, type = "psp")
    } else if (alpha == 0 & Type == 20) {
        C.uv = pfrechetCopula(u, v, type = "pi")
    } else if (alpha == 1 & Type == 21) {
        C.uv = pfrechetCopula(u, v, type = "w")
    } else if (alpha == 0 & Type == 22) {
        C.uv = pfrechetCopula(u, v, type = "pi")
    } else {
        C.uv = .invPhi(.Phi(u, alpha, type) + .Phi(v, alpha, type), alpha, type)
    } 
    
    # Requires special attention:
    if (type == "20") {
        C.uv = C.uv + (1-sign(C.uv)) * pfrechetCopula(u, v, type = "m")
    }
    
    # Simulates max function:
    C.uv = (C.uv + abs(C.uv))/2       
    
    # C(u,v) on Boundary of Unit Square:
    C.uv[is.na(C.uv)] = 0      
    C.uv[which(u == 0)] = 0
    C.uv[which(u == 1)] = v[which(u == 1)]
    C.uv[which(v == 0)] = 0
    C.uv[which(v == 1)] = u[which(v == 1)]
    C.uv[which(u*v == 1)] = 1
    C.uv[which(u+v == 0)] = 0
    
    # Add Control Attribute:
    control = list(alpha = alpha[[1]], copula = "archm", type = type)
    attr(C.uv, "control")<-unlist(control)
    
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


.parchm2Copula = 
function(u = 0.5, v = u, alpha = NULL, type = paste(1:22),
output = c("vector", "list") )
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    #   see function: parchmCopula
    
    # Example:
    #   Diagonal Value: .parchm2Copula((0:10)/10)
    #   persp(.parchm2Copula(u = grid2d(), output = "list"))
    
    # FUNCTION:
    
    # Match Arguments:
    output = match.arg(output)
    
    # Type:
    type = match.arg(type)
    Type = as.integer(type)
    
    # Settings:
    if (is.null(alpha)) {
        alpha = .archmParam(type)$param
    }
    if (is.list(u)) {
        v = u[[2]]
        u = u[[1]]
    }
    if (is.matrix(u)) {
        v = u[, 1]
        u = u[, 2]
    }
    
    # Copula:
    if (alpha == 0 & Type == 1) {
        C.uv = pfrechetCopula(u, v, type = "pi")
    } else if (alpha == 1 & Type == 3) {
        C.uv = pfrechetCopula(u, v, type = "psp") 
    } else if (alpha == 0 & Type == 7) {
        C.uv = pfrechetCopula(u, v, type = "w")
    } else if (alpha == 0 & Type == 9) {
        C.uv = pfrechetCopula(u, v, type = "pi")
    } else if (alpha == 0 & Type == 10) {
        C.uv = pfrechetCopula(u, v, type = "pi")
    } else if (alpha == 0 & Type == 11) {
        C.uv = pfrechetCopula(u, v, type = "pi")
    } else if (alpha == 1 & Type == 13) {
        C.uv = pfrechetCopula(u, v, type = "pi")
    } else if (alpha == 0 & Type == 19) {
        C.uv = pfrechetCopula(u, v, type = "psp")
    } else if (alpha == 0 & Type == 20) {
        C.uv = pfrechetCopula(u, v, type = "pi")
    } else {    
        if (Type == 1) {# Clayton Copula
            C.uv = (u^(-alpha)+v^(-alpha)-1)^(-1/alpha) 
        }   
        if (Type == 2) {
            X = 1-((1-u)^alpha+(1-v)^alpha)^(1/alpha)
            Y = rep(0, times = length(X))
            C.uv = apply(cbind(X, Y), 1, max) 
        } 
        if (Type == 3) {
            C.uv = u*v/(1-alpha*(1-u)*(1-v)) 
        }  
        if (Type == 4) { # Gumbel Copula 
            C.uv = exp( -((-log(u))^(alpha)+(-log(v))^(alpha))^(1/alpha)) 
        }  
        if (Type == 5) { # Frank Copula
            C.uv = -1/alpha*log(1+(exp(-alpha*u)-1)*
                (exp(-alpha*v)-1)/(exp(-alpha)-1)) }
        if (Type == 6) {
            C.uv = 1-((1-u)^alpha+(1-v)^alpha-(1-u)^alpha*
                (1-v)^alpha)^(1/alpha) 
        }  
        if (Type == 7) {
            X = alpha*u*v+(1-alpha)*(u+v-1)
            Y = rep(0, times = length(X))
            C.uv = apply(cbind(X, Y), 1, max) 
        }  
        if (Type == 8) {
            X = (alpha^2*u*v-(1-u)*(1-v))/(alpha^2-(alpha-1)^2*(1-u)*(1-v))
            Y = rep(0, times = length(X))
            C.uv = apply(cbind(X, Y), 1, max) 
        }     
        if (Type == 9) {
            C.uv = u*v*exp(-alpha*log(u)*log(v)) 
        }   
        if (Type == 10) {
            C.uv = u*v/(1+(1-u^alpha)*(1-v^alpha))^(1/alpha) 
        }  
        if (Type == 11) {
            X = (u^alpha*v^alpha-2*(1-u^alpha)*(1-v^alpha))^(1/alpha)
            Y = rep(0, times = length(X))
            C.uv = apply(cbind(X, Y), 1, max) 
        }     
        if (Type == 12) {
            C.uv = (1+((u^(-1)-1)^alpha+(v^(-1)-1)^alpha)^(1/alpha))^(-1) 
        } 
        if (Type == 13) {
            C.uv = exp(1-((1-log(u))^alpha+(1-log(v))^alpha-1)^(1/alpha)) 
        }
        if (Type == 14) {
            C.uv = (1+((u^(-1/alpha)-1)^alpha +
                (v^(-1/alpha)-1)^alpha)^(1/alpha))^(-alpha) 
        }
        if (Type == 15) {
            X = (1-((1-u^(1/alpha))^alpha + 
                (1-v^(1/alpha))^alpha )^(1/alpha) )^alpha
            Y = rep(0, times = length(X))
            C.uv = apply(cbind(X, Y), 1, max) 
        }       
        if (Type == 16) {
            C.uv = 1/2*((u+v-1-alpha*(1/u+1/v-1))+
                sqrt((u+v-1-alpha*(1/u+1/v-1))^2+4*alpha)) 
        }
        if (Type == 17) {
            C.uv = (1+((1+u)^(-alpha)-1)*
                ((1+v)^(-alpha)-1)/(2^(-alpha)-1))^(-1/alpha)-1 
        }
        if (Type == 18) {
            eps = 1/10^8
            u = u - eps*(1-sign(1-u))
            v = v - eps*(1-sign(1-v))
            X = 1+alpha/log(exp(alpha/(u-1))+exp(alpha/(v-1)))
            Y = rep(0, times = length(X))
            C.uv = apply(cbind(X, Y), 1, max) 
        }  
        if (Type == 19) {
            C.uv = alpha/log(exp(alpha/u)+exp(alpha/v)-exp(alpha)) 
        }
        if (Type == 20) {
            a.range = "(0, Inf)"
            C.uv = (log(exp(1/u^alpha)+exp(1/v^alpha)-exp(1)))^(-1/alpha) 
            C.uv = C.uv + (1-sign(C.uv)) * pfrechetCopula(u, v, type = "m")
        }  
        if (Type == 21) {
            # NOT YET IMPLEMENTED
            warning("No. 21 alternative not active")
            C.uv = NA
            # USE:
            C.uv = .parchm1Copula(u = u, v = v, alpha = alpha, type = type, 
                output = output )
            return(C.uv)
        }
        if (Type == 22) {
            # NOT YET IMPLEMENTED
            warning("No. 22 alternative not active")
            C.uv = NA
            # USE:
            C.uv = .parchm1Copula(u = u, v = v, alpha = alpha, type = type, 
                output = output )
            return(C.uv)
        }
    }
        
    # Simulates max function:
    C.uv = (C.uv + abs(C.uv))/2       
    
    # C(u,v) on Boundary of Unit Square:
    C.uv[is.na(C.uv)] = 0      
    C.uv[which(u == 0)] = 0
    C.uv[which(u == 1)] = v[which(u == 1)]
    C.uv[which(v == 0)] = 0
    C.uv[which(v == 1)] = u[which(v == 1)]
    C.uv[which(u*v == 1)] = 1
    C.uv[which(u+v == 0)] = 0
    
    # Add Control Attribute:
    control = list(alpha = alpha[[1]], copula = "archm", type = type)
    attr(C.uv, "control")<-unlist(control)
    
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


.parchmPerspSlider =
function(B = 5)
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
        Counter = c(1,2,3,2,4,2,5,2,5,5,6,2,7,2,2,7,4,8,7,7,2,5)
        Copula = as.integer(.sliderMenu(no = 1))
        No = Counter[Copula]
        N = .sliderMenu(no = 2)
        alpha = .sliderMenu(no = No+2)
        theta = .sliderMenu(no = 11)
        phi = .sliderMenu(no = 12)
          
        # Skip:
        if (Copula == 11) if (alpha == 0.5) return(invisible())
        if (Copula == 13) if (alpha == 0)  return(invisible())
        
        # Do we have a strict Copula?
        strict = c(
            "Yes","No","Yes","Yes","Yes","Yes","No","No","Yes","Yes",
            "No","Yes","Yes","Yes","No","Yes","Yes","No","Yes","Yes", 
            "No","Yes")[Copula]
        if (alpha < 0 & Copula == 1) strict[1] = "No"
        if (alpha == 0 & Copula == 16) strict[16] = "No"
        
        # What is the Range?
        RANGE = c(
            "[-1|Inf)", "[1|Inf)", "[-1|1)", "(-Inf|Inf)", "(0|1]", 
            "(0|0.5]", "(0|Inf)", "[2|Inf)")[No]
                 
        # Which one is the Limit Copula?
        limitTitle = rep("NA", times = 22)
        if (alpha == -1) 
            limitTitle = c(
                "W ", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                "NA", "NA", "NA", "NA", "NA", "NA", "Pi", "NA", "NA", "NA",
                "NA", "NA")   
        if (alpha == 0) 
            limitTitle = c(
                "Pi", "NA", "Pi", "NA", "Pi", "NA", "W ", "NA", "Pi", "Pi",
                "Pi", "NA", "NA", "NA", "NA", "W ", "NA", "NA", "L ", "Pi",
                "NA", "Pi")    
        if (alpha == 1) 
            limitTitle = c(
                "L ", "W ", "L ", "Pi", "NA", "Pi", "Pi", "W ", "NA", "NA",
                "NA", "L ", "Pi", "L ", "W ", "NA", "NA", "NA", "NA", "NA",
                "W ", "NA")
        limitTitle = limitTitle[Copula]
        if (limitTitle == "NA") {
            limitTitle = " "
        } else {
            limitTitle = paste("  Copula = ", limitTitle[1])
        }
        
        # Tau/Rho:
        Tau = round(approx(.ALPHA[, Copula], .TAU[, Copula], xout = alpha)$y,
            digits = 3)
        Rho = round(approx(.ALPHA[, Copula], .RHO[, Copula], xout = alpha)$y,
            digits = 3)
  
        # Title:
        Names = c(
            "- Clayton", "", "- Ali-Mikhail-Hag", "- Gumbel-Hougard", "- Frank",
            "- Joe-Frank", "", "", "- Gumbel-Barnett", "",
            "", "", "", "", "- Genest-Ghoudi", "", "", "", "", "", "", "")      
        Title = paste("Archimedean Copula No:", as.character(Copula), 
            Names[Copula], "\n", RANGE, " alpha =", as.character(alpha), 
            " tau =", as.character(Tau), " rho =", as.character(Rho)) 
        
        # Plot: 
        uv = grid2d(x = (0:N)/N)
        P = .parchm1Copula(u = uv, alpha = alpha, type = Copula, 
            output = "list")
        persp(P, theta = theta, phi = phi, col = "steelblue", shade = 0.5,
            ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
            zlab = "C(u,v)" )
        title(main = Title)
                           
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
   
    # Open Slider Menu:
    B = 5
    C1 = "1: [-1,Inf]"
    C2 = "2-4-6-8-12-14-15-21: [1,Inf)"
    C3 = "3: [-1,1)"
    C4 = "5-17: (-Inf,Inf)|{0}"
    C5 = "7-9-10-22: (0,1]"
    C6 = "11: (0, 1/2]"
    C7 = "13-16-19-20: (0,Inf)"
    C8 = "18: [2, Inf)" 
    C = c(   C1, C2,   C3,   C4,   C5,   C6,   C7,  C8 )  
    L = c(   -1,  1,   -1,   -B,    0,    0,    0,   2 )
    U = c(    B,  B,    1,    B,    1,  0.5,    B,   B )
    A = c(  0.5,  2,  0.5,    1,  0.5,  0.2,    1,   3 ) 
    V = rep(0.01, 8)
    plot.names = c("Plot - theta", "... phi")
    .sliderMenu(refresh.code,
        names       = c("Copula", "N", C, plot.names),
        minima      = c(       1,  10, L, -180,    0),
        maxima      = c(      22, 100, U,  180,  360),
        resolutions = c(       1,  10, V,    1,    1),
        starts      = c(       1,  10, A,  -40,   30)) 
}


# ------------------------------------------------------------------------------


.parchmContourSlider =
function(B = 5)
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
        #           1       5        10        15        20
        Counter = c(1,2,3,2,4,2,5,2,5,5,6,2,7,2,2,7,4,8,7,7,2,5)
        Copula = as.integer(.sliderMenu(no = 1))
        No = Counter[Copula]
        N = .sliderMenu(no = 2)
        alpha = .sliderMenu(no = No+2)
        n.lev = .sliderMenu(no = 11)
        n.col = .sliderMenu(no = 12)
          
        # Skip:
        if (Copula == 11) if (alpha == 0.5) return(invisible())
        if (Copula == 13) if (alpha == 0)  return(invisible())
        
        # Do we have a strict Copula?
        strict = c(
            "Yes","No","Yes","Yes","Yes","Yes","No","No","Yes","Yes",
            "No","Yes","Yes","Yes","No","Yes","Yes","No","Yes","Yes", 
            "No","Yes")[Copula]
        if (alpha < 0 & Copula == 1) strict[1] = "No"
        if (alpha == 0 & Copula == 16) strict[16] = "No"
        
        # What is the Range?
        RANGE = c(
            "[-1|Inf)", "[1|Inf)", "[-1|1)", "(-Inf|Inf)", "(0|1]", 
            "(0|0.5]", "(0|Inf)", "[2|Inf)")[No]
                 
        # Which one is the Limit Copula?
        limitTitle = rep("NA", times = 22)
        if (alpha == -1) 
            limitTitle = c(
                "W ", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                "NA", "NA", "NA", "NA", "NA", "NA", "Pi", "NA", "NA", "NA",
                "NA", "NA")   
        if (alpha == 0) 
            limitTitle = c(
                "Pi", "NA", "Pi", "NA", "Pi", "NA", "W ", "NA", "Pi", "Pi",
                "Pi", "NA", "NA", "NA", "NA", "W ", "NA", "NA", "L ", "Pi",
                "NA", "Pi")    
        if (alpha == 1) 
            limitTitle = c(
                "L ", "W ", "L ", "Pi", "NA", "Pi", "Pi", "W ", "NA", "NA",
                "NA", "L ", "Pi", "L ", "W ", "NA", "NA", "NA", "NA", "NA",
                "W ", "NA")
        limitTitle = limitTitle[Copula]
        if (limitTitle == "NA") {
            limitTitle = " "
        } else {
            limitTitle = paste("  Copula = ", limitTitle[1])
        }
        
        # Tau/Rho:
        Tau = round(approx(.ALPHA[, Copula], .TAU[, Copula], xout = alpha)$y,
            digits = 3)
        Rho = round(approx(.ALPHA[, Copula], .RHO[, Copula], xout = alpha)$y,
            digits = 3)
  
        # Title:
        Names = c(
            "- Clayton", "", "- Ali-Mikhail-Hag", "- Gumbel-Hougard", "- Frank",
            "- Joe-Frank", "", "", "- Gumbel-Barnett", "",
            "", "", "", "", "- Genest-Ghoudi", "", "", "", "", "", "", "")      
        Title = paste("Archimedean Copula No:", as.character(Copula), 
            Names[Copula], "\n", RANGE, " alpha =", as.character(alpha), 
            " tau =", as.character(Tau), " rho =", as.character(Rho)) 
        
        # Plot:   
        uv = grid2d(x = (0:N)/N)
        P = .parchm1Copula(u = uv, alpha = alpha, type = Copula, 
            output = "list")
        image(P, col = heat.colors(n.col) )
        contour(P, xlab = "u", ylab = "v", nlevels = n.lev, add = TRUE)
        title(main = Title)
                           
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    C1 = "1: [-1,Inf]"
    C2 = "2-4-6-8-12-14-15-21: [1,Inf)"
    C3 = "3: [-1,1)"
    C4 = "5-17: (-Inf,Inf)|{0}"
    C5 = "7-9-10-22: (0,1]"
    C6 = "11: (0, 1/2]"
    C7 = "13-16-19-20: (0,Inf)"
    C8 = "18: [2, Inf)" 
    C = c(   C1, C2,   C3,   C4,   C5,   C6,   C7,  C8 )  
    L = c(   -1,  1,   -1,   -B,    0,    0,    0,   2 )
    U = c(    B,  B,    1,    B,    1,  0.5,    B,   B )
    A = c(  0.5,  2,  0.5,    1,  0.5,  0.2,    1,   3 ) 
    V = rep(0.01, 8)
    plot.names = c("Plot - levels", "... colors")
    .sliderMenu(refresh.code,
        names       = c("Copula", "N", C, plot.names),
        minima      = c(       1,  10, L,    5,   12),
        maxima      = c(      20, 100, U,  100,  256),
        resolutions = c(       1,  10, V,    5,    1),
        starts      = c(       1,  10, A,   10,   12)) 
}


################################################################################
# FUNCTION:                  ARCHIMEDEAN COPULAE DENSITY:
#  darchmCopula               Computes Archimedean copula density 
#  darchmSlider                Displays interactively archimedean density 
#  .darchm1Copula              Utility Function
#  .darchm2Copula              Utility Function
#  .darchmPerspSlider          Utility Function
#  .darchmContourSlider        Utility Function


darchmCopula = 
function(u = 0.5, v = u, alpha = NULL, type = paste(1:22),
output = c("vector", "list"), alternative = FALSE )
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes extreme value copula density
    
    # Arguments:
    #   u, v - two numeric values or vectors of the same length at
    #       which the copula will be computed. If 'u' is a list then the
    #       the '$x' and '$y' elements will be used as 'u' and 'v'.
    #       If 'u' is a two column matrix then the first column will
    #       be used as 'u' and the the second as 'v'.
    #   alpha - a numeric value or vector of named parameters as 
    #       required by the copula specified by the variable 'type'.
    #       If set to NULL, then the parameters will be taken as
    #       specified by the function 'archmParam'.
    #   type - the type of the Archimedean copula. An integer or character
    #       string selected from: "1", ..., "22".
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
    #   Diagonal Value: darchmCopula((0:10)/10)
    #   persp(darchmCopula(u=grid2d(), output="list"), theta=-40, phi=30, xlab="x")
    
    # FUNCTION:
    
    # Copula:
    if (alternative) {
        ans = .darchm2Copula(u, v, alpha, type, output)
    } else {
        ans = .darchm1Copula(u, v, alpha, type, output)
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


darchmSlider =
function(type = c("persp", "contour"), B = 10)
{   # A function implemented by Diethelm Wuertz
        
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
    
    # Plot:
    if (type == "persp")
        .darchmPerspSlider(B = B)
    if (type == "contour")
        .darchmContourSlider(B = B)
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.darchm1Copula = 
function(u = 0.5, v = u, alpha = NULL, type = paste(1:22), output = 
c("vector", "list")) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Density of Maximum Extreme Value Copulae
    
    # References:
    #   Nelsen
    #   Matteis, Diploma Thesis
    #   Carmona, Evanesce
    
    # FUNCTION:
    
    # Match Arguments:
    output = match.arg(output)
    
    # Type:
    type = match.arg(type)
    Type = as.integer(type)
    
    # Settings:
    if (is.null(alpha)) {
        alpha = .archmParam(type)$param
    }
    if (is.list(u)) {
        v = u[[2]]
        u = u[[1]]
    }
    if (is.matrix(u)) {
        v = u[, 1]
        u = u[, 2]
    }
        
    # Density:
    c.uv = .invPhiSecondDer( 
        .Phi(u, alpha, type) + .Phi(v, alpha, type), alpha, type ) / ( 
        .invPhiFirstDer(.Phi(u, alpha, type), alpha, type) * 
        .invPhiFirstDer(.Phi(v, alpha, type), alpha, type) ) 
    # c.uv[which(u*v == 0 | u*v == 1)] = 0
    
    # Replace NAs:
    # c.uv[is.na(c.uv)] = 0
    
    # Add Control Attribute:
    control = list(alpha = alpha[[1]], copula = "archm", type = type)
    attr(c.uv, "control")<-unlist(control)
    
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


.darchm2Copula = 
function(u = 0.5, v = u, alpha = NULL, type = paste(1:22), output = 
c("vector", "list")) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extreme Value Copulae
    
    # References:
    #   Carmona, Evanesce
    #   Matteis, Diploma Thesis
    
    # Notes:
    #   "4" Gumbel(alpha->1) -> m-Copula min(u,v)
    
    # Example:
    #   persp(z = matrix(.darchm1Copula(.gridCoord()$x, .gridCoord()$y, 1.1, "4"), 101))
   
    # FUNCTION:
    
    # Match Arguments:
    output = match.arg(output)
    
    # Type:
    type = match.arg(type)
    Type = as.integer(type)
    
    # Settings:
    if (is.null(alpha)) {
        alpha = .archmParam(type)$param
    }
    a = alpha
    if (is.list(u)) {
        v = u[[2]]
        u = u[[1]]
    }
    if (is.matrix(u)) {
        v = u[, 1]
        u = u[, 2]
    }
    
    # Density:
    if (Type == 1) { 
        c.uv = (1 + a)*u^(-1 - a)*v^(-1 - a) *
            (-1 + u^(-a) + v^(-a))^(-2 - a^(-1)) 
    }
    if (Type == 2) { 
        # NOT YET IMPLEMENTED!
        c.uv = NA 
        # USE:
        warning("No 2 alternative not available")
        c.uv = .darchm1Copula(u = u, v = v, alpha = alpha, type = type,
            output = output)  
        return(c.uv)
    }
    if (Type == 3) { 
        c.uv = (-1 + a^2*(-1 + u + v - u*v) - 
            a*(-2 + u + v + u*v)) /
            (-1 + a*(-1 + u)*(-1 + v))^3 
    }
    if (Type == 4) { 
        # Matteis yields wrong results!
        # c.uv = ((-log(u))^(-1 + a)*(-1 + a + ((-log(u))^a + 
        #    (-log(v))^a)^a^(-1))*((-log(u))^a + 
        #    (-log(v))^a)^(-2 + a^(-1))*(-log(v))^(-1 + a))/
        #    (exp((-log(u))^a + (-log(v))^a)^a^(-1)*u*v) 
        # Use instead:
        c.uv = exp(-((-log(u))^alpha+(-log(v))^alpha)^(1/alpha)) * (-
            (-log(u))^alpha*(-log(v))^alpha*((-log(u))^alpha +
            (-log(v))^alpha)^(1/alpha)+(-log(u))^alpha*(-log(v))^alpha * (
            (-log(u))^alpha+(-log(v))^alpha)^(1/alpha)*alpha +
            (-log(u))^(3*alpha)*(-log(v))^alpha*((-log(u))^alpha + 
            (-log(v))^alpha)^(-2*(alpha-1)/alpha)+2*(-log(u))^(2*alpha) *
            (-log(v))^(2*alpha)*((-log(u))^alpha +
            (-log(v))^alpha)^(-2*(alpha-1)/alpha)+(-log(u))^alpha *
            (-log(v))^(3*alpha)*((-log(u))^alpha +
            (-log(v))^alpha)^(-2*(alpha-1)/alpha))/log(v)/log(u)/v/u / (
            (-log(u))^(2*alpha)+2*(-log(u))^alpha*(-log(v))^alpha +
            (-log(v))^(2*alpha))
    }
    if (Type == 5) { 
        c.uv = (a*exp(a*(1 + u + v))*(-1 + exp(a)))/(exp(a) - 
            exp(a + a*u) + exp(a*(u + v)) - exp(a + a*v))^2 
    }
    if (Type == 6) { 
        c.uv = (1 - u)^(-1 + a)*(a - (-1 + (1 - u)^a)*(-1 + 
            (1 - v)^a)) * ((1 - u)^a + (1 - v)^a - (1 - u)^a *
            (1 - v)^a)^(-2 + a^(-1)) * (1 - v)^(-1 + a) 
    }
    if (Type == 7) { 
        # NOT YET IMPLEMENTED!
        c.uv = NA 
        # USE:
        warning("No 7 alternative not available")
        c.uv = .darchm1Copula(u = u, v = v, alpha = alpha, type = type,
            output = output) 
        return(c.uv)
    }
    if (Type == 8) { 
        # NOT YET IMPLEMENTED!
        c.uv = NA 
        # USE:
        warning("No 8 alternative not available")
        c.uv = .darchm1Copula(u = u, v = v, alpha = alpha, type = type,
            output = output)  
        return(c.uv)
    }
    if (Type == 9) { 
        c.uv = (1 - a - a*log(v) + a*log(u)*(-1 + a*log(v))) /
            exp(a*log(u)*log(v)) 
    }
    if (Type == 10) { 
        c.uv = (2 - v^a + u^a*(-1 + v^a))^(-2 - a^(-1)) *
            (4 - 2*v^a + u^a*(-2 - (-1 + a)*v^a)) 
    }
    if (Type == 11) {
        # NOT YET IMPLEMENTED!
        c.uv = NA 
        # USE:
        warning("No 11 alternative not available")
        c.uv = .darchm1Copula(u = u, v = v, alpha = alpha, type = type,
            output = output)  
        return(c.uv) 
    }
    if (Type == 12) {
        c.uv = ((-1+u^(-1))^a*(-1+a+((-1+u^(-1))^a + 
            (-1+v^(-1))^a)^a^(-1)+a*((-1+u^(-1))^a + 
            (-1+v^(-1))^a)^a^(-1))*((-1+u^(-1))^a + 
            (-1+v^(-1))^a)^(-2+a^(-1))*(-1+v^(-1))^a)/
            ((-1+u)*u*(1+((-1+u^(-1))^a +
            (-1+v^(-1))^a)^a^(-1))^3*(-1+v)*v) 
    }
    if (Type == 13) {
        c.uv = (exp(1 - (-1 + (1 - log(u))^a + 
            (1 - log(v))^a)^a^(-1)) *
            (1 - log(u))^(-1 + a)*(-1 + a + (-1 + 
            (1 - log(u))^a + 
            (1 - log(v))^a)^a^(-1))*(-1 + (1 - log(u))^a + 
            (1 - log(v))^a)^(-2 + a^(-1)) *
            (1 - log(v))^(-1 + a))/(u*v) 
    }
    if (Type == 14) {
        c.uv = ((-1+u^(-a^(-1)))^a*(-1+v^(-a^(-1)))^a *
            ((-1+u^(-a^(-1)))^a + 
            (-1+v^(-a^(-1)))^a)^(-2+a^(-1)) *
            (1+((-1+u^(-a^(-1)))^a + 
            (-1+v^(-a^(-1)))^a)^a^(-1))^(-2-a) *
            (-1+a+2*a*((-1+u^(-a^(-1)))^a + 
            (-1+v^(-a^(-1)))^a)^a^(-1))) /
            (a*u*(-1+u^a^(-1))*v*(-1+v^a^(-1))) 
    }
    if (Type == 15) {
        # NOT YET IMPLEMENTED!
        c.uv = NA 
        # USE:
        warning("No 15 alternative not available")
        c.uv = .darchm1Copula(u = u, v = v, alpha = alpha, type = type,
            output = output) 
        return(c.uv)
    }
    if (Type == 16) {
        c.uv = (2*a*(a^2 + u^2*v^2 + a*(u^2 + v^2))) /
            (sqrt(4*a + (-1 + u - a*(-1 + u^(-1) + v^(-1)) + v)^2) *
            (u^2*v^2*(-1 + u + v)^2 + a^2*(u + v - u*v)^2 + 
            2*a*u*v*(u^2*(-1 + v) - (-1 + v)*v + u*(1 - v + v^2)))) 
    }
    if (Type == 17) {
        c.uv = (2^a*((-1 + 2^a)*a*(1 + u)^a*(1 + v)^a + 2^a*(-1 + 
            (1 + u)^a) * (-1 + (1 + v)^a)))/((1 + u)*(1 + v)*(2^a - 
            2^a*(1 + u)^a - 2^a*(1 + v)^a + (1 + u)^a*(1 + v)^a)^2 *
            (1 + ((-1 + (1 + u)^(-a)) * (-1 + (1 + v)^(-a))) /
            (-1 + 2^(-a)))^a^(-1)) 
    }
    if (Type == 18) {
        # NOT YET IMPLEMENTED!
        c.uv = NA 
        # USE:
        warning("No 18 alternative not available")
        c.uv = .darchm1Copula(u = u, v = v, alpha = alpha, type = type,
            output = output) 
        return(c.uv) 
    }
    if (Type == 19) {
        c.uv = (a^3*exp(a*(u^(-1) + v^(-1)))*(2 + log(-exp(a) + 
            exp(a/u) + exp(a/v))))/((-exp(a) + exp(a/u) + 
            exp(a/v))^2*u^2*v^2*log(-exp(a) + exp(a/u) + exp(a/v))^3) 
    }
    if (Type == 20) {
        c.uv = (exp(u^(-a) + v^(-a))*u^(-1 - a)*v^(-1 - a) *
            log(-exp(1) + exp(u^(-a)) + exp(v^(-a)))^(-2 - a^(-1)) *
            (1 + a + a*log(-exp(1) + exp(u^(-a)) + exp(v^(-a))))) /
            (-exp(1) + exp(u^(-a)) + exp(v^(-a)))^2 
    }
    if (Type == 21) {
        # NOT YET IMPLEMENTED!
        c.uv = NA 
        # USE:
        warning("No 21 alternative not available")
        c.uv = .darchm1Copula(u = u, v = v, alpha = alpha, type = type,
            output = output) 
        return(c.uv) 
    }
    if (Type == 22) {
        # NOT YET IMPLEMENTED!
        c.uv = NA 
        # USE:
        warning("No 22 alternative not available")
        c.uv = .darchm1Copula(u = u, v = v, alpha = alpha, type = type,
            output = output)  
        return(c.uv)
    }
    
    # Replace NAs:
    # c.uv[is.na(c.uv)] = 0
    
    # Add Control Attribute:
    control = list(alpha = alpha[[1]], copula = "archm", type = type)
    attr(c.uv, "control")<-unlist(control)
    
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


.darchmPerspSlider =
function(B = 10)
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
        Counter = c(1,2,3,2,4,2,5,2,5,5,6,2,7,2,2,7,4,8,7,7,2,5)
        Copula = as.integer(.sliderMenu(no = 1))
        No = Counter[Copula]
        N = .sliderMenu(no = 2)
        alpha = .sliderMenu(no = No+2)
        theta = .sliderMenu(no = 11)
        phi = .sliderMenu(no = 12)
          
        # Skip:
        if (Copula == 11) if (alpha == 0.5) return(invisible())
        if (Copula == 13) if (alpha == 0)  return(invisible())
        
        # Do we have a strict Copula?
        strict = c(
            "Yes","No","Yes","Yes","Yes","Yes","No","No","Yes","Yes",
            "No","Yes","Yes","Yes","No","Yes","Yes","No","Yes","Yes", 
            "No","Yes")[Copula]
        if (alpha < 0 & Copula == 1) strict[1] = "No"
        if (alpha == 0 & Copula == 16) strict[16] = "No"
        
        # What is the Range?
        RANGE = c(
            "[-1|Inf)", "[1|Inf)", "[-1|1)", "(-Inf|Inf)", "(0|1]", 
            "(0|0.5]", "(0|Inf)", "[2|Inf)")[No]
                 
        # Which one is the Limit Copula?
        limitTitle = rep("NA", times = 22)
        if (alpha == -1) 
            limitTitle = c(
                "W ", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                "NA", "NA", "NA", "NA", "NA", "NA", "Pi", "NA", "NA", "NA",
                "NA", "NA")   
        if (alpha == 0) 
            limitTitle = c(
                "Pi", "NA", "Pi", "NA", "Pi", "NA", "W ", "NA", "Pi", "Pi",
                "Pi", "NA", "NA", "NA", "NA", "W ", "NA", "NA", "L ", "Pi",
                "NA", "Pi")    
        if (alpha == 1) 
            limitTitle = c(
                "L ", "W ", "L ", "Pi", "NA", "Pi", "Pi", "W ", "NA", "NA",
                "NA", "L ", "Pi", "L ", "W ", "NA", "NA", "NA", "NA", "NA",
                "W ", "NA")
        limitTitle = limitTitle[Copula]
        if (limitTitle == "NA") {
            limitTitle = " "
        } else {
            limitTitle = paste("  Copula = ", limitTitle[1])
        }
        
        # Tau/Rho:
        Tau = round(approx(.ALPHA[, Copula], .TAU[, Copula], xout = alpha)$y,
            digits = 3)
        Rho = round(approx(.ALPHA[, Copula], .RHO[, Copula], xout = alpha)$y,
            digits = 3)
  
        # Title:
        Names = c(
            "- Clayton", "", "- Ali-Mikhail-Hag", "- Gumbel-Hougard", "- Frank",
            "- Joe-Frank", "", "", "- Gumbel-Barnett", "",
            "", "", "", "", "- Genest-Ghoudi", "", "", "", "", "", "", "")      
        Title = paste("Archimedean Copula No:", as.character(Copula), 
            Names[Copula], "\n", RANGE, " alpha =", as.character(alpha), 
            " tau =", as.character(Tau), " rho =", as.character(Rho)) 
        
        # Plot: 
        uv = grid2d(x = (1:(N-1))/N)
        D = .darchm1Copula(u = uv, alpha = alpha, type = Copula, 
            output = "list")
        persp(D, theta = theta, phi = phi, col = "steelblue", shade = 0.5,
            ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
            zlab = "C(u,v)" )
        title(main = Title)
                           
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    B = 5
    C1 = "1: [-1,Inf]"
    C2 = "2-4-6-8-12-14-15-21: [1,Inf)"
    C3 = "3: [-1,1)"
    C4 = "5-17: (-Inf,Inf)|{0}"
    C5 = "7-9-10-22: (0,1]"
    C6 = "11: (0, 1/2]"
    C7 = "13-16-19-20: (0,Inf)"
    C8 = "18: [2, Inf)" 
    C = c(   C1, C2,   C3,   C4,   C5,   C6,   C7,  C8 )  
    L = c(   -1,  1,   -1,   -B,    0,    0,    0,   2 )
    U = c(    B,  B,    1,    B,    1,  0.5,    B,   B )
    A = c(  0.5,  2,  0.5,    1,  0.5,  0.2,    1,   3 ) 
    V = rep(0.1, 8)
    plot.names = c("Plot - theta", "... phi")
    .sliderMenu(refresh.code,
        names       = c("Copula", "N", C, plot.names),
        minima      = c(       1,  10, L, -180,    0),
        maxima      = c(      22, 100, U,  180,  360),
        resolutions = c(       1,  10, V,    1,    1),
        starts      = c(       1,  20, A,  -40,   30)) 
}


# ------------------------------------------------------------------------------


.darchmContourSlider =
function(B = 10)
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
        Counter = c(1,2,3,2,4,2,5,2,5,5,6,2,7,2,2,7,4,8,7,7,2,5)
        Copula = as.integer(.sliderMenu(no = 1))
        No = Counter[Copula]
        N = .sliderMenu(no = 2)
        alpha = .sliderMenu(no = No+2)
        n.lev = .sliderMenu(no = 11)
        n.col = .sliderMenu(no = 12)
          
        # Skip:
        if (Copula == 11) if (alpha == 0.5) return(invisible())
        if (Copula == 13) if (alpha == 0)  return(invisible())
        
        # Do we have a strict Copula?
        strict = c(
            "Yes","No","Yes","Yes","Yes","Yes","No","No","Yes","Yes",
            "No","Yes","Yes","Yes","No","Yes","Yes","No","Yes","Yes", 
            "No","Yes")[Copula]
        if (alpha < 0 & Copula == 1) strict[1] = "No"
        if (alpha == 0 & Copula == 16) strict[16] = "No"
        
        # What is the Range?
        RANGE = c(
            "[-1|Inf)", "[1|Inf)", "[-1|1)", "(-Inf|Inf)", "(0|1]", 
            "(0|0.5]", "(0|Inf)", "[2|Inf)")[No]
                 
        # Which one is the Limit Copula?
        limitTitle = rep("NA", times = 22)
        if (alpha == -1) 
            limitTitle = c(
                "W ", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                "NA", "NA", "NA", "NA", "NA", "NA", "Pi", "NA", "NA", "NA",
                "NA", "NA")   
        if (alpha == 0) 
            limitTitle = c(
                "Pi", "NA", "Pi", "NA", "Pi", "NA", "W ", "NA", "Pi", "Pi",
                "Pi", "NA", "NA", "NA", "NA", "W ", "NA", "NA", "L ", "Pi",
                "NA", "Pi")    
        if (alpha == 1) 
            limitTitle = c(
                "L ", "W ", "L ", "Pi", "NA", "Pi", "Pi", "W ", "NA", "NA",
                "NA", "L ", "Pi", "L ", "W ", "NA", "NA", "NA", "NA", "NA",
                "W ", "NA")
        limitTitle = limitTitle[Copula]
        if (limitTitle == "NA") {
            limitTitle = " "
        } else {
            limitTitle = paste("  Copula = ", limitTitle[1])
        }
        
        # Tau/Rho:
        Tau = round(approx(.ALPHA[, Copula], .TAU[, Copula], xout = alpha)$y,
            digits = 3)
        Rho = round(approx(.ALPHA[, Copula], .RHO[, Copula], xout = alpha)$y,
            digits = 3)
  
        # Title:
        Names = c(
            "- Clayton", "", "- Ali-Mikhail-Hag", "- Gumbel-Hougard", "- Frank",
            "- Joe-Frank", "", "", "- Gumbel-Barnett", "",
            "", "", "", "", "- Genest-Ghoudi", "", "", "", "", "", "", "")      
        Title = paste("Archimedean Copula No:", as.character(Copula), 
            Names[Copula], "\n", RANGE, " alpha =", as.character(alpha), 
            " tau =", as.character(Tau), " rho =", as.character(Rho)) 
        
        # Plot:   
        uv = grid2d(x = (1:(N-1)/N))
        D = .darchm1Copula(u = uv, alpha = alpha, type = Copula, 
            output = "list")
        image(D, xlim = c(0, 1), ylim = c(0,1), col = heat.colors(n.col) )
        contour(D, xlab = "u", ylab = "v", nlevels = n.lev, add = TRUE)
        title(main = Title)
                           
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    B = 5
    C1 = "1: [-1,Inf]"
    C2 = "2-4-6-8-12-14-15-21: [1,Inf)"
    C3 = "3: [-1,1)"
    C4 = "5-17: (-Inf,Inf)|{0}"
    C5 = "7-9-10-22: (0,1]"
    C6 = "11: (0, 1/2]"
    C7 = "13-16-19-20: (0,Inf)"
    C8 = "18: [2, Inf)" 
    C = c(   C1, C2,   C3,   C4,   C5,   C6,   C7,  C8 )  
    L = c(   -1,  1,   -1,   -B,    0,    0,    0,   2 )
    U = c(    B,  B,    1,    B,    1,  0.5,    B,   B )
    A = c(  0.5,  2,  0.5,    1,  0.5,  0.2,    1,   3 ) 
    V = rep(0.1, 8)
    plot.names = c("Plot - levels", "... colors")
    .sliderMenu(refresh.code,
        names       = c("Copula", "N", C, plot.names),
        minima      = c(       1,  10, L,   10,   12),
        maxima      = c(      22, 100, U,  100,  256),
        resolutions = c(       1,  10, V,   10,    1),
        starts      = c(       1,  30, A,   30,   64)) 
}


################################################################################

