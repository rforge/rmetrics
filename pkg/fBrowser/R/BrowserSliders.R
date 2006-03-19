
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
# FUNCTION:                 
#  .gevSlider
#  .gpdSlider
#  .unitrootSlider
#  .snormSlider
#  .sstdSlider
#  .sgedSlider
# FUNCTIONS
#  .heavisideSlider
#  .besselSlider
#  .kummerSlider
#  .whittakerSlider
################################################################################


.gevSlider =  
function(GenerateRandomNumbers = FALSE)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N      = .sliderMenu(no = 1)
        xi     = .sliderMenu(no = 2)
        mu     = .sliderMenu(no = 3)
        sigma  = .sliderMenu(no = 4)
        
        # Compute Data:  
        xmin = round(qgev(0.00, xi, mu, sigma), digits = 2)
        xmax = round(qgev(0.99, xi, mu, sigma), digits = 2)
        s = seq(xmin, xmax, length = N)
        y1 = dgev(s, xi, mu, sigma)
        y2 = pgev(s, xi, mu, sigma)
        main1 = paste("GEV Density\n", 
            "xi = ", as.character(xi), " | ",
            "mu = ", as.character(mu), " | ",
            "sigma = ", as.character(sigma) )
        main2 = paste("GEV Probability\n",
            "xmin [0.00] = ", as.character(xmin), " | ",
            "xmax [0.99] = ", as.character(xmax) )       
        
        # Random Numbers:
        if (GenerateRandomNumbers)
            x <<- tkSaveAs(
                data = rgev(N, xi, mu, sigma),
                subject = "GEV Random Deviates",
                what = "x")
         
        # Frame:
        par(mfrow = c(2, 1), cex = 0.7)
        
        # Density:
        if (GenerateRandomNumbers) {
            hist(x, probability = TRUE, col = "steelblue", border = "white",
                xlim = c(xmin, xmax), ylim = c(0, 1.1*max(y1)), main = main1,
                nclass = 20 )
            lines(s, y1, col = "orange")
        } else {
            plot(s, y1, type = "l", xlim = c(xmin, xmax), col = "steelblue")
            abline (h = 0, lty = 3)
            title(main = main1)  
        }    
        
        # Probability:
        plot(s, y2, type = "l", xlim = c(xmin, xmax), ylim = c(0, 1),
            col = "steelblue" )
        abline (h = 0, lty = 3)
        title(main = main2) 
        
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c(   "N", "xi",  "mu", "sigma"),
       minima =      c(   50,  0.05, -5.00,   0.10 ),
       maxima =      c( 1000,  1.10, +5.00,   5.00 ),
       resolutions = c(   50,  0.05,  0.10,   0.10 ),
       starts =      c(  500,  0.25,  0.00,   1.00 )
    )
}


# ******************************************************************************


.gpdSlider = 
function(GenerateRandomNumbers = FALSE)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N     = .sliderMenu(no = 1)
        xi    = .sliderMenu(no = 2)
        mu    = .sliderMenu(no = 3)
        beta  = .sliderMenu(no = 4)
        
        # Compute Data:       
        xmin = round(qgpd(0.00, xi, mu, beta), digits = 2)
        xmax = round(qgpd(0.99, xi, mu, beta), digits = 2)
        s = seq(xmin, xmax, length = N)
        y1 = dgpd(s, xi, mu, beta)
        y2 = pgpd(s, xi, mu, beta)
        main1 = paste("GPD Density\n", 
            "xi = ", as.character(xi), " | ",
            "mu = ", as.character(mu), " | ",
            "beta = ", as.character(beta))
        main2 = paste("GPD Probability\n",
            "xmin [0.00] = ", as.character(xmin), " | ",
            "xmax [0.99] = ", as.character(xmax))   
            
        # Random Numbers:
        if (GenerateRandomNumbers)
            x <<- tkSaveAs(
                data = rgpd(N, xi, mu, beta),
                subject = "GPD Random Deviates",
                what = "x")
        
        # Frame:  
        par(mfrow = c(2, 1), cex = 0.7)
        
        # Density:
        if (GenerateRandomNumbers) {
            hist(x, probability = TRUE, col = "steelblue", border = "white",
                xlim = c(xmin, xmax), ylim = c(0, 1.1*max(y1)), main = main1,
                nclass = 50 )
            lines(s, y1, col = "orange")
        } else {
            plot(s, y1, type = "l", xlim = c(xmin, xmax), col = "steelblue")
            abline (h = 0, lty = 3)
            title(main = main1)  
        }   
        
        # Probability:
        plot(s, y2, type = "l", xlim = c(xmin, xmax), ylim = c(0, 1),
            col = "steelblue" )
        abline (h = 0, lty = 3)
        abline (v = mu, lty = 3)
        title(main = main2)   
        
        # Reset Frame:    
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c(   "N",  "xi",  "mu", "beta"),
       minima =      c(   50,  0.02, -5.00,   0.10 ),
       maxima =      c( 1000,  1.10, +5.00,   5.00 ),
       resolutions = c(   50,  0.01,  0.10,   0.10 ),
       starts =      c(  500,  0.25,  0.00,   1.00 )
    )
}


# ------------------------------------------------------------------------------


.unitrootSlider = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N         = .sliderMenu(no = 1)
        Samples   = .sliderMenu(no = 2)
        Trend     = .sliderMenu(no = 3)
        Statistic = .sliderMenu(no = 4)
         
        # Plot Data: 
        xmin = round(qunitroot(p = 0.01, n.sample = Samples, 
            trend = c("c", "nc", "ct", "ctt")[Trend], 
            statistic = c("t", "n")[Statistic]),
            digits = 2)
        xmax = round(qunitroot(p = 0.99, n.sample = Samples, 
            trend = c("c", "nc", "ct", "ctt")[Trend], 
            statistic = c("t", "n")[Statistic]), 
            digits = 2)
        x = seq(xmin, xmax, length = N)
        y = punitroot(q = x, n.sample = Samples, 
            trend = c("c", "nc", "ct", "ctt")[Trend], 
            statistic = c("t", "n")[Statistic])
        
        par(mfrow = c(1, 1), cex = 0.7)
        
        plot(x, y, col = "steelblue")
        abline (h = 0, lty = 3)
        
        samplesTitle = as.character(Samples)
        if (Samples == 0) samplesTitle = "Asymptotics"
        trendTitle = c("c", "nc", "ct", "ctt")[Trend]
        statisticsTitle = c("t", "n")[Statistic]
        main1 = paste(
            "Unitroot Probability  -  ", 
            "Samples = ", samplesTitle, " | ",
            "Trend = ",  trendTitle, " | ",
            "Statistics = ", statisticsTitle) 
        main2 = paste("\n\n", 
            "xmin [0.00] = ", as.character(xmin), " | ",
            "xmax [0.99] = ", as.character(xmax))    
        title(main = main1) 
        title(main = main2)      
        
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c(   "N", "Samples", "Trend", "Statistic"),
       minima =      c(   10,         0,       1,            1),
       maxima =      c(  200,       500,       4,            2),
       resolutions = c(   10,        10,       1,            1),
       starts =      c(   50,         0,       1,            1)
    )
}


################################################################################


.snormSlider = 
function(GenerateRandomNumbers = FALSE)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   dsnorm(x, mean = 0, sd = 1, xi = 1.5)
    
    # FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N      = .sliderMenu(no = 1)
        mean   = .sliderMenu(no = 2)
        sd     = .sliderMenu(no = 3)
        xi     = .sliderMenu(no = 4)
        invert = .sliderMenu(no = 5)
        
        # Compute Data:  
        if (invert == 1) xi = 1/xi
        xmin = round(qsnorm(0.001, mean, sd, xi), digits = 2)
        xmax = round(qsnorm(0.999, mean, sd, xi), digits = 2)
        s = seq(xmin, xmax, length = N)
        y1 = dsnorm(s, mean, sd, xi)
        y2 = psnorm(s, mean, sd, xi)
        main1 = paste("Skew Normal Density\n", 
            "mean = ", as.character(mean), " | ",
            "sd = ", as.character(sd), " | ",
            "xi = ", as.character(xi) )
        main2 = paste("Ske Normal Probability\n",
            "xmin [0.001] = ", as.character(xmin), " | ",
            "xmax [0.999] = ", as.character(xmax) ) 
            
        # Random Numbers:
        if (GenerateRandomNumbers) {
            x <<- rsnorm(N, mean, sd, xi)
            # Info:
            activeDataSet <<- paste("x = Skew Normal Random Deviates")
            infoLabelText <<- tclVar(paste("Active Series Data:", activeDataSet))
            tkconfigure(infoLabel, textvariable = infoLabelText)
            tkgrid(infoLabel)  
        }      
             
        # Frame:
        par(mfrow = c(2, 1), cex = 0.7)
        
        # Density:
        if (GenerateRandomNumbers) {
            hist(x, probability = TRUE, col = "steelblue", border = "white",
                xlim = c(xmin, xmax), ylim = c(0, 1.1*max(y1)), main = main1 )
            lines(s, y1, col = "orange")
        } else {
            plot(s, y1, type = "l", xlim = c(xmin, xmax), col = "steelblue")
            abline (h = 0, lty = 3)
            title(main = main1)  
        }
            
        # Probability:
        plot(s, y2, type = "l", xlim = c(xmin, xmax), ylim = c(0, 1),
            col = "steelblue" )
        abline (h = 0, lty = 3)
        title(main = main2) 
        
        # Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c(   "N", "mean",   "sd",  "xi", "xi.inv"),
       minima =      c(   10,    -5.0,    0.1,   1.0,       0 ),
       maxima =      c(  500,    +5.0,    5.0,  10.0,       1 ),
       resolutions = c(   10,     0.1,    0.1,   0.1,       1 ),
       starts =      c(  100,     0.0,    1.0,   1.0,       0 )
    )
}


# ------------------------------------------------------------------------------


.sstdSlider = 
function(GenerateRandomNumbers = FALSE)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Description:
    #   dsstd(x, mean = 0, sd = 1, nu = 5, xi = 1.5)
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N      = .sliderMenu(no = 1)
        mean   = .sliderMenu(no = 2)
        sd     = .sliderMenu(no = 3)
        nu     = .sliderMenu(no = 4)
        xi     = .sliderMenu(no = 5)
        invert = .sliderMenu(no = 6)
        
        # Compute Data:  
        if (invert == 1) xi = round(1/xi, digits = 4)
        xmin = round(qsstd(0.01, mean, sd, nu, xi), digits = 2)
        xmax = round(qsstd(0.99, mean, sd, nu, xi), digits = 2)
        s = seq(xmin, xmax, length = N)
        y1 = dsstd(s, mean, sd, nu, xi)
        y2 = psstd(s, mean, sd, nu, xi)
        main1 = paste("Skew Student-t Density\n", 
            "mean = ", as.character(mean), " | ",
            "sd = ", as.character(sd), " | ",
            "nu = ", as.character(nu), " | ",
            "xi = ", as.character(xi) )
        main2 = paste("Ske Student-t Probability\n",
            "xmin [0.01] = ", as.character(xmin), " | ",
            "xmax [0.99] = ", as.character(xmax) )   
            
        # Random Numbers:
        if (GenerateRandomNumbers) {
            x <<- rsstd(N, mean, sd, nu, xi)
            # Info:
            activeDataSet <<- paste("x = Skew Student-t Random Deviates")
            infoLabelText <<- tclVar(paste("Active Series Data:", activeDataSet))
            tkconfigure(infoLabel, textvariable = infoLabelText)
            tkgrid(infoLabel)  
        }      
        
        # Frame:    
        par(mfrow = c(2, 1), cex = 0.7)
        
        # Density:
        if (GenerateRandomNumbers) {
            hist(x, probability = TRUE, col = "steelblue", border = "white",
                xlim = c(xmin, xmax), ylim = c(0, 1.1*max(y1)), main = main1 )
            lines(s, y1, col = "orange")
        } else {
            plot(s, y1, type = "l", xlim = c(xmin, xmax), col = "steelblue")
            abline (h = 0, lty = 3)
            title(main = main1)  
        }
            
        # Probability:
        plot(s, y2, type = "l", xlim = c(xmin, xmax), ylim = c(0, 1),
            col = "steelblue" )
        abline (h = 0, lty = 3)
        title(main = main2) 
        
        # Frame:
        par(mfrow = c(1, 1), cex = 0.7) 
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c(   "N", "mean",  "sd",  "nu",  "xi", "xi.inv"),
       minima =      c(   10,   -5.0,    0.1,   2.1,   1.0,       0 ),
       maxima =      c(  500,   +5.0,    5.0,  10.0,  10.0,       1 ),
       resolutions = c(   10,    0.1,    0.1,   0.1,   0.1,       1 ),
       starts =      c(  100,    0.0,    1.0,   5.0,   1.0,       0 )
    )
}


# ------------------------------------------------------------------------------


.sgedSlider = 
function(GenerateRandomNumbers = FALSE)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Description:
    #   dsstd(x, mean = 0, sd = 1, nu = 5, xi = 1.5)
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N      = .sliderMenu(no = 1)
        mean   = .sliderMenu(no = 2)
        sd     = .sliderMenu(no = 3)
        nu     = .sliderMenu(no = 4)
        xi     = .sliderMenu(no = 5)
        invert = .sliderMenu(no = 6)
        
        # Compute Data:  
        if (invert == 1) xi = round(1/xi, digits = 4)
        xmin = round(qsged(0.01, mean, sd, nu, xi), digits = 2)
        xmax = round(qsged(0.99, mean, sd, nu, xi), digits = 2)
        s = seq(xmin, xmax, length = N)
        y1 = dsstd(s, mean, sd, nu, xi)
        y2 = psstd(s, mean, sd, nu, xi)
        main1 = paste("Skew GED Density\n", 
            "mean = ", as.character(mean), " | ",
            "sd = ", as.character(sd), " | ",
            "nu = ", as.character(nu), " | ",
            "xi = ", as.character(xi) )
        main2 = paste("Skew GED Probability\n",
            "xmin [0.01] = ", as.character(xmin), " | ",
            "xmax [0.99] = ", as.character(xmax) )   
            
        # Random Numbers:
        if (GenerateRandomNumbers) {
            x <<- rsstd(N, mean, sd, nu, xi)
            # Info:
            activeDataSet <<- paste("x = Skew GEDt Random Deviates")
            infoLabelText <<- tclVar(paste("Active Series Data:", activeDataSet))
            tkconfigure(infoLabel, textvariable = infoLabelText)
            tkgrid(infoLabel)  
        }      
        
        # Frame:    
        par(mfrow = c(2, 1), cex = 0.7)
        
        # Density:
        if (GenerateRandomNumbers) {
            hist(x, probability = TRUE, col = "steelblue", border = "white",
                xlim = c(xmin, xmax), ylim = c(0, 1.1*max(y1)), main = main1 )
            lines(s, y1, col = "orange")
        } else {
            plot(s, y1, type = "l", xlim = c(xmin, xmax), col = "steelblue")
            abline (h = 0, lty = 3)
            title(main = main1)  
        }
            
        # Probability:
        plot(s, y2, type = "l", xlim = c(xmin, xmax), ylim = c(0, 1),
            col = "steelblue" )
        abline (h = 0, lty = 3)
        title(main = main2) 
        
        # Frame:
        par(mfrow = c(1, 1), cex = 0.7) 
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c(   "N", "mean",  "sd",  "nu", "xi", "xi.inv"),
       minima =      c(   10,    -5.0,   0.1,   2.1,  1.0,       0 ),
       maxima =      c(  500,    +5.0,   5.0,  10.0, 10.0,       1 ),
       resolutions = c(   10,     0.1,   0.1,   0.1,  0.1,       1 ),
       starts =      c(  100,     0.0,   1.0,   5.0,  1.0,       0 )
    )
}


################################################################################


.heavisideSlider = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N     = .sliderMenu(no = 1)
        from  = .sliderMenu(no = 2)
        to    = .sliderMenu(no = 3)
        a     = .sliderMenu(no = 4)
         
        # Compute Data:  
        xmin = from
        xmax = to
        x = seq(xmin, xmax, length = N)
        y.H = H(x, a)
        y.Sign = Sign(x, a)
        y.Boxcar = Boxcar(x, a)
        y.Ramp = Ramp(x, a)
        
        # Plot:
        par(mfrow = c(2, 2), cex = 0.7) 
        plot(x, y.H, main = "H", ylab = "H", col = "steelblue")
        abline (h = 0, lty = 3)
        plot(x, y.Sign, main = "Sign", ylab = "Sign", col = "steelblue")
        abline (h = 0, lty = 3)
        plot(x, y.Boxcar, main = "Boxcar", ylab = "Boxcar", col = "steelblue")
        abline (h = 0, lty = 3)
        plot(x, y.Ramp, main = "Ramp", ylab = "Ramp", col = "steelblue")
        abline (h = 0, lty = 3)
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c(   "N", "from",   "to", "a"),
       minima =      c(    5,     -4,      6,   -40),
       maxima =      c(  101,    -40,     60,   +60),
       resolutions = c(    1,      1,      1,     1),
       starts =      c(   11,     -4,      6,     1)
    )
}


# ------------------------------------------------------------------------------


.besselSlider = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N      = .sliderMenu(no = 1)
        nu     = .sliderMenu(no = 2)
        scaled = .sliderMenu(no = 3)
         
        # Plot Data:  
        xmin = 0
        xmax = 10
        par(mfrow = c(2, 2), cex = 0.7)
        x = seq(xmin, xmax, length = N)
        
        y1 = BesselI(x, nu, scaled)    
        plot(x, y1, , col = "steelblue")
        abline (h = 0, lty = 3)
        title(main = "I - Bessel Function of the 1st Kind")    
        if (scaled == 1) title(main = "\n\n Exponentially Scaled")
        
        y2 = BesselK(x[-(1:2)], nu, scaled)    
        plot(x[-(1:2)], y2, , col = "steelblue", xlim = c(xmin, xmax))
        abline (h = 0, lty = 3)
        title(main = "K - Bessel Function of the 2nd Kind")    
        if (scaled == 1) title(main = "\n\n Exponentially Scaled")  
        abline(v = 0, lty = 3)
        
        y3 = BesselDI(x, nu)    
        plot(x, y3, , col = "steelblue")
        abline (h = 0, lty = 3)
        title(main = "I - Derivative")  
        
        y4 = BesselDK(x[-(1:2)], nu)    
        plot(x[-(1:2)], y4, , col = "steelblue", xlim = c(xmin, xmax))
        abline (h = 0, lty = 3)
        title(main = "K - Derivative")  
        abline(v = 0, lty = 3)  
            
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c(   "N", "nu", "scaled"),
       minima =      c(   10,     1,        0),
       maxima =      c(  200,    10,        1),
       resolutions = c(   10,     1,        1),
       starts =      c(   50,     1,        0)
    )
}


# ------------------------------------------------------------------------------


.kummerSlider = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N = .sliderMenu(no = 1)
        a = .sliderMenu(no = 2)
        b = .sliderMenu(no = 3)
         
        # Plot Data:  
        xmin = -5.0
        xmax =  5.0
        par(mfrow = c(2, 1), cex = 0.7)
        x = seq(xmin, xmax, length = N)
        
        M = kummerM(x, a, b)    
        plot(x, M, col = "steelblue", pch = 19)
        abline (h = 0, lty = 3)
        title(main = "Kummer M Function") 
        title(main = paste("\n\na =", as.character(a), 
            "| b = ", as.character(b) ) ) 
        
        U = kummerU(x, a, b)    
        plot(x, U, col = "steelblue", pch = 19)
        abline (h = 0, lty = 3)
        title(main = "Kummer U Function") 
        title(main = paste("\n\na =", as.character(a), 
            "| b = ", as.character(b) ) )   
            
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c(   "N",   "a",   "b"),
       minima =      c(   10,   -6.0,  -6.0),
       maxima =      c(  200,   +6.0,  +6.0),
       resolutions = c(   10,    0.2,   0.2),
       starts =      c(   50,    0.2,  -0.2)
    )
}


# ------------------------------------------------------------------------------


.whittakerSlider = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N = .sliderMenu(no = 1)
        kappa = .sliderMenu(no = 2)
        mu = .sliderMenu(no = 3)
         
        # Plot Data:  
        xmin = -5.0
        xmax =  5.0
        par(mfrow = c(2, 1), cex = 0.7)
        x = seq(xmin, xmax, length = N)
        
        M = whittakerM(x, kappa, mu)    
        plot(x, M, col = "steelblue", pch = 19)
        abline (h = 0, lty = 3)
        title(main = "Whittaker M Function") 
        title(main = paste("\n\na =", as.character(kappa), 
            "| b = ", as.character(mu) ) ) 
        
        W = whittakerW(x, kappa, mu)     
        plot(x, W, col = "steelblue", pch = 19)
        abline (h = 0, lty = 3)
        title(main = "Whittaker W Function") 
        title(main = paste("\n\na =", as.character(kappa), 
            "| b = ", as.character(mu) ) )   
            
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c(   "N", "kappa",  "mu"),
       minima =      c(   10,     -6.0,  -6.0),
       maxima =      c(  200,     +6.0,  +6.0),
       resolutions = c(   10,      0.2,   0.2),
       starts =      c(   50,      0.2,  -0.2)
    )
}


################################################################################

