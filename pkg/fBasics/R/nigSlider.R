
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             DESCRIPTION:
#  nigSlider             Displays normal inverse Gausssian distribution function
################################################################################


nigSlider <-
    function()
{
    # A function implemented by Diethelm Wuertz

    # Normal Inverse Gaussian Distribution:
    #   dnig(x, alpha = 1, beta = 0, delta = 1, mu = 0)

    # FUNCTION:

    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N     = fUtilities:::.sliderMenu(no = 1)
        alpha = fUtilities:::.sliderMenu(no = 2)
        beta  = fUtilities:::.sliderMenu(no = 3)
        delta = fUtilities:::.sliderMenu(no = 4)
        mu    = fUtilities:::.sliderMenu(no = 5)

        # Plot Data:
        xmin = round(qnig(0.01, alpha, beta, delta, mu), digits = 2)
        xmax = round(qnig(0.99, alpha, beta, delta, mu), digits = 2)
        s = seq(xmin, xmax, length = N)
        y1 = dnig(s, alpha, beta, delta, mu)
        y2 = pnig(s, alpha, beta, delta, mu)
        main1 = paste("NIG Density\n",
            "alpha = ", as.character(alpha), " | ",
            "beta = ", as.character(beta), " | ",
            "delta = ", as.character(delta), " | ",
            "mu = ", as.character(mu))
        main2 = paste("NIG Probability\n",
            "xmin 0.01% = ", as.character(xmin), " | ",
            "xmax 0.99% = ", as.character(xmax), " | ")

        # Frame:
        par(mfrow = c(2, 1), cex = 0.7)

        # Density:
        plot(s, y1, type = "l", xlim = c(xmin, xmax), col = "steelblue")
        abline (h = 0, lty = 3)
        title(main = main1)

        # Probability:
        plot(s, y2, type = "l", xlim = c(xmin, xmax), ylim = c(0, 1),
            col = "steelblue" )
        abline(h = 0.0, lty = 3)
        abline(h = 1.0, lty = 3)
        abline(h = 0.5, lty = 3)
        abline(v = mu, lty = 3, col = "red")
        title(main = main2)

        # Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }

    # Open Slider Menu:
    fUtilities:::.sliderMenu(refresh.code,
       names =       c( "N", "alpha", "beta", "delta", "mu"),
       minima =      c(  50,   0.00,   -2.00,    0.00, -5.0),
       maxima =      c(1000,   2.00,   +2.00,   10.00, +5.0),
       resolutions = c(  50,   0.20,    0.20,    1.00,  1.0),
       starts =      c(  50,   1.00,    0.00,    1.00,  0.0))
}


################################################################################

