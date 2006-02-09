
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
# FUNCTION:                 EXTREMAL INDEX:
#  exindexesPlot             Calculate and Plot Theta(1,2,3)
#  exindexPlot               Calculate Theta(1,2) and Plot Theta(1)
################################################################################


exindexesPlot = 
function (x, block=20, quantiles = seq(0.990,0.999,0.001), doplot = TRUE, ...)
{   # A function written by D. Wuertz
    
    # Description:
    # 	Calculates and Plots Theta(1,2,3)
    
    # FUNCTION:
    
    # Settings:
    main = "Extremal Index"
    doprint = FALSE
    
    # Block Size:
    blocklength = block # argument renamed
    
    # Note, in finance the x's should be residuals
    resid = x 
    
    # Extremal Index - Theta_1, Theta_2 and Theta_3
    k = floor(length(resid)/blocklength) # Number of blocks
    n = k*blocklength # Number of data points 
    
    # Now organize your residuels:
    # 1) truncate the rest of the time series,
    # 2) arrange them in matrix form,
    # 3) sort them in reverse order, ie. from high (pos) to low (neg)
    resid1 = resid[1:(k*blocklength)]
    resid1 = matrix(resid1, ncol = blocklength, byrow = TRUE)
    ordered1 = sort(resid1)
    
    # Threshold values associated to quantiles:
    z0 = ordered1[floor(quantiles*length(resid1))]
    
    # Printing:
    if (doprint) {print(z0); print(n); print(k) }
    
    # Presettings:
    theta1 = theta2 = theta3 = rep(0, times = length(quantiles))
    
    # Calculate Extremal Imdex:
    run = 0
    for ( z in z0 ) {
        run = run + 1
        # N - number of exceedences:
        N = length(resid1[resid1>z])
        # K - number of blocks with exceedences:
        K = sum(sign(apply(resid1,1,max)-z)+1)/2
        if (K/k < 1) theta1[run] = (k/n) * log(1-K/k) / log(1-N/n)
          else theta1[run] = NA 
        theta2[run] = K/N
        x = 1:n
        xx = diff(x[resid1 > z])
        xx = xx[xx>blocklength]
        theta3[run] = length(xx)/N
        # Printing: 
        if (doprint) {
            print(c(N, K, quantiles[run], z)) 
            print(c(theta1[run], theta2[run], theta3[run]))} }
    
    # Plotting:
    if (doplot) {
        plot(quantiles, theta1, 
            xlim = c(quantiles[1], quantiles[length(quantiles)]),
            ylim = c(0, 1.2), type = "b", pch = 1,
            ylab = " Theta 1,2,3", main = main, ...)
        points(quantiles, theta2, pch = 2, col = 3)
        points(quantiles, theta3, pch = 4, col = 4) }     
    
    # Return Value:
    data.frame(thresholds=z0, theta1=theta1, theta2=theta2, theta3=theta3)
}


# -----------------------------------------------------------------------------


exindexPlot = 
function(x, block = "month", start = 5, end = NA, 
plottype = c("thresh", "K"), labels = TRUE, autoscale = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #	Calculates Theta(1,2) and plots Theta(1)
 
    # Notes:
    #   Wraps "exindex" from Alexander McNeil's evir package

    # FUNCTION:
    
	# Wrapper:
	plottype = plottype[1]
	reverse = FALSE
	if (plottype == "K") reverse = TRUE
	ans = exindex(data = x, block = block , start = start, end = end, 
		reverse = reverse, auto.scale = autoscale, labels = labels, ...) 

    # Return Value:
    ans
}


# ******************************************************************************
