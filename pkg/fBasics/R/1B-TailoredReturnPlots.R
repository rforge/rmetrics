
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
# You should have received A copy of the GNU Library General 
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
# FUNCTION:                 TAILORED PLOT FUNCTIONS:     
#  seriesPlot                Returns a tailored return series plot
#  histPlot                  Returns a tailored histogram plot
#  densityPlot               Returns a tailored kernel density estimate plot
#  qqbayesPlot               Returns a tailored quantile-quantile plot
################################################################################


seriesPlot = 
function(x, col = "steelblue", main = x@units, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns time series graphs in a common plot
  
    # Arguments:
    #   x - an univariate time series
    
    # Example:
    # tS=timeSeries(cbind(rnorm(12),rt(12,4)),timeCalendar(),units=c("N","T"))
    # seriesPlot(tS)
    
    # FUNCTION:

    # timeSeries:
    stopifnot(is.timeSeries(x))
    units = x@units
    DIM = dim(x@Data)[2]
    
    # Series Plots:
    for (i in 1:DIM) {
        X = x[, i]
        plot(x = X, type = "l", col = col, main = main[i], ylab = X@units, ...)
        grid()
        abline(h = 0, col = "grey")
        rug(as.vector(X), ticksize = 0.01, side = 4)
    }
         
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


histPlot = 
function(x, col = "steelblue", main = x@units, add.fit = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a probability histogram plot for each column of a 
    #   timeSeries object
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Settings:
    xlim = NULL
    
    # timeSeries:
    stopifnot(is.timeSeries(x))
    units = x@units
    DIM = dim(x@Data)[2]
      
    # Construct output list:
    ans = paste( " hist", 1:DIM, " = NULL", sep = "", collapse = ",")
    ans = paste( "list(",  ans, ")" )
    ans = eval(parse(text = ans))
    
    # Histogram Plots:
    for (i in 1:DIM) {
        
        # Histogram:
        Values = as.vector(x@Data[, i])
        mean = mean(Values)
        median = median(Values)
        sd = sd(Values)
        if (is.null(xlim)) 
            xlim = c(qnorm(0.001, mean, sd), qnorm(0.999, mean, sd)) 
        result = hist(x = Values, col = col, border = "white", 
            breaks = "FD", main = main[i], xlim = xlim, probability = TRUE,
            ...) 
             
        # Add Fit:  
        if (add.fit) {
            s = seq(xlim[1], xlim[2], length = 201)
            lines(s, dnorm(s, mean, sd), lwd = 2, col = "brown")
        }
        ans[[i]] = result  
        
        # Add Mean/Median:
        abline(v = mean, lwd = 2, col = "orange")
        abline(v = median(Values), lwd = 2, col = "darkgreen")
        Text = paste("Median:", round(median, 2), "| Mean:", round(mean, 2))
        mtext(Text, side = 4, adj = 0, col = "darkgrey", cex = 0.7)
  
        # Add Zero Line:
        abline(h = 0, col = "grey")
        
        # Add Rug Plot:
        rug(Values)
    }
    
    # Return Value:
    invisible()
}  


# ------------------------------------------------------------------------------


densityPlot = 
function(x, col = "steelblue", main = x@units, add.fit = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns density plots for each column of a 
    #   timeSeries object

    # Changes:
    #
    
    # FUNCTION:
    
    # Transform 'timeSeries':
    stopifnot(is.timeSeries(x))
    units = x@units
    DIM = dim(x@Data)[2]
    xlim = NULL
      
    # Construct output list:
    ans = paste( " hist", 1:DIM, " = NULL", sep = "", collapse = ",")
    ans = paste( "list(",  ans, ")" )
    ans = eval(parse(text = ans))
    
    # Histogram Plots:
    for (i in 1:DIM) {
        
        # Density:
        Values = as.vector(x@Data[, i])
        mean = mean(Values)
        median = median(Values)
        sd = sd(Values)
        if (is.null(xlim)) 
            xlim = c(qnorm(0.001, mean, sd), qnorm(0.999, mean, sd)) 
        Density = density(Values, ...)
        plot(x = Density, xlim = xlim, col = col, type = "l", 
            lwd = 2, main = main[i], ...)  
        ans[[i]] = Density  
        
        # Grid:
        grid()
        
        # Add Fit:
        if (add.fit) {
            s = seq(xlim[1], xlim[2], length = 201)
            lines(s, dnorm(s, mean, sd), lwd = 2, col = "brown")
        }
        
        # Add Mean/Median:
        abline(v = mean, lwd = 2, col = "orange")
        abline(v = median(Values), lwd = 2, col = "darkgreen")
        Text = paste(
            "Median:", round(median, 2), 
            "| Mean:", round(mean, 2),
            "| Bandwidth:", round(Density$bw, 3) )
        mtext(Text, side = 4, adj = 0, col = "darkgrey", cex = 0.7)
  
        # Add Zero Line:
        abline(h = 0, col = "grey")
        
        # Add Rug Plot:
        rug(Values)     
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


quantilePlot = 
function(x, col = "steelblue", main = x@units, labels = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Example of a Normal quantile plot of data x to provide a visual
    #   assessment of its conformity with a normal (data is standardised    
    #   first).

    # Details:
    #   The ordered data values are posterior point estimates of the 
    #   underlying quantile function. So, if you plot the ordered data 
    #   values (y-axis) against the exact theoretical quantiles (x-axis),   
    #   you get a scatter that should be close to a straight line if the 
    #   data look like a random sample from the theoretical distribution. 
    #   This function chooses the normal as the theory, to provide a 
    #   graphical/visual assessment of how normal the data appear.
    #   To help with assessing the relevance of sampling variability on 
    #   just "how close" to the normal the data appears, we add (very) 
    #   approximate posterior 95% intervals for the uncertain quantile 
    #   function at each point (Based on approximate theory) .

    # Author:
    #   Based on code written by Mike West, mw@stat.duke.edu 
    
    # Note:
    #   Source from
    #   http://www.stat.duke.edu/courses/Fall99/sta290/Notes/

    # FUNCTION:
    
    # timeSeries:
    stopifnot(is.timeSeries(x))
    DIM = dim(x@Data)[2]
    Main = main
    
    # QQ Plots:
    X = x
    for (i in 1:DIM) {
        x = X[, i]
    
        # Settings
        mydata = as.vector(X[, i])
        n = length(mydata) 
        p = (1:n)/(n+1)
        x = (mydata-mean(mydata))/sqrt(var(mydata))
        x = sort(x)
        z = qnorm(p)
     
        # Plot:
        if (labels) {
            xlab = "Standard Normal Quantiles"
            ylab = paste(Main[i], "Ordered Data")
            main = paste(Main[i], "with 95% CI")  
        } else {
            main = xlab = ylab = ""
        }
        if (labels) {
            plot(z, x, pch = 19, col = col, 
                xlab = xlab, ylab = ylab, main = main, ...)
            abline(0, 1, col = "grey")
            grid() 
        } else {
            plot(z, x, 
                xlab = xlab, ylab = ylab, main = main, ...)
            abline(0, 1, col = "grey")
        }
        rug(z, ticksize = 0.01, side = 3)
        rug(x, ticksize = 0.01, side = 4)
      
        # 95% Intervals:
        s = 1.96*sqrt(p*(1-p)/n)
        pl = p-s
        i = pl<1&pl>0
        lower = quantile(x, probs = pl[i])
        lines(z[i], lower, col = "brown")
        pl = p+s
        i = pl < 1 & pl > 0
        upper = quantile(x, probs = pl[i])
        lines(z[i], upper, col = "brown")
    }
    
    # Return Value:
    invisible()  
}


################################################################################

