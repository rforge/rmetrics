
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                TAILORED DENSITY FUNCTIONS:
#  histPlot                 Returns a tailored histogram plot
#  .cumHistPlot             Returns a tailored cumulated histogram plot
#  densityPlot              Returns a tailored kernel density estimate plot
#  logDensityPlot           Returns a tailored log kernel density estimate plot
################################################################################


histPlot <-  
    function(x, labels = TRUE, col = "steelblue", add.fit = TRUE, 
    grid = FALSE, rug = TRUE, skipZeros = TRUE, ...) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a probability histogram plot for each column of a 
    #   timeSeries object
    
    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.timeSeries()' into an object of class 'timeSeries'.
   
    # FUNCTION:
    
    # Settings:
    xlim = NULL
    
    # timeSeries:
    if (!is.timeSeries(x)) x = as.timeSeries(x)
    Units = x@units
    DIM = dim(x@Data)[2]
    if (length(col) == 1) col = rep(col, times = DIM)
      
    # Construct output list:
    ans = paste( " hist", 1:DIM, " = NULL", sep = "", collapse = ",")
    ans = paste( "list(",  ans, ")" )
    ans = eval(parse(text = ans))
    
    # Histogram Plots:
    for (i in 1:DIM) {
        
        # Histogram:
        Values = as.vector(x@Data[, i])
        if (skipZeros) Values = Values[Values != 0]
        mean = mean(Values)
        median = median(Values)
        sd = sd(Values)
                    
        # Plot:
        if (labels) {
            xlim = c(qnorm(0.001, mean, sd), qnorm(0.999, mean, sd)) 
            result = hist(x = Values, col = col[i], 
            border = "white", breaks = "FD", main = Units[i], 
            xlim = xlim, probability = TRUE, ...) 
            box()
        } else {
            result = hist(x = Values, ...)
        }
             
        # Add Fit:  
        if (add.fit) {
            s = seq(xlim[1], xlim[2], length = 201)
            lines(s, dnorm(s, mean, sd), lwd = 2, col = "brown")
        }
        ans[[i]] = result  
        
        # Add Mean/Median:
        abline(v = mean, lwd = 2, col = "orange")
        abline(v = median(Values), lwd = 2, col = "darkgreen")
        if (labels) {
            Text = paste("Median:", round(median, 2), "| Mean:", 
                signif(mean, 3))
            if (skipZeros) Text = paste(Text, "| Zeros skipped")
            mtext(Text, side = 4, adj = 0, col = "darkgrey", cex = 0.7)
        }
  
        if (grid) grid()
        
        # Add Zero Line:
        abline(h = 0, col = "grey")
    
        # Add Rug Plot:
        if(rug) {
            rug(Values, ticksize = 0.01, quiet = TRUE)
        }
    }
    
    # Return Value:
    invisible()
}  


# ------------------------------------------------------------------------------


 
.cumHistPlot =
function(x, ...)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns a tailored cumulated histogram plot
    
    # Source: 
    #   http://addictedtor.free.fr/graphiques/RGraphGallery.php?graph=126
    
    # Example:
    #   cumHist(x = rnorm(1000))
    
    # FUNCTION:
    
    # Check:
    stopifnot(isUnivariate(x))
    
    # Histogram:
    x <- as.vector(x)
    h <- hist(x, plot = FALSE, breaks = "FD")
    
    # Quick hack to transform histogram into cumulative histogram. 
    # Actually, only the first command is required but this is 
    # cleaner to do the 3 of them
    
    h$counts     <- cumsum(h$counts)
    h$density    <- cumsum(h$density)
    h$itensities <- h$density
    
    # Plot:
    plot(h, freq = TRUE, 
        main = "(Cumulative) histogram of x", 
        col = "steelblue", 
        border = "white", ...)
    box()
    
    # Return Value:
    invisible(h)
}


# ------------------------------------------------------------------------------


densityPlot <-  
    function(x, labels = TRUE, col = "steelblue", add.fit = TRUE, 
    grid = FALSE, rug = TRUE, skipZeros = TRUE, ...) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns density plots for each column of a timeSeries object
    
    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.timeSeries()' into an object of class 'timeSeries'.

    # FUNCTION:
    
    # timeSeries:
    if (!is.timeSeries(x)) x = as.timeSeries(x)
    Units = x@units
    DIM = dim(x@Data)[2]
    if (length(col) == 1) col = rep(col, times = DIM)
    
     
    # Histogram Plots:
    for (i in 1:DIM) {
        
        # Density:
        Values = as.vector(x@Data[, i])
        if (skipZeros) Values = Values[Values != 0]
        
        # Statistics:
        mean = mean(Values)
        median = median(Values)
        sd = sd(Values) 
        
        # Density Plot:
        Density = density(Values, ...)
        if (labels) {
            xlim = c(qnorm(0.001, mean, sd), qnorm(0.999, mean, sd)) 
            plot(x = Density, xlim = xlim, col = col[i], type = "l", 
                lwd = 2, main = units[i], ...)   
            if (grid) grid()
        } else {
            plot(x = Density, ...)
        }   
        
        # Add Fit:
        if (add.fit) {
            s = seq(xlim[1], xlim[2], length = 201)
            lines(s, dnorm(s, mean, sd), lwd = 2, col = "brown")
        }
        
        # Add Mean/Median:
        abline(v = mean, lwd = 2, col = "orange")
        abline(v = median(Values), lwd = 2, col = "darkgreen")
        if (labels) {
            Text = paste(
                "Median:", round(median, 2), 
                "| Mean:", round(mean, 2),
                "| Bandwidth:", round(Density$bw, 3) )
            if (skipZeros) Text = paste(Text, "| Zeros skipped")
            mtext(Text, side = 4, adj = 0, col = "darkgrey", cex = 0.7)
        }
  
        # Add Zero Line:
        abline(h = 0, col = "grey")
        
        # Add Rug Plot:
        if(rug) {
            rug(Values, ticksize = 0.01, quiet = TRUE)
        }     
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


logDensityPlot <- 
    function(x, labels = TRUE, col = "steelblue", 
    estimator = c("hubers", "sample", "both"), 
    grid = FALSE, rug = TRUE, skipZeros = TRUE, ...)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays a pdf plot on logarithmic scale 
    
    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.timeSeries()' into an object of class 'timeSeries'.
    #   estimator - the type of estimator to fit the mean and variance 
    #       of the density.
    #   doplot - a logical flag, by default TRUE. Should a plot be 
    #       displayed?
    #   labels - a logical flag, by default TRUE. Should a default main  
    #       title and labels addet to the plot?
    #   ... - 
    
    # Details:
    #   Returns a pdf plot on a lin-log scale in comparison to a Gaussian 
    #   density plot Two type of fits are available: a normal density with
    #   fitted sample mean and sample standard deviation, or a normal 
    #   density with Hubers robust mean and standard deviation corfrected
    #   by the bandwidth of the Kernel estimator.
    
    # FUNCTION:
    
    # timeSeries:
    if (!is.timeSeries(x)) x = as.timeSeries(x)
    Units = x@units
    DIM = dim(x@Data)[2]
    if (length(col) == 1) col = rep(col, times = DIM)
    
    # Select Type:
    estimator = match.arg(estimator)
    
    # Labels:
    if (labels) {
        main = "log PDF"
        xlab = "x"
        ylab = "log PDF"    
    } else {
        main = xlab = ylab = ""
    }
    
    X = x
    
    for (i in 1:ncol(x)) {
        
        # Transform Data:
        x = as.vector(X[, i])
        if (skipZeros) x = x[x != 0]
                
        if (labels) main = Units[i]
                
        # Kernel and Histogram Estimators: 
        Density = density(x)
        Histogram = hist(x, breaks = "FD", plot = FALSE)
        result = list(density = Density, hist = Histogram)
         
        # Plot Frame:
        plot(Histogram$mids, log(Histogram$density), type = "n",
            lwd = 5, main = Units[i], xlab = xlab, ylab = ylab,
            xlim = range(Density$x), ylim = log(range(Density$y)),
            col = col, ...)

        # Plot Density:
        points(Density$x, log(Density$y), pch = 19, col = "darkgrey",
            cex = 0.7)
        
        # Sample Line Fit:
        s = seq(min(Density$x), max(Density$x), length = 1001)
        if (estimator == "sample" || estimator == "both") {
            lines(s, log(dnorm(s, mean(x), sd(x))), col = "red", lwd = 2)
        } 
        
        # Robust Huber Line Fit:
        if (estimator == "hubers" || estimator == "both") {
            h = MASS::hubers(x)
            logDensity = log(dnorm(s, 
                mean = h[[1]], 
                sd = sqrt(h[[2]]^2+Density$bw^2)))
            minLogDensity = log(min(Density$y))
            lines(
                x = s[logDensity > minLogDensity], 
                y = logDensity[logDensity > minLogDensity], 
                col = "orange", lwd = 2)
        }
        
        # Plot Histogram:
        points(Histogram$mids, log(Histogram$density), pch = 19,
            col = "steelblue", ...)
          
        # Grid:
        if (labels) grid()
        
        # Add Rug Plot:
        if(rug) {
            rug(x, ticksize = 0.01, quiet = TRUE)
        }     
    }
    
    # Return Value:
    invisible(result)
}


################################################################################

