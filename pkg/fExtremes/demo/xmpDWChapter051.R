#
# Examples from the Monograph:
# 	"Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 5.1
#   Extreme Value Plots
#
# List of Examples, Exercises and Code Snippets:
#
#   5.1.1 Example: Quantile-Quantile Plot
#       * Code Snipptet: qqPlot
#       * Example: Create Figure 5.1.1 - DAX Data
#       * Example: Create Figure 5.1.1 - BMW Data
#   5.1.2 Example: Mean Excess Function Plot - Create Figure 5.1.2
#       * Example: Mean Residual Life Plot - Create Figure 5.1.3
#
#   *** This list is not yet complete ***
#
# Author:
#	(C) 2002-2004, Diethelm Wuertz, GPL
# 	  www.rmetrics.org
# 	  www.itp.phys.ethz.ch
# 	  www.finance.ch
#


################################################################################


### Load Library:

	# Load:
	require(fExtremes)
	###
	
	
# ------------------------------------------------------------------------------


### 5.1.1 Example: Quantile-Quantile Plot

	# Load and Plot the Data 
    DAX.RET = as.timeSeries(data(dax.ret))
    class(DAX.RET)
    head(DAX.RET)
    qqPlot(DAX.RET, pch = 19, col = "steelblue", cex = 0.7)
    ###
    
    
# ------------------------------------------------------------------------------


### Code Snipptet: qqPlot
    
    # Quantile-Quantile Plot:
    .qqPlot = function(x, ...)
    {
	    x = as.vector(x)
	    qqnorm(x, ...)
	    qqline(x)
	    invisible()
    }
    .qqPlot(DAX.RET)
    ###
    
  
# ------------------------------------------------------------------------------


### Example: Create Figure 5.1.1 - DAX Data

    # Graph Frame:
    par(mfcol = c(2, 2), cex = 0.7)
    ###
    
    # Load and Plot the Data 
    DAX.RET = as.timeSeries(data(dax.ret))  
    plot(DAX.RET, main = "DAX Daily log Returns", ylab = "log Return")
    qqPlot(DAX.RET)
    ###
    
   
# ------------------------------------------------------------------------------
    
    
### Example: Create Figure 5.1.1 - BMW Data

	# Load and Plot the Data 
    BMW.RET = as.timeSeries(data(bmw.ret))
    plot(BMW.RET, main = "BMW Daily log Returns", ylab = "log Return")
    qqPlot(BMW.RET)
    ###
    
    
# ------------------------------------------------------------------------------
    
 
### 5.1.2 Example: Mean Excess Function Plot - Create Figure 5.1.2

    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Exponential Variates:
    set.seed(4711)
    mxfPlot(rexp(1000, rate = 2), tail = 0.20)
   	title(main = "\n\nExponential DF")
    abline(0.5, 0)
    ###

    # Normal Variates:
    set.seed(4711)
    mxfPlot(rlnorm(1000, meanlog = 0, sdlog = 2), tail = 0.20)
    title(main = "\n\nLognormal DF")
    ###

    # Symmetric Stable Variates:
    set.seed(4711)
    mxfPlot(rsymstb(1000, alpha = 1.7), tail = 0.20)
    title(main = "\n\n1.7 stable DF")
    abline(0, 0.7)
    ###
    
    # DAX log Returns:
    mxfPlot(-100*DAX.RET, tail = 0.20)
    title(main = "\n\nDAX log Returns %")
	###
	
    
# ------------------------------------------------------------------------------
    
    
### Example: Mean Residual Life Plot - Create Figure 5.1.3


	# Graph Frame:
	par(mfrow = c(2, 2), cex = 0.7)	
	###
	
	# Settings:	
	seed = c(456, 745, 145, 120) 		
	mains = c(
		"MRL Plot - Sample 1", "MRL Plot - Sample 2", 
		"MRL Plot - Sample 3", "MRL Plot - Sample 4") 
	n = 5000

	# Create Plots With Your Own Labels and Title:
	for (i in 1:4) {
		set.seed(seed[i])
		mrlPlot(rsymstb(n, alpha = 1.7), nint = 100, 
			labels = FALSE, plottype = "", xlim = c(0, 60), 
			ylim = c(-50, 150))
		title(xlab = "u", ylab = "e", main = mains[i])	
		grid()		
	} 
	###
	
	
# ------------------------------------------------------------------------------

		
### Example: Mean Excess Function Plot
	
	# Exponential distribution function:
	set.seed(7138)
	mxfPlot(rexp(n, rate = 2), tail = 1, labels = FALSE)
	title(xlab = "Threshold: u", ylab = "Mean Excess: e",
		main = "Exponential DF")
	abline(0.5, 0)
	###
	
	# Lognormal distribution function
	set.seed(6952)
	mxfPlot(rlnorm(n, meanlog = 0, sdlog = 2), tail = 1,
		xlim = c(0, 90), ylim = c(0, 150), labels = FALSE)
	title(xlab = "Threshold: u", ylab = "Mean Excess: e",
		main = "Lognormal DF")
	###
		
	# Alpha-stable distribution function:
	set.seed(9835)
	mxfPlot(rsymstb(n, alpha = 1.7), tail = 0.1,
		xlim = c(0, 10), ylim = c(0, 6), labels = FALSE)
	title(xlab = "Threshold: u", ylab = "Mean Excess: e",
		main = "1.7 stable DF")
	abline(0, 0.7)
	###

	
# ------------------------------------------------------------------------------

     
### Example 5.1.4: Subsample Records Plot}

	
    # Graph Frame:
    par(mfrow = c(3, 2), cex = 0.7)
    ###
    
    # Simulate Stable Data:
    stable = rsymstb(n = 8000, alpha = 1.7)
    ###

    # Load BMW and NYSE Data:
    data(bmwres)
    data(nyseres)
    ###

    # Plot on Logarithmic Scale:
    ssrecordsPlot(stable, subsamples = 8, plottype = "log")
    title(main = "\n\n1.7-stable Returns")
    ssrecordsPlot(bmwres, subsamples = 6, plottype = "log")
    title(main = "\n\nBMW Returns")
    ssrecordsPlot(nyseres, subsamples = 8, plottype = "log")
    title(main = "\n\nNYSE Returns")
    ###

    # Plot on Linear Scale:
    ssrecordsPlot(stable, subsamples = 8, plottype = "lin")
    title(main = "\n\n1.7-stable Returns")
    ssrecordsPlot(bmwres, subsamples = 6, plottype = "lin")
    title(main = "\n\nBMW Returns")
    ssrecordsPlot(nyseres, subsamples = 8, plottype = "lin")
    title(main = "\n\nNYSE Returns")
    ###
    
  
# ------------------------------------------------------------------------------

  
### Example 5.1.5: Plot of Records}

    # Graph Frame:
    par(mfrow = c(1, 1))
    ###
    
    # Normal Records Plot:
    ans = recordsPlot(rnorm(50000))
    print(ans)
    ###

    
# ------------------------------------------------------------------------------


### Example 5.1.6: Ratio of Maximum and Sum Plot

    # Graph Frame:
    par(mfrow = c(3, 2), cex = 0.7)
    
    # Load Data:
    data(bmwres)
    data(nyseres)
    ###

    # Create Plots:
    msratioPlot (rnorm(1000))
    title(main = "\n\nStandard Normal")
    msratioPlot (rexp(10000))
    title(main = "\n\nExponential")
    msratioPlot (rt(10000, 4))
    title(main = "\n\nStudent-t")
    msratioPlot (rsymstb(1000, 1))
    title(main = "\n\nCauchy")
    msratioPlot (bmwres)
    title(main = "\n\nBMW Returns")
    msratioPlot (nyseres)
    title(main = "\n\nNYSE Returns")
    ###
    
 
################################################################################

   