#
# Examples from the Monograph:
#   "Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 2.4
#   Rmetrics 'timeSeries' Objects
#
# List of Examples:                
#   
#	Example: Monthly Data
#	Example: Daily Data
#
# Author:
#   (C) 1997-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################
# Functions and methods for 'timeSeries' objects


	# Description:
	#   These are test cases for 'timeSeries' objects. We used these
	#   examples to check if the functions and methods included in the
	#   two files 'timeDateClass.R', 'timeDateMethods' and 'timeSeries'
	#	work properly. 
	# References:
	#   POSIXt from R's "base" package.


# ------------------------------------------------------------------------------


### Load Packages:

	require(fBasics)
	require(fCalendar)
	###
	

# ------------------------------------------------------------------------------


### Example: Monthly Data

	# Use JohnsonJohnson from 'stats'
	# Quarterly earnings (dollars) per Johnson & Johnson share 1960–80
	require(stats)
	data(JohnsonJohnson)
	JohnsonJohnson
	###
	  
	# Create a 'timeSeries' object
	args(timeSeries)
	myFinCenter = "GMT"
	dates = timeCalendar(y = rep(1960:1980, each = 4),
	 m = rep(c(3,6,9,12), 21), d = rep(c(31,30,30,31), 21))
	ts = timeSeries(as.vector(JohnsonJohnson), dates, units = "JJ")
	class(ts)
	is.timeSeries(ts)
	ts[1:3, ]
	###
	  
	# 'timeSeries' Data and Positions:
	ts.mat = ts@Data
	class(ts.mat)
	ts.mat[1:3, ]
	positions = ts@positions
	positions
	class(positions)
	# ts.df = as.data.frame(ts)
	# ts.df[1:3, ]
	###
	  
	# Apply - Aggregate Annually:
	c(start(ts), end(ts))
	from = timeCalendar(y = 1960:1980, m = 1)
	to = timeCalendar(y = 1960:1980, m = 12, d = 31)
	data.frame(from, to)[1:3, ]
	applySeries(ts, from, to, FUN = sum)
	###
	   
	# Merge 'timeSeries' with matrix:
	args(mergeSeries)
	ts2 = mergeSeries(x = ts, y = round(log(ts@Data), 4))
	ts2@units = c("JJ", "logJJ")
	colnames(ts2@Data) = ts2@units
	ts2[1:3, ]
	###
	 
	# Cut Out a Piece from a 'timeSeries':
	args(cutSeries)
	# The Last five years:
	cutSeries(ts2, from = "1976-03-31", to = "1980-12-31")
	###
    
  
# ------------------------------------------------------------------------------


### Example: Daily Data
	
	# Load SP 500 Data:
	data(sp.raw)
	###	
	
	# Create a 'timeSeries' Object:
	ts = timeSeries(sp.raw[,2], sp.raw[,1], units = "SP500", 
		format = "%m/%d/%Y")
	class(ts)
	ts[1:3,]
	c(start(ts), end(ts))
	###
		
	# Cut out April Data from 1980:
	ts.Apr80 = cutSeries(ts, "1980-04-01", "1980-04-30") 
	ts.Apr80
	###
		
	# Compute Returns:
	args(returnSeries)
	# Continuous Returns:
	returnSeries(ts.Apr80)
	# Discrete Returns:
	returnSeries(ts.Apr80, type = "discrete")
	# Don't trim:
	returnSeries(ts.Apr80, trim = FALSE)
	# Use Percentage Values:
	returnSeries(ts.Apr80, percentage = TRUE, trim = FALSE)
	###
	
	# Merge Series with Returns:
	# Include last Day from March:
	ts.APR80 = cutSeries(ts, "1980-03-31", "1980-04-30") 
	ts.merged = mergeSeries(x = ts.APR80, 
		y = returnSeries(ts.APR80, trim = FALSE)@Data,
		units = c("SP500", "Returns"))
	ts.merged
	###
		
	# Align with NA:
	args(alignDailySeries)
	ts.ret = returnSeries(ts.APR80, trim = TRUE)
	GoodFriday(1980)   # is a holiday
	EasterMonday(1980) # is not a holiday !?
	alignDailySeries(ts.ret, method = "fillNA")
	alignDailySeries(ts.ret, method = "fillNA", include.weekends = TRUE)
	###
		
	# Interpolate:
	ts.ret
	alignDailySeries(ts.ret, method = "interp")
	alignDailySeries(ts.ret, method = "interp", include.weekend = FALSE)
	###
		
	# Aggregate weekly:
	GoodFriday(1980)
	to = timeSequence(from = "1980-04-11", length.out = 3, 
		by = "weeks") 
	from = to - 6*24*3600
	data.frame(from, to)
	applySeries(ts.ret, from, to, sum)
	###
		
	# Plot:
	plot(ts, col = "steelblue4", xlab = "Year", ylab = "Index",
		main = "SP500")
	grid(lty = "solid")
	###
	
	
################################################################################

