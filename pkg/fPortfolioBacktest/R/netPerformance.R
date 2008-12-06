
### fBacktest - Utilities

################################################################################
    
netPerformance = 
    	function(object, format = "%Y-%m-%d"){
    		
    	# Settings:
    	cumP = object$portfolioReturns
    	cumB = object$benchmarkReturns
    	P = as.numeric(cumP)
    	B = as.numeric(cumB)
    	monthlyP = object$P
    	monthlyB = object$B
		char.dates = rownames(cumP)
    	dates = strptime(char.dates, format =  format)   	
    	# nye = new years eve
    	nye = as.character(dates[dates$mon == 11] )
    	years = substr(nye, 1,4)    	
    	nYears = length(years)
    	
    	## NET PERFORMANCE Plots:
    	.netPerformancePlot(dates, char.dates, years, nye, P)
    
    	## Tables::
    	
    	# NET PERFORMANCE TO YTD:
		netYTD = rbind(.netPerformanceYTD(char.dates, monthlyP, P, nYears),
					             .netPerformanceYTD(char.dates, monthlyB, B, nYears))
		rownames(netYTD) = c("Portfolio", "Benchmark")					   
    	
    	# NET PERFORMANCE CALENDAR YEAR:
    	netCalendar = rbind(.netPerformanceCalendar(nye, char.dates, P),
						    .netPerformanceCalendar(nye, char.dates, B))
		rownames(netCalendar) = c("Portfolio", "Benchmark")
		
		# print summary:
		cat("\nNET PERFORMANCE (%) TO", paste(rev(char.dates)[1], ":",sep = ""), "\n")
		print(round(netYTD,2))
		cat("\n\n# NET PERFORMANCE (%) CALENDAR YEAR:\n")
		print(round(netCalendar,2))
		cat("\n")
		
		# Return value:
		ans = list(YTD = netYTD, Calendar = netCalendar)
		invisible(ans)
		    	
}
   
# ------------------------------------------------------------------------------------------ #

# NET PERFORMANCE TO YTD:
    		
# summaries for last 1, 3, 6 months, 
# 1 year, 3 years, 5 years, 3 years annualised, 5 years annualised (if possible)
    		
    	.netPerformanceYTD = 
    		function(char.dates, monthlyP, P, nYears, ...){ 	
    
    		monthly = c(rev(monthlyP)[1], sum(rev(monthlyP)[1:3]), sum(rev(monthlyP)[1:6]))
		    
		    if (nYears >= 5){
				IDX = 1 + c(0,1,2,5)*12
    		  	yearly = numeric(length(IDX)-1)
    		  	for (i in 1:(length(IDX)-1)){
    			  	yearly[i] = rev(P)[IDX[1]] - rev(P)[IDX[i+1]]	
    		  	}
   
   			  	annualised = c((1+yearly[2]/100)^(1/3) - 1, (1+yearly[3]/100)^(1/5) - 1) * 100
   		
   			  	combine = c(monthly, yearly, annualised)
   			  	names(combine) = c("1 mth", paste(c(3,6), "mths"), 
   								     "1 yr", paste(c(3,5), "yrs"), 
   								     paste(c(3,5), "yrs p.a."))
   			} else {
   				IDX = 1 + seq(0, nYears) * 12
   				yearly = numeric(length(IDX)-1)
    		 	for (i in 1:(length(IDX)-1)){
    			  	yearly[i] = rev(P)[IDX[1]] - rev(P)[IDX[i+1]]	
    		 	 }
    		 	 Names = paste(seq(1:nYears), "yrs")
   			     # calculate some annualised rates
   			     if (nYears > 1){
   			     	ys = seq(nYears)
   			     	annualised = NULL
   			     	for (i in 2:nYears){
   			     	annualised = c(annualised, (1+yearly[i]/100)^(1/i)-1)   			     
   			     	}
   			     	annualised = annualised * 100
   			     	yearly = c(yearly, annualised)
   			     	Names = c(Names, paste(2:nYears, "yrs p.a."))
   			     }
   			     
   				combine = c(monthly, yearly)
   				names(combine) = c("1 mth", paste(c(3,6), "mths"), Names)
   			}
   				
			# return:   						   
   			combine
    	}

# ------------------------------------------------------------------------------------------ #    	
    	
 # NET PERFORMANCE CALENDER YEAR:   	
 
   	.netPerformanceCalendar = 
    		function(nye, char.dates, P, ...){
    		
    		nye1 = c(nye, char.dates[length(char.dates)])
    	
    		# first entry is the cumulated return at the end of first year
    		# assume we start with 0 returns
    		annuals = P[char.dates == nye[1]]
    		for (i in 1:(length(nye1)-1)){
    			annuals = c(annuals, P[char.dates == nye1[i+1]] - P[char.dates == nye1[i]])
    		}
    			
			Annual = c(annuals, sum(annuals))
			names(Annual) = c(substr(nye,1,4), "YTD", "Total")

			# Return value:
			Annual
    	}   	
   
# ------------------------------------------------------------------------------------------ #    	

# NET PERFORMANCE PLOT:

	.netPerformancePlot = 
    		function(dates, char.dates, years, nye, P, base = 100){
    		
    		
    		### Setup figure frame:
			Opar = par(oma = rep(0,4), mar = rep(0,4))
			
			mat = matrix(c(1,2,3), nr = 3, nc = 1)
			mat = rbind(0, cbind(0, mat, 0))
			
			layout(mat, widths = c(0, 1, 0), heights = c(lcm(0.3), lcm(0.8), 1, lcm(1)))

			### Add title: #####################
			plot.new()
			plot.window(xlim = c(0,1), ylim = c(0,1))
			rect(0,0,1,1,col = "grey50", border = NA)
			text(0.01,0.5, "Net Performance (rebased to 100)", font = 2, col = "white", adj = 0, cex = 1.8)
			
			###################################
			
    		# rebased to 100
    		newP = c(base, P + base)
    	
    		# limits:
    		ylim.pretty = pretty(newP)
    		yLim = range(ylim.pretty)
    		shortCalendar = 11 - rev(dates$mon)[1] # extend to the end of calendar year
    		xLim = c(1, length(char.dates) + shortCalendar)
    	
    		# create empty canvas:
    		opar = par(mar = c(2,5,1,4))
    		plot.new()
    		plot.window(xlim = xLim, ylim = yLim, xaxs = "i", yaxs = "i")    		
    	
    		# add bottom axes:
    		IDX = match(nye, char.dates)
    		temp.d = ifelse(dates$mon[1] != 0, dates$mon[1], 12) 
    		temp.y = ifelse(temp.d == 12, as.numeric(years)[1]-1, as.numeric(years)[1])     		
    		labs = c(paste(temp.d, temp.y, sep = "/"), paste(12,  years, sep = "/"))
    		if (shortCalendar !=0 ) labs = c(labs, paste(12, as.numeric(rev(years))[1] + 1, sep = "/"))
    		axis(1, at = sort(c(xLim,IDX + 1)), labels = labs, cex.axis = 1, padj = 0.5)#, tck = -0.05) 
    	
    		# add left axes:
    		yseq = seq(min(ylim.pretty), max(ylim.pretty), by = 5)
    		axis(2, at = yseq, las = 1, tick = FALSE, line = -0.7)
    		abline(h = yseq, col = "grey50")
    	
    		# draw portfolio performance:
    		lines(newP, col = "red", lwd = 2)
    	
    		# return
    		par(opar)
    		
    		########################################
    		
    		# add legend and extra text:
    		plot.new()
    		plot.window(xlim = c(0,1), ylim = c(0,1))
    		legend(0.015, 0.8, legend = "Portfolio", lty = "solid", lwd = 2, col = "red", bty = "n", cex = 1.1)
    		
			########################################

    		# Return value:
    		par(Opar)
    		invisible()
    	}
    	
################################################################################

    	
