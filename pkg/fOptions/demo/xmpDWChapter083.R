#
# Examples from the forthcoming Monograph:
# 	Rmetrics - Financial Engineering and Computational Finance
#   A Practitioner's Guide for R and SPlus Programmers
#   written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Examples from Chapter 7.3
#   Exponential Brownian Motion 
#
# List of Examples, Exercises and Code Snippets
#
#   *** This list is not yet complete ***
#
# Author:
#	(C) 2002-2005, Diethelm Wuertz, GPL
# 	  www.rmetrics.org
# 	  www.itp.phys.ethz.ch
# 	  www.finance.ch
#


################################################################################


# xmpEBMTables        Tables from Continuous Sampled Options by Wuertz
# xmpEBMFigures       Figures from Continuous Sampled Options by Wuertz
# xmpEBMExamples      Examples from Continuous Sampled Options by Wuertz



################################################################################
# Tables:


### Example: Table 1 - Moment Matched Asian Option Prices
    
    # Take the same parameter settings as used in Zhang's Table
    Zhang = ZhangTable()
    options(digits = 4)
    Zhang
    ###
    
    # Calculate entries for Table 1:
    CallRST = BoundsOnAsianOption(table = Zhang, method = "RST")$price
    CallLN  = MomentMatchedAsianOption(table = Zhang, method = "LN")$price 
    CallRG  = MomentMatchedAsianOption(table = Zhang, method = "RG")$price
    CallJI  = MomentMatchedAsianOption(table = Zhang, method = "JI")$price
    CallT   = BoundsOnAsianOption(table = Zhang, method = "T")$price            
    CallZ   = ZhangTable()[,6]
    ###
    
    # Print Call Option Values: 
    Table.1 = data.frame(cbind(Zhang[, 1:5], 
        CallRST, CallLN, CallRG, CallJI, CallT, CallZ))
    Table.1
    ###
    
    # Save Table.1 and beautify Spreadsheet file:
    # write.table(Table.1, "TableApprox.csv", sep=";")
    ###
    
    # Compute Inside Bounds:
    inside = c(
        LN = sum((CallRST < CallLN)+ (CallLN < CallT) == 2),
        RG = sum((CallRST < CallRG)+ (CallRG < CallT) == 2),
        JI = sum((CallRST < CallJI)+ (CallJI < CallT) == 2))
    inside
    ###

    
# ------------------------------------------------------------------------------


### Example: Table 2 - Gram Charlier Moment Matched Asian Option Prices
    
    # Take the same parameter settings as used in Zhang's Table
    Zhang = ZhangTable()
    options(digits = 4)
    Zhang
    ###
    
    # Calculate entries for Table 1:
    CallRST = BoundsOnAsianOption(table = Zhang, method = "RST")$price
    CallLN  = GramCharlierAsianOption(table = Zhang, method = "LN")$price 
    CallRG  = GramCharlierAsianOption(table = Zhang, method = "RG")$price
    CallJI  = GramCharlierAsianOption(table = Zhang, method = "JI")$price
    CallT   = BoundsOnAsianOption(table = Zhang, method = "T")$price            
    CallZ   = ZhangTable()[,6]
    ###
    
    # Print Call Option Values: 
    Table.2 = data.frame(cbind(Zhang[, 1:5], 
        CallRST, CallLN, CallRG, CallJI, CallT, CallZ))
    Table.2
    ###
    
    # Save Table.2 and beautify Spreadsheet file:
    # write.table(Table.2, "TableApprox.csv", sep=";")
    ###
    
    # Compute Inside Bounds:
    inside = c(
        LN = sum((CallRST < CallLN) + (CallLN < CallT) == 2),
        RG = sum((CallRST < CallRG) + (CallRG < CallT) == 2),
        JI = sum((CallRST < CallJI) + (CallJI < CallT) == 2))
    inside
    ###
    

################################################################################
# Figures


### Example: Create Figure 1  

    # Chart Asian call price deviations by Moment Matching - Create 3 x 3 Graph
    # Investigate the deviations from a reference line which is the mean
    # of the lower and upper price bounds.
	###
   
    # Create Table Function:
    createTable = 
    function(S, X, Time, r, sigma) 
    {
        CallRST = BoundsOnAsianOption(
            TypeFlag = "c", S, X, Time, r, sigma, method = "RST")$price 
        CallLN = MomentMatchedAsianOption(
            TypeFlag = "c", S, X, Time, r, sigma, method = "LN")$price 
        CallRG = MomentMatchedAsianOption(
            TypeFlag = "c", S, X, Time, r, sigma, method = "RG")$price  
        CallJI = MomentMatchedAsianOption(
            TypeFlag = "c", S, X, Time, r, sigma, method = "JI")$price 
        CallT = BoundsOnAsianOption(
            TypeFlag = "c", S, X, Time, r, sigma, method = "T")$price
        data.frame(cbind(S, X, Time, r, sigma, 
            CallRST, CallLN, CallRG, CallJI, CallT)) 
    }  
    ###
   
    # Create Plot Function:
    createPlot = 
    function(Table, x, ylim, xlab, main) 
    {
        RefLine = ( Table[,6] + Table[,10] ) / 2 
        plot(x = x, y = 100*(Table[,6]/RefLine - 1), type = "l", ylim = ylim, 
            xlab = xlab, ylab = "Call Price - % Deviation", main = main, 
            cex.main = 1)
        col = c(2, 3, 4, 1)
        lty = c(2, 3, 4, 1) # dashed - dotted - dashdotted - solid
        for (i in 7:10) {
            lines(x = x, y = 100*(Table[,i]/RefLine - 1), 
                col = col[i-6], lty = lty[i-6])
        }
    } 
    ### 
        
    # Create Tables: 
    options(digits = 9)
    V = rep(1, times = 40)
    # Calculate First Row Tables:
    r = seq(0.01, 0.10, length=length(V))
    Table11 = createTable(
        S = 100*V, X = 100*V, Time = V, r = r, sigma = 0.02*V)
    r = seq(0.01, 0.20, length = length(V))
    Table12 = createTable(
        S = 100*V, X = 100*V, Time = V, r = r, sigma = 0.20*V)
    r = seq(0.001, 0.20, length = length(V))
    Table13 = createTable(
        S = 100*V, X = 100*V, Time = V, r = r, sigma = 2.00*V)
    # Calculate Second Row Tables:
    X = seq(80, 120, length=length(V))
    Table21 = createTable(
        S = 100*V, X = X, Time = V, r = 0.05*V, sigma = V)
    Table22 = createTable(
        S = 100*V, X = X, Time = V, r = 0.09*V, sigma = 0.30*V)
    Table23 = createTable(
        S = 100*V, X = X, Time = V, r = 0.15*V, sigma = 0.10*V)
    # Calculate Third Row Tables:
    sigma = seq(0.05, 1.50, length=length(V))
    Table31 = createTable(
        S = 100*V, X = 80*V, Time = V, r = 0.09*V, sigma = sigma)
    Table32 = createTable(
        S = 100*V, X = 100*V, Time = V, r = 0.09*V, sigma = sigma)
    Table33 = createTable(
        S = 100*V, X = 120*V, Time = V, r = 0.09*V, sigma = sigma)
    ###
        
    # Create Plots:
    par(mfrow = c(4, 3), cex = 0.7)
    # First Row Plots:
    createPlot(Table = Table11, x = 2*Table11[,4]/0.05^2-1, 
        ylim = c(-0.05, 0.05), 
        xlab = expression(paste("Interest:  ", 2 * r / sigma^2 - 1)),
        main = expression(paste(sigma^2 * T / 4 == 10^{-4}, "   ", X / S == 1))) 
    createPlot(Table = Table12, x = 2*Table12[,4]/0.30^2-1, 
        ylim = c(-0.50, 0.50), 
        xlab = expression(paste("Interest:  ", 2 * r / sigma^2 - 1)),
        main = expression(paste(sigma^2 * T / 4 == 0.01, "    ", X / S == 1)))  
    createPlot(Table = Table13, x = 2*Table13[,4]/1.00^2-1, 
        ylim = c(-50.0, 50.0), 
        xlab = expression(paste("Interest:  ", 2 * r / sigma^2 - 1)),
        main = expression(paste(sigma^2 * T /4 == 1, "     ", X / S == 1)))
    # Second Row Plots: 
    createPlot(Table = Table21, x = Table21[,2]/Table21[,1], 
        ylim = c(-10.0, 5.0), 
        xlab = expression(paste("Strike:  ", X / S)),
        main = expression(paste(rT == 0.05, "      ", sigma^2 * T == 1))) 
    createPlot(Table = Table22, x = Table22[,2]/Table22[,1], 
        ylim = c(-2.0, 1.0), 
        xlab = expression(paste("Strike:  ", X / S)),
        main = expression(paste(rT == 0.09, "      ", sigma^2 * T  == 0.09)))   
    createPlot(Table = Table23, x = Table23[,2]/Table23[,1], 
        ylim = c(-10.0, 5.0), 
        xlab = expression(paste("Strike:  ", X / S)),
        main = expression(paste(rT == 0.15, "      ", sigma^2 * T == 0.01)))
    ## Third Row Plots: 
    createPlot(Table = Table31, x = Table31[,5], 
        ylim = c(-15.0, 15.0), 
        xlab = expression(paste("Volatility:  ", sigma^2 * T / 4)), 
        main = expression(paste(X / S == 0.8, "      ", rT == 0.09)))
    createPlot(Table = Table32, x = Table32[,5], 
        ylim = c(-15.0, 15.0), 
        xlab = expression(paste("Volatility:  ", sigma^2 * T / 4)),  
        main = expression(paste(X / S == 1.0, "      ", rT == 0.09)))
    createPlot(Table = Table33, x = Table33[,5], 
        ylim = c(-15.0, 15.0),
        xlab = expression(paste("Volatility:  ", sigma^2 * T / 4)),
        main = expression(paste(X / S == 1.2, "      ", rT == 0.09)))
    ###
    

# ------------------------------------------------------------------------------


### Example: Create Figure 2:

    # Calculate state price density of Asian options in the log-Normal,
    # reciprocal-Gamma and Johnson Type-I approximations
    ###
    
    # Create Density Plots
    par(mfrow = c(2, 2), cex = 0.7)
    Time = 1; r = 0.045; sigma = 0.30
    nu = 2*r / sigma^2 -1
    tau = sigma^2 * Time / 4
    x = seq(0.4, 2.8, length = 100) 
    ###
       
    ## Calculate Densities and Normalize with tau:
    d1 = MomentMatchedAsianDensity(x, Time, r, sigma, method = "LN") / tau
    d2 = MomentMatchedAsianDensity(x, Time, r, sigma, method = "RG") / tau
    d3 = MomentMatchedAsianDensity(x, Time, r, sigma, method = "JI") / tau
    ### 
           
    # Make a Linear Density Plot:
    plot(x, d2, type = "l", 
        ylab = "Density", main = "Moment Matched Densities")  
    lines(x, d1, col = "red", lty = 2)
    lines(x, d2, col = "green", lty = 3)
    lines(x, d3, col = "blue", lty = 4)
    ###
       
    ## Make a Log-Log Density Plot:
    plot(log(x), log(d2), type = "l", 
        ylab = "log(Density)", main = "Moment Matched Densities")  
    lines(log(x), log(d1), col = "red", lty = 2)
    lines(log(x), log(d2), col = "green", lty = 3)
    lines(log(x), log(d3), col = "blue", lty = 4)
    ###
    
    
# ------------------------------------------------------------------------------


### Example: Create Figure 3:

    # Calculate moment matched Asian call prices 
    # using Gram-Charlier's Series Expansion
    ###
    
    # Create Gram-Charlier Table Function:
    createTableGC = 
    function(S, X, Time, r, sigma) {
      CallRST = BoundsOnAsianOption(TypeFlag = "c", S = S, X = X, 
        Time = Time, r = r, sigma = sigma, method = "RST")$price 
      CallLN = GramCharlierAsianOption(TypeFlag = "c", S = S, X = X, 
        Time = Time, r = r, sigma = sigma, method = "LN")$price  
      CallRG = GramCharlierAsianOption(TypeFlag = "c", S = S, X = X, 
        Time = Time, r = r, sigma = sigma, method = "RG")$price 
      CallJI = GramCharlierAsianOption(TypeFlag = "c", S = S, X = X, 
        Time = Time, r = r, sigma = sigma, method = "JI")$price
      CallT = BoundsOnAsianOption(TypeFlag = "c", S = S, X = X, 
        Time = Time, r = r, sigma = sigma, method = "T")$price
      data.frame(S, X, Time, r, sigma, 
          CallRST, CallLN, CallRG, CallJI, CallT) 
    }  
    ###
    
    # Calculate Option Prices:
    options(digits = 9)
    # Calculate First Row Tables:
    V = rep(1, times = 40)
    X = seq(80, 120, length = length(V))
    r = seq(0.001, 1.50, length = length(V))
    sigma  = seq(0.05, 1.0, length = length(V))
    Table11 = createTable(
        S = 100*V, X = 100*V, Time = V, r = r/4, sigma = 0.5*V)    
    Table12 = createTable(
        S = 100*V, X = 100*V, Time = V, r = 0.09*V, sigma = sigma)
    Table13 = createTable(
        S = 100*V, X = X, Time = V, r = 0.09*V, sigma = 0.5*V)
    Table21 = createTableGC(
        S = 100*V, X = 100*V, Time = V, r = r/4, sigma = 0.5*V)    
    Table22 = createTableGC(
        S = 100*V, X = 100*V, Time = V, r = 0.09*V, sigma = sigma)
    Table23 = createTableGC(
        S = 100*V, X = X, Time = V, r = 0.09*V, sigma = 0.5*V)
    ###
     
    # Create Plot Function:    
    createPlot = 
    function(Table, x, ylim, xlab, main) {
        plot(x = x, y = Table[,6], type = "l", xlab = xlab, 
            ylim = ylim, ylab = "Call Price", main = main, cex.main = 1.0)
        col = c(2, 3, 4, 1)
        lty = c(2, 3, 4, 1) # dashed - dotted - dashdotted - solid
        for (i in 7:10) lines(x = x, y = Table[,i], 
            col = col[i-6], lty = lty[i-6])  
    }
    ###
      
    # Plot the Charts:
    par(mfrow = c(4, 3), cex = 0.7)
    # Moment Matched Plots:
    createPlot(Table = Table11, x = 2*Table11[,4]/0.5^2-1, ylim = c(5, 25),
        xlab = expression(paste("Interest:  ", 2 * r / sigma^2 - 1)),
        main = expression(paste(sigma^2 * T  == 1/4, "      ", X / S == 1)))  
    createPlot(Table = Table12, x = Table12[,5]^2/4, ylim = c(5, 25),
        xlab = expression(paste("Volatility:  ", sigma^2 * T / 4)),  
        main = expression(paste(X / S == 1, "      ", rT == 0.09))) 
    createPlot(Table = Table13, x = Table13[,2]/Table13[,1], ylim = c(5, 25),
        xlab = expression(paste("Strike:  ", X / S)),
        main = expression(paste(rT == 0.09, "      ", sigma^2 * T  == 1/4))) 
    # Gram-Charlier Plots:
    createPlot(Table = Table21, x = 2*Table21[,4]/0.5^2-1, ylim = c(5, 25),
        xlab = expression(paste("Interest:  ", 2 * r / sigma^2 - 1)),
        main = expression(paste(sigma^2 * T  == 1/4, "      ", X / S == 1)))  
    createPlot(Table = Table22, x = Table22[,5]^2/4, ylim = c(5, 25),
        xlab = expression(paste("Volatility:  ", sigma^2 * T / 4)),  
        main = expression(paste(X / S == 1, "      ", rT == 0.09))) 
    createPlot(Table = Table23, x = Table23[,2]/Table23[,1], ylim = c(5, 25),
        xlab = expression(paste("Strike:  ", X / S)),
        main = expression(paste(rT == 0.09, "      ", sigma^2 * T  == 1/4))) 
    ###
    

# ------------------------------------------------------------------------------


### Example: Create Figure 4 

	# Plot First four Moments, Skewness and Kurtosis of Asian Density
	###

	# Settings:
   	par(mfrow = c(3, 2), cex = 0.7)
   	zlab = c("M1", "M2", "M3", "M4")
   	titles = paste(c("First", "Second", "Third", "Fourth"), "Moment")
   	###
   	 	
   	# Plot the first four Moments:
   	for (M in 1:4) {
		n = 51
		x = sigma2Time = seq(0.0001, 0.5, length = n)
		sigma = sqrt(sigma2Time)
		y = seq(-0.19, 0.25, length = n)
		r = y + sigma2Time/2
		moments = matrix(rep(0, n*n), n)
		for (i in 1:n) {
			for (j in 1:n) {  
				# Use Dufresne's Formula
				moments[i, j] = AsianOptionMoments(
					M = M, Time = 1, r = r[j], sigma=sigma[i], method = "D")[M] 
		   }
		}
		persp(x = sigma, y = r, z = moments, theta = 315, phi = 30, 
			col = "steelblue3", scale = TRUE, ltheta = 0, axes = TRUE, 
			box = TRUE, shade = 0.75) 
		# persp(x=sigma, y=r, z=moments, xlab="s^2*T", ylab="r*T", zlab=zlab[M])
		title(main = titles[M])
	} 
	###
	
	# Add Skewness and Kurtosis
    titles = zlab = c("Skewness", "Kurtosis") 
	n = 51
	x = sigma2Time = seq(0.01, 0.5, length = n)
	sigma = sqrt(sigma2Time)
	y = seq(-0.19, 0.25, length=n)
	r = y + sigma2Time/2
	# Calculate Moments:
	M1 = M2 = M3 = M4 = matrix(rep(0, n*n), n)
	for (i in 1:n) {
   	 	for (j in 1:n) {
   	   		m = AsianOptionMoments(
   	   			M = 4, Time = 1, r = r[j], sigma = sigma[i], method = "D")
   	   		M1[i,j] = m[1]
   	   		M2[i,j] = m[2]
   	   		M3[i,j] = m[3]
   	   		M4[i,j] = m[4] 
		} 
	}   
   	# Moments around the Mean:	
   	m1 = M1
   	m2 = M2 - M1^2 
   	m3 = M3 - 3*M2*M1 + 2*M1^3 
   	m4 = M4 - 4*M3*M1 + 6*M2*M1^2 - 3*M1^4 	
   	# Calculate Mean, StdDev, Skewness Kurtosis:
   	stdev = sqrt(m2)	
   	skewness = m3 / stdev^3
   	kurtosis = m4 / stdev^4
   	# Plot Skewness:
   	persp(x=sigma, y=r, z=skewness, xlab="s^2*T", ylab="r*T", zlab=zlab[1])
   	title(main=titles[1])
   	# Plot Kurtosis:
   	persp(x=sigma, y=r, z=kurtosis, xlab="s^2*T", ylab="r*T", zlab=zlab[2])
   	title(main = titles[2])
   	####
    
   	
################################################################################
# Moment Matched Approach:
	

### Example: Usefule Examples for Pricing Asian Options by Moment Matching
	
	# Description:
	#   Example 1: 
	#       Calculate moment matched Asian call prices together with lower
	#       and upper bounds, this reproduces Table 5 of Zhang [2002]
	#   Example 2: 
	#       Calculate long and short tenor moment matched Asian call prices, 
	#       this reproduces Table 7 of Zhang [2002]
	#   Example 3:
	#       Calculate moment matched Asian call price deviations and chart 
	#       the deviations from the mean lower/upper bound reference line,
	#       this creates Figure 1 in Wuertz[2003]
	#   Example 4:
	#       Calculate state price density of Asian options in the log-Normal,
	#       reciprocal-Gamma and Johnson Type-I approximations
	#   Example 5:
	#       Calculate moment matched Asian call prices by integrating 
	#       over approximated densities
	#   Example 6:
	#       Calculate state price density from the second derivative of 
	#       Asian call prices as function of X/S 
	###
	

# ------------------------------------------------------------------------------


### Example 1:

	# Calculate Log-Normal Gram-Charlier series Expanded Option Prices:
	# Calculate Option Call Prices:
	###
	
	# Table:
   	ZhangTable()
    options(digits = 5)
    CallRST = BoundsOnAsianOption(
                table = ZhangTable(), method = "RST")$price 
    CallLN  = MomentMatchedAsianOption(
                table = ZhangTable(), method = "LN")$price 
    CallRG  = MomentMatchedAsianOption(
                table = ZhangTable(), method = "RG")$price
    CallJI  = MomentMatchedAsianOption(
                table = ZhangTable(), method = "JI")$price
    CallT   = BoundsOnAsianOption(
                table = ZhangTable(), method = "T")$price  
    CallZ   = ZhangTable()[,6]

    # Print Call Option Values:    
    Table = data.frame(cbind(ZhangTable()[,1:5], 
      CallRST, CallLN, CallRG, CallJI, CallT, CallZ))
    Table
    # write.table(Table, "TableApprox.csv", sep=",")
   

# ------------------------------------------------------------------------------


### Example 2:

	# Calculate Values of long-tenor and short-tenor Asian call 
	# options, this reproduces Table 6 and 7 in: J.E. Zhang,
	# "A semianalytical method for pricing and hedging ...", [2001].
	###
	
	# Calculate Option Call Prices:
    options(digits = 6)
    CallPM  = MomentMatchedAsianOption(table = ZhangLongTable(), 
    	method = "JI")$price 
    CallZPM = ZhangLongTable()[, 6]
    CallCT  = BoundsOnAsianOption(table = ZhangLongTable(), 
    	method = "CT")$price   
    CallZCT = ZhangLongTable()[, 7]

    # Print Call Option Values:    
    TableLong = data.frame(cbind(ZhangLongTable()[, 1:5], 
      	CallPM, CallZPM, CallCT, CallZCT))
   	TableLong
   	# write.table(TableLong, "TableLongTenor.csv", sep = ",")
   	
	# Calculate Option Call Prices:
    options(digits = 6)
    CallPM  = MomentMatchedAsianOption(table = ZhangShortTable(), 
    	method = "JI")$price 
    CallZPM = ZhangShortTable()[, 6]
    CallCT  = BoundsOnAsianOption(table=ZhangShortTable(), 
    	method = "CT")$price   
    CallZCT = ZhangShortTable()[, 7]

    # Print Call Option Values:    
    TableShort = data.frame(cbind(ZhangShortTable()[, 1:5], 
      round(cbind(CallPM, CallZPM, CallCT, CallZCT), digits = 7)))
    TableShort
    # write.table(TableShort, "TableShortTenor.csv", sep=",")
  
   
# ------------------------------------------------------------------------------


### Example 3:

	# Chart Asian call price deviations by Moment Matching - Create 3 x 3 Graph
	# Investigate the deviations from a reference line which is the mean
	# of the lower and upper price bounds.
	###
	
	# Create Table Function:
   	createTable = 
   	function(S, X, Time, r, sigma) {
		CallRST = BoundsOnAsianOption(
        	TypeFlag = "c", S, X, Time, r, sigma, method = "RST")$price 
     	CallLN  = MomentMatchedAsianOption(
        	TypeFlag = "c", S, X, Time, r, sigma, method = "LN")$price 
     	CallRG  = MomentMatchedAsianOption(
         	TypeFlag = "c", S, X, Time, r, sigma, method = "RG")$price  
     	CallJI  = MomentMatchedAsianOption(
         	TypeFlag = "c", S, X, Time, r, sigma, method = "JI")$price 
     	CallT   = BoundsOnAsianOption(
          	TypeFlag = "c", S, X, Time, r, sigma, method = "T")$price
     	data.frame(cbind(S, X, Time, r, sigma, CallRST, CallLN, CallRG, 
     		CallJI, CallT)) 
	}  

	# Create Plot Function:
   	createPlot = 
   	function(Table, x, ylim, xlab, main) {
		RefLine = ( Table[,6] + Table[,10] ) / 2 
      	plot(x = x, y = 100*(Table[,6]/RefLine - 1), type="l", ylim = ylim, 
			xlab=xlab, ylab="Call Price - % Deviation", main = main, 
			cex.main = 1)
      	col = c(2, 3, 4, 1)
      	lty = c(2, 3, 4, 1) # dashed - dotted - dashdotted - solid
      	for (i in 7:10) {
			lines(x = x, y = 100*(Table[,i]/RefLine - 1), col = col[i-6],
				lty = lty[i-6])
		}
	}  
	 
	# Option Settings: 
   	options(digits=9)
   	V = rep(1, times = 40)
	
   	# Calculate First Row Tables:
  	r       = seq(0.01, 0.10, length=length(V))
  	Table11 = createTable(S=100*V, X=100*V, Time=V, r=r, sigma=0.02*V)
  	r       = seq(0.01, 0.20, length=length(V))
  	Table12 = createTable(S=100*V, X=100*V, Time=V, r=r, sigma=0.20*V)
  	r       = seq(0.001, 0.20, length=length(V))
  	Table13 = createTable(S=100*V, X=100*V, Time=V, r=r, sigma=2.00*V)
	# Calculate Second Row Tables:
  	X       = seq(80, 120, length=length(V))
  	Table21 = createTable(S=100*V, X=X, Time=V, r=0.05*V, sigma=V)
  	Table22 = createTable(S=100*V, X=X, Time=V, r=0.09*V, sigma=0.30*V)
  	Table23 = createTable(S=100*V, X=X, Time=V, r=0.15*V, sigma=0.10*V)
	# Calculate Third Row Tables:
  	sigma   = seq(0.05, 1.50, length=length(V))
  	Table31 = createTable(S=100*V, X=80*V, Time=V, r=0.09*V, sigma=sigma)
  	Table32 = createTable(S=100*V, X=100*V, Time=V, r=0.09*V, sigma=sigma)
  	Table33 = createTable(S=100*V, X=120*V, Time=V, r=0.09*V, sigma=sigma)
		
  	# Start with Plots:
  	par(mfrow = c(4, 3), cex = 0.5)
   	# First Row Plots:
	createPlot(Table = Table11, x = 2*Table11[,4]/0.05^2-1, 
		ylim = c(-0.05, 0.05), 
		xlab = expression(paste("Interest:  ", 2 * r / sigma^2 - 1)),
		main = expression(paste(sigma^2 * T / 4 == 10^{-4}, "   ", X / S == 1))) 
	createPlot(Table = Table12, x = 2*Table12[,4]/0.30^2-1, 
		ylim = c(-0.50, 0.50), 
		xlab = expression(paste("Interest:  ", 2 * r / sigma^2 - 1)),
		main = expression(paste(sigma^2 * T / 4 == 0.01, "    ", X / S == 1)))  
	createPlot(Table = Table13, x = 2*Table13[,4]/1.00^2-1, 
		ylim = c(-50.0, 50.0), 
		xlab = expression(paste("Interest:  ", 2 * r / sigma^2 - 1)),
		main = expression(paste(sigma^2 * T /4 == 1, "     ", X / S == 1)))
   	# Second Row Plots: 
	createPlot(Table = Table21, x = Table21[,2]/Table21[,1], 
		ylim = c(-10.0, 5.0), 
		xlab = expression(paste("Strike:  ", X / S)),
		main = expression(paste(rT == 0.05, "      ", sigma^2 * T == 1))) 
	createPlot(Table = Table22, x = Table22[,2]/Table22[,1], 
		ylim = c(-2.0, 1.0), 
		xlab = expression(paste("Strike:  ", X / S)),
		main = expression(paste(rT == 0.09, "      ", sigma^2 * T  == 0.09)))   
	createPlot(Table = Table23, x = Table23[,2]/Table23[,1], 
		ylim = c(-10.0, 5.0), 
		xlab = expression(paste("Strike:  ", X / S)),
		main = expression(paste(rT == 0.15, "      ", sigma^2 * T == 0.01)))
	## Third Row Plots: 
	createPlot(Table = Table31, x = Table31[,5], 
		ylim = c(-15.0, 15.0), 
		xlab = expression(paste("Volatility:  ", sigma^2 * T / 4)), 
		main = expression(paste(X / S == 0.8, "      ", rT == 0.09)))
	createPlot(Table = Table32, x = Table32[,5], 
		ylim = c(-15.0, 15.0), 
		xlab = expression(paste("Volatility:  ", sigma^2 * T / 4)),  
		main = expression(paste(X / S == 1.0, "      ", rT == 0.09)))
	createPlot(Table = Table33, x = Table33[,5], 
		ylim = c(-15.0, 15.0),
		xlab = expression(paste("Volatility:  ", sigma^2 * T / 4)),
		main = expression(paste(X / S == 1.2, "      ", rT == 0.09)))
  	###
  	
   
# ------------------------------------------------------------------------------


### Example 4:

	# Calculate state price density of Asian options in the log-Normal,
	# reciprocal-Gamma and Johnson Type-I approximations
	###
	
	# Create Density Plots
    par(mfrow = c(2, 2))
    Time = 1; r = 0.045; sigma = 0.30
    nu = 2*r / sigma^2 -1
    tau = sigma^2 * Time / 4
    x = seq(0.4, 2.8, length=100) 
	###
	
    # Calculate Densities and Normalize with tau:
    d1 = MomentMatchedAsianDensity(x, Time, r, sigma, method = "LN") / tau
    d2 = MomentMatchedAsianDensity(x, Time, r, sigma, method = "RG") / tau
    d3 = MomentMatchedAsianDensity(x, Time, r, sigma, method = "JI") / tau
 	###
 	
    # Make a Linear Density Plot:
    plot(x, d2, type="l", ylab="Density", 
     	main="Moment Matched Density")  
    lines(x, d1, type = "l", col = "red")
    lines(x, d2, type = "l", col = "green")
    lines(x, d3, type = "l", col = "blue")
	###
	
    # Make a Log-Log Density Plot:
    plot(log(x), log(d2), type="l", ylab="log(Density)", 
     	main = "Moment Matched Density")  
    lines(log(x), log(d1), type = "l", col = "red")
    lines(log(x), log(d2), type = "l", col = "green")
    lines(log(x), log(d3), type = "l", col = "blue")
 	###  

# ------------------------------------------------------------------------------


### Example 5:

	# Calculate Asian call prices by integrating over approximated densities
	###
	
	# Test for methods = "LN", "RG", "JI", and times = 1, 3
    Method = c("LN", "RG", "JI", "LN", "RG", "JI")
    Time = c(1, 1, 1, 3, 3, 3)
    S = 100; X = 100; r = 0.09; sigma = 0.30
    Call.Integrated = Call.Direct = rep(0, times=6)
    func = function(x, S, X, Time, roh, sigma, method) { 
      	(x - X/S) * MomentMatchedAsianDensity(x, Time, roh, sigma, method)
    }
    for (i in 1:6) {
      Call.Integrated[i] = S* exp(-r*Time[i]) * integrate(
        func, lower=X/S, upper=20, S=S, X=X, Time=Time[i], roh=r, 
        sigma=sigma, method=Method[i])$value
      Call.Direct[i] = MomentMatchedAsianOption(TypeFlag="c", S=S, X=X, 
        Time=Time[i], r=r, sigma=sigma, method=Method[i])$price }
	###
	
    # Print Call Prices:
    names = paste(Method, "-", as.character(Time), sep="")
    data.frame(cbind(Time, Call.Integrated, Call.Direct), row.names=names)
 	###
 	
   
# ------------------------------------------------------------------------------


### Example 6:

	# Calculate state price density from the second derivative of 
	# Asian call prices as function of X/S 
	###
	
	## Settings:
    par(mfrow=c(2, 2), CEX = 0.7)
    method = "JI"
    S = 1; X = seq(0.400, 2.500, by = 0.005); Time = 1; r = 0.09; sigma=0.30
	###
	
	# Calculate Option Prices:
    price = rep(0, times=length(X))
    for ( i in 1:length(X) ) {
        price[i] = MomentMatchedAsianOption(TypeFlag="c", S=S, X=X[i], 
            Time=Time, r=r, sigma=sigma, method=method)$price }

    # Take the Second Derivative and Calculate Density:
    z = derivative(x=X, y=price, deriv=2)
    x = z$x; density.1 = exp(r*Time)*z$y 
	###
	
    # Calculate Density directly: 
    density.2 = MomentMatchedAsianDensity(x = x, Time = Time, r = r, 
     	sigma = sigma, method = method)    
	###
	
   	# Plot on Linear and Double-Log Scale: 
    plot (x, density.1, type="l", xlab="x", ylab="Density", 
     	main=paste(method, "- Density"))
   	lines(x, density.1, col="green")
   	lines(x, density.2, col="red")
   	plot (log(x), log(density.1), type="l", xlab="log(x)", 
      	ylab="log(Density)", main=paste(method, "- Log-Density"))
   	lines(log(x), log(density.1), col="green")
   	lines(log(x), log(density.2), col="red")
	###
	
   	# Note, that the two curves cannot be distinguished!  
    difference = 100*(density.1-density.2)/density.2
    cbind(x, density.1, density.2, difference)
    plot(x, difference, type="l", main=paste(method, "- % Difference"))
    plot(log(x), difference, type="l", main=paste(method, "- % Difference"))
 	###
 	
   
################################################################################



