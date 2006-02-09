#
# Examples from the forthcoming Monograph:
# 	Rmetrics - Financial Engineering and Computational Finance
#   A Practitioner's Guide for R and SPlus Programmers
#   written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Examples from Chapter 7.1
#   The Basics of Option Pricing
#
# List of Examples, Exercises and Code Snippets
#      
#   * Example: Plot Profit/Loss versus Asset Price Graphs  
# 	* Example: Generalized Black-Scholes Option Prices
# 	* Example: Plots 2-D Graphs of GBS Option Prices
# 	* Example: Plots 3-D Graphs of GBS Option Prices
# 	* Example: Generalized Black-Scholes Option Sensitivities
# 	* Example: Plots 2-D Graphs of GBS Option Sensitivities
# 	* Example: Plots 3-D Graphs of GBS Option Sensitivities
# 	* Example: Americam Calls on Dividend Paying Stocks
# 	* Example: Barone-Adesi and Whaley Approximation
# 	* Example: Bjerksund and Stensland Approximation
# 	* Example: Binomial Tree Option Model
# 	* Example: Trinomial Tree Option Model  
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
# Generalized Black Scholes Options


### Example: Plot Profit/Loss versus Asset Price Graphs

	# Plot graphs displaying the profit/loss versus the
	# asset price for the following four situations:
	# Buying a call, writing a call, buying a put, and
	# writing a put.

	# Buying a Call:
    par(mfrow = c(3, 2))
    plot(x = c(50, 150), y = c(0, 0), xlim = c(50, 150), ylim = c(-16, 16), 
        type = "l", main = "Buying a Call", xlab = "Asset Price", 
        ylab = " Loss / Profit")
    lines(x = c(50, 100), y = c(-8, -8))
    lines(x = c(100, 130), y = c(-8, 14))

	# Writing a Call:
    plot(x = c(50, 150), y = c(0, 0), xlim = c(50, 150), ylim = c(-16, 16), 
        type = "l", main = "Writing a Call", xlab = "Asset Price", 
        ylab = " Loss / Profit")
    lines(x = c(50, 100), y = c(8, 8))
    lines(x = c(100, 130), y = c(8, -14))

	# Buying a Put:
    plot(x = c(50, 150), y = c(0, 0), xlim = c(50, 150), ylim = c(-16, 16), 
        type = "l", main = "Buying a Put", xlab = "Asset Price", 
        ylab = " Loss / Profit")
    lines(x = c(100, 150), y = c(-8, -8))
    lines(x = c(70, 100), y = c(14, -8))
	
    # Writing a Put:
    plot(x = c(50, 150), y = c(0, 0), xlim = c(50, 150), ylim = c(-16, 16), 
        type = "l", main = "Writing a Put", xlab = "Asset Price", 
        ylab = " Loss / Profit")
    lines(x = c(100, 150), y = c(8, 8))
    lines(x = c(70, 100), y = c(-14, 8))


# ------------------------------------------------------------------------------


### Example: Calculate the option prices for

	# 1) an European Options on a Stock with Cash Dividends,
	# 2) an Option on Stock Indexes,
	# 3) an Option on Futures, and
	# 4) a Currency Option.

	# European Option on a Stock with Cash Dividends:
    S = 100 - 2*exp(-0.10*0.25) - 2*exp(-0.10*0.50)
    r = 0.10
    GBSOption("c", S = S, X = 90, Time = 0.75, r = r, b = r, 
        sigma = 0.25) 
    
	# Option on Stock Indexes:
    r = 0.10
    q = 0.05
    GBSOption("p", S = 100, X = 95, Time = 0.5, r = r, b = r-q, 
        sigma = 0.20) 

	# Option on Futures:
    FuturesPrice = 19
    b = 0
    GBSOption("c", S = FuturesPrice, X = 19, Time = 0.75, r = 0.1, 
        b = b, sigma = 0.28) 
    GBSOption("p", S = FuturesPrice, X = 19, Time = 0.75, r = 0.1, 
        b = b, sigma = 0.28) 
    
	# Currency Option:
    r = 0.06
    rf = 0.08
    GBSOption("c", S = 1.56, X = 1.60, Time = 0.5, r = r, b = r-rf, 
        sigma = 0.12) 


# ------------------------------------------------------------------------------


### Example: Plot the scaled Price of a Generalized Black Scholes Call
	
	#   Use the function GBSoption()} to plot the scaled call
	#   price c/X
	#   1) as a function of S/X, the scaled asset price (note
	#          doubling the asset price and strike price yields the
	#      same option values), use as parameter sigma^2 T for
	#      a low and a high interest rate r,
	#   2) as a function of $\sigma^2 T$, the volatility and
	#      Time to maturity (note doubling the volatility and
	#      decreasing the time to maturity by a factor of four 
	#      yields the same option values), use as parameter S/X 
	#      for a low and a high interest rate $r$.:
	###
	
	# BS Call Prices as a Function of S/X:  
    par(mfrow = c(2, 2), cex = 0.7)
    # Figure 1:
    S = seq(from = 0.4, to = 1.2, by = 0.05)
    plot(S, S, ylim = c(0, 0.5), type = "n", 
         main = "r=0.01", xlab = "S/X", ylab = "c/X")
    for (Time in seq(from = 1/12, to = 2, by = 1/12)) {
        c = GBSOption("c", S = S, X = 1, Time = Time, r = 0.01, 
            b = 0.01, sigma = 0.4)$price
        col = "steelblue3"; if (Time == 1) col = "red"
        lines (S, c, col = col) }       
    # Figure 2:
    S = seq(from = 0.4, to = 1.2, by = 0.05)
    plot(S, S, ylim = c(0, 0.5), type = "n", 
        main = "r=0.10", xlab = "S/X", ylab = "c/X")
    for (Time in seq(from = 1/12, to = 2, by = 1/12)) {
        c = GBSOption("c", S = S, X = 1, Time = Time, r = 0.10, 
            b = 0.10, sigma = 0.4)$price
        col = "steelblue3"; if (Time == 1) col = "red"
        lines (S, c, col = col) }
                           
	# BS Call Prices as a Function of Time to Maturity: 
    # Figure 3:
    Time = seq(from = 1/12, to = 2, by = 1/12)
    plot(Time, Time, ylim = c(0, 0.5), type = "n", 
        main = "r=0.01", xlab = "Time", ylab = "c/X")
    for (S in seq(from = 0.4, to = 1.2, by = 0.05)) {
        c = GBSOption("c", S = S, X = 1, Time = Time, r = 0.01, 
            b = 0.01, sigma = 0.4)$price
        col = "steelblue3"; if (S == 1) col = "red"
        lines (Time, c, col = col) }        
    # Figure 4:
    Time = seq(from = 1/12, to = 2, by = 1/12)
    plot(Time, Time, ylim = c(0, 0.5), type = "n", 
        main = "r=0.10", xlab = "Time", ylab = "c/X")
    for (S in seq(from=0.4, to=1.2, by=0.05)) {
        c = GBSOption("c", S = S, X = 1, Time = Time, r = 0.10, 
            b = 0.10, sigma = 0.4)$price
        col = "steelblue3"; if (S == 1) col = "red" 
        lines (Time, c, col = col) }


# ------------------------------------------------------------------------------


### Example: Perspective plot of GBS Call and Put Prices 

	# Write the perspective plot function:
	GBSOption3DPlot = 
	function (TypeFlag = c("c", "p"), S, X, Time, r, b, sigma, theta = 30, 
	phi = 30, expand = 0.75, col = "lightblue", ltheta = 120, shade = 0.75, 
	ticktype = "detailed", cex = 0.6, main = "GBS Option Price", ...)
	{   # A function written by Diethelm Wuertz
	     
	    # Description:
	    #   Plot the premium values for a Generalized Black-Scholes 
	    #   option either for a call or a put option.
	    
	    # References:
	    #   Haug E.G., The Complete Guide to Option Pricing Formulas
	    
	    # FUNCTION:
	    
	    # Plotting Premium - 3D: 
	    # x=S, y=Time, z=Premium  
	    TypeFlag = TypeFlag[1] 
	    premium3D = function(S, Time, TypeFlag, X, r, b, sigma) {
	        GBSOption(TypeFlag, S, X, Time, r, b, sigma)$price }
	    
	    # Prices:
	    Price = outer(S, Time, FUN=premium3D, TypeFlag, X, r, b, sigma)
	    
	    # Perspective Plot:
	    persp(x=S, y=Time, z=Price, xlab="S", ylab="Time", zlab=TypeFlag, 
	        theta=theta, phi=phi, expand=expand, col=col, ltheta=ltheta,
	        shade=shade, ticktype=ticktype, cex=cex, main=main, ...) 
	    
	    # Return Value:
	    invisible(list(S = S, Time = Time, Price = Price))  
	}


	# BS Call - Plot3D Premium: 
    par(mfrow = c(2, 1), cex = 1/3)
    GBSOption3DPlot("c", S = seq(from = 75, to = 125, 
        length = 40), X = 100, Time = seq(from = 1/52, to = 1, 
        length = 40), r = 0.1, b = 0.1, sigma = 0.4)    
        
	# BS Put - Plot3D Premium:
    GBSOption3DPlot("p", S = seq(from = 75, to = 125, 
        length = 40), X = 100, Time = seq(from = 1/52, to = 1, 
            length = 40), r = 0.1, b = 0.1, sigma = 0.4)


# ------------------------------------------------------------------------------


### Example: Calculate the sensitivities for some selected options

	# Delta - Haug [1997], Chapter 1.3.1:
    GBSGreeks("Delta", "c", S = 105, X = 100, Time = 0.5, r = 0.1, 
        b = 0, sigma = 0.36)
    GBSGreeks("Delta", "p", S = 105, X = 100, Time = 0.5, r = 0.1, 
        b = 0, sigma = 0.36)  
    
	# Theta - Haug [1997], Chapter 1.3.5:
    GBSGreeks("Theta", "p", S = 430, X = 405, Time = 1/12, r = 0.07, 
        b = 0.07-0.05, sigma = 0.20)
    GBSGreeks("Theta", "p", S = 430, X = 405, Time = 1/365, r = 0.07, 
        b = 0.07-0.05, sigma = 0.20)

	# Vega - Haug [1997], Chapter 1.3.4:
    GBSGreeks("Vega", "c", S = 55, X = 60, Time = 0.75, r = 0.10, 
        b = 0.10, sigma = 0.30)

	# Rho - Haug [1997], Chapter 1.3.6:
    GBSGreeks("Rho", "c", S = 72, X = 75, Time = 1, r = 0.09, 
        b = 0.09, sigma = 0.19)

	# Lambda - Haug [1997], Chapter 1.3.2:
    GBSGreeks("Lambda", "p", S = 105, X = 100, Time = 0.5, r = 0.1, 
        b = 0, sigma = 0.36) 

	# Gamma - Haug [1997], Chapter 1.3.3:
    GBSGreeks("Gamma", "c", S = 55, X = 60, Time = 0.75, r = 0.10, 
        b = 0.10, sigma = 0.30)

        
# ------------------------------------------------------------------------------


### Example: Investigate Premium and Sensitivities of a Currency Option

	#   As a simple illustration of how the Greeks can be used consider
	#   someone holding an at-the-money currency call on USD put on DEM
	#   with the following characteristics: underlying price 1.7000,
	#   strike price 1.700$, time to maturity 270 days, DEM (domestic)
	#   interest rate 6% p.a., USD (foreign) interest rate 3% p.a.,
	#   and volatility 10% p.a.. Calculate the option value and the
	#   sensitivities for these parameters and then plot the sensitivities 
	#   in the range between 1.4000 and 2.0000 for times to maturity of 
	#   1, 30, 90, and 270 days to get an overview about the whole range 
	#   of prices of the underlying.
	#   Suppose after one week the underlying price were to rise to
	#   1.7500 DEM, interest rates were to fall 1%, and volatility
	#   were to rise 2%. what effect would this combination have on the
	#   price of the option? Analyze the separate impacts, and calculate 
	#   the combined effect to the option rise. 
	###
	
	# Characterisitics of the BS Option:
    GBSCharacteristics("c", S = 1.7000, X = 1.7000, 
        Time = 270/365, r = 0.06, b = 0.06-0.03, sigma = 0.10)

	# Plot Greeks:
    par(mfrow = c(2, 2), cex = 0.7)
    S = seq(from = 1.4000, to = 2.0000, length = 250)
    Selection = c("Delta", "Theta", "Vega", "Rho")
    PlotYmin  = c(    0.0,    -0.6,   0.0,    0.0)
    PlotYmax  = c(    1.0,     0.0,   0.6,    1.2)
    for (i in 1:4){
        plot(S, S, ylim = c(PlotYmin[i], PlotYmax[i]), type = "n", 
             xlab = "S", ylab = Selection[i], main = Selection[i])
        for (Days in c(1, 30, 90, 270)) {
            greek = GBSGreeks(Selection[i], "c", S = S, X = 1.7000, 
                Time = Days/365, r = 0.06, b = 0.06-0.03, sigma = 0.1)
            lines (S, greek) } }

	# Analyzing the Separate Impacts:
    Characteristics = 
        GBSCharacteristics("c", S = 1.7000, X = 1.7000, 
        Time = 270/365, r = 0.06, b = 0.06-0.03, sigma = 0.10)
    Changes = list (
        DueToPrice = (1.7500 - 1.7000) * Characteristics$delta,
        DueToTime = 7/365 * Characteristics$theta,
        DueToVolatility = (0.12 - 0.10) * Characteristics$vega,
        DueToInterest = (0.05 - 0.06) * Characteristics$rho)
    print(Changes)
    TotalChange = Changes$DueToPrice + Changes$DueToTime + 
        Changes$DueToVolatility + Changes$DueToInterest
    print(TotalChange)
    OldPremium = Characteristics$premium
    print(OldPremium)
    NewPremium = GBSOption("c", S = 1.7500, X = 1.7000, 
        Time = 263/365, r = 0.05, b = 0.05-0.03, sigma = 0.12)$price
    print(NewPremium)
    RealTotalChange = NewPremium - OldPremium
    print(RealTotalChange)  

 
# ------------------------------------------------------------------------------


# Example: Perspective plot of GBS Greeks for Call and Put Prices 

	#   Plot the sensitivities Delta, Theta, Vega, Rho, Lambda and 
	#   Gamma as a function of the scaled asset price S/X and the 
	#   time-scaled volatility sigma^2 T. 
	###
	
	# Write the perspective plot function:
	GBSGreeks3DPlot = 
	function (Selection = c("Delta", "Theta", "Vega", "Rho", "Lambda", "Gamma",
	"CofC"), TypeFlag = c("c", "p"), S, X, Time, r, b, sigma, theta = 30, 
	phi = 30, expand = 0.75, col = "lightblue", ltheta = 120, shade = 0.75, 
	ticktype = "detailed", cex = 0.6, main = "GBS Option Sensitivity", ...)
	{   # A function written by Diethelm Wuertz
	     
	    # Description:
	    #   Plot the Greeks for a Generalized Black-Scholes 
	    #   option either for a call or a put option.
	    
	    # References:
	    #   Haug E.G., The Complete Guide to Option Pricing Formulas
	    
	    # FUNCTION:
	    
	    # Plotting Greeks - 3D: 
	    # x = S, y = Time, z = Greek    
	    
	    # Selection:
	    # "delta", "theta", "vega", "rho", "lambda", "gamma", "cofc"
	    greeks3D = function(S, Time, Selection, TypeFlag, X, r, b, sigma) 
	        { GBSGreeks(Selection, TypeFlag, S, X, Time, r, b, sigma)}
	    
	    # Sensitivities:
	    Greeks = outer(S, Time, FUN=greeks3D, Selection, TypeFlag, X, 
	        r, b, sigma)
	    
	    # Perspective Plot:
	    persp(x=S, y=Time, z=Greeks, xlab="S", ylab="Time", zlab=Selection, 
	        theta=theta, phi=phi, expand=expand, col=col, ltheta=ltheta,
	        shade=shade, ticktype=ticktype, cex=cex, main=main, ...) 
	    
	    # Return Value:
	    invisible(list(S = S, Time = Time, Sensitivity = Greeks))
	}


	# BS - Plot3D Greeks:
    
    # pdf(file = "xmpGBSgreeks3D.pdf", width = 7, height = 9)
    par(mfrow = c(3, 2), cex = 1/3)
    for (Selection in c("delta", "theta", "vega", "rho", "lambda", "gamma"))
        GBSGreeks3DPlot(Selection, "c", 
            S = seq(from = 75, to = 125, length = 25), X = 100,
            Time = seq(from = 1/52, to = 1, length = 25), r = 0.1, 
            b = 0.1, sigma = 0.40)
    # dev.off()

  
################################################################################
# Approximations for American Options


### Example: Price American Stock Options with Known Dividends
	 
	#	American Calls on stocks paying a single dividend can 
	#	be priced by the Roll-Geske-Whaley formula.
	#	Consider an American-style call option on a stock that
	#  	will pay a dividend of $4$ in exactly three months. The 
	#	stock price is $80$, the strike price is $82$, time to 
	#	maturity is 4 months, the risk-free interest rate is 6%, 
	#	and the volatility is 30%. Note, that the result will 
	#	be 4.3860, whereas the value of a similar European call 
	#	would be $3.5107. What happens when no dividends are 
	#	paid, D=0?
	
	# American Stock Options with known Dividends:
	RollGeskeWhaleyOption(S = 80, X = 82, time1 = 1/4, Time2 = 1/3, 
		r = 0.06, D = 4, sigma = 0.30)
	
	# What happens when D=0?	
	RollGeskeWhaleyOption(S = 80, X = 82, time1 = 1/4, Time2 = 1/3, 
		r = 0.06, D = 0, sigma = 0.30)

		
# ------------------------------------------------------------------------------


### Example: Price American Options Using the BAW Approximation
	
	#	The quadratic approximation method by Barone-Adesi and
	#	Whaley can be used to price American call and put options 
	#	on an underlying asset with cost-of-carry rate b.
	#
	#	Compare option values of the BAW approximation with the 
	#   Black-76 formula, European style. 
	#
	#   Choose r=0.1, b=0, and X=100 with volatilities sigma of 
	#   15%, 25%, and 35%, and prices taking values S of 90, 100, 
	#   and 110 for two time periods, Time=0.1 and Time=0.5, 
	#   respectively.
	###
	
	# American Calls:
	FuturesPrice = rep(c(90,90,90, 100,100,100, 110,110,110), times = 2)
	Time = c(rep(0.1, times = 9), rep(0.5, times = 9))
	Sigma = rep(c(0.15, 0.25, 0.35), times=6)
    CallPrice = PutPrice = rep(0, times=18)
    for (i in 1:18) {
    	CallPrice[i] = BAWAmericanApproxOption("c", S = FuturesPrice[i], 
    		X = 100, Time = Time[i], r = 0.1, b = 0, sigma = Sigma[i]) 
    	PutPrice[i] = BAWAmericanApproxOption("p", S = FuturesPrice[i], 
    		X = 100, Time = Time[i], r = 0.1, b = 0, sigma = Sigma[i]) }		
    cbind(FuturesPrice, Time, Sigma, CallPrice, PutPrice)
   
	# Compare with Black76Option:
    CallPrice = PutPrice = rep(0, times = 18)
    for (i in 1:18) {
    	CallPrice[i] = Black76Option("c", FT = FuturesPrice[i], 
    		X = 100, Time = Time[i], r = 0.1, sigma = Sigma[i])$price
    	PutPrice[i] = Black76Option("p", FT=FuturesPrice[i], 
    		X = 100, Time = Time[i], r = 0.1, sigma = Sigma[i])$price }		
    cbind(FuturesPrice, Time, Sigma, CallPrice, PutPrice)   
    
    
# ------------------------------------------------------------------------------


### Example: Price American Options Using the BS Approximation

	#  	The Bjerksund and Stensland approximation an be used to
	# 	price American Options on stocks, futures and currencies. 
	#	Consider a call option with nine months to expiry. The
	#	stock price is 42, the strike price is 40, the risk free 
	#	rate is 4% p.a., the dividend yield is 8% p.a., the volatility 
	#	is 35% p.a..
	###

	# Price American Options using the BS Approximation:
	BSAmericanApproxOption("c", S = 42, X = 40, Time = 0.75, r = 0.04, 
		b = 0.04-0.08, sigma = 0.35)

	# Compare with a similar European Call:
	GBSOption("c", S = 42, X = 40, Time = 0.75, r = 0.04, 
		b = 0.04-0.08, sigma = 0.35)
	
		
################################################################################
# Option Trees


### Example: Binomial Tree Option
     
	# Reference:
	#   J.C. Hull [1996], Chapter 16
	#   E.G. Haug [1997], Chapter 1.3, page 11-16
	#   Leisen D.P., Reimer M., [1996] 
	###

	# Cox-Ross-Rubinstein Binomial Tree Option Model:
    # Example 14.1 from Hull's Book:
    CRRBinomialTreeOption(TypeFlag = "pa", S = 50, X = 50, 
      	Time = 5/12, r = 0.1, b = 0.1, sigma = 0.4, n = 5)
    # Example 3.1.1 from Haug's Book:
  	CRRBinomialTreeOption(TypeFlag = "pa", S = 100, X = 95, 
		Time = 0.5, r = 0.08, b = 0.08, sigma = 0.3, n = 5)
    # A European Call - Compare with Black Scholes: 
    CRRBinomialTreeOption(TypeFlag = "ce", S = 100, X = 100, 
   	  	Time = 1, r = 0.1, b = 0.1, sigma = 0.25, n = 50)
    GBSOption(TypeFlag = "c", S = 100, X = 100, 
   	 	Time = 1, r = 0.1, b = 0.1, sigma = 0.25)$price 
  	 	 
	# CRR - JR - TIAN Model Comparison
    # Hull's Example as Function of "n":
    par(mfrow = c(2, 1), cex = 0.7)
    steps = 50
    CRROptionValue =  JROptionValue = TIANOptionValue = 
     	rep(NA, times = steps)
    for (n in 3:steps) { 
   	 	CRROptionValue[n] = CRRBinomialTreeOption(TypeFlag = "pa", S = 50, 
	   		X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)
	 	JROptionValue[n] = JRBinomialTreeOption(TypeFlag = "pa", S = 50, 
	   		X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)	
	 	TIANOptionValue[n] = TIANBinomialTreeOption(TypeFlag = "pa", S = 50, 
	   		X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n) }			
   	plot(CRROptionValue[3:steps], type = "l", col = "red", 
   	 	ylab = "Option Value")
   	lines(JROptionValue[3:steps], col = "green")
   	lines(TIANOptionValue[3:steps], col = "blue")
   	# Add Result from BAW Approximation:
   	BAWValue =  BAWAmericanApproxOption(TypeFlag = "p", S = 50, X = 50, 
   	 	Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4)
   	abline(h = BAWValue, lty = 3)
   	title(main = "Convergence")
   	data.frame(CRROptionValue, JROptionValue, TIANOptionValue)
  
    
	# Plot CRR Option Tree:
   	# Again Hull's Example:
   	CRRTree = BinomialTreeOption(TypeFlag = "pa", S = 50, X = 50, 
	 	Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = 5)
   	BinomialTreePlot(CRRTree, dy = 1, cex = 0.8, ylim = c(-6, 7),
   	 	xlab = "n", ylab = "Option Value")
   	title(main = "Option Tree")

# ------------------------------------------------------------------------------


# Exercise: Trinomial Tree

	#	Write a function to compute the call and put price of an 
	#	European or American style option using a trinomial tree
	#	approach.

	TrinomialTreeOption  = 
	function(AmeEurFlag, CallPutFlag, S, X, Time, r, b, sigma, n)
	{	# A function implemented by Diethelm Wuertz           
	  	 
		# Description:
		#	Calculates option prices from the Trinomial tree model.
		 
		# Arguments:
		#	AmeEurFlag - a character value, either "a" or "e" for 
		#		a European or American style option
		#	CallPutFlag - a character value, either "c" or "p" for 
		#		a call or put option
		#   S, X, Time, r, b, sigma - the usual option parameters
		#	n - an integer value, the depth of the tree
		 
		# Value:
		#	Returns the price of the options.
	 
		# Details:
		#	Trinomial trees in option pricing are similar to
		#	binomial trees. Trinomial trees can be used to 
		#	price both European and American options on a single 
		#	underlying asset.
		#	Because the asset price can move in three directions 
		#	from a given node, compared with only two in a binomial
		#	tree, the number of time steps can be reduced to attain
		#	the same accuracy as in the binomial tree. 
	 
		# Reference:
		#   E.G Haug, The Complete Guide to Option Pricing Formulas
		#   Chapter 3.2
	
	    # FUNCTION:
	    
	    # Settings:            
		OptionValue  =  rep(0, times=2*n+1)  
		
		# Call-Put Flag:
		if (CallPutFlag == "c") z  =  +1 
		if (CallPutFlag == "p") z  =  -1  
		
		# Time Interval: 
		dt  =  Time/n
		
		# Up-and-down jump sizes:
		u  =  exp(+sigma * sqrt(2*dt))
		d  =  exp(-sigma * sqrt(2*dt)) 
		
		# Probabilities of going up and down:  
		pu  =  ((exp(b * dt/2) - exp( -sigma * sqrt(dt/2))) / 
		       (exp(sigma * sqrt(dt/2)) - exp(-sigma * sqrt(dt/2)))) ^ 2
		pd  =  (( exp(sigma * sqrt(dt/2)) - exp( b * dt/2)) / 
			(exp(sigma * sqrt(dt/2)) - exp(-sigma * sqrt(dt/2)))) ^ 2
			
		# Probability of staying at the same asset price level:
		pm  =  1 - pu - pd
	    Df  =  exp(-r*dt)   
		for (i in 0:(2*n)) {
			OptionValue[i+1]  =  max(0, z*(S*u^max(i-n, 0) * 
				d^max(n*2-n-i, 0) - X))}
		for (j in (n-1):0) {
			for (i in 0:(j*2)) {
				# European Type:
				if (AmeEurFlag == "e") {
					OptionValue[i+1]  =  (
						pu * OptionValue[i+3] + 
						pm * OptionValue[i+2] + 
						pd * OptionValue[i+1]) * Df }
				# American Type:
				if (AmeEurFlag == "a") {
					OptionValue[i+1]  =  max((z*(S*u^max(i-j, 0) * 
						d ^ max(j*2-j-i, 0) - X)), (
						pu * OptionValue[i+3] + 
						pm * OptionValue[i+2] + 
						pd * OptionValue[i+1]) * Df) } } }
		TrinomialTree  =  OptionValue[1]
		
		# Return Value:
		TrinomialTree
	}

	# Example:  
   	TrinomialTreeOption(AmeEurFlag = "a", CallPutFlag = "p", S = 100, 
   	 	X = 110, Time = 0.5, r = 0.1, b = 0.1, sigma = 0.27, n = 30)


################################################################################  	
   		
