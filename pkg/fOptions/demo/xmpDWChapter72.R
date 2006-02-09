#
# Examples from the forthcoming Monograph:
# 	Rmetrics - Financial Engineering and Computational Finance
#   A Practitioner's Guide for R and SPlus Programmers
#   written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Examples from Chapter 7.2
#   Pricing Formulas for Exotic Options
#
# List of Examples, Exercises and Code Snippets
#
# 	Example: Chooser Options
# 	Example: Multiple Assets Options
# 	Example: Lookback Options
# 	Example: Barrier Options
# 	Example: Binary Options
# 	Example: Asian Options
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
# Pricing Formulas for Exotic Options


### Example: Chooser Options

	# This script gives examples on some binary options:
	#   Simple Chooser Option
	#   Complex Chooser Option
	# Reference:
	#	E.G. Haug [1997], Chapter 2
	###

	# Simple Chooser Option:
	SimpleChooserOption(S = 50, X = c(45, 50, 55), Time = 0.5, r = 0.08, 
	  b = 0.08, sigma = 0.25, time = 0.25)
	###
	
	# Complex Chooser Option:
	ComplexChooserOption(S = 50, Xc = 55, Xp = 48, Time = 0.25, 
	  Timec = 0.5, Timep = 0.5833, r = 0.10, b = 0.10-0.05, 
	  sigma = 0.35, doprint = TRUE)
	###
	
	  
# ------------------------------------------------------------------------------


### Example: Multiple Assets Options

	# This script gives examples on some multiple assets options:
	# 	Two Asset Correlation Options
	# 	European Exchange Options
	# 	American Exchange Options
	# 	Exchange Options On Exchange Options
	# 	Two Risky Assets Options
	# 	Spread-Option Approximation
	# Reference:
	#	E.G. Haug [1997]
	###

	# Two Asset Correlation Options:	
   	TwoAssetCorrelationOption(TypeFlag = "c", S1 = 52, S2 = 65, 
     	X1 = 50, X2 = 70, Time = 0.5, r = 0.10, b1 = 0.10, b2 = 0.10, 
     	sigma1 = 0.2, sigma2 = 0.3, rho = 0.75) 
	###

	# European Exchange Options: 
   	EuropeanExchangeOption(S1 = 22, S2 = 0.20, Q1 = 1, Q2 = 1, 
     	Time = 0.1, r = 0.1, b1 = 0.04, b2 = 0.06, sigma1 = 0.2, 
     	sigma2 = 0.25, rho = -0.5)
	###
	 
	# American Exchange Options:
   	AmericanExchangeOption(S1 = 22, S2 = 0.20, Q1 = 1, Q2 = 1, 
    	Time = 0.1, r = 0.1, b1 = 0.04, b2 = 0.06, sigma1 = 0.2, 
     	sigma2 = 0.25, rho = -0.5)
	###

	# Exchange Options On Exchange Options:
   	for (flag in 1:4) print(
   		ExchangeOnExchangeOption(TypeFlag = as.character(flag), 
     		S1 = 105, S2 = 100, Q = 0.1, time1 = 0.75, Time2 = 1.0, 
     		r = 0.1, b1 = 0.10, b2 = 0.10, sigma1 = 0.20, 
     		sigma2 = 0.25, rho = -0.5))
	###

	# Two Risky Assets Options:
   	TwoRiskyAssetsOption(TypeFlag = "cmax", S1 = 100, S2 = 105, 
     	X = 98, Time = 0.5, r = 0.05, b1 = -0.01, b2 = -0.04, 
     	sigma1 = 0.11, sigma2 = 0.16, rho = 0.63)
   	TwoRiskyAssetsOption(TypeFlag = "pmax", S1 = 100, S2 = 105, 
     	X = 98, Time = 0.5, r = 0.05, b1 = -0.01, b2 = -0.04, 
     	sigma1 = 0.11, sigma2 = 0.16, rho = 0.63)
	###

	# Spread-Option Approximation:
   	SpreadApproxOption(TypeFlag = "c", S1 = 28, S2 = 20, X = 7, 
     	Time = 0.25, r = 0.05, sigma1 = 0.29, sigma2 = 0.36, 
     	rho = 0.42)
	###
	 
     	
# ------------------------------------------------------------------------------

 
### Example: Lookback Options
	
	# This script gives examples on some lookback options:
	#   Floating Strike Lookback Option
	#	Fixed Strike Lookback Option
	#	Partial-Time Floating Strike Lookback Option
	#	Partial-Time Fixed Strike Lookback Option
	# Reference:
	#	E.G. Haug [1997], Chapter 2
	###
	
	# Floating Strike Lookback Option:
    FloatingStrikeLookbackOption(TypeFlag = "c", S = 120, 
      SMinOrMax = 100, Time = 0.5, r = 0.10, b = 0.10-0.06, 
      sigma = 0.30)
	###
  
	# Fixed Strike Lookback Option:
    FixedStrikeLookbackOption(TypeFlag = "c", S = 100, 
      SMinOrMax = 100, X = 105, Time = 0.5, r = 0.10, b = 0.10, 
      sigma = 0.30)
	###
   
	# Partial Time Floating Strike Lookback Option:
    PTFloatingStrikeLookbackOption(TypeFlag = "p", S = 90, 
      SMinOrMax = 90, time1 = 0.5, Time2 = 1, r = 0.06, b = 0.06, 
      sigma = 0.20, lambda  = 1)
   
	# Partial Time Fixed Strike Lookback Option:
    PTFixedStrikeLookbackOption(TypeFlag = "c", S = 100, X = 90, 
	  time1 = 0.5, Time2 = 1, r = 0.06, b = 0.06, sigma = 0.20)

	  
# ------------------------------------------------------------------------------


### Example: Barrier Options

	# This script gives examples on some barrier options:
	#   Standard Barrier Options
	#	Double-Barrier Options
	#	Partial-Time Single-Asset Barrier Options
	#	Two-Asset Barrier Options
	#	Partial-Time Two-Asset Barrier Options
	#	Look-Barrier Options
	#	Soft-Barrier Options
	# Reference:
	#	E.G. Haug [1997], Chapter 2
	###

	# Standard Barrier Option:
   	# down-and-out Barrier Call
   	StandardBarrierOption(TypeFlag = "cdo", S = 100, X = 90, 
      H = 95, K = 3, Time = 0.5, r = 0.08, b = 0.04, sigma = 0.25)
	###
   
	# Double-Barrier Option:
    DoubleBarrierOption(TypeFlag = "co", S = 100, X = 100, L = 50, 
      U = 150, Time = 0.25, r = 0.10, b = 0.10, sigma = 0.15, 
      delta1 = -0.1, delta2 = 0.1)
	###
   
	# Partial-Time Single-Asset Barrier Option:
    PTSingleAssetBarrierOption(TypeFlag = "coB1", S = 95, X = 110, 
      H = 100, time1 = 0.5, Time2 = 1, r = 0.20, b = 0.20, 
      sigma = 0.25)
	###
   
	# Two-Asset Barrier Option:
    TwoAssetBarrierOption(TypeFlag = "puo", S1 = 100, S2 = 100, 
      X = 110, H = 105, Time = 0.5, r = 0.08, b1 = 0.08, b2 = 0.08, 
      sigma1 = 0.2, sigma2 = 0.2, rho = -0.5)
	###
   
	# Partial-Time Two-Asset Barrier Option:
    PTTwoAssetBarrierOption(TypeFlag = "pdo", S1 = 100, S2 = 100, 
      X = 100, H = 85, time1 = 0.5, Time2 = 1, r = 0.1, b1 = 0.1, 
      b2 = 0.1, sigma1 = 0.25, sigma2 = 0.30, rho = -0.5)
	###
   
	# Look-Barrier Option:
    LookBarrierOption(TypeFlag = "cuo", S = 100, X = 100, H = 130, 
      time1 = 0.25, Time2 = 1, r = 0.1, b = 0.1, sigma = 0.15)
    LookBarrierOption(TypeFlag = "cuo", S = 100, X = 100, H = 110, 
      time1 = 1, Time2 = 1, r = 0.1, b = 0.1, sigma = 0.30)
	###
    
	# Soft-Barrier Option:
    SoftBarrierOption(TypeFlag = "cdo", S = 100, X = 100, L = 70, 
      U = 95, Time = 0.5, r = 0.1, b = 0.05, sigma = 0.20)
	###
 


# ------------------------------------------------------------------------------


### Example: Binary Options
 
	# This script gives examples on some binary options:
	#   Gap Option
	#   Cash-Or-Nothing Option
	#   Two-Asset Cash-Or-Nothing Option
	#   Asset-Or-Nothing Option
	#	Super-Share Option
	# Reference:
	#	E.G. Haug [1997]
	###
	
	# Gap Option:
	GapOption("c", S = 50, X1 = 50, X2 = 57, Time = c(0.25, 0.50, 1), 
		r = 0.09, b = 0.09, sigma = 0.20)
	###

	# Cash-Or-Nothing Option:
	CashOrNothingOption("p", S = 100, X = 80, K = 10, Time = 0.75, 
		r = 0.06, b = 0, sigma = c(0.15, 0.25, 0.35))
	###
		
	# Two-Asset Cash-Or-Nothing Option:
    for (TypeFlag in c("c", "p", "ud", "du"))
	print(TwoAssetCashOrNothingOption(TypeFlag, S1 = 100, S2 = 100, 
		X1 = 110, X2 = 90, K = 10, Time = 0.5, r = 0.10, b1 = 0.05, 
		b2 = 0.06, sigma1 = 0.20, sigma2 = 0.25, rho = 0))
	###
		
	# Asset-Or-Nothing Option:
	AssetOrNothingOption("p", S = 70, X = 65, Time = 0.5, r = 0.07, 
		b = 0.07-0.05, sigma = 0.27)		
	###
	
	# Super-Share Option:
	SuperShareOption(S = 100, XL = 90, XH = 110, Time = 0.25, 
		r = 0.10, b = 0, sigma = 0.20)				
	###


# ------------------------------------------------------------------------------


### Example: Asian Options

	# This script gives examples on some Asian options:
	#   Geometric Average Rate Option
	#   Approximations for the Arithmetic Average Rate Option
	#	  Turnbull's and Wakeman's Approximation
	#	  Levy's Approximation
	# Reference:
	#	E.G. Haug [1997], Chapter 2
	###

	# Geometric Average Rate Option:
   	GeometricAverageRateOption("p", S = 80, X = 85, 
      Time = 0.25, r = 0.05, b = 0.08, sigma = 0.20)
	###

	# Turnbull Wakeman Approximation:
    TurnbullWakemanAsianApproxOption("p", S = 90, SA = 88, 
      X = 95, Time = 0.50, time = 0.25, tau = 0.0, r = 0.07, 
      b = 0.02, sigma = 0.25)
	###
      
	# Levy Asian Approximation:   
    LevyAsianApproxOption("c", S = 100, SA = 100, X = 105, 
      Time = 0.75, time = 0.50, r = 0.10, b = 0.05, sigma = 0.15)
	###
    	
      
################################################################################	