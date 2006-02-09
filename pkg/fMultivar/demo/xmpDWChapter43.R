#
# Examples from the forthcoming Monograph:
# 	Rmetrics - Financial Engineering and Computational Finance
#   written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 4.3
#   Demand and Supply Models:
#
# List of Examples, Exercises and Code Snippets:
#   
#   Example: Estimation of Grunfeld's Model Data with OLS and SUR 
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


### Example: Estimation of Grunfeld's Model Data with OLS and SUR 
	
	# Details:
	#   PART I: Create data matrix from Grunfeld's book
	#   PART II: Convert the matrix into a data frame
	#   PART III: Perform an OLS Estimation
	#   PART IV: Perform a SUR Estimation
	# Description:
	#   Different stock prices often move in the same direction at a given 
	#   point in time. The SUR technique may provide more efficient estimates 
	#   than OLS in this situation. The example was used by Zellner in his 
	#   classic 1962 paper on seemingly unrelated regressions. 
	# References:
	#   A. Zellner, "An Efficient Method of Estimating Seemingly   
	#       Unrelated Regressions and Tests for Aggregation Bias,"     
	#       JASA 57(1962) pp.348-364                                     
	#   J.C.G. Boot, "Investment Demand: an Empirical Contribution 
	#       to the Aggregation Problem", IER 1(1960) pp.3-30.      
	#   Y. Grunfeld, "The Determinants of Corporate Investment,"   
	#       Unpublished Thesis, Chicago, 1958      
	#   http://www.stanford.edu/~clint/bench/,
	#       Econometric Benchmarks  
	# Grunfeld Data:
	#   Year
	#   ge_i = 'Gross Investment, GE'
	#   ge_c = 'Capital Stock Lagged, GE'
	#   ge_f = 'Value of Outstanding Shares Lagged, GE'
	#   wh_i = 'Gross Investment, WH'
	#   wh_c = 'Capital Stock Lagged, WH'
	#   wh_f = 'Value of Outstanding Shares Lagged, WH';
	#   The GE abbreviates General Electric and WH Westinghouse.
	# Notes:
	#	Results from other software packages are presented.
	###

	# PART I:
	# Data matrix:
    grunfeld.mat = matrix(c(
       1935,      33.1,    1170.6,     97.8,    12.93,     191.5,       1.8,
       1936,      45.0,    2015.8,    104.4,    25.90,     516.0,       0.8,
       1937,      77.2,    2803.3,    118.0,    35.05,     729.0,       7.4,
       1938,      44.6,    2039.7,    156.2,    22.89,     560.4,      18.1,
       1939,      48.1,    2256.2,    172.6,    18.84,     519.9,      23.5,
       1940,      74.4,    2132.2,    186.6,    28.57,     628.5,      26.5,
       1941,     113.0,    1834.1,    220.9,    48.51,     537.1,      36.2,
       1942,      91.9,    1588.0,    287.8,    43.34,     561.2,      60.8,
       1943,      61.3,    1749.4,    319.9,    37.02,     617.2,      84.4,
       1944,      56.8,    1687.2,    321.3,    37.81,     626.7,      91.2,
       1945,      93.6,    2007.7,    319.6,    39.27,     737.2,      92.4,
       1946,     159.9,    2208.3,    346.0,    53.46,     760.5,      86.0,
       1947,     147.2,    1656.7,    456.4,    55.56,     581.4,     111.1,
       1948,     146.3,    1604.4,    543.4,    49.56,     662.3,     130.6,
       1949,      98.3,    1431.8,    618.3,    32.04,     583.8,     141.8,
       1950,      93.5,    1610.5,    647.4,    32.24,     635.2,     136.7,
       1951,     135.2,    1819.4,    671.3,    54.38,     723.8,     129.7,
       1952,     157.3,    2079.7,    726.1,    71.78,     864.1,     145.5,
       1953,     179.5,    2371.6,    800.3,    90.08,    1193.5,     174.8,
       1954,     189.6,    2759.9,    888.9,    68.60,    1188.9,     213.5),
       byrow = TRUE, ncol = 7)
	###
   
	# PART II: 
	# Convert to data frame:
    # for R use:
    grunfeld.df = data.frame(grunfeld.mat[,-1])
    rownames(grunfeld.df) <- 
     	paste(as.character(grunfeld.mat[,1]), "-12-31", sep="")
    colnames(grunfeld.df) <- 
     	c("ge.i", "ge.f", "ge.c", "wh.i", "wh.f", "wh.c")
   	grunfeld.df
    # For S-Plus use:
    # grunfeld.df = data.frame(grunfeld.mat[,-1])
    # rowIds(grunfeld.df) <- 
    #   paste(as.character(grunfeld.mat[,1]), "-12-31", sep="")
    # colIds(grunfeld.df) <- 
    #   c("ge.i", "ge.f", "ge.c", "wh.i", "wh.f", "wh.c")
    # grunfeld.df
    ###

	# PART III: 
	# Perform an OLS Estimation: 
	formulas = list( GE = ge.i ~ ge.f + ge.c, WH = wh.i ~ wh.f + wh.c )
	FITOLS = eqnsFit(formulas, data = grunfeld.df)
	FITOLS
	# Results:
	# Coefficients   systemfit    S-Plus     SAS   
	#   Intercept     -9.9563       =         =
	#   ge.f           0.0266       =         =
	#   ge.c           0.1517       =         =
	#   Intercept     -0.5094       =         =
	#   wh.f           0.0529       =         =
	#   wh.c           0.0924       =         =
	# t-values 
	#   Intercept     -0.32         =         =      
	#   ge.f           1.71         =         =
	#   ge.c           5.90         =         =
	#   Intercept     -0.06         =         =
	#   wh.f           3.37         =         =
	#   wh.c           1.65         =         =   
	# Comparison:
	#   All three packages yield the same results.
	# Source:
	#   SAS 
	#     gsbwww.uchicago.edu/computing/research/SASManual/ets/chap19/sect44.htm
	#   S-PLUS 
	#     Version 6.2 for Windows running under Windows XP Professional
	###
   
    #  PART IV: 
    # SUR Estimation:
	# FITSUR = eqnsFit(formulas, data = grunfeld.df, method = "SUR")
	# the same as ...
	# FITSUR = SUR(formulas, data = grunfeld.df)
	# or the same as ...
	# require(systemfit)
	formulas = list( GE = ge.i ~ ge.f + ge.c, WH = wh.i ~ wh.f + wh.c )
	FITSUR = systemfit(method = "SUR", eqns = formulas, data = grunfeld.df)
	FITSUR
	# Results:
	# Coefficient      systemfit        S-Plus        SAS            TSP
	#   Intercept     -27.7193        -28.1577      -27.7193       -27.7193
	#   ge_f            0.0383          0.0386        0.0383         0.0383
	#   ge_c            0.1390          0.1385        0.1390         0.1390
	#   Intercept      -1.2520         -1.3180       -1.2520        -1.2520
	#   wh_f            0.0576          0.0579        0.0576         0.0576
	#   wh_c            0.0640          0.0626        0.0640         0.0640
	# t-values         
	#   Intercept      -0.95           -0.96         -0.95          -1.03
	#   ge_f            2.66            2.69          2.66           2.88
	#   ge_c            5.56            5.55          5.56           6.04
	#   Intercept      -0.17           -0.18         -0.17          -0.18
	#   wh_f            3.96            4.00          3.96           4.30
	#   wh_c            1.21            1.18          1.21           1.31
	# R-Squared:
	#   Mult. R-Sq.     0.7404          0.6918
	#   Adj.  R-Sq.     0.7099          0.6555
	###

	# NOTES:
	# Compare results from those obtained by other software   
  
	# *** SAS *** 
	# gsbwww.uchicago.edu/computing/research/SASManual/ets/chap19/sect44.htm
	#
	# The SYSLIN Procedure 
	# Seemingly Unrelated Regression Estimation 
	# System Weighted MSE 0.9719 
	# Degrees of freedom 34 
	# System Weighted R-Square 0.6284 
	#
	# Model GE 
	# Dependent Variable ge_i 
	# Label Gross Investment, GE 
	# Parameter Estimates 
	# Variable DF Parameter
	# Estimate Standard Error t Value Pr > |t| Variable
	# Label 
	# Intercept 1 -27.7193 29.32122 -0.95 0.3577 Intercept 
	# ge_f 1 0.038310 0.014415 2.66 0.0166 Value of Outstanding Shares Lagged, GE 
	# ge_c 1 0.139036 0.024986 5.56 <.0001 Capital Stock Lagged, GE 
	#
	# Model WESTING 
	# Dependent Variable wh_i 
	# Label Gross Investment, WH 
	# Parameter Estimates 
	# Variable DF Parameter
	# Estimate Standard Error t Value Pr > |t| Variable
	# Label 
	# Intercept 1 -1.25199 7.545217 -0.17 0.8702 Intercept 
	# wh_f 1 0.057630 0.014546 3.96 0.0010 Value of Outstanding Shares Lagged, WH 
	# wh_c 1 0.063978 0.053041 1.21 0.2443 Capital Stock Lagged, WH 
     
	# *** SPLUS ***
	# Version 6.2 for Windows running under Windows XP Professional
	#
	# Seemingly Unrelated Regression:
	#
	# Eq. 1: ge.i ~ ge.f + ge.c
	# Coefficients:
	#                Value Std. Error  t value Pr(>|t|) 
	# (Intercept) -28.1577  29.2273    -0.9634   0.3489
	#        ge.f   0.0386   0.0144     2.6902   0.0155
	#        ge.c   0.1385   0.0250     5.5526   0.0000
	# Regression Diagnostics:                       
	#          R-Squared 0.6918
	# Adjusted R-Squared 0.6555
	# Durbin-Watson Stat 0.9826
	# Degrees of freedom: 20 total; 17 residual
	# Residual scale estimate: 28.5167 
	#
	# Eq. 2: wh.i ~ wh.f + wh.c
	# Coefficients:
	#               Value Std. Error t value Pr(>|t|) 
	# (Intercept) -1.3180  7.5237    -0.1752  0.8630 
	#        wh.f  0.0579  0.0145     3.9953  0.0009 
	#        wh.c  0.0626  0.0529     1.1832  0.2530 
	# Regression Diagnostics:                         
	#          R-Squared 0.7400
	# Adjusted R-Squared 0.7094
	# Durbin-Watson Stat 1.3619
	# Degrees of freedom: 20 total; 17 residual
	# Residual scale estimate: 10.3014 
	# Log determinant of residual covariance: 10.4468 
    
	# *** systemfit Version 0.7-2 ***
	# R Version 1.9.1 running under Windows XP Professional
	#
	# SUR estimates for 1  (equation 1 )
	# Model Formula: ge_i ~ ge_f + ge_c
	#               Estimate Std. Error   t value Pr(>|t|)    
	# (Intercept) -27.719317  29.321219 -0.945367 0.357716    
	# ge_f           0.03831   0.014415  2.657634 0.016575   *
	# ge_c          0.139036   0.024986  5.564656  3.4e-05 ***
	# ---
	# Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 
	# Residual standard error: 28.479483 on 17 degrees of freedom
	# Number of observations: 20 Degrees of Freedom: 17 
	# SSR: 13788.375833 MSE: 811.080931 Root MSE: 28.479483 
	# Multiple R-Squared: 0.692557 Adjusted R-Squared: 0.656388 
	#
	# SUR estimates for 2  (equation 2 )
	# Model Formula: wh_i ~ wh_f + wh_c
	#              Estimate Std. Error   t value Pr(>|t|)   
	# (Intercept) -1.251988   7.545217 -0.165931 0.870168   
	# wh_f          0.05763   0.014546  3.961822 0.001007 **
	# wh_c         0.063978   0.053041   1.20621 0.244256   
	# ---
	# Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 
	# Residual standard error: 10.293633 on 17 degrees of freedom
	# Number of observations: 20 Degrees of Freedom: 17 
	# SSR: 1801.300878 MSE: 105.958875 Root MSE: 10.293633 
	# Multiple R-Squared: 0.740401 Adjusted R-Squared: 0.70986 
    
	# *** TSP *** 
	# www.stanford.edu/~clint/bench/grunsur2.tsp
	# ? 2. SUR (2-step, not iterated)
	# ?              Theil       Kmenta
	# ?              p.300       p.645        TSP
	# ? GE Const.   -27.7      -27.7193    -27.7193
	# ?    (SEasy)  (27.0)                 (27.0328)
	# ?    (SEdf)              (29.3212)
	# ?    F(-1)       .038       .0383       .038310
	# ?               (.013)                 (.013290)
	# ?                          (.0145)
	# ?    K(-1)       .139       .1390       .139036
	# ?               (.023)                 (.023036)
	# ?                          (.0250)
	# ? WH Const.    -1.3       -1.2520     -1.25199
	# ?              (7.0)                  (6.95635)
	# ?                         (7.5452)
	# ?    F(-1)       .058       .0576       .057630
	# ?               (.013)                 (.013411)
	# ?                          (.0145)
	# ?    K(-1)       .064       .0640       .063978
	# ?               (.049)                 (.048901)
	# ?                          (.0530)
	#
	# t-values calculated from coefficients and errors:
	# > -27.7193/27.0328  [1] -1.025395
	# > 0.038310/0.013290 [1]  2.882619
	# > .139036/0.023036  [1]  6.035596
	# > -1.25199/6.95635  [1] -0.179978
	# > 0.057630/0.013411 [1]  4.297219
	# > 0.063978/0.048901 [1]  1.308317
    
    
################################################################################
    
