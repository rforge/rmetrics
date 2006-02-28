
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
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
# DATASET:        DESCRIPTION:
# Chapter 1        Financial Time Series and Their Characteristics
#  dibmln.txt       Daily log returns of IBM (62/7/3 to 97/12)  
#  dvwew.txt        Daily simple returns of valueweighted and equalweighted indexes 
#  dintc.txt        Daily simple returns of Intel stock   
#  dmmm.txt         Daily simple returns of 3M stock 
#  dmsft.txt        Daily simple returns of Microsoft stock 
#  dciti.txt        Daily simple returns of Citigroup stock 
#  mbnd.txt         Monthly bond returns (30 yrs, 20 yrs, ..., 1 yr) 
#  mgs.txt          Monthly Treasury rates (10 yrs, 5 yrs, ..., 1 yr) 
#  wtb3ms.txt       3M Weekly Treasury Bill rates 
#  wtb6ms.txt       6M Weekly Treasury Bill rates
# Chapter 2        Linear Time Series Analysis and Its Applications
#  qgnp.txt         US quarterly growth rates of GNP 
#  mvw.txt          Monthly valueweighted index returns 
#  mew.txt          Monthly equalweighted index returns 
#  m3m4699.txt      Monthly log returns of 3M stock 
#  jnj.txt          Quarterly earnings per share of Johnson and Johnson 
#  wgs1yr.txt       Weekly US Treasury 1y constant maturity rates  
#  wgs3yr.txt       Weekly US Treasury 3y constant maturity rates
# Chapter 3        Conditional Heteroscedastic Models
#  mintc.txt        Monthly simple returns of Intel stock 
#  exchperc.txt     10m log returns of FX (MarkUS) 
#  sp500.txt        Excess returns of S&P500 
#  mibmln.txt       Monthly log returns of IBM stock 
#  dhwp3dx8099.txt  Daily log returns of SP500 index 
#  mibmspln.txt     Monthly log returns of IBM stock and SP500 
#  mibmsplnsu.txt   Data set for Example 3.5   
# Chapter 4        Nonlinear Models and Their Applications
#  mew.txt          Monthly simple returns of equalweighted index 
#  dibmln99.txt     Daily log returns of IBM stock 
#  mmmm.txt         Monthly simple returns of 3M stock 
#  qgnp.txt         Quarterly growth rates of US gnp 
#  mibmln99.txt     Monthly log returns of IBM stock 
#  qunemrate.txt    Quarterly unemployment rates
# Chapter 5        HighFrequency Data Analysis and Market Microstructure
#  ibm.txt          IBM transactions data 1990-11-01 1991-01-31   
#  ibm9912tp.txt    IBM transactions data of December 1999   
#  ibmdurad.txt     IBM Adjusted time durations between trades 
#  ibm1to5dur.txt   IBM Adjusted durations for first 5 trading days 
#  ibm91ads.txt     IBM Data for Example 5.2 - ADS file 
#  ibm91adsx.txt    IBM explanatory variables as defined 
#  day15ori.txt     IBM Transactions data 1990-11-21, original data 
#  day15.txt        IBM Data for PCD models.}  
# Chapter 7        Extreme Values, Quantile Estimation, and Value at Risk
#  dibmln98.txt     Daily perdentage log returns of IBM stock 
#  dintc7297.txt    Daily log returns of Intel stock, Example 7.4 
#  ibmln98wm.txt    Meancorrected daily log returns of IBM 
#  ibml25x.txt      The explanatory variables on page 294  
# Chapter 8        Multivariate Time Series Analysis and Its Applications
#  mibmspln.txt     Monthly log returns of IBM and SP 500 
#  mbnd.txt         Monthly simple returns of bond indexes  
#  mgs1n3.txt       Monthly US interest rates, Example 8.6  
#  sp5may.txt       Log prices of SP500 index futures and shares 
#  m5cln.txt        Monthly log returns of IBM, HWP, INTC, MER and MWD
# Chapter 9        Multivariate Volatility Models and Their Applications
#  hkja.txt         Daily log returns of HK and Japan market index 
#  mibmspln.txt     Monthly log returns of IBM and SP 500 
#  dcscointc.txt    Daily log returns of SP 500, Cisco and Intel stocks    
# Chapter 10       Markov Chain Monte Carlo Methods with Applications
#  wgs3n1c.txt      Change series of weekly US interest rates, 3Y and 1Y 
#  wgs3c.txt        Change series of weekly US 3yr interest rate 
#  msp6299.txt      Monthly log returns of SP 500 index 
#  mibmsp6299.txt   Monthly log returns of IBM stock and SP 500 
#  mgeln.txt        Monthly log returns of GE stock   
################################################################################

