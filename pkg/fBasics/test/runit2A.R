
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
# You should have received a copy of the GNU Library General 
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
# FUNCTION:             DESCRIPTION:
#  fWEBDATA              Class Representation
#  show.fWEBDATA         S4 Show Method
# FUNCTION:             IMPORT DATA FUNCTIONS:
#  economagicImport      Downloads market data from EconoMagic's web site
#  yahooImport           Downloads market data from Yahoo's web site 
#  .yahooImport          ... the old download function 
#  keystatsImport        Downloads key statistics from Yahoo's web site  
#  fredImport            Downloads market data from St. Louis FED web site
#  forecastsImport       Downloads monthly data from www.forecasts.org
# FUNCTION              EASY TO USE ROUTINES:
#  yahooSeries           Easy to use download from Yahoo
#  .yahooSeries          Utility function  called by 'yahooSeries'
# FUNCTION:             ONLY FOR SPLUS VERSION:
#  as.Date               Converts date represenatation
#  data                  Loads or lists specified data sets
#  download.file         Downloads files from Internet using "lynx" or "wget"
#  strsplit              Splits elements of a character vector into substrings
################################################################################


test.helpFile = 
function()
{
## SOURCE("fBasics.012A-TimeSeriesData")

## economagicImport -
   xmpBasics("\nStart: Daily Foreign Exchange Rates > ")
   USDEUR = economagicImport(query = "fedny/day-fxus2eu", 
     frequency = "daily", colname = "USDEUR")
   # Print Data Slot if Internet Download was Successful:
   if (!is.null(USDEUR)) print(USDEUR@data[1:20, ])

## economagicImport -
   xmpBasics("\nNext: USFEDFUNDS Monthly US FedFunds Rates > ")
   USFEDFUNDS = economagicImport(query = "fedstl/fedfunds+2", 
     frequency = "monthly", colname = "USFEDFUNDS")
   if (!is.null(USFEDFUNDS)) print(USFEDFUNDS@data[1:20, ])
   
## economagicImport -
   xmpBasics("\nNext: USDGNP Quarterly GNP Data Records > ")
   USGNP = economagicImport(query = "fedstl/gnp", 
     frequency = "quarterly", colname = "USGNP")
   if(!is.null(USGNP)) print(USGNP@data[1:20, ])  
     
## yahooImport -
   xmpBasics("\nNext: IBM Shares from Yahoo > ")
   # [test 19/20 century change 01-12-1999 -- 31-01-2000]
   query = "s=IBM&a=11&b=1&c=1999&d=0&q=31&f=2000&z=IBM&x=.csv"
   IBM = yahooImport(query)  
   if (!is.null(IBM)) print(IBM@data[1:20, ])
   
## keystatsImport -
   xmpBasics("\nNext: Key Statistics IBM Shares from Yahoo > ")  
   keystatsImport("IBM")  
   
## fredImport -
   xmpBasics("\nNext: DPRIME Daily Bank Prime Load Rate Records > ")
   DPRIME = fredImport("DPRIME")
   if (!is.null(DPRIME)) print(DPRIME@data[1:20, ])       
}

   
# ------------------------------------------------------------------------------

   
test.economagic = 
function()
{  
    # economagicImport - Daily Rates:
    X = economagicImport(query = "fedny/day-fxus2eu", 
        frequency = "daily", colname = "USDEUR")
    checkIdentical(
        target = X@data[6, 1], 
        current = "1999-01-13")
    checkIdentical(
        target = X@data[6, 2], 
        current = 1.1698)

    # economagicImport - Monthly Rates:
    X = economagicImport(query = "fedstl/fedfunds+2", 
         frequency = "monthly", colname = "USFEDFUNDS")
    checkIdentical(
        target = X@data[6, 1], 
        current = "1955-02-01")
    checkIdentical(
        target = X@data[6, 2], 
        current = 1.29)
    
    # economagicImport - Quarterly Rates:
    X = economagicImport(query = "fedstl/gnp", 
        frequency = "quarterly", colname = "USGNP")
    checkIdentical(
        target = X@data[6, 1], 
        current = "1948-12-01")
    checkIdentical(
        target = X@data[6, 2], 
        current = 276.6)
        
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------
       
        
test.yahoo = 
function()
{       
    # yahooImport -
    # [test 19/20 century change 01-12-1999 -- 31-01-2000]
    query = "s=IBM&a=11&b=29&c=1999&d=0&e=5&f=2000&z=IBM&x=.csv"
    X = yahooImport(query)  
    checkIdentical(
        target = as.vector(X@data[3:4, 1]), 
        current = c("1999-12-31", "2000-01-03"))
    checkIdentical(
        target = sum(X@data[, "Volume"]), 
        current = 40312900)
         
    # keystatsImport - 
    keystatsImport("IBM")  
    # ... html Format problems
        
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------
       
        
test.fred = 
function()
{     
    # fredImport -
    # fredImport(query, file = "tempfile", 
    #    source = "http://research.stlouisfed.org/fred2/series/", 
    #   frequency = "daily", save = FALSE, sep = ";", try = TRUE) 
    X = fredImport("DPRIME")@data 
    
    # fredSeries -     
    fredSeries = function(symbol = "DPRIME")
    {   
        data = fredImport(query = symbol)@data
        as.timeSeries(data)
    }
    X = fredSeries("DPRIME")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------

    
if (FALSE) {
    testResult <- runTestFile("C:/Rmetrics/trunk/fBasics/test/runit012A.R")
    printTextProtocol(testResult)
}   


################################################################################

    