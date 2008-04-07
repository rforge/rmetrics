
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             DESCRIPTION: 
#  forecastsCategories   Lists categories of symbols from www.forecasts.org
################################################################################


forecastsCategories <-
    function()
{
    # Description:
    #   Lists categories of symbols from www.forecasts.org
    
    # FUNCTION:
    
    # Categories and Description:
    Categories = matrix(c(
        "business",       "Business Data",              
        "banking",        "Banking Data",               
        "cpi",            "Consumer Price Indices",     
        "interest",       "Interest Rates",             
        "exchange",       "Exchange Rates",             
        "price",          "Prices",                                  
        "employment",     "Employment and Compensation",
        "monetary",       "Monetary Data",              
        "gdp",            "GDP and Components",         
        "government",     "Federal, State, and Local",  
        "ppi",            "Producer Price Indices",     
        "regional",       "Regional Data",              
        "population",     "US Population Data",         
        "weekly",         "Weekly Data Series"),         
        byrow = TRUE, ncol = 2)
    Categories = data.frame(Categories)
    colnames(Categories) <- c("Category", "Description")
    rownames(Categories) <- NULL
    attr(Categories, "control") <- 
        c(source = "forecast.org", categories = NROW(Categories))
    
    # Return Value:
    Categories
}  


################################################################################

