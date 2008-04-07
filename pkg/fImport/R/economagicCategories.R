
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
#  economagicCategories  Lists categories of symbols from www.economagic.com
################################################################################


economagicCategories <-
    function()
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Lists categories of symbols from www.economagic.com
    
    # Example:
    #   economagicCategories()
    
    # FUNCTION:
    
    # Categories and Description:
    Categories = matrix(c(
        "fedbog",       "FED Interest Rates",     
        "frbg17",       "FED Production, Capacity, Utilization",    
        "frbz1",        "FED Credit Market Debt Outstanding",      
        "frbg19",       "FED Consumer Credit",     
        "frbfor",       "FED Household Debt Service",  
        "frbsls",       "FED Senior Loan Officer Opinion Survey",   
        "fedstl",       "FED St. Louis Macroeconomic data",
        "feddal",       "FED Dallas Macroeconomic data",
        "fedny",        "FED New York Daily Foreign Exchange",
        "fedphl",       "FED Philadelphia Business Outlook Survey",   
        "blslf",        "BLS Employment and Unemployment US",
        "blsint",       "BLS International Employment and Prices", 
        "cbo",          "BLS Congressional Budget Potential Output",
        "treas",        "DOT US Treasury Public Debt CData",        
        "doeme",        "DOE Monthly Energy Prices", 
        "doewkly",      "DOE Weekly Gasoline Prices",               
        "libor",        "LIBOR London Interbank Offered Rates",     
        "aus",          "Data of the Reserve Bank of Australia",
        "japan",        "Bank of Japan Economic Planning Agency",
        "ecb",          "European Centrasl Bank Time Series",       
        "crb",          "Bridge/CRB Spot Indices", 
        "ism",          "ISM Institute for Supply Management", 
        "sp",           "Stock Price Indexes"),   
        byrow = TRUE, ncol = 2)
    Categories = data.frame(Categories)
    colnames(Categories) <- c("Category", "Description")
    rownames(Categories) <- NULL
    attr(Categories, "control") <- 
        c(source = "economagic.com", categories = NROW(Categories))
    
    # Return Value:
    Categories
}


################################################################################
   
