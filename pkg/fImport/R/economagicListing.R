
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
#  economagicListing     Lists symbols from www.economagic.com
################################################################################


# *** DW: UNDER CONSTRUCTION ***
# *** DO NOT USE IT YET AS A PRODUCTION TOOL ***


.economagicListing <- 
    function(category = c(
        "fedbog", "frbg17", "frbz1", "frbg19", "frbfor", "frbsls", "fedstl",   
        "feddal", "fedny", "fedphl", "blslf", "blsint", "cbo", "treas",    
        "doeme", "doewkly", "libor", "aus", "japan", "ecb", "crb", "ism",      
        "sp"), abbreviate = 60)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Lists symbols from www.economagic.com
    
    # Example:
    #   summary(economagicListing("fedbog"))
    
    # Details:         Major Categories:
    #   "fedbog",       "FED Interest Rates",     
    #   "frbg17",       "FED Production, Capacity, Utilization",    
    #   "frbz1",        "FED Credit Market Debt Outstanding",      
    #   "frbg19",       "FED Consumer Credit",     
    #   "frbfor",       "FED Household Debt Service",  
    #   "frbsls",       "FED Senior Loan Officer Opinion Survey",   
    #   "fedstl",       "FED St. Louis Macroeconomic data",
    #   "feddal",       "FED Dallas Macroeconomic data",
    #   "fedny",        "FED New York Daily Foreign Exchange",
    #   "fedphl",       "FED Philadelphia Business Outlook Survey",   
    #   "blslf",        "BLS Employment and Unemployment US",
    #   "blsint",       "BLS International Employment and Prices", 
    #   "cbo",          "BLS Congressional Budget Potential Output",
    #   "treas",        "DOT US Treasury Public Debt CData",        
    #   "doeme",        "DOE Monthly Energy Prices", 
    #   "doewkly",      "DOE Weekly Gasoline Prices",               
    #   "libor",        "LIBOR London Interbank Offered Rates",     
    #   "aus",          "Data of the Reserve Bank of Australia",
    #   "japan",        "Bank of Japan Economic Planning Agency",
    #   "ecb",          "European Centrasl Bank Time Series",       
    #   "crb",          "Bridge/CRB Spot Indices", 
    #   "ism",          "ISM Institute for Supply Management", 
    #   "sp",           "Stock Price Indexes". 
    
    # FUNCTION:
    
    # Match Arguments:
    Category = match.arg(category)
       
    # Load Data: 
    file = paste("http://www.economagic.com/", Category, ".htm", sep = "")
    x = readLines(file)
    
    # Compose:
    y = x[grep('em-cgi/data.exe/', x, perl = TRUE)]
    y = sub('^(.*?)data.exe/', '', y)
    y = gsub('<span style=.color:white.>', "", y)
    y = gsub('<font.color=white>(.*?)font>', "", y)
    y = gsub('<.>', "", y)
    y = gsub('<..>', "", y)
    y = gsub("[ \t]+", " ", y)  
    y = sub('.>', "@", y)
     
    y = sub(';', '', y)
    y = sub('^ ', '', y)
    y = sub(' $', '', y)
    y = sub('^, ', '', y)
      
    # Category: fedbog
    if (Category == "fedbog") {
        # Category = "fedbog" - Cut extra long descriptions ...
        y = sub(
            'Long-Term U.S. Government Securities Including Flower Bonds', 
            'Lpng-Term US Gov Secs incl. Flower Bonds', y)
        y = sub(
            'State and Local 20-year Bond Index - The Bond Buyer',
            'State/Local 20Y Bond Index - The Bond Buyer', y)
        y = sub(
            '30 Year Conventional Mortgages - Federal Home Loan Mortgage Corporation',
            '30Y Conv. Mortgages - Federal HLM Corporation', y)
        y = sub(
            'Yield on US Treasury bonds with maturity over 10 years',
            'US Treasury Bond Yield, maturity over 10Y', y)
    }  
    
    # Category: frbg
    if (Category == "frbg17") {
        # Category = "frbg17" - Reduce to major series ...
        y = y[grep('^frbg17/T5', y, perl = TRUE)]
    }
    
    # Category: frbz1
    if (Category == "frbz1") {
        # Category = "frbz1" - Reduce to major series ...
        y = sub(' by:', '', y)
        y[1] = sub('@', '@TCMD ', y[1])
        y[2:25] = sub('@', '@TCMD owed by ', y[2:25])
        y[26] = sub('@', '@TCMA ', y[26])
        y[26:59] = sub('@', '@TCMA held by ', y[26:59])
    }
    
    # Category: fedstl
    if (Category == "fedstl") {
        # Category = "fedstl" - Reduce to major series ...
    }

    # Category:
    if (Category == "NA") {   
        y = sub('Exchange Rate:', 'FX', y)
        y = sub('United States', 'US', y)
        y = sub('in the U.S.', 'US', y)
        y = sub('consists(.*?)Sectors', '', y)
        y = sub('Secondary Market Rate', 'Secondary Market', y)
        y = sub('Federal Reserve Bank', 'FED', y)
        y = sub(' at Depository Institutions', '', y)
        y = sub('Board of Governors', 'BOG', y)
        y = sub('Changes in ', '', y)
        head(y)
    }
    
    # Split and Make Listing:
    s = strsplit(y, split = '@')
    listing = matrix(unlist(s), byrow = TRUE, ncol = 2) 
    NAs = rep(NA, times = NROW(listing)) 
    listing = cbind(listing[,1], NAs, NAs, listing[, 2])
    colnames(listing) = c("Symbol", "ISIN", "Valor", "Description")
    rownames(listing) = 1:NROW(listing)
    
    # Abbreviate ?
    listing[, 4] = abbreviate(listing[, 4], minlength = abbreviate, 
        use.classes = TRUE, dot = FALSE, method = "left.kept")
        
    # For the moment skip columns 2 and 3:
    listing = listing[, c(1, 4)]
    listing = data.frame(listing)  
    
    # Attributes:
    attr(listing, "control") <- 
        c(source = "economagic", category = Category, symbols = NROW(listing))
    
    # Return Value:
    listing
}


################################################################################
   
