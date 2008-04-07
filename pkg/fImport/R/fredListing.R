
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
#  fredListing           Lists symbols from research.stlouisfed.org  
################################################################################


# *** DW: UNDER CONSTRUCTION ***

  
.fredListing <- 
    function(category = c(
        "business", "banking", "cpi", "interest", "fx", "price", 
        "employment", "monetary", "reserves", "gdp", "government", "ppi",
        "regional", "bop"), abbreviate = 60)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Lists symbols from research.stlouisfed.org
    
    # Example:
    #   fredListing("fx")
    
    # FUNCTION:

    # Match:
    listing = match.arg(category) 
 
    # Banking:                                                               
    #   "93"    "Condition of Banks US"                                                                                                    
    #   "101"   "Commercial Credit"                              
    #   "100"   "Loans"                                          
    #   "99"    "Securities and Investments" 
    if(listing == "banking") categories = c(93, 1001:99)
                
    # Business:                              
    #   "4"     "Employment Cost Index"                          
    #   "5"     "Federal Government Debt"                        
    #   "97"    "Household Sector"                               
    #   "3"     "Industrial Production"                          
    #   "2"     "Productivity &amp; Cost"                        
    #   "6"     "Retail Sales"                                   
    #   "98"    "Other Economic Indicators"  
    if(listing == "business") categories = c(4,5,97,3,2,6,98)
    
    # CPI:              
    #   "9"     "Consumer Price Indexes (CPI)"  
    if(listing == "cpi") CPI = 9
               
    # Employment:                    
    #   "11"    "Establishment Survey Data"                      
    #   "12"    "Household Survey Data"                          
    #   "104"   "Population"   
    if(listing == "employment") categories = c(11, 12, 104) 
                               
    # Foreign Exchange:                                
    #   "94"    "Daily Rates"                                    
    #   "95"    "Monthly Rates"                                  
    #   "105"   "Trade-Weighted Indexes"                                    
    #   "32145" "Foreign Exchange Intervention"  
    if(listing == "fx") categories = c(94, 95, 105, 32145)
              
    # GDP:
    #   "106"   "GDP/GNP"                                        
    #   "107"   "Gov't Receipts, Expenditures &amp; Investment"  
    #   "108"   "Imports &amp; Exports"                          
    #   "109"   "Industry"                                       
    #   "110"   "Personal Income &amp; Outlays"                  
    #   "21"    "Price Indexes &amp; Deflators"                  
    #   "112"   "Saving &amp; Investment"                        
    if(listing == "gdp") categories = c(106:110, 21, 112)
            
    # Interest:                           
    #   "121"   "Certificates of Deposit"                        
    #   "120"   "Commercial Paper"                               
    #   "119"   "Corporate Aaa and Baa"                        
    #   "118"   "FRB Rates - discount, fed funds, primary credit"
    #   "117"   "Prime Bank Loan Rate"                           
    #   "116"   "Treasury Bills"                                 
    #   "115"   "Treasury Constant Maturity"                     
    #   "82"    "Treasury Inflation-Indexed Securities"          
    #   "114"   "30yr Mortgage"                                  
    #   "113"   "Other"
    if(listing == "interest") categories = c(121:115, 82, 114, 113)  
                                      
    # Monetary:                           
    #   "25"    "M1 and Components"                              
    #   "29"    "M2 and Components"                              
    #   "96"    "M2 Minus Small Time Deposits"                   
    #   "28"    "M3 and Components"                              
    #   "30"    "MZM"                                            
    #   "26"    "Memorandum Items"                               
    #   "52"    "Other"  
    if(listing == "monetary") categories = c(25, 29, 96,28, 30, 26, 52)
    
    # PPI:                                        
    #   "31"    "Producer Price Indexes (PPI)"  
    if(listing == "ppi") categories = PPI = 31
            
    # Reserves:                 
    #   "45"    "Reserves and Monetary Base"                     
    #   "122"   "Borrowings"                                     
    #   "124"   "Monetary Base"                                  
    #   "123"   "Reserves"                                       
    #   "342"   "Other"                                          
    #   "13"    "U.S. Trade &amp; International Transactions"    
    #   "16"    "Exports"                                        
    #   "17"    "Imports"                                        
    #   "3000"  "Income Payments &amp; Receipts"                 
    #   "125"   "Trade Balance"                                  
    #   "127"   "U.S. International Finance"                     
    #   "46"    "U.S. Financial Data"                            
    #   "49"    "Commercial Banking"                             
    #   "32141" "Exchange Rates"                                 
    #   "47"    "Interest Rates"                                 
    #   "48"    "Monetary"                                       
    #   "50"    "Reserves"                                       
    #   "51"    "Discontinued"   
    #   if(listing == "reserve") 

    # Download:
    URL = "http://research.stlouisfed.org/fred2/categories/"
    X = NULL
    for (i in 1:length(categories)) {
        x = readLines(paste(URL, categories[i], sep = ""))
        x = sub("[ \t]+", " ", x)
        # Symbols:
        y = x[grep("/series", x)]
        y = sub(" <a(.*?)>", "", y, perl = TRUE)
        y = sub("</(.*?)a>$", "", y, perl = TRUE)
        # y
        # Description:
        z = x[grep("</td>            $", x)]
        z = z[grep("^ [A-Z]", z)]
        z = sub("</td>            ", "", z)
        z = sub("^ ", "", z)
        # z
        X = rbind(X, cbind(y, z))
    }
    
    NAs = rep(NA, times = NROW(X))
    listing = cbind(X[, 1], NAs, NAs, X[, 2]) 
    colnames(listing) = c("Symbol", "ISIN", "Valor", "Description")
    rownames(listing) = 1:NROW(listing)
    
    # Abbreviate ?
    listing[, 4] = abbreviate(listing[, 4], minlength = abbreviate, 
        use.classes = TRUE, dot = FALSE, method = "left.kept")
    
    # Return Value:
    listing
}
 

################################################################################

     