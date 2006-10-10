#
# Examples from the Monograph:
#   "Rmetrics - Financial Engineering and Computational Finance"
#     written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 1.1
#   Economic and Finanacial Markets
#
# List of Examples, Exercises and Code Snippets:
#
#   1.1.1 Example: List all Countries in the CIA World Factbook
#   1.1.2 Example: Swiss Indicators from the CIA World Factbook
#   1.1.3 Example: Code 2001 - Worldwide GPD from the CIA World Factbook
#   1.1.4 Example: Pie Chart of Oil Production 2004 from the CIA World Factbook
#   1.1.5 Example: Bar Chart - Capitalization of Major Exchanges from WFE
#
# Author:
#   (C) 1997-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################


### Load Packages:

    require(fBasics)
    ###
    
    
# ------------------------------------------------------------------------------


### Example: List all Countries in the CIA World Factbook

    # Show Argument List:
    args(ciaCountries)
    ###
    
    # List Countries and Codes:
    ciaCountries() 
    ###
    
    
# ------------------------------------------------------------------------------

    
### Example: List all Inticators in the CIA World Factbook

    # Show Argument List:
    args(ciaIndicators)
    ###
    
    # List Indicators and their Code Number:
    ciaIndicators() 
    ###
    
    
# ------------------------------------------------------------------------------
    

### Example: Swiss Indicators from the CIA World Factbook

    # Show Argument List:
    args(ciaByCountry)
    # function (code = "CH", from = FALSE, names = FALSE)
    ###
    
    # List Switzerland:
    ciaByCountry("CH")
    #   Code Rank        Value
    # 1 2001   37 251900000000
    # ...
    ###
    
    # Use 'from':
    ciaByCountry("CH", from = TRUE)
    #    Code Rank        Value        From
    # 1  2001   37 251900000000     2004est
    # ...
    ###
    
    # Use 'names':
    ciaByCountry("CH", from = TRUE, names = TRUE)
    #    Code Rank        Value    From Indicator
    # 1  2001   37 251900000000 2004est       GDP
    # ...
    ciaByCountry("CH")
    ###
    
    
# ------------------------------------------------------------------------------

    
### Example: Code 2001 - Worldwide GPD from CIA World Factbook

    # Show Argument List:
    args(ciaByIndicator)
    # function (code = 2001, from = FALSE)
    ###
    
    # List Ranked GDP:
    head(ciaByIndicator("2001"), 5)
    # Code 2001: GDP 
    #           Country                Value 
    # 1           World       55500000000000 
    # 2   United States       11750000000000 
    # 3  European Union       11650000000000 
    # 4           China        7262000000000 
    # 5           Japan        3745000000000 
    ###
    
    # Add Reporting Time/Date:
    ciaByIndicator("2001", from = TRUE)
    ###

    
# ------------------------------------------------------------------------------


### Example: Pie Chart frrom CIA Oil Production from CIA World Factbook

    # Search for Code:
    ciaIndicators()
    # Oil Production - Code 2173: 
    ###
    
    # Pie Chart:
    OilProduction = as.integer(as.vector(
        ciaByIndicator(2173)[2:11, 2]))
    names(OilProduction) = as.vector(ciaByIndicator(2173)[2:11,1])
    OilProduction 
    pie(OilProduction,col = rainbow(10))
    title(main = "Oil Production 2004\n bbl/day")
    mtext("Source: CIA World Factbook", side = 1)
    ###

     
# ------------------------------------------------------------------------------


### Example: Capitilization of Major Stock Markets from WFE
     
    # Extract Capitalization of/at:
    # NYSE: 7, Tokyo: 37, London: 22, Frankfurt: 15
    # 1991 - 2003 triannual: 3, 6, 9, 12, 15
    ###
    
    # Create Table from 'wfe1':
    data(wfe1)
    Table =t(wfe1[c(7,37,22,15),c(3,6,9,12,15)])/1e6
    colnames(Table) = c("NewYork", "Tokyo", "London", "Frankfurt")
    rownames(Table) = as.character(seq(1991, 2003, by = 3))
    Table
    ###
    
    # Create Barplot:
    barplot(Table, beside = TRUE, legend = rownames(Table),
        col = c("lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk"))
    title(main = "Stock Market Capitalization\n 1991 - 2003")
    mtext("Source: World Federation of Exchanges", side = 4, 
        line = -2, cex = 0.7)
    ###
     
     
################################################################################

