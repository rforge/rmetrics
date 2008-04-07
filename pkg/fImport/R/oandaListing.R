
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
#  oandaListing          Lists symbols from www.oanda.com
################################################################################


.oandaListing <-
    function(category = "fx", abbreviate = 60)
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Symbols:
    symbols = c(
    "AFN", "ALL", "DZD", "ADF", "ADP", "AOA", "AON", "ARS", "AMD", "AWG",
    "AUD", "ATS", "AZM", "AZN", "BSD", "BHD", "BDT", "BBD", "BYR", "BEF",
    "BZD", "BMD", "BTN", "BOB", "BAM", "BWP", "BRL", "GBP", "BND", "BGN",
    "BIF", "XOF", "XAF", "XPF", "KHR", "CAD", "CVE", "KYD", "CLP", "CNY",
    "COP", "KMF", "CDF", "CRC", "HRK", "CUC", "CUP", "CYP", "CZK", "DKK",
    "DJF", "DOP", "NLG", "XEU", "XCD", "ECS", "EGP", "SVC", "EEK", "ETB",
    "FKP", "FJD", "FIM", "FRF", "GMD", "GEL", "DEM", "GHC", "GHS", "GIP",
    "XAU", "GRD", "GTQ", "GNF", "GYD", "HTG", "HNL", "HKD", "HUF", "ISK",
    "INR", "IDR", "IRR", "IQD", "IEP", "ILS", "ITL", "JMD", "JPY", "JOD",
    "KZT", "KES", "KWD", "KGS", "LAK", "LVL", "LBP", "LSL", "LRD", "LYD",
    "LTL", "LUF", "MOP", "MKD", "MGA", "MGF", "MWK", "MYR", "MVR", "MTL",
    "MRO", "MUR", "MXN", "MDL", "MNT", "MAD", "MZM", "MZN", "MMK", "ANG",
    "NAD", "NPR", "NZD", "NIO", "NGN", "KPW", "NOK", "OMR", "PKR", "XPD",
    "PAB", "PGK", "PYG", "PEN", "PHP", "XPT", "PLN", "PTE", "QAR", "ROL",
    "RON", "RUB", "RWF", "WST", "STD", "SAR", "RSD", "SCR", "SLL", "XAG",
    "SGD", "SKK", "SIT", "SBD", "SOS", "ZAR", "KRW", "ESP", "LKR", "SHP",
    "SDD", "SDP", "SDG", "SRD", "SRG", "SZL", "SEK", "CHF", "SYP", "TWD",
    "TZS", "THB", "TOP", "TTD", "TND", "TRL", "TRY", "TMM", "USD", "UGX",
    "UAH", "UYU", "AED", "VUV", "VEB", "VEF", "VND", "YER", "YUN", "ZMK",
    "ZWD")

    # Description:
    description = c(
    "Afghanistan Afghani",    "Albanian Lek",         "Algerian Dinar",           
    "Andorran Franc",         "Andorran Peseta",      "Angolan Kwanza",           
    "Angolan New Kwanza",     "Argentine Peso",       "Armenian Dram",            
    "Aruban Florin",          "Australian Dollar",    "Austrian Schilling",       
    "Azerbaijan Manat",       "Azerbaijan New Manat", "Bahamian Dollar",          
    "Bahraini Dinar",         "Bangladeshi Taka",     "Barbados Dollar",          
    "Belarusian Ruble",       "Belgian Franc",        "Belize Dollar",            
    "Bermudian Dollar",       "Bhutan Ngultrum",      "Bolivian Boliviano",       
    "Bosnian Mark",           "Botswana Pula",        "Brazilian Real",           
    "British Pound",          "Brunei Dollar",        "Bulgarian Lev",            
    "Burundi Franc",          "CFA Franc BCEAO",      "CFA Franc BEAC",           
    "CFP Franc",              "Cambodian Riel",       "Canadian Dollar",          
    "Cape Verde Escudo",      "Cayman Islands Dollar","Chilean Peso",             
    "Chinese Yuan Renminbi",  "Colombian Peso",       "Comoros Franc",            
    "Congolese Franc",        "Costa Rican Colon",    "Croatian Kuna",            
    "Cuban Convertible Peso", "Cuban Peso",           "Cyprus Pound",             
    "Czech Koruna",           "Danish Krone",         "Djibouti Franc",           
    "Dominican R. Peso",      "Dutch Guilder",        "ECU",                      
    "East Caribbean Dollar",  "Ecuador Sucre",        "Egyptian Pound",           
    "El Salvador Colon",      "Estonian Kroon",       "Ethiopian Birr",           
    "Falkland Islands Pound", "Fiji Dollar",          "Finnish Markka",           
    "French Franc",           "Gambian Dalasi",       "Georgian Lari",            
    "German Mark",            "Ghanaian Cedi",        "Ghanaian New Cedi",        
    "Gibraltar Pound",        "Gold (oz.)",           "Greek Drachma",            
    "Guatemalan Quetzal",     "Guinea Franc",         "Guyanese Dollar",          
    "Haitian Gourde",         "Honduran Lempira",     "Hong Kong Dollar",         
    "Hungarian Forint",       "Iceland Krona",        "Indian Rupee",             
    "Indonesian Rupiah",      "Iranian Rial",         "Iraqi Dinar",              
    "Irish Punt",             "Israeli New Shekel",   "Italian Lira",             
    "Jamaican Dollar",        "Japanese Yen",         "Jordanian Dinar",          
    "Kazakhstan Tenge",       "Kenyan Shilling",      "Kuwaiti Dinar",            
    "Kyrgyzstanian Som",      "Lao Kip",              "Latvian Lats",             
    "Lebanese Pound",         "Lesotho Loti",         "Liberian Dollar",          
    "Libyan Dinar",           "Lithuanian Litas",     "Luxembourg Franc",         
    "Macau Pataca",           "Macedonian Denar",     "Malagasy Ariary",          
    "Malagasy Franc",         "Malawi Kwacha",        "Malaysian Ringgit",        
    "Maldive Rufiyaa",        "Maltese Lira",         "Mauritanian Ouguiya",      
    "Mauritius Rupee",        "Mexican Peso",         "Moldovan Leu",             
    "Mongolian Tugrik",       "Moroccan Dirham",      "Mozambique Metical",       
    "Mozambique New Metical", "Myanmar Kyat",         "NL Antillian Guilder",     
    "Namibia Dollar",         "Nepalese Rupee",       "New Zealand Dollar",       
    "Nicaraguan Cordoba Oro", "Nigerian Naira",       "North Korean Won",         
    "Norwegian Kroner",       "Omani Rial",           "Pakistan Rupee",           
    "Palladium (oz.)",        "Panamanian Balboa",    "Papua New Guinea Kina",    
    "Paraguay Guarani",       "Peruvian Nuevo Sol",   "Philippine Peso",          
    "Platinum (oz.)",         "Polish Zloty",         "Portuguese Escudo",        
    "Qatari Rial",            "Romanian Lei",         "Romanian New Lei",         
    "Russian Rouble",         "Rwandan Franc",        "Samoan Tala",              
    "Sao Tome/Principe Dobra","Saudi Riyal",          "Serbian Dinar",            
    "Seychelles Rupee",       "Sierra Leone Leone",   "Silver (oz.)",             
    "Singapore Dollar",       "Slovak Koruna",        "Slovenian Tolar",          
    "Solomon Islands Dollar", "Somali Shilling",      "South African Rand",       
    "South-Korean Won",       "Spanish Peseta",       "Sri Lanka Rupee",          
    "St. Helena Pound",       "Sudanese Dinar",       "Sudanese Old Pound",       
    "Sudanese Pound",         "Suriname Dollar",      "Suriname Guilder",         
    "Swaziland Lilangeni",    "Swedish Krona",        "Swiss Franc",              
    "Syrian Pound",           "Taiwan Dollar",        "Tanzanian Shilling",       
    "Thai Baht",              "Tonga Pa'anga",        "Trinidad/Tobago Dollar",   
    "Tunisian Dinar",         "Turkish Lira",         "Turkish New Lira",         
    "Turkmenistan Manat",     "US Dollar",            "Uganda Shilling",          
    "Ukraine Hryvnia",        "Uruguayan Peso",       "Utd. Arab Emir. Dirham",   
    "Vanuatu Vatu",           "Venezuelan Bolivar",   "Venezuelan Bolivar Fuerte",
    "Vietnamese Dong",        "Yemeni Rial",          "Yugoslav Dinar",           
    "Zambian Kwacha",         "Zimbabwe Dollar")
    
    # Compose:
    listing = cbind(symbols, description)
    NAs = rep(NA, times = NROW(listing))
    listing = cbind(listing[,1], NAs, NAs, listing[, 2])
    colnames(listing) = c("Symbol", "ISIN", "Valor", "Description")
    rownames(listing) = 1:NROW(listing)
       
    # Abbreviate ?
    listing[,2] = abbreviate(listing[, 2], minlength = abbreviate, 
        use.classes = TRUE, dot = FALSE, method = "left.kept")
        
    # Attrubutes:
    attr(listing, "control") <- c(catgory = category, symbols = NCOL(listing))
    
    # Return Value:
    listing    
} 
 
            
################################################################################

