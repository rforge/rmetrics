
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
# FINCTIONS:                     WORLD FACTBOOK FROM CIA:
#  ciaCountries                   Returns a list of CIA country codes
#  print.ciaCountries             S3 print method for 'ciaIndicators'
#  ciaIndicators                  Returns a list of CIA indicator codes
#  print.ciaIndicators            S3 print method for 'ciaIndicators'
#  ciaByCountry                   Returns all Indicators by country 
#  ciaByIndicator                 Returns for all countries indicator ranking
# FUNCTIONS:                     FOR RUNIT TESTING:
#  .ciaByCountry.RUnit            R Unit test for 'ciaByCountry' 
#  .ciaByIndicator.RUnit          R Unit test for 'ciaByIndicator' 
# FUNCTIONS:                     FOR INTERNAL USE, ONLY - DO NOT RUN:
#  .createFactbook                Creates CIA Data for use with Rmetrics
#  .createIndicators              List of indicator for use with  Rmetrics
################################################################################

     
ciaCountries = 
function()
{   # A Function Implemented by Diethelm Wuertz

    # Description:
    #   Returns a list of CIA indicators
    
    # Arguments:
    #   - none -
    
    # FUNCTION:
    
    # Countries - Don't change long country names:
    ans = matrix(c(
     "af", ".af", "AF", "Afghanistan",
     "al", ".al", "AL", "Albania",
     "ag", ".dz", "DZ", "Algeria",
     "aq", ".as", "AS", "American Samoa",
     "an", ".ad", "AD", "Andorra",
     "ao", ".ao", "AO", "Angola",
     "av", ".ai", "AI", "Anguilla",
     "ay", ".aq", "AQ", "Antarctica",
     "ac", ".ag", "AG", "Antigua and Barbuda",
     "ar", ".ar", "AR", "Argentina",
     "am", ".am", "AM", "Armenia",
     "aa", ".aw", "AW", "Aruba",
     "as", ".au", "AU", "Australia",
     "au", ".at", "AT", "Austria",
     "aj", ".az", "AZ", "Azerbaijan",
     "bf", ".bs", "BS", "Bahamas, The",
     "ba", ".bh", "BH", "Bahrain",
     "bg", ".bd", "BD", "Bangladesh",
     "bb", ".bb", "BB", "Barbados",
     "bo", ".by", "BY", "Belarus",
     "be", ".be", "BE", "Belgium",
     "bh", ".bz", "BZ", "Belize",
     "bn", ".bj", "BJ", "Benin",
     "bd", ".bm", "BM", "Bermuda",
     "bt", ".bt", "BT", "Bhutan",
     "bl", ".bo", "BO", "Bolivia",
     "bk", ".ba", "BA", "Bosnia and Herzegovina",
     "bc", ".bw", "BW", "Botswana",
     "bv", ".bv", "BV", "Bouvet Island",
     "br", ".br", "BR", "Brazil",
     "io", ".io", "IO", "British Indian Ocean Territory",
     "vi", ".vg", "VG", "British Virgin Islands",
     "bx", ".bn", "BN", "Brunei",
     "bu", ".bg", "BG", "Bulgaria",
     "uv", ".bf", "BF", "Burkina Faso",
     "bm", ".mm", "MM", "Burma",
     "by", ".bi", "BI", "Burundi",
     "cb", ".kh", "KH", "Cambodia",
     "cm", ".cm", "CM", "Cameroon",
     "ca", ".ca", "CA", "Canada",
     "cv", ".cv", "CV", "Cape Verde",
     "cj", ".ky", "KY", "Cayman Islands",
     "ct", ".cf", "CF", "Central African Republic",
     "cd", ".td", "TD", "Chad",
     "ci", ".cl", "CL", "Chile",
     "ch", ".cn", "CN", "China",
     "kt", ".cx", "CX", "Christmas Island",
     "ck", ".cc", "CC", "Cocos (Keeling) Islands",
     "co", ".co", "CO", "Colombia",
     "cn", ".km", "KM", "Comoros",
     "cg", ".cd", "CG", "Congo, Democratic Republic of the",
     "cf", ".cg", "CD", "Congo, Republic of the",
     "cw", ".ck", "CK", "Cook Islands",
     "cs", ".cr", "CR", "Costa Rica",
     "iv", ".ci", "CI", "Cote d'Ivoire",
     "hr", ".hr", "HR", "Croatia",
     "cu", ".cu", "CU", "Cuba",
     "cy", ".cy", "CY", "Cyprus",
     "ez", ".cz", "CZ", "Czech Republic",
     "da", ".dk", "DK", "Denmark",
     "dj", ".dj", "DJ", "Djibouti",
     "do", ".dm", "DM", "Dominica",
     "dr", ".do", "DO", "Dominican Republic",
     "tt", ".tl,", "TL", "East Timor",
     "ec", ".ec", "EC", "Ecuador",
     "eg", ".eg", "EG", "Egypt",
     "es", ".sv", "SV", "El Salvador",
     "ek", ".gq", "GQ", "Equatorial Guinea",
     "er", ".er", "ER", "Eritrea",
     "en", ".ee", "EE", "Estonia",
     "et", ".et", "ET", "Ethiopia",
     "ee", ".eu", "EU", "European Union",
     "fk", ".fk", "FK", "Falkland Islands (Islas Malvinas)",
     "fo", ".fo", "FO", "Faroe Islands",
     "fj", ".fj", "FJ", "Fiji",
     "fi", ".fi", "FI", "Finland",
     "fr", ".fr", "FR", "France",
     "fg", ".gf", "GF", "French Guiana",
     "fp", ".pf", "PF", "French Polynesia",
     "fs", ".tf", "TF", "French Southern and Antarctic Lands",
     "gb", ".ga", "GA", "Gabon",
     "ga", ".gm", "GM", "Gambia, The",
     "gz", ".ps", "PS", "Gaza Strip",
     "gg", ".ge", "GE", "Georgia",
     "gm", ".de", "DE", "Germany",
     "gh", ".gh", "GH", "Ghana",
     "gi", ".gi", "GI", "Gibraltar",
     "gr", ".gr", "GR", "Greece",
     "gl", ".gl", "GL", "Greenland",
     "gj", ".gd", "GD", "Grenada",
     "gp", ".gp", "GP", "Guadeloupe",
     "gq", ".gu", "GU", "Guam",
     "gt", ".gt", "GT", "Guatemala",
     "gk", ".gg", "GG", "Guernsey",
     "gv", ".gn", "GN", "Guinea",
     "pu", ".gw", "GW", "Guinea-Bissau",
     "gy", ".gy", "GY", "Guyana",
     "ha", ".ht", "HT", "Haiti",
     "hm", ".hm", "HM", "Heard Island and McDonald Islands",
     "vt", ".va", "VA", "Holy See (Vatican City)",
     "ho", ".hn", "HN", "Honduras",
     "hk", ".hk", "HK", "Hong Kong",
     "hu", ".hu", "HU", "Hungary",
     "ic", ".is", "IS", "Iceland",
     "in", ".in", "IN", "India",
     "id", ".id", "ID", "Indonesia",
     "ir", ".ir", "IR", "Iran",
     "iz", ".iq", "IQ", "Iraq",
     "ei", ".ie", "IE", "Ireland",
     "is", ".il", "IL", "Israel",
     "it", ".it", "IT", "Italy",
     "jm", ".jm", "JM", "Jamaica",
     "ja", ".jp", "JP", "Japan",
     "je", ".je", "JE", "Jersey",
     "jo", ".jo", "JO", "Jordan",
     "kz", ".kz", "KZ", "Kazakhstan",
     "ke", ".ke", "KE", "Kenya",
     "kr", ".ki", "KI", "Kiribati",
     "kn", ".kp", "KP", "Korea, North",
     "ks", ".kr", "KR", "Korea, South",
     "ku", ".kw", "KW", "Kuwait",
     "kg", ".kg", "KG", "Kyrgyzstan",
     "la", ".la", "LA", "Laos",
     "lg", ".lv", "LV", "Latvia",
     "le", ".lb", "LB", "Lebanon",
     "lt", ".ls", "LS", "Lesotho",
     "li", ".lr", "LR", "Liberia",
     "ly", ".ly", "LY", "Libya",
     "ls", ".li", "LI", "Liechtenstein",
     "lh", ".lt", "LT", "Lithuania",
     "lu", ".lu", "LU", "Luxembourg",
     "mc", ".mo", "MO", "Macau",
     "mk", ".mk", "MK", "Macedonia",
     "ma", ".mg", "MG", "Madagascar",
     "mi", ".mw", "MW", "Malawi",
     "my", ".my", "MY", "Malaysia",
     "mv", ".mv", "MV", "Maldives",
     "ml", ".ml", "ML", "Mali",
     "mt", ".mt", "MT", "Malta",
     "im", ".im", "IM", "Man, Isle of",
     "rm", ".mh", "MH", "Marshall Islands",
     "mb", ".mq", "MQ", "Martinique",
     "mr", ".mr", "MR", "Mauritania",
     "mp", ".mu", "MU", "Mauritius",
     "mf", ".yt", "YT", "Mayotte",
     "mx", ".mx", "MX", "Mexico",
     "fm", ".fm", "FM", "Micronesia, Federated States of",
     "md", ".md", "MD", "Moldova",
     "mn", ".mc", "MC", "Monaco",
     "mg", ".mn", "MN", "Mongolia",
     "mh", ".ms", "MS", "Montserrat",
     "mo", ".ma", "MA", "Morocco",
     "mz", ".mz", "MZ", "Mozambique",
     "wa", ".na", "NA", "Namibia",
     "nr", ".nr", "NR", "Nauru",
     "np", ".np", "NP", "Nepal",
     "nl", ".nl", "NL", "Netherlands",
     "nt", ".an", "AN", "Netherlands Antilles",
     "nc", ".nc", "NC", "New Caledonia",
     "nz", ".nz", "NZ", "New Zealand",
     "nu", ".ni", "NI", "Nicaragua",
     "ng", ".ne", "NE", "Niger",
     "ni", ".ng", "NG", "Nigeria",
     "ne", ".nu", "NU", "Niue",
     "nf", ".nf", "NF", "Norfolk Island",
     "cq", ".mp", "MP", "Northern Mariana Islands",
     "no", ".no", "NO", "Norway",
     "mu", ".om", "OM", "Oman",
     "pk", ".pk", "PK", "Pakistan",
     "ps", ".pw", "PW", "Palau",
     "pm", ".pa", "PA", "Panama",
     "pp", ".pg", "PG", "Papua New Guinea",
     "pa", ".py", "PY", "Paraguay",
     "pe", ".pe", "PE", "Peru",
     "rp", ".ph", "PH", "Philippines",
     "pc", ".pn", "PN", "Pitcairn Islands",
     "pl", ".pl", "PL", "Poland",
     "po", ".pt", "PT", "Portugal",
     "rq", ".pr", "PR", "Puerto Rico",
     "qa", ".qa", "QA", "Qatar",
     "re", ".re", "RE", "Reunion",
     "ro", ".ro", "RO", "Romania",
     "rs", ".ru", "RU", "Russia",
     "rw", ".rw", "RW", "Rwanda",
     "sh", ".sh", "SH", "Saint Helena",
     "sc", ".kn", "KN", "Saint Kitts and Nevis",
     "st", ".lc", "LC", "Saint Lucia",
     "sb", ".pm", "PM", "Saint Pierre and Miquelon",
     "vc", ".vc", "VC", "Saint Vincent and the Grenadines",
     "ws", ".ws", "WS", "Samoa",
     "sm", ".sm", "SM", "San Marino",
     "tp", ".st", "ST", "Sao Tome and Principe",
     "sa", ".sa", "SA", "Saudi Arabia",
     "sg", ".sn", "SN", "Senegal",
     "yi", ".cs", "CS", "Serbia and Montenegro",
     "se", ".sc", "SC", "Seychelles",
     "sl", ".sl", "SL", "Sierra Leone",
     "sn", ".sg", "SG", "Singapore",
     "lo", ".sk", "SK", "Slovakia",
     "si", ".si", "SI", "Slovenia",
     "bp", ".sb", "SB", "Solomon Islands",
     "so", ".so", "SO", "Somalia",
     "sf", ".za", "ZA", "South Africa",
     "sx", ".gs", "GS", "South Georgia and the South Sandwich Islands",
     "sp", ".es", "ES", "Spain",
     "ce", ".lk", "LK", "Sri Lanka",
     "su", ".sd", "SD", "Sudan",
     "ns", ".sr", "SR", "Suriname",
     "sv", ".sj", "SJ", "Svalbard",
     "wz", ".sz", "SZ", "Swaziland",
     "sw", ".se", "SE", "Sweden",
     "sz", ".ch", "CH", "Switzerland",
     "sy", ".sy", "SY", "Syria",
     "tw", ".tw", "TW", "Taiwan",
     "ti", ".tj", "TJ", "Tajikistan",
     "tz", ".tz", "TZ", "Tanzania",
     "th", ".th", "TH", "Thailand",
     "to", ".tg", "TG", "Togo",
     "tl", ".tk", "TK", "Tokelau",
     "tn", ".to", "TO", "Tonga",
     "td", ".tt", "TT", "Trinidad and Tobago",
     "ts", ".tn", "TN", "Tunisia",
     "tu", ".tr", "TR", "Turkey",
     "tx", ".tm", "TM", "Turkmenistan",
     "tk", ".tc", "TC", "Turks and Caicos Islands",
     "tv", ".tv", "TV", "Tuvalu",
     "ug", ".ug", "UG", "Uganda",
     "up", ".ua", "UA", "Ukraine",
     "ae", ".ae", "AE", "United Arab Emirates",
     "uk", ".uk", "GB", "United Kingdom",
     "us", ".us", "US", "United States",
     "uy", ".uy", "UY", "Uruguay",
     "uz", ".uz", "UZ", "Uzbekistan",
     "nh", ".vu", "VU", "Vanuatu",
     "ve", ".ve", "VE", "Venezuela",
     "vm", ".vn", "VN", "Vietnam",
     "vq", ".vi", "VG", "Virgin Islands",
     "wf", ".wf", "WF", "Wallis and Futuna",
     "we", ".ps", "PS", "West Bank",
     "wi", ".eh", "EH", "Western Sahara",
     "ym", ".ye", "YE", "Yemen",
     "za", ".zm", "ZM", "Zambia",
     "zi", ".zw", "ZW", "Zimbabwe"), byrow = TRUE, ncol = 4)
     colnames(ans) = c("Factbook", "Internet", "Country", "Name")
     
     # Set Class:
     class(ans) = c("ciaCountries", "matrix")
     
     # Return Value:
     ans      
}


# ------------------------------------------------------------------------------


print.ciaCountries =
function(x, ...)
{   # A Function Implemented by Diethelm Wuertz

    # Description:
    #   Print Method
    
    # Arguments:
    #   x - an S3 object of class 'ciaCountries'
    
    # FUNCTION:
    
    # Print:
    .Countries = ciaCountries()
    ans = data.frame(.Countries[,c(4,3)])
    colnames(ans) = c("Country", "Code")
    print(ans)
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


ciaIndicators =
function()
{   # A Function Implemented by Diethelm Wuertz

    # Description:
    #   Returns a list of CIA indicators
    
    # Arguments:
    #   - none -
    
    # FUNCTION:
    
    # These data were created by the function '.creteIndicators'
    ans = matrix(c(
    "01", "2001",                                             "GDP",
    "02", "2003",                       "GDP - real growth rate(%)",
    "03", "2004",                                "GDP - per capita",
    "04", "2034",       "Military expenditures - percent of GDP(%)",
    "05", "2038",                   "Electricity - production(kWh)",
    "06", "2042",                  "Electricity - consumption(kWh)",
    "07", "2053",                                        "Airports",
    "08", "2054",             "Birth rate(births/1,000 population)",
    "09", "2066",             "Death rate(deaths/1,000 population)",
    "10", "2067",           "Military expenditures - dollar figure",
    "11", "2078",                                         "Exports",
    "12", "2079",                                 "Debt - external",
    "13", "2085",                                    "Highways(km)",
    "14", "2087",                                         "Imports",
    "15", "2089",            "Industrial production growth rate(%)",
    "16", "2091", "Infant mortality rate(deaths/1,000 live births)",
    "17", "2092",             "Inflation rate (consumer prices)(%)",
    "18", "2093",                                   "Waterways(km)",
    "19", "2095",                                     "Labor force",
    "20", "2102",                 "Life expectancy at birth(years)",
    "21", "2108",                                 "Merchant marine",
    "22", "2119",                                      "Population",
    "23", "2121",                                    "Railways(km)",
    "24", "2127",       "Total fertility rate(children born/woman)",
    "25", "2129",                            "Unemployment rate(%)",
    "26", "2147",                                     "Area(sq km)",
    "27", "2150",                  "Telephones - main lines in use",
    "28", "2151",                    "Telephones - mobile cellular",
    "29", "2153",                                  "Internet users",
    "30", "2155",             "HIV/AIDS - adult prevalence rate(%)",
    "31", "2156",          "HIV/AIDS - people living with HIV/AIDS",
    "32", "2157",                               "HIV/AIDS - deaths",
    "33", "2173",                       "Oil - production(bbl/day)",
    "34", "2174",                      "Oil - consumption(bbl/day)",
    "35", "2175",                          "Oil - imports(bbl/day)",
    "36", "2176",                          "Oil - exports(bbl/day)",
    "37", "2178",                      "Oil - proved reserves(bbl)",
    "38", "2179",             "Natural gas - proved reserves(cu m)",
    "39", "2180",                  "Natural gas - production(cu m)",
    "40", "2181",                 "Natural gas - consumption(cu m)",
    "41", "2182",                     "Natural gas - imports(cu m)",
    "42", "2183",                     "Natural gas - exports(cu m)",
    "43", "2184",                                  "Internet hosts",
    "44", "2185",              "Investment (gross fixed)(% of GDP)",
    "45", "2186",                           "Public debt(% of GDP)",
    "46", "2187",                         "Current account balance",
    "47", "2188",           "Reserves of foreign exchange and gold"),
    byrow = TRUE, ncol = 3)[, -1]
    colnames(ans) = c("Code", "Indicator")
    
    
    # Set Class:
    class(ans) = c("ciaIndicators", "matrix")
     
    # Return Value:
    ans 
}


# ------------------------------------------------------------------------------


print.ciaIndicators =
function(x, ...)
{   # A Function Implemented by Diethelm Wuertz

    # Description:
    #   Print Method
    
    # Arguments:
    #   x - an S3 object of class 'ciaIndicators'
    
    # FUNCTION:
    
    # Print:
    .Indicators = ciaIndicators()
    ans = data.frame(.Indicators)
    colnames(ans) = c("Code", "Indicator")
    print(ans)
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


ciaByCountry = 
function(code = "CH", from = FALSE, names = FALSE, details = FALSE)
{   # A Function Implemented by Diethelm Wuertz

    # Description:
    #   Print Method
    
    # Arguments:
    #   code - ISO Country Code
    #   from - a logical Flag, if TRUE then the date of
    #       data registration will be given
    #   names - a logical flag, if TRUE then long indicator
    #       names willl be a added
    #   details - a logical flag, if TRUE then details will 
    #       be printed
    
    # FUNCTION:
    
    # Load data if not already done ...
    if (!exists("ciaFactbook")) data(ciaFactbook)
    
    # Country List:
    .Countries = ciaCountries()
    country = .Countries[,4][.Countries[,3] == code]
    names(country) = NULL
    if (details) cat(paste("\n",country, 
        ": CIA Factbook 2005\n", sep = ""), "\n")
    
    # Factbook Data:
    .Factbook = as.matrix(ciaFactbook)
    ### DW: Country = paste(" ", country, " ", sep = "")
    Country = country
    ### DW
    TEST = { .Factbook[, 3] == Country}
    Z = gsub(", ", "", .Factbook[TEST, ])
    Z = gsub(" ", "", Z)
    Z = matrix(Z, ncol = 5)
    Z[, 5] = gsub("\\.", "", Z[, 5])
    Z = data.frame(Z[, -3])
    colnames(Z) = c("Code", "Rank", "Value", "From")
    
    # Indicators:
    .Indicators = ciaIndicators()
    ans = matrix(rep(NA, dim(.Indicators)[1]*4), ncol = 4)
    ans[, 1] = .Indicators[, 1]

    # Make complete:
    Counter = 1:47
    names(Counter) = .Indicators[,1]
    ans[Counter[as.vector(Z[,1])], ] = as.matrix(Z)
    ans = cbind(ans, .Indicators[,2])
    colnames(ans) = c("Code", "Rank", "Value", "From", "Indicator")
    if (!from & names) ans = ans[, -4]
    if (from & !names) ans = ans[, -5]
    if (!from & !names) ans = ans[, -(4:5)]
    
    # Return Value:
    data.frame(ans)
}


# ------------------------------------------------------------------------------


.ciaByCountry.RUnit =
function()
{   # A Function Implemented by Diethelm Wuertz

    # Description:
    #   Unit Testing
    
    # Arguments:
    #   - none -
    
    # FUNCTION:
    
    # Print:
    print(ciaByCountry(code = "CH"))
    print(ciaByCountry(code = "CH", from = TRUE))
    print(ciaByCountry(code = "CH", names = TRUE))
    print(ciaByCountry(code = "CH", from = TRUE, names = TRUE))
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


ciaByIndicator = 
function(code = 2001, from = FALSE, details = FALSE)
{   # A Function Implemented by Diethelm Wuertz

    # Description:
    #   Load CIA Data by Indicator
    
    # Arguments:
    #   code - CIA Indicator Code
    #   from - a logical Flag, if TRUE then the date of
    #       data registration will be given
    #   details - a logical flag, if TRUE then details will 
    #       be printed
    
    # FUNCTION:
    
    # Load data if not already done ...
    if (!exists("ciaFactbook")) data(ciaFactbook)
    
    # Indicator Name:
    Code = as.character(code)
    .Indicators = ciaIndicators()
    Indicator = (.Indicators[.Indicators[, 1] == Code])[2]
    if (details) 
        cat(paste("\nCode ", Code, ": ", Indicator, sep = ""), "\n")
    
    # Factbook Data:
    .Factbook = as.matrix(ciaFactbook)
    TEST = { .Factbook[, 1] == Code}
    Z = .Factbook[TEST, ]
    Z = data.frame(Z[, -(1:2)])
    colnames(Z) = c("Country", "Value", "From")
    if (!from) Z = Z[, -3]
    
    # Return Value:
    data.frame(Z)
}


# ------------------------------------------------------------------------------


.ciaByIndicator.RUnit =
function()
{   # A Function Implemented by Diethelm Wuertz

    # Description:
    #   R Unit Testing
    
    # Arguments:
    #   - none -
    
    # FUNCTION:
    
    # Print
    print(ciaByIndicator(code = "2001"))
    
    # Return Value:
    invisible()
}

    
################################################################################

    
.createFactbook = 
function()
{   # A Function Implemented by Diethelm Wuertz

    # Description:
    #   Create the CIA data for use with  Rmetrics
    
    # Arguments:
    #   - none -
    
    # Note:
    #   For internal use only.
    
    # FUNCTION:
    
    # First copy the directory 'rankorder' to R's
    # working directory, then continue ...
    Files = dir("rankorder")
    
    # Compose Data Matrix:
    S = NULL
    for (file in Files) {
        code = substring(file, 1, 4)
        x = scan(paste("rankorder/", file, sep = ""), skip = 2, 
            sep = "\n", what = character(0), quiet = TRUE)
        x = x[-length(x)]
        N = length(x)
        Code = rep(code, length = N)
        s = matrix(unlist(strsplit(x, "\t")), byrow = TRUE, nrow = N)
        s = cbind(Code, s)
        S = rbind(S, s)
    }
    S[, 4] = gsub("\\$", ", ", S[, 4]) 
    S[, 4] = gsub(",", "", S[, 4]) 
    S[, 5] = gsub("\\.", "", S[, 5]) 
    S[, 5] = gsub(" ", "", S[, 5]) 
    ans = data.frame(S)
    colnames(ans) = c("Code", "Rank", "Country", "Value", "From")
    
    # Dump Data:
    factbook = ans
    dump("factbook", "factbook.R")
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------
    
    
.createIndicators = 
function()
{   # A Function Implemented by Diethelm Wuertz

    # Description:
    #   Create the CIA list of indicator for use with  Rmetrics
    
    # Arguments:
    #   - none -
    
    # Note:
    #   For internal use only.
    
    # FUNCTION:
    
    # First copy the directory 'rankorder' to R's
    # working directory, then continue ...
    Files = dir("rankorder")
    
    # Codes and Indicators:
    Codes = Names = NULL
    for (file in Files) {
        Codes = c(Codes, as.integer(substring(file, 1, 4)) )
        x = scan(paste("rankorder/", file, sep = ""), skip = 1, 
            sep = "\n", what = character(0), quiet = TRUE, n = 2)
        Name = strsplit(x, "\t")[[1]][3]
        Name = substring(Name, 2, nchar(Name)-1)
        Names = c(Names, Name)
    }
    Codes = data.frame(Codes, Names)
    colnames(Codes) = c("Code", "Indicator")
    
    # Copy the Return Value to the function ciaIndicators ...
    
    # Return Value:
    Codes
}


################################################################################

