
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
#  dbImport              Downloads market data from finance.google.com
#  dbSeries              Easy to use download from www.oanda.com
################################################################################
  

.dbImport <-
    function(query, file = "tempfile", 
    frequency = "auto", 
    from = NULL, to = Sys.timeDate(), nDaysBack = NULL,
    save = FALSE, sep = ";", try = TRUE)          
{
    # A function implemented by Diethelm Wuertz 

    # Description:
    #   Downloads market data from boerse-frankfurt.com

    # Arguments:
    #   query - ISIN code number of asset
    #   file - 
    #   frequency - 
    #   from, to, nDaysback -
    #   save, sep, try - 

    # Example:
    #   .dbImport("CH0009980894")

    # FUNCTION:

    # Settings:
    stopifnot(length(query) == 1)
    
    # Check - Overwrite:
    frequency == "daily"

    # Source:
    source = "http://boerse-frankfurt.com/ServerDataPump/dbpi/ifbTest.srf?"
    
    # URL:
    URL = paste(source, "module=In_HistData", "&count=999", "&wp=", query,
        "&wpbpl=ETR", 
        "&navpath=http://boerse-frankfurt.com/pip/dispatch/de/pip/private_investors/home", 
        sep = "")
        
    # Download:
    if (try) {
        # Try for Internet Connection:
        z = try(.dbImport(query = query, file = file, frequency = frequency,
            save = save, sep = sep, try = FALSE))
        if (inherits(z, "try-error") || inherits(z, "Error")) {
            return("No Internet Access")
        } else {
            return(z)
        }
    } else {     
        # Download:
        download.file(URL, destfile = "tempfile")
        x = scan("tempfile", what = "", sep = "\n")
        n = grep("content_small", x); x = x[n]
        n = grep("td align", x); x = x[n]  
        x = sub("</td", "", x)
        x = sub(",", ".", x)
        x = matrix(unlist(strsplit(x, ">")), byrow = TRUE, ncol = 2)[,2]
        x = matrix(x, byrow = TRUE, ncol = 4)
        x = x[-1, ] 
        # timeSeries Input:
        charvec = paste(substr(x[, 1], 7, 10), substr(x[, 1], 4, 5), 
            substr(x[, 1], 1, 2), sep = "-") 
        data = matrix(as.numeric(x[, 2:4]), ncol = 3)
        data[, 3] = 1000*data[, 3]
        # Data Frame:
        df = data.frame(charvec, data)
        colnames(df) = c("%Y-%m-%d",query, "Volume", "Turnover")
    }
    
    # Save in file?
    if (save) {
        # Header:
        write.table(paste("%Y-%m-%d", query, sep = sep), file, 
            quote = FALSE, row.names = FALSE, col.names = FALSE)
        # Data:
        write.table(df, file, quote = FALSE, append = TRUE, 
            col.names = FALSE, sep = sep)
    } else {
        unlink(file)
    }
    
    # Result:
    ans = new("fWEBDATA",     
        call = match.call(),
        param = c(
            "Symbol" = query,
            "Frequency" = frequency,
            "Start" = as.character(charvec[1]),
            "End" = as.character(charvec[length(charvec)]),
            "Format" = "%Y-%m-%d"),
        data = df, 
        title = "Data Import from boerse-frankfurt.com", 
        description = description() )

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------
  
    
.dbSeries <- 
    function(symbols, from = NULL, to = Sys.timeDate(), nDaysBack = 366, ...) 
{
    # A function implemented by Diethelm Wuertz 
    
    # Description:
    #   Easy to use download from www.oanda.com
    
    # Examples:
    #   Funds Portfolio:
    #   .dbSeries("DE0009769554") # cominvest Adireth
    #   .dbSeries("LU0094488615") # GS High Yield Euro
    #   .dbSeries("LU0066902890") # HSBC Indian Equity
    #   .dbSeries("LU0123357419") # Invesco Energy
    #   .dbSeries("IE0003561788") # Invesco High Income
    #   .dbSeries("LU0072462426") # Merrill Global Allocation
    #   .dbSeries("LU0075056555") # Merrill Global Mining
    #   .dbSeries("LU0072463663") # Merrill Latin America
    #   .dbSeries("LU0055631609") # Merrill World Gold
    #   .dbSeries("LU0148538712") # Pictet Pacific
    #   .dbSeries("LU0130729220") # Pictet Emerging Markets
    #   .dbSeries("LU0052750758") # Templeton China
    #   .dbSeries("LU0078277505") # Templeton Eastern Europe
    #   .dbSeries("LU0029871042") # Templeton Global Bond
    #   .dbSeries("IE0000805634") # Baring Eastern Europe
    #   .dbSeries("FR0010149302") # Carmignac Emergents
    #   .dbSeries("LU0105925696") # WestLB European Convergence 

    # FUNCTION:
    
    # Settings:
    query = symbols
    
    # Download:
    Y = .dbImport(query = query[1], ...)@data
    X = as.timeSeries(Y)
    N = length(query)
    if (N > 1) {
        for (i in 2:N){
            Y = .dbImport(query = query[i], ...)@data
            X = cbind(X, as.timeSeries(Y))
        }
    }    
    # colnames(X)<-query
    
    # Time Window:
    if (is.null(from)) from = to - nDaysBack*24*3600
    X = window(X, from, to)
        
    # Return Value:
    X  
}
   
    
# ------------------------------------------------------------------------------ 
    
    
.dbInvest <- 
    function(ISIN)
{     
    # Examples:
    #   Funds Portfolio:
    #   .dbInvest("LU0072462426") # ok
    #   .dbInvest("LU0075056555") # ok
    #   .dbInvest("LU0055631609") # ok
    #   .dbInvest("LU0123357419") # ok
    #   .dbInvest("IE0000805634") # ok
    #   .dbInvest("LU0066902890") # ok
    #   .dbInvest("LU0072463663") # ok
    #   .dbInvest("LU0103015565") # ok
    
    # FUNCTION:
    
    # URL:
    URL = paste(
        " -url \"boerse-frankfurt.com/pip/dispatch/en/pip/private_investors/",
        "home?module=InOverview_Equi&wp=", ISIN,
        "&foldertype=_Equi&wplist=", ISIN, 
        "&active=overview&wpbpl=&navpath=",
        "http://boerse-frankfurt.com/pip/dispatch/en/pip/private_investors/home\"",
        sep = "")
    
    # Open Browser:
    system(paste('"c:/Program Files/Mozilla Firefox/firefox.exe"', URL),
         wait = FALSE)
         
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------ 


.dbXTracker <- 
    function()
{
    DBXTRACKER = matrix(c(
        "DAX® ETF", "176",
        "DBLCI – OY BALANCED ETF ", "221",
        "DJ EURO STOXX 50® SHORT ETF", "223",
        "DJ EURO STOXX SELECT DIVIDEND® 30 ETF", "209",
        "DJ EURO STOXX50® ETF", "179",
        "DJ STOXX® 600 BANKS ETF", "214",
        "DJ STOXX® 600 BASIC RESOURCES ETF", "211",
        "DJ STOXX® 600 FOOD & BEVERAGE ETF", "219",
        "DJ STOXX® 600 HEALTH CARE ETF ", "213",
        "DJ STOXX® 600 INDUSTRIAL GOODS ETF", "220",
        "DJ STOXX® 600 INSURANCE ETF", "218",
        "DJ STOXX® 600 OIL & GAS ETF", "212",
        "DJ STOXX® 600 TECHNOLOGY ETF", "216",
        "DJ STOXX® 600 TELECOM ETF", "215",
        "DJ STOXX® 600 UTILITIES ETF", "217",
        "DJ STOXX® GLOBAL SELECT DIVIDEND 100 ETF", "210",
        "EONIA TOTAL RETURN INDEX ETF ", "234",
        "FTSE 100 ETF", "196",
        "FTSE 250 ETF ", "198",
        "FTSE ALL-SHARE ETF", "199",
        "FTSE/XINHUA CHINA 25 ETF", "208",
        "IBOXX € INFLATION-LINKED TR-INDEX  ETF", "233",
        "IBOXX € SOVEREIGNS €-ZONE 10-15 TR-NDEX ETF", "229",
        "IBOXX € SOVEREIGNS €-ZONE 1-3 TR-INDEX ETF", "225",
        "IBOXX € SOVEREIGNS €-ZONE 15+ TR-INDEX ETF", "230",
        "IBOXX € SOVEREIGNS €-ZONE 25+ TR-INDEX ETF", "231",
        "IBOXX € SOVEREIGNS €-ZONE 3-5 TR-INDEX ETF", "226",
        "IBOXX € SOVEREIGNS €-ZONE 5-7 TR-INDEX ETF", "227",
        "IBOXX € SOVEREIGNS €-ZONE 7-10 TR-INDEX ETF", "228",
        "IBOXX € SOVEREIGNS €-ZONE TR-INDEX ETF", "224",
        "IBOXX GLOBAL INFLATION-LINKED TR-INDEX HEDGED ETF", "232",
        "ITRAXX CROSSOVER 5-YEAR TOTAL RETURN INDEX ETF ", "237",
        "ITRAXX EUROPE 5-YEAR TOTAL RETURN INDEX ETF ", "235",
        "ITRAXX HIVOL 5-YEAR TOTAL RETURN INDEX ETF ", "236",
        "MSCI BRAZIL TRN INDEX ETF ", "205",
        "MSCI EM ASIA TRN INDEX ETF", "201",
        "MSCI EM EMEA TRN INDEX ETF", "203",
        "MSCI EM LATAM  TRN INDEX ETF", "202",
        "MSCI EMERGING MARKETS  TRN INDEX ETF", "200",
        "MSCI EUROPE TRN INDEX ETF", "184",
        "MSCI JAPAN TRN INDEX ETF", "183",
        "MSCI TAIWAN TRN INDEX ETF", "204",
        "MSCI USA TRN INDEX ETF", "182",
        "MSCI WORLD TRN INDEX ETF", "185",
        "S&P/MIB INDEX ETF", "178",
        "SHORTDAX® ETF", "222",
        "SMI® ETF", "177"), byrow = TRUE, ncol = 2)
    DBXTRACKER = DBXTRACKER[, c(2,1)]
    
    URL= "http://www.dbxtrackers.de/DE/showpage.asp?pageid=143&inrnr=151&pkpnr=179"
    download.file(URL, destfile = "tempfile", method = "wget")
    x = scan("tempfile", what = "", sep = "\n")
}


################################################################################

