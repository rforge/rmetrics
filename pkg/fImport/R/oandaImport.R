
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
#  oandaImport           Downloads market data from www.oanda.com
#  oandaSeries           Easy to use download from www.oanda.com
################################################################################


.oandaImport <-  
    function(query, file = "tempfile", 
    source = "http://www.oanda.com/convert/fxhistory?lang=en&", 
    frequency = "daily", save = FALSE, sep = ";", try = TRUE) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Downloads market data from www.oanda.com
    
    # Value:
    #   An One Column data frame with row names denoting the dates
    #   given in the POSIX format "%Y%m%d". The column lists the
    #   downloaded data records.      
  
    # FUNCTION:
    
    # Check:
    stopifnot (frequency == "daily")
    
    # Download:
    if (try) {
        # Try for Internet Connection:
        z = try(.oandaImport(query = query, file = file, source = source, 
            save = save, try = FALSE))
        if (inherits(z, "try-error") || inherits(z, "Error")) {
            return("No Internet Access") 
        } else {
            return(z) 
        } 
    } else { 
        # Loop over all Queries:
        for(i in 1:length(query)) {
            to = trunc(to, "days")
            if (is.null(from)) from = to - 1000*24*3600
            from.date <- format(timeDate(from),
                "date1=%m%%2F%d%%2F%y&")
            to.date <- format(timeDate(to),
                "date=%m%%2F%d%%2F%y&date_fmt=us&")
            ccy.pair <- strsplit(toupper(query[[i]]),"/")[[1]]
            download.file(
                paste(source, from.date, to.date, "exch=", ccy.pair[1], 
                    "&expr2=", ccy.pair[2], "&margin_fixed=0&SUBMIT=Get+Table&",
                    "format=CSV&redirected=1", sep = ""), 
                destfile = file)
            ### fx <- readLines(tmp)   
            fx <- unlist(strsplit(
                gsub("<PRE>|</PRE>", "", 
                fx[(grep("PRE",fx)[1]):(grep("PRE",fx)[2])]),","))
            fx = matrix(fx, byrow = TRUE, ncol = 2)
            fx <- timeSeries(data = as.numeric(fx[, 2]), charvec = fx[, 1],
                format = "%m/%d/%Y")
            if (i == 1) {
                FX = fx  
            } else {
                FX = merge(FX, fx)
            }
        }
        colnames(FX) = query
    }
    
    # Save in file?
    if (save) {
            write.table(paste("%Y%m%d", query, sep = ";"), file, 
                quote = FALSE, row.names = FALSE, col.names = FALSE)
            write.table(z, file, quote = FALSE, append = TRUE, 
                col.names = FALSE, sep = ";")
    }
    else {
        unlink(file)
    }
    
    # Return Value:
    ans = new("fWEBDATA",     
        call = match.call(),
        param = c(
            "Instrument Query" = query,
            "Frequency" = frequency),
        data = FX, 
        title = "Web Data Import from www.oanda.com", 
        description = description())
}


# ------------------------------------------------------------------------------


.oandaSeries <-
    function(symbols, from = NULL, to = Sys.timeDate(), nDaysBack = 366,  ...) 
{ 
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Easy to use download from www.oanda.com
    
    # FUNCTION:
    
    # Settings:
    query = symbols
    
    # URL:
    oanda.URL <- "http://www.oanda.com/convert/fxhistory?lang=en&"
    
    # Loop over all Queries:
    to = trunc(to, "days")
    if (is.null(from)) from = to - nDaysBack*24*3600
    for(i in 1:length(query)) {
          
        from.date <- format(timeDate(from),
            "date1=%m%%2F%d%%2F%y&")
        to.date <- format(timeDate(to),
            "date=%m%%2F%d%%2F%y&date_fmt=us&")
        
        ccy.pair <- strsplit(toupper(query[[i]]),"/")[[1]]
        tmp <- tempfile()
        download.file(
            paste(oanda.URL, from.date, to.date, "exch=", ccy.pair[1], 
                "&expr2=", ccy.pair[2], "&margin_fixed=0&SUBMIT=Get+Table&",
                "format=CSV&redirected=1", sep = ""), 
            destfile = tmp)
        fx <- readLines(tmp)  
             
        fx <- unlist(strsplit(
            gsub("<PRE>|</PRE>", "", 
            fx[(grep("PRE",fx)[1]):(grep("PRE",fx)[2])]),","))
        fx = matrix(fx, byrow = TRUE, ncol = 2)
        fx <- timeSeries(data = as.numeric(fx[, 2]), charvec = fx[, 1],
            format = "%m/%d/%Y")
        
        if (i == 1) {
            FX = fx  
        } else {
            FX = merge(FX, fx)
        }
    }
    colnames(FX) = query
    
    # Time Window:
    if (is.null(from)) from = to - nDaysBack*24*3600
    FX = window(FX, from, to)  
    
    # Return Value:
    FX
}

         
################################################################################

