
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
#  economagicImport      Downloads market data from www.economagic.com
#  economagicSeries      Easy to use download from www.economagic.com
################################################################################


economagicImport <-
    function(query, file = "tempfile",
    frequency = c("auto", "quarterly", "monthly", "daily"),
    from = NULL, to = Sys.timeDate(), nDaysBack = NULL,
    save = FALSE, sep =";", try = TRUE)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Downloads market data from www.economagic.com
    
    # Notes:
    #   Note, only the first column is returned, the remaining are  
    #     usually percentual changes which can be calculated otherwise.

    # Examples:
    #    economagicImport("fedny/day-fxus2eu")              # daily
    #    economagicImport("fedny/day-fxch2us")
    #    economagicImport("fedstl/fedfunds+2")              # monthly
    #    economagicImport("fedstl/gnp")                     # quarterly
    
    # FUNCTION:
    
    # Settings:
    stopifnot(length(query) == 1)
    
    # Source:
    if (is.null(source)) 
        source = "http://www.economagic.com/em-cgi/data.exe/"
    
    # Frequency:
    freq = match.arg(frequency)
              
    # Download:
    if (try) {
        # First try if the Internet can be accessed:
        z = try(economagicImport(query = query, 
            file = file, source = source, frequency = freq, 
            save = save, colname = colname, try = FALSE)) 
        if (inherits(z, "try-error") || inherits(z, "Error")) {
            return("No Internet Access") 
        } else {
            return(z) 
        }
    } else {  
        # Settings:
        # n <- switch(freq, "quarterly" =, "monthly" = 2, "daily" = 3)
       
        # For S-Plus Compatibility:
        method <- if(is.R()) NULL else "lynx"
    
        # Download the file:
        url = paste(source, query, sep = "")
        download.file(url = url, destfile = file, method = method)
        SCAN = scan(file, what = "", sep = "\n")
        
        # Extract all the data records and build the table:
        lines = grep("font color=white", SCAN)
        # Skipping of the first three lines should be improved ... 
        z1 <<- SCAN[lines][-(1:3)]  
        
        # Remove irrelevant HTML markup strings
        z2 = gsub("font", "          ", x = z1)  
        z1 = gsub("color=white........", " ", x = z2)      
        z2 = gsub(">", " ", x = z1) 
        z1 = gsub("<", " ", x = z2) 
        z1 = gsub("/", " ", x = z1) 
        for (i in 1:20) z1 = gsub("  ", " ", x = z1) 
       
        # Next - Compose Matrix: 
        n.rows = length(z1)                         
        z2 = unlist(apply(matrix(z1, ncol = 1), 2, strsplit, split = " "))
        z1 = as.numeric(z2[z2 != ""])
        n.fields = length(z1)     
        z = matrix(z1, byrow = TRUE, ncol = n.fields/n.rows) 
        
        # Try "auto" detect Format:
        if (freq == "auto") {
            # Auto detections works only with daily and monthly formats ...
            # For this we check column No 3.
            test = (length(unique(sort(z[,3]))) < 32)
            if(test) freq = "daily" else freq = "monthly"
            if(length(unique(sort(z[,2]))) == 4) freq = "quarterly"
        }
        
        n <- switch(freq, "quarterly" =, "monthly" = 2, "daily" = 3)
        if (n == 2) z = cbind(z[,1]*100+z[,2], z[,3])
        if (n == 3) z = cbind(z[,1]*10000+z[,2]*100+z[,3], z[,4])
        # Create the dates in ISO-8601 format:
        # For quarterly data multiplay quarters by 3 to get monthly base
        if (freq == "quarterly") z[,1] = 100*(z[,1]%/%100)+3*z[,1]%%100
        z = data.frame(cbind(z[, 1], z[, 2]))
        
        ## znames = as.character(1:(length(names(z)) - 1))
        names(z) = c("DATE", colname)   
        
        # DW - add hyphens:
        rowNames = as.character(z[, 1])
        if (freq == "daily") {
            rowNames = paste(
                substring(rowNames, 1, 4), "-",
                substring(rowNames, 5, 6), "-",
                substring(rowNames, 7, 8), sep = "")
        } else {
            rowNames = paste(
                substring(rowNames, 1, 4), "-",
                substring(rowNames, 5, 6), "-01", sep = "")
        }
        z[, 1] = rowNames   
    }
    
    # Save to file:
    if (save) {
        # Header:
        # ?
        # Data:
        write.table(z, file, quote = FALSE, sep = ";", row.names = FALSE) 
    } else {
        unlink(file) 
    }
    
    # Result:
    new("fWEBDATA",     
        call = match.call(),
        param = c(
            "Instrument Query" = query, 
            "Frequency" = frequency, 
            "Instrument Name" = colname),
        data = z, 
        title = "Web Data Import from www.economagic.com", 
        description = description() )
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


economagicSeries <- 
    function(symbols, from = NULL, to = Sys.timeDate(), nDaysBack = 366, ...)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Downloads easily data www.economagic.com
    
    # Arguments:
    #   query - a character vector of symbol names
    #   from - from date
    #   to - to date
    #   nDaysBack - number of n-days back
    #   ... - arguments passed to the economagicImport
    
    # Examples:
    #    economagicSeries("fedny/day-fxus2eu")                  # daily
    #    economagicSeries("fedny/day-fxch2us")
    #    economagicSeries(c("fedny/day-fxus2eu", "fedny/day-fxch2us"))
    #    economagicSeries("fedstl/fedfunds+2")                  # monthly
    #    economagicSeries("fedstl/gnp")                         # quarterly
    #    economagicSeries("fedstl/gnp", nDaysBack = 10*366)     # yearsBack
    
    # FUNCTION:
    
    # Settings:
    query = symbols
    
    # Download:
    Y = economagicImport(query = query[1], ...)@data
    X = as.timeSeries(Y)
    N = length(query)
    if (N > 1) {
        for (i in 2:N){
            Y = economagicImport(query = query[i], ...)@data
            X = merge(X, as.timeSeries(Y))
        }
    }    
    colnames(X)<-query
    
    # Time Window:
    if (is.null(from)) from = to - nDaysBack*24*3600
    X = window(X, from, to)
        
    # Return Value:
    X  
}


################################################################################
  
