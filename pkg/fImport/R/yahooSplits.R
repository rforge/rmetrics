
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
#  yahooSplits
################################################################################


# *** DW: UNDER CONSTRUCTION ***


.yahooSplits <-
function(symbol, start, end) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    
    # Example:
    #   yahooSplits("IBM", timeDate("1962-10-01"), "2007-10-30")
    
    # FUNCTION:
    
    # Check dates
    beg <- as.POSIXlt(as.timeDate(start))
    end <- as.POSIXlt(as.timeDate(end))
    
    # Construct URL for 'beg' to 'end'
    url <- paste( "http://ichart.finance.yahoo.com/x?s=", symbol,
        "&a=", beg$mon, "&b=", beg$mday, "&c=", beg$year+1900,
        "&d=", end$mon, "&e=", end$mday, "&f=", end$year+1900,
        "&g=", "v", "&y=0&z=30000", sep="" )

    # Fetch data
    ohlc <- read.table(url, skip = 1, sep = ",", fill = TRUE, as.is = TRUE)
    div  <- data.frame( Date = ohlc$V2[ohlc$V1 =="DIVIDEND"],
        Adj.Div = as.numeric(ohlc$V3[ohlc$V1 == "DIVIDEND"]),
        stringsAsFactors = FALSE )
    spl  <- data.frame( Date = ohlc$V2[ohlc$V1 == "SPLIT"],
        Split = as.character(ohlc$V3[ohlc$V1 == "SPLIT"]),
        stringsAsFactors = FALSE )
    ohlc <- merge(div, spl, by.col = "Date", all = TRUE)
    ohlc$Date <- as.Date(as.character(ohlc$Date), "%Y%m%d")

    # Create split adjustment ratio, (always = 1 if no splits exist)
    s.ratio <- rep(1, NROW(ohlc))
    if(NROW(spl)!=0) {
        ohlc$Split <- sub(":","/", ohlc$Split)
        ohlc$Split <- 1 / sapply( parse( text = ohlc$Split ), eval)
        # Start loop at most recent data
        for( i in NROW(ohlc):2 ) {  
            if( is.na( ohlc$Split[i] ) ) {
                s.ratio[i-1] <- s.ratio[i]
            } else {
                s.ratio[i-1] <- s.ratio[i] * ohlc$Split[i]
            } 
        }
    }

    # Un-adjust dividends for Splits
    ohlc$Div <- ohlc$Adj.Div * ( 1 / s.ratio )
    ohlc$Split <- as.numeric(ohlc$Split)

    # Return (empty) data
    if(NROW(ohlc)==0) return(NA)
       
    # Convert to timeSeries:
    timeSeries(data = ohlc[, -1], charvec = ohlc[, 1])
}


################################################################################

