
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
#  adjYahooImport        Downloads market data from finance.yahoo.com
#  adjYahooSeries        Easy to use download from finance.yahoo.com
################################################################################


.adjYahooImport <-
    function(
        symbol = "IBM", 
        start = timeDate("2007-11-01"), 
        end = "2007-11-30",
        type = "price", 
        adjust = TRUE) 
{
    # Check dates
    beg <- as.POSIXlt(as.timeDate(start))
    end <- as.POSIXlt(as.timeDate(end))
 
    # Get freqeucy and type parameters
    freq <- "daily"
    type <- "price"
    adjust = TRUE

    # Download
    if(type == "price") { 
        
        if(adjust) {
            # adjusted:
            # Get price, dividend, and split data from 'beg' to present
            ohlc   <- .adjYahooSeries(symbol, start, end, type = "price", !adjust)
            divspl <- .adjYahooSeries(symbol, start, end, type = "split", !adjust)
            ohlc   <- merge(ohlc, divspl, by.col = "Date", all = TRUE)
            # Create split adjustment ratio, always 1 if no splits exist
            s.ratio <- rep(1, NROW(ohlc))
            if( !all( is.na(ohlc$Split) ) ) {
                # Start loop at most recent data
                for( i in NROW(ohlc):2 ) {
                    if( is.na( ohlc$Split[i] ) ) {
                        s.ratio[i-1] <- s.ratio[i]
                    } else {
                        s.ratio[i-1] <- s.ratio[i] * ohlc$Split[i]
                    } 
                }
            }
            # Un-adjusted dividends for Splits
            ohlc$Div <- ohlc$Adj.Div * ( 1 / s.ratio )
            # Create dividend adjustment ratio, always 1 if no dividends exist
            d.ratio <- rep(1, NROW(ohlc))
            if( !all( is.na(ohlc$Adj.Div) ) ) {
                # Start loop at most recent data
                for( i in NROW(ohlc):2 ) {
                    if( is.na( ohlc$Adj.Div[i] ) ) {
                        d.ratio[i-1] <- d.ratio[i]
                    } else {
                        d.ratio[i-1] <- d.ratio[i] * ( 1 - ohlc$Div[i] / ohlc$Close[i-1] ) 
                    } 
                }
            }
            # Adjust OHLC and Volume
            ohlc$Unadj.Close <- ohlc$Close
            ohlc$Open   <- ohlc$Open  * d.ratio * s.ratio
            ohlc$High   <- ohlc$High  * d.ratio * s.ratio
            ohlc$Low    <- ohlc$Low   * d.ratio * s.ratio
            ohlc$Close  <- ohlc$Close * d.ratio * s.ratio
            ohlc$Volume <- ohlc$Volume * ( 1 / d.ratio )
            # Order Columns:
            ohlc <- ohlc[, c(
                "Date", "Open", "High", "Low", "Close", "Volume",
                "Unadj.Close", "Div", "Split", "Adj.Div")]
    
        } else {
            # Non- adjusted:
            
            # Construct URL for 'beg' to 'end'
            url <- paste( "http://ichart.finance.yahoo.com/table.csv?s=", symbol,
                      "&a=", beg$mon, "&b=", beg$mday, "&c=", beg$year+1900,
                      "&d=", end$mon, "&e=", end$mday, "&f=", end$year+1900,
                      "&g=", "d", "&ignore=.csv", sep = "" )
    
            # Fetch data:
            ohlc <- read.table(url, header = TRUE, sep = ",")
            ohlc$Date <- as.Date(as.character(ohlc$Date), "%Y-%m-%d")
            ohlc$Adj.Close <- NULL
    
        } # END ADJUST
    
    } else {
  
        # Construct URL for 'beg' to 'end'
        url <- paste( "http://ichart.finance.yahoo.com/x?s=", symbol,
            "&a=", beg$mon, "&b=", beg$mday, "&c=", beg$year+1900,
            "&d=", end$mon, "&e=", end$mday, "&f=", end$year+1900,
            "&g=", "v", "&y=0&z=30000", sep = "")
        # Fetch Data:
        ohlc <- read.table(url, skip = 1, sep=",", fill = TRUE, as.is = TRUE)
        div  <- data.frame( Date=   ohlc$V2[ohlc$V1=="DIVIDEND"],
            Adj.Div=as.numeric(ohlc$V3[ohlc$V1 == "DIVIDEND"]),
            stringsAsFactors = FALSE )
        spl  <- data.frame( Date=   ohlc$V2[ohlc$V1=="SPLIT"],
            Split=as.character(ohlc$V3[ohlc$V1 == "SPLIT"]),
            stringsAsFactors = FALSE )
        ohlc <- merge(div, spl, by.col="Date", all = TRUE)
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
        # Un-adjusted dividends for Splits
        ohlc$Div <- ohlc$Adj.Div * ( 1 / s.ratio )
        ohlc$Split <- as.numeric(ohlc$Split)

        # Order data columns
        ohlc <- ohlc[,c("Date","Div","Split","Adj.Div")]

        # Return (empty) data
        if(NROW(ohlc)==0) return(ohlc)
    }
    # Order Dates, and only return requested data (drop 'end' to present)
    ohlc <- ohlc[order(ohlc$Date),]
    row.names(ohlc) <- 1:NROW(ohlc)
    ohlc <- ohlc[ ( ohlc$Date >= as.Date(beg) & ohlc$Date <= as.Date(end) ), ] 
    # Check to see if supplied dates occur in data set
    if( max(ohlc$Date) != as.Date(end) ) {
        message("End date out of range, "  , max(ohlc$Date), 
            " is last available date.")
    }
    if( min(ohlc$Date) != as.Date(beg) ) {
        message("Start date out of range, ", min(ohlc$Date), 
            " is first available date.")
    }

    # Return Value:
    return(ohlc)
}


# ------------------------------------------------------------------------------


.adjYahooSeries <-
    function(symbols, from, to, nDaysBack, ...) 
{
    NA
}


################################################################################

