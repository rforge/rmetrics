

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


################################################################################
# INTERNAL:                     DESCRIPTION:
#  .getYahooData                 Downloads time series data from yahoo finance
################################################################################


.getYahooData <-
function(
    symbol, 
    start = format(Sys.Date()-366, format = "%Y%m%d"),
    end = format(Sys.Date(), format = "%Y%m%d"), 
    freq = c("daily", "weekly", "monthly"),
    type = c("price", "split"), 
    adjust = TRUE,
    quiet = TRUE) 
{
    # A copy of a function implemented by Josh Ulrich
    
    # Description:
    #   Downloads time series data from yahoo finance
    
    # Arguments:
    #   symbol - character, instrument symbol
    #   start - numeric, starting date, in ISO-8601 format as ccyymmdd,
    #       default is series' first date)
    #   end - numeric, ending date, in ISO-8601 format as ccyymmdd,
    #       default is today
    #   freq - character, frequency of data
    #       either 'daily', 'weekly', 'monthly'
    #   type - character, either 'price' or 'split'
    #   adjust - ogical, adjusts the Open, High, Low, and Close prices 
    #       for dividends and splits, and adjusts Volume for dividends.
    
    # References:
    #   http://help.yahoo.com/l/us/yahoo/finance/quotes
    #   http://help.yahoo.com/l/us/yahoo/finance/quotes/quote-12.html
    #   http://ichart.finance.yahoo.com/x?s=MSFT&g=d&y=0&z=30000
    
    # Examples:
    #   ans = .getYahooData("IBM"); colnames(ans)
    #         "Date"  "Open" "High" "Low"    "Close"   "Volume" 
    #         "Unadj.Close"  "Div"  "Split"  "Adj.Div"  
    #   ans = .getYahooData("IBM", type = "split"); colnames(ans)
    #         "Date" "Div" "Split" "Adj.Div"
    
    # Note:
    #   DW:
    #       This is Josh's function "getYahooData()" for udjusted prices and 
    #       dividends+splits. We rename his function to ".getYahooData()" 
    #       leave it untouched, and use the wrappers around it ...
    #   JU:
    #       Thank you to Giorgio Beltrame for the URL to download dividends 
    #       _and_ splits, and for his correct adjustment procedure.
    #       Many thanks to Ion Georgiadis for helpful suggestions and testing.
    #   DW:
    #       Modifications:
    #           function renamed to .getYahooData
    #           description added
    #           examples added
    #           argument matching added
    #           attribute "update" added
    #           code shortened to max 80 characters per line
    
    # Author:
    #   Package: TTR
    #   Type: Package
    #   Title: Technical Trading Rules
    #   Version: 0.14-0
    #   Date: 2008-01-23
    #   Author: Josh Ulrich
    #   Maintainer: Josh Ulrich <josh.m.ulrich@gmail.com>
    #   Enhances: quantmod
    #   Description: Functions and data to construct technical trading rules  
    #   License: GPL-3 
    
    # FUNCTION:

    # Match Arguments:
    freq = match.arg(freq)
    type = match.arg(type)
    
    # Check Dates:
    if (missing(start)) {
        beg <- as.POSIXlt( "1900-01-01" )
    } else {
        beg <- as.POSIXlt( as.Date( as.character(start), "%Y%m%d" ) )
    }
    if (missing(end)) {
        end <- as.POSIXlt(Sys.Date())
    } else {
        end <- as.POSIXlt( as.Date( as.character( end ), "%Y%m%d" ) )
    }

    if( beg > end ) stop("Start date must be before end date.")
    if( beg > as.POSIXlt(Sys.Date()) ) stop("Start date is after today's date.")

    # Get freqeucy and type parameters
    freq <- match.arg( freq, c("daily","weekly","monthly") )
    type <- match.arg( type, c("price","split") )
    
    
    if(type == "price") {
        freq.url <- substr(freq,1,1)
    } else {
        freq.url <- "v"
        if(freq!="daily" & !quiet) 
            message("Only freq=\"daily\" data available for type=\"split\".\n",
                "Setting freq=\"daily\"...")
    }
    
    flush.console()

    if(type == "price") {

        if(adjust) {
    
          if(freq == "daily") {
    
            # Get price, dividend, and split data from 'beg' to present
            ohlc   <- .getYahooData(symbol, start, freq="daily", type = "price",
                adjust = FALSE, quiet = TRUE)
            divspl <- .getYahooData(symbol, start, freq="daily", type = "split",
                adjust = FALSE, quiet = TRUE)
            ohlc   <- merge(ohlc, divspl, by.col="Date", all = TRUE)
    
            # Create split adjustment ratio, (always = 1 if no splits exist)
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
    
            # Un-adjust dividends for Splits
            ohlc$Div <- ohlc$Adj.Div * ( 1 / s.ratio )
    
            # Create dividend adjustment ratio, 
            #   (always = 1 if no dividends exist)
            d.ratio <- rep(1, NROW(ohlc))
            if( !all( is.na(ohlc$Adj.Div) ) ) {
                # Start loop at most recent data
                for( i in NROW(ohlc):2 ) {
                    if( is.na( ohlc$Adj.Div[i] ) ) {
                        d.ratio[i-1] <- d.ratio[i]
                    } else {
                        d.ratio[i-1] <- d.ratio[i] * ( 1 - ohlc$Div[i] / 
                            ohlc$Close[i-1] ) 
                    } 
                }
            }
    
            # Adjust OHLC and volume
            ohlc$Unadj.Close <- ohlc$Close
            ohlc$Open   <- ohlc$Open  * d.ratio * s.ratio
            ohlc$High   <- ohlc$High  * d.ratio * s.ratio
            ohlc$Low    <- ohlc$Low   * d.ratio * s.ratio
            ohlc$Close  <- ohlc$Close * d.ratio * s.ratio
            ohlc$Volume <- ohlc$Volume * ( 1 / d.ratio )
    
            # Order columns
            ohlc <- ohlc[,c("Date","Open","High","Low","Close","Volume",
                            "Unadj.Close","Div","Split","Adj.Div")]
    
        } else {
            stop("Only freq=\"daily\" adjusted data is currently supported.")
        }
    
        # For other frequencies, get daily data and use a routine to
        # aggregate to desired frequency.
    
        } else {
    
            # Construct URL for 'beg' to 'end'
            url <- paste( 
                "http://ichart.finance.yahoo.com/table.csv?s=", symbol,
                "&a=", beg$mon, 
                "&b=", beg$mday, 
                "&c=", beg$year+1900,
                "&d=", end$mon, "&e=", end$mday, 
                "&f=", end$year+1900,
                "&g=", freq.url, "&ignore=.csv", sep="" )
        
            # Fetch data:
            ohlc <- read.table(url, header=TRUE, sep=",")
            ohlc$Date <- as.Date(as.character(ohlc$Date), "%Y-%m-%d")
            ohlc$Adj.Close <- NULL
    
        }

    } else {

        if(!quiet) 
            message("Unadjusted and adjusted dividend data are always returned.")

        # Construct URL for 'beg' to 'end'
        url <- paste( "http://ichart.finance.yahoo.com/x?s=", symbol,
                    "&a=", beg$mon, "&b=", beg$mday, "&c=", beg$year+1900,
                    "&d=", end$mon, "&e=", end$mday, "&f=", end$year+1900,
                    "&g=", freq.url, "&y=0&z=30000", sep="" )

        # Fetch data
        ohlc <- read.table(url, skip=1, sep=",", fill=TRUE, as.is=TRUE)
        div  <- data.frame( Date=   ohlc$V2[ohlc$V1=="DIVIDEND"],
                          Adj.Div=as.numeric(ohlc$V3[ohlc$V1=="DIVIDEND"]),
                          stringsAsFactors=FALSE )
        spl  <- data.frame( Date=   ohlc$V2[ohlc$V1=="SPLIT"],
                          Split=as.character(ohlc$V3[ohlc$V1=="SPLIT"]),
                          stringsAsFactors=FALSE )

        ohlc <- merge(div, spl, by.col="Date", all=TRUE)
        ohlc$Date <- as.Date(as.character(ohlc$Date), "%Y%m%d")

        # Create split adjustment ratio, (always = 1 if no splits exist)
        s.ratio <- rep(1, NROW(ohlc))
        if(NROW(spl)!=0) {
            ohlc$Split <- sub(":","/", ohlc$Split)
            ohlc$Split <- 1 / sapply( parse( text=ohlc$Split ), eval )
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

        # Order data columns
        ohlc <- ohlc[,c("Date","Div","Split","Adj.Div")]

        # Return (empty) data
        if(NROW(ohlc)==0) return(ohlc)
    }

    # Order Dates, and only return requested data (drop 'end' to present)
    ohlc <- ohlc[order(ohlc$Date),]
    row.names(ohlc) <- 1:NROW(ohlc)
    ohlc <- ohlc[ ( ohlc$Date >= as.Date(beg) & ohlc$Date <= as.Date(end) ), ]

    ### Check to see if supplied dates occur in data set
    if( max(ohlc$Date) != as.Date(end) ) {
        if(!quiet) message("End date out of range, "  , max(ohlc$Date), 
            " is last available date.")
    }
    if( min(ohlc$Date) != as.Date(beg) ) {
        if(!quiet) message("Start date out of range, ", min(ohlc$Date), 
            " is first available date.")
    }
       
    # Add Attributes:
    attr(ohlc, "update") <- format(Sys.time())

    # Return Value:
    return(ohlc)
}


################################################################################

