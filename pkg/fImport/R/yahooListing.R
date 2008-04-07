
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
# FUNCTION:                         DESCRIPTION:
#  updateNasdaqYahooListing
#  getNasdaqYahooListing
# FUNCTION:                         DESCRIPTION:
#  updateAmexYahooListing
#  getAmexYahooListing
# FUNCTION:                         DESCRIPTION:
#  updateNyseYahooListing
#  getNyseYahooListing
# FUNCTION:                         DESCRIPTION:
#  updateGermanYahooListing
#  getGermanYahooListing
################################################################################


# *** DW: UNDER CONSTRUCTION ***


# inspired by Author:
#   TTR, copyright (C) Joshua M. Ulrich, 2007 
#   Distributed under GNU GPL version 3 
#
# Thanks:
#   Many thanks to Ion Georgiadis for helpful suggestions and testing.
#   See "NYSE "behind the dot" or Nasdaq 5th-letter codes and other special
#   codes" here:
#   http://en.wikipedia.org/wiki/Ticker_symbol
#   http://help.yahoo.com/l/us/yahoo/finance/quotes/quote-02.html
# 
#   AMEX / NYSE Mappings (NASDAQ doesn't need transformation?):
#   Exchanges -> Yahoo
#   /WS       -> -WT
#   /U        -> -U
#   .[A-Z]    -> NA (special notes/bonds - IG)
#   :[AP]     -> NA (after-hours / pre-market)
#   ^         -> -P
#   /         -> -
#   $         -> NA (NYSE Only)
#   ~         -> NA (NYSE Only)
  

################################################################################
  
    
.nasdaqYahooListing <-
    function() 
{
    # Description:
    
    # FUNCTION:
   
    # Download:    
    url <- "http://www.nasdaq.com/asp/symbols.asp?exchange=Q&start=0"
    table <- read.csv(url, skip = 2, colClasses = c("character", "character", 
        "NULL", "NULL", "character", "NULL"), header = FALSE, as.is = TRUE)
    NAs = rep(NA, NROW(table))
    listing = as.matrix(cbind(table[, 2], NAs, NAs, table[, c(1, 3)]))
    listing = listing[-NROW(table), ]
    listing[, 5] = sub("^.", "", listing[, 5])
    colnames(listing) = c("Symbol", "ISIN", "Valor", "Name", "Cap")
    rownames(listing) = 1:NROW(listing)
    
    # Return Value:
    listing
}


# ------------------------------------------------------------------------------


.amexYahooListing <-
    function() 
{
    # Description:
    
    # FUNCTION:
   
    # Download:  
    url <- "http://www.nasdaq.com/asp/symbols.asp?exchange=1&start=0"
    table <- read.csv(url, skip = 2, colClasses = c("character", 
        "character", "character","NULL"), header = FALSE, as.is = TRUE)
            
    # Compose:    
    NAs = rep(NA, NROW(table))
    listing = as.matrix(cbind(table[, 2], NAs, NAs, table[, c(1, 3)]))
    listing = listing[-NROW(table), ]
    listing[, 5] = sub("^.", "", listing[, 5])
       
    # Transform Symbols to Yahoo format
    listing[, 1] <- gsub("/WS$", "-WT", listing[, 1])   
    listing[, 1] <- gsub("/WS/", "-WT", listing[, 1])   
    listing[, 1] <- gsub("/U",   "-U",  listing[, 1])  
    listing[, 1] <- gsub("\\^",  "-P",  listing[, 1])   
    listing[, 1] <- gsub("/",    "-",   listing[, 1])   
            
    # Drop symbols Yahoo doesn't provide
    drop <- c( 
        grep("\\.", listing[, 1]),   
        grep("\\$", listing[, 1]),    
        grep(":",   listing[, 1]) )   
    if(NROW(drop) != 0) table <- listing[-drop, ]
    
    # Column and Rownames:
    colnames(listing) = c("Symbol", "ISIN", "Valor", "Name", "Cap")
    rownames(listing) = 1:NROW(listing)
    
    # Return Value:
    listing
}


# ------------------------------------------------------------------------------


.nyseYahooListing <-
    function() 
{
    # Description:
    
    # FUNCTION:
   
    # Download    
    url <- "http://www.nasdaq.com/asp/symbols.asp?exchange=N&start=0"
    table <- read.csv(url, skip = 2, colClasses = c("character", 
        "character", "character","NULL"), header = FALSE, as.is = TRUE)

    # Compose:    
    NAs = rep(NA, NROW(table))
    listing = as.matrix(cbind(table[, 2], NAs, NAs, table[, c(1, 3)]))
    listing = listing[-NROW(table), ]
    listing[, 5] = sub("^.", "", listing[, 5])
    
    # Transform Symbols to Yahoo format
    listing[, 1] <- gsub("/WS$", "-WT", listing[, 1])   
    listing[, 1] <- gsub("/WS/", "-WT", listing[, 1])   
    listing[, 1] <- gsub("\\^",  "-P",  listing[, 1])  
    listing[, 1] <- gsub("/",    "-",   listing[, 1])  
    
    # Drop symbols Yahoo doesn't provide
    drop <- c( 
        grep("\\$", listing[,2]),  
        grep(":",   listing[,2]),   
        grep("~",   listing[,2]) )  
    if(NROW(drop)!= 0) listing <- listing[-drop, ]
        
    # Column and Rownames:
    colnames(listing) = c("Symbol", "ISIN", "Valor", "Name", "Cap")
    rownames(listing) = 1:NROW(listing)
     
    # Return Value:
    listing
} 

 
# ------------------------------------------------------------------------------
                    

.germanYahooListing =
    function()
{
    # Descrption
    #   Lists equities traded at the  Deutsche Boerse Frankfurt
    
    # Source:
    #   http://aktienkurs-orderbuch.finanznachrichten.de
    
    # Example:
    #   x = .germanYahooListing()
    
    # FUNCTION:
    
    Y = NULL
    for (l in c("0", letters)) {
        URL = "http://aktienkurs-orderbuch.finanznachrichten.de/aktien-"
        x = readLines(paste(URL, l, ".aspx", sep = ""))
        x = x[grep("background-image", x)]

        y = gsub("^(.*?)href=.", "", x)
        y = gsub("</a(.*?)$", "", y)[-(1:2)]
        y = gsub(".aspx.>", "@", y)
        y = gsub("[ \t]+", " ", y)
        
        Y = c(Y, y)
    }
    
    s = strsplit(Y, "@")
    listing = matrix(unlist(s), byrow = TRUE, ncol = 2)
    NAs = rep(NA, times = NROW(listing))
    listing = cbind(paste(listing[, 1], ".DE", sep = ""), NAs, NAs, listing[, 2])
    colnames(listing) = c("Symbol", "ISIN", "Valor", "Name")
    rownames(listing) = 1:NROW(listing)
    
    # Return Value:
    listing
}


################################################################################

