
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
#  yahooKeystats         Downloads key statistics from Yahoo's web site
################################################################################


yahooKeystats <-
    function (query, file = "tempfile", 
    source = "http://finance.yahoo.com/q/ks?s=", 
    save = FALSE, try = TRUE)
{
    # A function implemented by Diethelm Wuertz and Matthew C.Keller

    # Description:
    #   Downloads Key Statistics on shares from Yahoo's Internet site

    # Example:
    #   yahooKeystats("YHOO")
    #   yahooKeystats("IBM")
    #   DEBUG:
    #       query = "IBM"
    #       file = "tempfile"; source = "http://finance.yahoo.com/q/ks?s="
    #       save = FALSE; try = TRUE; method = NULL

    # Changes:
    #   2006-08-26 update by MCK

    # FUNCTION:

    # Download:
    if (try) {
        # First try if the Internet can be accessed:
        z = try(yahooKeystats(file = file, source = source,
            query = query, save = save, try = FALSE))
        if (class(z) == "try-error" || class(z) == "Error") {
            return("No Internet Access")
        }
        else {
            return(z)
        }
    } else {
        # For S-Plus Compatibility:
        if (class(version) != "Sversion") {
            method = NULL
        } else {
            method = "wget"
        }

        # Download and Scan:
        url = paste(source, query, sep = "")
        download.file(url = url, destfile = file, method = method)
        x = scan(file, what = "", sep = "\n")

        # Extract Data Records:
        x = x[grep("datamodoutline1", x)]

        # Clean up HTML:
        x = gsub("/", "", x, perl = TRUE)
        x = gsub(" class=.yfnc_datamodoutline1.", "", x, perl = TRUE)
        x = gsub(" colspan=.2.", "", x, perl = TRUE)
        x = gsub(" cell.......=...", "", x, perl = TRUE)
        x = gsub(" border=...", "", x, perl = TRUE)
        x = gsub(" class=.yfnc_tablehead1.", "", x, perl = TRUE)
        x = gsub(" class=.yfnc_tabledata1.", "", x, perl = TRUE)
        x = gsub(" width=.75%.>", "", x, perl = TRUE)
        x = gsub(" width=.100%.", "", x, perl = TRUE)
        x = gsub(" size=.-1.", "", x, perl = TRUE)
        x = gsub("<.>", "", x, perl = TRUE)
        x = gsub("<..>", "", x, perl = TRUE)
        x = gsub("<....>", "", x, perl = TRUE)
        x = gsub("<table>", "", x, perl = TRUE)
        x = gsub("<sup>.<sup>", "", x, perl = TRUE)
        x = gsub("&amp;", "&", x, perl = TRUE)
        x = gsub("<td", " @ ", x, perl = TRUE)
        x = gsub(",", "", x, perl = TRUE)

        # Create Matrix:
        x = unlist(strsplit(x, "@" ))
        x = x[ grep(":", x) ]
        x = gsub("^ ", "", x, perl = TRUE)
        Index = grep("^ ", x)
        if (length(Index) > 0) x = x[-Index]
        x = gsub(" $", "", x, perl = TRUE)
        x = gsub(":$", ":NA", x, perl = TRUE)

        # If there are two ":" in a line ...
        x = sub(":", "@", x)
        x = sub(":", "/", x)

        # Convert to matrix:
        x = matrix(unlist(strsplit(x, "@" )), byrow = TRUE, ncol = 2)

        # Add Current Date:
        stats = as.character(Sys.Date())
        x = rbind(c("Symbol", query), c("Date", stats), x)
        X = as.data.frame(x[, 2])
        rownames(X) = x[, 1]
        colnames(X) = "Value"
    }

    # Return Value:
    X
}


################################################################################

