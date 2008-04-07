
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
#  yahooBriefing         Downloads briefings from Yahoo's Internet site
################################################################################


yahooBriefing <-
    function (query, file = "tempfile", 
    source = "http://finance.yahoo.com/q/ud?s=", save = FALSE, try = TRUE)
{
    # A function implemented by Diethelm Wuertz and Matthew C.Keller

    # Description:
    #   Downloads Key Statistics on shares from Yahoo's Internet site

    # Example:
    #   yahooBriefing("YHOO")
    #   yahooBriefing("IBM")
    #   DEBUG:
    #       query = "IBM"
    #       file = "tempfile"; source = "http://finance.yahoo.com/q/ks?s="
    #       save = FALSE; try = TRUE; method = NULL

    # FUNCTION:

    # Download:
    if (try) {
        # First try if the Internet can be accessed:
        z = try(yahooBriefing(file = file, source = source,
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

        # Download:
        url = paste(source, query, sep = "")
        download.file(url = url, destfile = file, method = method)
        x = scan(file, what = "", sep = "\n")

        # Extract Data Records:
        x = x[grep("Briefing.com", x)]

        x = gsub("</", "<", x, perl = TRUE)
        x = gsub("/", " / ", x, perl = TRUE)
        x = gsub(" class=.yfnc_tabledata1.", "", x, perl = TRUE)
        x = gsub(" align=.center.", "", x, perl = TRUE)
        x = gsub(" cell.......=...", "", x, perl = TRUE)
        x = gsub(" border=...", "", x, perl = TRUE)
        x = gsub(" color=.red.", "", x, perl = TRUE)
        x = gsub(" color=.green.", "", x, perl = TRUE)
        x = gsub("<.>", "", x, perl = TRUE)
        x = gsub("<td>", "@", x, perl = TRUE)
        x = gsub("<..>", "", x, perl = TRUE)
        x = gsub("<...>", "", x, perl = TRUE)
        x = gsub("<....>", "", x, perl = TRUE)
        x = gsub("<table>", "", x, perl = TRUE)
        x = gsub("<td nowrap", "", x, perl = TRUE)
        x = gsub("<td height=....", "", x, perl = TRUE)
        x = gsub("&amp;", "&", x, perl = TRUE)

        x = unlist(strsplit(x, ">"))

        x = x[ grep("-...-[90]", x, perl = TRUE) ]
        nX = length(x)
        # The last record has an additional @, remove it ...
        x[nX] = gsub("@$", "", x[nX], perl = TRUE)
        x = unlist(strsplit(x, "@"))
        x[x == ""] = "NA"
        x = matrix(x, byrow = TRUE, ncol = 9)[, -c(2,4,6,8)]
        x[, 1] = as.character(strptime(x[, 1], format = "%d-%b-%y"))
        colnames(x) = c("Date", "ResearchFirm", "Action", "From", "To")
        x = x[nrow(x):1, ]
        X = as.data.frame(x)
    }

    # Return Value:
    X
}


################################################################################

