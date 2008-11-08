
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


################################################################################
# FUNCTION:             DESCRIPTION:
#  economagicImport      Downloads market data from www.economagic.com
#  economagicSeries      Easy to use download from www.economagic.com
################################################################################


economagicImport <-
    function(query, file = "tempfile", source = NULL,
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
        z = try(economagicImport(query, file, source, frequency, from, to,
            nDaysBack, save, sep, try = FALSE))
        if (inherits(z, "try-error") || inherits(z, "Error")) {
            return("No Internet Access")
        } else {
            return(z)
        }
    } else {
        # Download File:
        url = paste(source, query, sep = "")
        tmp = tempfile()
        download.file(url = url, destfile = tmp)
        SCAN = scan(tmp, what = "", sep = "\n")

        # Extract all the data records and build the table:
        lines = grep("font color=white", SCAN)

        # Skipping of the first three lines should be improved ...
        ### YC
        z1 <<- SCAN[lines]

        ### YC
        # Remove irrelevant HTML markup strings
        z1 <- sub('^[[:space:]]', '', z1)
        z1 <- gsub('<a href=.*</a>', '', z1)
        z1 <- gsub('<font color=white>.{0,3}</font[[:space:]]*>', ' ', z1)
        z1 <- gsub('[[:space:]]+', ' ', z1)
        z1 <- gsub('&#149;', '.', z1)

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

        ### YC - added more columns of data if available
        z <-
            switch(freq,
                   # Create the dates in ISO-8601 format:
                   # For quarterly data multiplay quarters by 3 to
                   # get monthly base
                   "quarterly"={cbind(z[,1]*100+3*z[,2],z[,-(1:2)])},
                   "monthly"  ={cbind(z[,1]*100+z[,2], z[,-(1:2)])},
                   "daily"    ={cbind(z[,1]*10000+z[,2]*100+z[,3],z[,-(1:3)])})

        # DW - add hyphens:
        ### YC - added more columns of data if available
        data = z[, -1]
        charvec = as.character(z[, 1])
        if (freq == "daily") {
            charvec = paste(
                substring(charvec, 1, 4), "-",
                substring(charvec, 5, 6), "-",
                substring(charvec, 7, 8), sep = "")
        } else {
            charvec = paste(
                substring(charvec, 1, 4), "-",
                substring(charvec, 5, 6), "-01", sep = "")
        }
        frequency = freq
        X = timeSeries(data, charvec)
        colnames(X) <- c(query, colnames(X[,-1]))
    }

    # Save to file:
    if (save) {
        write.table(as.data.frame(X))
    } else {
        unlink(file)
    }

    # Result:
    ans = new("fWEBDATA",
        call = match.call(),
        param = c(
            "Instrument" = query,
            "Frequency " = frequency),
        data = X,
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
    #   symbols - a character vector of symbol names
    #   from - from date
    #   to - to date
    #   nDaysBack - number of n-days back
    #   ... - arguments passed to the *Import()

    # Examples:
    #    economagicSeries("fedny/day-fxus2eu")[1:10,]           # daily
    #    economagicSeries("fedstl/fedfunds+2")                  # monthly
    #    economagicSeries("fedstl/gnp")                         # quarterly
    #    economagicSeries(c("fedny/day-fxus2eu", "fedny/day-fxch2us"))[1:10, ]

    # FUNCTION:

    # Download:
    X = economagicImport(query = symbols[1], ...)@data
    N = length(symbols)
    if (N > 1) {
        for (i in 2:N){
            X = merge(X, economagicImport(query = symbols[i], ...)@data)
        }
    }

    # Time Window:
    if (is.null(from)) from = to - nDaysBack*24*3600
    X = window(X, from, to)

    # Return Value:
    X
}


################################################################################

