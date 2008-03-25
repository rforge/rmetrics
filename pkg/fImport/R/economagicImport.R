
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
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             DESCRIPTION:
#  economagicImport      Downloads market data from EconoMagic's web site
#  economagicSeries      Easy to use download from EconoMagic
################################################################################


################################################################################
# FUNCTION:             DESCRIPTION:
#  economagicImport      Downloads market data from EconoMagic's web site
#  economagicSeries      Easy to use download from EconoMagic


economagicImport <-
    function (query, file = "tempfile",
    source = "http://www.economagic.com/em-cgi/data.exe/",
    frequency = c("quarterly", "monthly", "daily"),
    save = FALSE, colname = "VALUE", try = TRUE)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Downloads market data from EconoMagic's web site

    # Notes:
    #   Note, only the first column is returned, the remaining are
    #     usually percentual changes which can be calculated otherwise.

    # Examples:
    #   USDEUR Foreign Exchange Rate:
    #    economagicImport("fedny/day-fxus2eu", "USDEUR.CSV",
    #       frequency = "daily", colname = "USDEUR")
    #   USFEDFUNDS US FedFunds Rate:
    #    economagicImport("fedstl/fedfunds+2", "USFEDFUNDS.CSV",
    #       frequency = "monthly", colname = "USFEDFUNDS")
    #   USDGNP:
    #    economagicImport("fedstl/gnp", "USGNP.CSV",
    #       frequency = "monthly", colname = "USGNP")
    # FUNCTION:

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
    n <- switch(freq,
            "quarterly" =, "monthly" = 2,
            "daily" = 3)

        # For S-Plus Compatibility:
        method <- if(is.R()) NULL else "lynx"

        # Download the file:
        url = paste(source, query, sep = "")
        download.file(url = url, destfile = file, method = method)
        SCAN = scan(file, what = "", sep = "\n")

        # Extract all the data records and build the table:
        lines = grep("font color=white", SCAN)
        z1 = SCAN[lines][-(1:2)]

        # Remove irrelevant HTML markup strings
        z2 = gsub("font", "          ", x = z1)
        z1 = gsub("color=white........", " ", x = z2)
        z2 = gsub(">", " ", x = z1)
        z1 = gsub("<", " ", x = z2)
        z1 = gsub("/", " ", x = z1)

        # Next - Compose Matrix:
        n.rows = length(z1)
        z2 = unlist(apply(matrix(z1, ncol = 1), 2, strsplit, split = " "))
        z1 = as.numeric(z2[z2 != ""])
        n.fields = length(z1)
        z = matrix(z1, byrow = TRUE, ncol = n.fields/n.rows)
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

        # Save to file:
        if (save) {
            write.table(z, file, quote = FALSE, sep = ";", row.names = FALSE)
        } else {
            unlink(file)
        }

        # Return Value:
        new("fWEBDATA",
            call = match.call(),
            param = c(
                "Instrument Query" = query,
                "Frequency" = frequency,
                "Instrument Name" = colname),
            data = z,
            title = "Web Data Import from Economagic",
            description = as.character(date()) )
    }
}


# ------------------------------------------------------------------------------


economagicSeries <-
    function (query, frequency = c("quarterly", "monthly", "daily"),
    returnClass = c("timeSeries", "ts", "matrix", "data.frame"),
    getReturns = FALSE, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Downloads easily time series data from Yahoo

    # Arguments:

    # Examples:
    #   USDEUR Foreign Exchange Rate:
    #    economagicSeries("fedny/day-fxus2eu", frequency = "daily")
    #   USFEDFUNDS US FedFunds Rate:
    #    economagicSeries("fedstl/fedfunds+2", frequency = "monthly")
    #   USDGNP:
    #    economagicSeries("fedstl/gnp", frequency = "monthly")

    # FUNCTION:

    # Match Arguments:
    frequency = match.arg(frequency)
    returnClass = match.arg(returnClass)

    # Download:
    Y = economagicImport(query = query, frequency = frequency)@data
    X = as.timeSeries(Y)
    colnames(X)<-colnames(Y)[-1]

    # Compute Return Series ?
    if (getReturns) X = returns(X, ...)

    # Return as Object ?
    if (returnClass == "matrix") {
        X = X@data
    } else if (returnClass == "data.frame") {
        X = data.frame(X@Data)
    } else if (returnClass == "ts") {
        X = as.ts(X@Data)
    }

    # Return Value:
    X
}


################################################################################

