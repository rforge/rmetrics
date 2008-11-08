
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
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             ASSETS SELECTION:
#  assetsSelect          Selects individual assets from a set of assets
#   method = "hclust"     hierarchical clustering of returns
#   method = "kmeans"     k-means clustering of returns
################################################################################


test.assetsSelectHClust =
function()
{
    if (FALSE) {
    
    # Hierarchical Clustering:
    # Select the 4 most dissimilar assets from Berndt's data set

    # The data set "berndtInvest" is from Berndt's textbook
    # "The Practice of Econometrics". It is a data.frame consisting
    # of 18 columns with the following entries:
    #  [1] %d/%B/%y "CITCRP" "CONED"  "CONTIL" "DATGEN" "DEC"
    #  [7] "DELTA"  "GENMIL" "GERBER" "IBM"    "MARKET" "MOBIL"
    # [13] "PANAM"  "PSNH"   "TANDY"  "TEXACO" "WEYER"  "RKFREE"
    # The first column holds the date, the 11th the market rate,
    # and the last (the 18th) the risk free rate.

    # Load the Data and Create an Object of Class 'timeSeries':
    data(berndtInvest)
    berndtInvest = as.timeSeries(berndtInvest)
    class(berndtInvest)
    head(berndtInvest)

    # Exclude the Date, Market Returns and Interest Rate Columns
    # from the data frame, then multiply by 100 for percentual returns ...
    allAssets = 100 * berndtInvest[, -c(1, 10, 17)]
    class(allAssets)
    head(allAssets)

    # Graph Frame:
    par(mfrow = c(2, 1), cex = 0.7)

    # Select the "n" Most Dissimilar Assets from
    # Hierarchical Clustering:
    n = 4
    args(assetsSelect)
    clustered = assetsSelect(allAssets, doplot = TRUE)
    # Create my Assets Set from the "n" selected Symbols:
    myAssets = allAssets[, c(clustered$order[1:n])]
    colnames(myAssets)
    # Print the Column Return:
    mu.vec = colMeans(myAssets)
    mu.vec
    # or ...
    mu.vec = colMeans(series(myAssets))
    mu.vec
    # Print the Covariance Matrix:
    cov.mat = cov(series(myAssets))
    cov.mat

    # Plot Cumulated Returns of the Assets:
    ts.plot(colCumsums(myAssets), col = 1:4)
    grid()
    legend(0, 250, legend = colnames(myAssets), pch = "----", col = 1:4)
    title(main = "Cumulated Returns", ylab = "Cumulated Returns")
    abline(h = 0, lty = 3)
    
    }
    NA

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsSelectKMeans =
function()
{
    # K-Means Clustering:

    if (FALSE) {
    
    # Load Data
    berndtInvest = as.timeSeries(data(berndtInvest))
    allAssets = 100 * berndtInvest[, -c(1, 10, 17)]
    allAssets = as.matrix(allAssets)
    head(allAssets)

    # assetsSelect(x, method = c("hclust", "kmeans"), kmeans.centers = 5,
    #   kmeans.maxiter = 10, doplot = TRUE, ...)
    clustered = assetsSelect(t(allAssets), method = "kmeans",
        kmeans.centers = 4, doplot = TRUE)
             
    }
    NA

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsSelectKMeans =
function()
{
    if (FALSE) {
    
    require(cluster)


    .assetsSelect =
    function (x, k,
    method = c("hclust", "kmeans", "agnes", "diana", "pam", "clara"),
    doplot = TRUE, control = FALSE, ...)
    {
        # Settings:
        X = as.matrix(x)
        method = match.arg(method)

        # Hierarchical Clustering:
        if (method == "hclust") {
            ans = hclust(dist(t(X)), ...)
            index = rev(ans$order)[1:k]
            if (doplot) plot(ans)
        }
        if (method == "agnes") {
            ans = agnes(t(X), ...)
            index = rev(ans$order)[1:k]
            if (doplot) plot(ans)
        }
        if (method == "diana") {
            ans = diana(t(X), ...)
            index = rev(ans$order)[1:k]
            if (doplot) plot(ans)
        }

        # K-Means Clustering:
        if (method == "kmeans") {
            ans = kmeans(x = X, centers = k, ...)
            Dist = rep(Inf, times = k)
            index = rep(NA, times = k)
            Center = ans$center
            Cluster = ans$cluster
            for (i in 1:length(Cluster)) {
                j = Cluster[i]
                d = as.vector(dist(rbind(Center[j,], X[,i])))
                if (d < Dist[j]) {
                    Dist[j] = d
                    index[j] = i
                }
            }
            if (doplot) {
                plot(t(X), col = ans$cluster)
                points(ans$centers, col = 1:k, pch = 8, cex = 2)
            }
        }
        if (method == "pam") {
            ans = pam(t(X), k, ...)
            index = ans$id.med
            if (doplot) plot(ans)
        }
        if (method == "clara") {
            ans = clara(t(X), k, ...)
            index = ans$i.med
            if (doplot) plot(ans)
        }

        # Select data and optionally add control:
        data = x[, index]
        if (control) attr(data, "control")<-ans

        # Return Value:
        data
    }

    # Data:
    berndtInvest = as.timeSeries(data(berndtInvest))
    X = 100 * berndtInvest[, -c(1, 10, 17)]

    # Selection:
    .assetsSelect(X, 4, "hclust", doplot = FALSE)
    .assetsSelect(X, 4, "agnes", doplot = FALSE)
    .assetsSelect(X, 4, "diana", doplot = FALSE)

    .assetsSelect(X, 4, "kmeans", doplot = FALSE)
    .assetsSelect(X, 4, "pam", doplot = FALSE)
    .assetsSelect(X, 4, "clara", doplot = FALSE)

    }
    
    NA

    # Return Value:
    return()
}


################################################################################

