
#
# Example:
#   Plot Nelson-Siegel-Svenson Coefficients from Bundesbank Data
#
# Description:
#
# Reference:
#   Deutsche Bundesbank Data Server
#   www.bundesbank.de
#


# ------------------------------------------------------------------------------
# http://www.bundesbank.de/
#   statistik/statistik_zeitreihen.en.php?func=list&tr=www_s300_it03c&print=no&


# Graph Frame:
par(mfcol = c(2, 3))

# Settings:             
coeffNSS = as.timeSeries(data(bundesbankNSS))
Units = coeffNSS@units

# Plot Nelson-Siegel-Svensson Coefficients:
for (i in c(4, 6, 1, 2, 3, 5)) {
    plot(coeffNSS[, i], type = "l", 
        col ="steelblue", main = Units[i], ylab = Units[i])
        abline(h = 0, col = "grey")
    grid()
}
    
 
################################################################################

