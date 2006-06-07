
#
# Example:
#   Compute Nelson-Siegel-Svenson Term Structures 
#
# Description:
#
# Reference:
#   Zivot - Wang
#


# ------------------------------------------------------------------------------


require(fBonds)
 
data(mk.zero2)
rate = as.numeric(mk.zero2[54,])[2:49]
data(mk.maturity)
maturity = as.numeric(mk.maturity[,1])[1:48]

rbind(maturity, rate)
plot(maturity, rate)

par(mfcol = c(2, 3), cex = 0.7)

NelsonSiegel(rate, maturity)
Svensson(rate, maturity)


# ------------------------------------------------------------------------------

