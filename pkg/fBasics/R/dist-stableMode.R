
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
# FUNCTIONS:            DESCRIPTION:
#  stableMode            Computes the mode of the stable DF
################################################################################


stableMode <-
function(alpha, beta)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes the mode of the stable DF

    # Notes:
    #   # Test for values close to beta = 1
    #   alpha = seq(0, 2, by = 0.1)
    #   ans = NULL
    #   for ( i in 1:length(alpha) ) {
    #     ans = rbind(ans, c(alpha[i],
    #       stableMode(alpha = alpha[i], beta = 0.99 ),
    #       stableMode(alpha = alpha[i], beta = 0.99999 ),
    #       stableMode(alpha = alpha[i], beta = 0.99999999 ),
    #       stableMode(alpha = alpha[i], beta = 0.99999999999 ) ) ) }
    #   ans
    #
    #   alpha          0.99       0.99999    0.99999999 0.99999999999
    #   0.0    0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00
    #   0.2   -3.214142e-01 -3.246759e-01 -3.246787e-01 -3.246788e-01
    #   0.4   -6.105318e-01 -6.158562e-01 -6.158616e-01 -6.158616e-01
    #   0.6   -6.550106e-01 -6.594746e-01 -6.594790e-01 -6.594790e-01
    #   0.8   -5.558811e-01 -5.590032e-01 -5.590063e-01 -5.590063e-01
    #   1.0   -4.271033e-01 -4.293078e-01 -4.293099e-01 -4.293099e-01
    #   1.2   -3.074015e-01 -3.090820e-01 -3.090804e-01 -3.090804e-01
    #   1.4   -2.050956e-01 -2.063979e-01 -2.063951e-01 -2.063951e-01
    #   1.6   -1.199623e-01 -1.208875e-01 -1.208853e-01 -1.208853e-01
    #   1.8   -5.098617e-02 -5.145758e-02 -5.145639e-02 -5.145639e-02
    #   2.0   -7.487432e-05 -7.487432e-05 -7.487432e-05 -7.487432e-05

    # FUNCTION:

    # Stable Mode:
    if (beta > 0.99999999999) beta = 0.99999999999
    if (beta == 0) {
        ans = 0
    } else {
        if (alpha == 0) {
            ans = 0
        } else {
            ans = optimize(f = dstable, interval = c(-0.7, 0),
                maximum = TRUE, alpha = alpha, beta = beta)$maximum
        }
    }

    # Attributes:
    attr(ans, "control") =
        cbind.data.frame(dist = "stable", alpha = alpha, beta = beta,
        row.names = "")

    # Return Value:
    ans
}


################################################################################

