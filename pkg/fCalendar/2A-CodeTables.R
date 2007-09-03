
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
# You should have received A copy of the GNU Library General 
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


# fEcofin::2A-CodeTables
################################################################################
# FUNCTION:                 CHARACTER, SYMBOL AND COLOR TABLES:
#  characterTable            Shows a table of character codes 
#  symbolTable               Shows a table of plot symbols
#  colorTable                Shows a table of plot color codes
################################################################################


################################################################################
# FUNCTION:                 CHARACTER, SYMBOL AND COLOR TABLES:
#  characterTable            Shows a table of character codes 
#  symbolTable               Shows a table of plot symbols
#  colorTable                Shows a table of plot color codes


characterTable = 
function(font = 1, cex = 0.7) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Prints numeric equivalents to all latin characters.

    # Notes:
    #   The printed version doesn't allways corresponds to the 
    #   screen display. The character on line "xy" and column 
    #   "z" of the table has code "xyz". These codes can be 
    #   used as any other characters. 
    #     e.g. title("\347\340 et \340")
    #   Note on S:
    #   As the command line window of Splus can't print special 
    #   characters 
    #     cat("\347\340 et \340") 
    #   will not print the special characters, at least under 
    #   4.5 and under 2000.
    
    # Author:
    #   Source from Pierre Joyet, pierre.joyet@bluewin.ch

    # Example:
    #   for (i in 1:20) characterTable(font = i)

    # FUNCTION:
    
    # Table:
    v = 40:377
    v = v[v %% 100 < 80 & v %% 10 < 8]
    par(mar = c(5, 5, 4, 2) + 0.1)
    plot(-1:7, seq(4, 33, length = 9), type = "n", axes = FALSE, 
        xlab = "", ylab = "", cex = cex, main = "Table of Characters")
    k = 1
    for(i in 4:31)
        for(j in 0:7) {
            text(j, 35 - i, eval(parse(text = paste("\"\\", v[k], "\"",
                    sep = ""))), font = font, cex = cex)
            k = k + 1 }
    
    text(0:7, rep(33, 7), as.character(0:7), font = 3, cex = cex)
    text(rep(-1, 28), 31:4, as.character(c(4:7, 10:17, 20:27, 
        30:37)), font = 3, cex = cex)
    
    # Return Value:
    invisible(font)
}


# ------------------------------------------------------------------------------


symbolTable = 
function(font = par('font'), cex = 0.7) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Shows a table of plot characters.
    
    # Example:
    #   plotcharacterTable()
    
    # Author:
    #   Unknown, code found on the internet.

    # FUNCTION:
    
    # Table:
    plot(0, 0, xlim = c(-1, 11), ylim = c(0, 26), type = 'n', 
        axes = FALSE, xlab = '', ylab = '', 
        main = "Table of Plot Characters")
    j = -1
    for(i in 0:255) {
        if(i %% 25 == 0) {j = j+1; k = 26}
        k = k-1
        points(j, k, pch = i, font = font, cex = cex, col = 2)
        text(j+0.50, k, i, cex = cex) 
    }
    
    # Return Value:
    invisible(font)
}


# ------------------------------------------------------------------------------


colorTable = 
function(cex = 0.7) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays a table of plot colors.
    
    # Author:
    #   Unknown, code found on the internet.
    
    # Example:
    #   colorTable()
  
    # FUNCTION:
    
    # Plot:
    plot(0, 0, xlim = c(-1, 10), ylim = c(0, 10), type = 'n', axes = FALSE, 
        xlab = '', ylab = '', cex = cex, main = "Table of Color Codes")
    j = -1
    for(i in 0:99) {
        if(i %% 10 == 0) {j = j+1; k = 10}
        k = k-1
        points(j, k, pch = 15, col = i, cex = 2)
        text(j+0.45, k, i, cex = cex)}
    
    # Return Value:
    invisible()
}


################################################################################

