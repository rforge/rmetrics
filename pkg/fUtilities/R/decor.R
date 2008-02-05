
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file

    
################################################################################
# FUNCTION:
#  decor
#  hgrid
#  vgrid
#  boxL
#  box_
#  xrug
#  yrug
#  copyright
################################################################################


decor <- 
function()
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    hgrid()
    boxL()
}


################################################################################


hgrid <- 
function(ny = NULL, ...) 
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    grid(NA, ny, ...) 
}


# ------------------------------------------------------------------------------


vgrid <- 
function(nx = NULL, ...) 
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    grid(nx, NA, ...) 
}


# ------------------------------------------------------------------------------


boxL <- 
function(col = "white") 
{
    box()
    box(bty = "7", col = col) 
}


# ------------------------------------------------------------------------------


box_ <- 
function(col = c("white", "black")) 
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    box(bty = "c", col = col[1])
    box(bty = "]", col = col[2])
    box(bty = "7", col = col[1])
}


# ------------------------------------------------------------------------------


.xrug <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    rug(as.vector(x), ticksize = 0.01, side = 1, quiet = TRUE) 
}


# ------------------------------------------------------------------------------


.yrug <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    rug(as.vector(x), ticksize = 0.01, side = 2, quiet = TRUE) 
}


# ------------------------------------------------------------------------------

    
copyright <- 
function() 
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    Year = substr(Sys.Date(), 1, 4)
    mtext(paste("\251 Rmetrics", Year), 
        side = 4, line = 0, adj = 0, 
        font = 1, cex = 0.7*par("cex"), col = "grey") 
}


################################################################################

    