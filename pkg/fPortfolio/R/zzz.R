
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
################################################################################


.First.lib =
function(lib, pkg)
{
###     # Startup Mesage and Desription:
###     MSG <- if(getRversion() >= "2.5") packageStartupMessage else message
###     dsc <- packageDescription(pkg)
###     if(interactive() || getOption("verbose")) {
###         # not in test scripts
###         MSG(sprintf("Rmetrics Package %s (%s) loaded.", pkg, dsc$Version))
###     }

    # Load dll:
    library.dynam("fPortfolio", pkg, lib)

}

    data(GCCINDEX.DF, package = "fPortfolio")
    data(SPISECTOR.DF, package = "fPortfolio") 
    data(SWX.DF, package = "fPortfolio")
    data(LPP2005.RET.DF, package = "fPortfolio") 
    data(SMALLCAP.RET.DF, package = "fPortfolio") 
    
    
    GCCINDEX <- as.timeSeries(GCCINDEX.DF)
    SPISECTOR <- as.timeSeries(SPISECTOR.DF)
    SWX <- as.timeSeries(SWX.DF)
    LPP2005 <- cumulated(as.timeSeries(LPP2005.RET.DF))
    SMALLCAP <- cumulated(as.timeSeries(SMALLCAP.RET.DF))
    
    
    GCCINDEX.RET <- returns(as.timeSeries(GCCINDEX.DF))
    SPISECTOR.RET <- returns(as.timeSeries(SPISECTOR.DF))
    SWX.RET <- returns(as.timeSeries(SWX.DF))
    LPP2005.RET <- as.timeSeries(LPP2005.RET.DF)
    SMALLCAP.RET <- as.timeSeries(SMALLCAP.RET.DF)

     
    if(!exists("Sys.setenv", mode = "function")) # pre R-2.5.0, use "old form"
        Sys.setenv <- Sys.putenv
        

################################################################################

