
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
    # Startup Mesage and Desription:
    MSG <- if(getRversion() >= "2.5") packageStartupMessage else message
    dsc <- packageDescription(pkg)
    if(interactive() || getOption("verbose")) {
        # not in test scripts
        MSG(sprintf("Rmetrics Package %s (%s) loaded.", pkg, dsc$Version))
    }

    # Load dll:
    # library.dynam("fPortfolio", pkg, lib)
    # use "Rdonlp2"
}

.onLoad <-
    function(libname, pkgname)
{

}


if(!exists("Sys.setenv", mode = "function")) # pre R-2.5.0, use "old form"
    Sys.setenv <- Sys.putenv


################################################################################
# some useful functions added
   
 
.lppData = 100* as.timeSeries(data(LPP2005REC))[, 1:6]

.mvSpec = portfolioSpec()
setType(.mvSpec) = "MV"
setTargetReturn(.mvSpec) = mean(.lppData)
setSolver(.mvSpec) = "solveRfoo"

.cvarSpec = portfolioSpec()
setType(.cvarSpec) = "CVaR"
setTargetReturn(.cvarSpec) = mean(.lppData)
setSolver(.cvarSpec) = "solveRfoo"

.madSpec = portfolioSpec()
setType(.madSpec) = "MAD"
setTargetReturn(.madSpec) = mean(.lppData)
setSolver(.madSpec) = "solveRfoo"

.Box = c("minW[3:4]=0.1", "maxW[5:6]=0.8")
.Group = c("minsumW[1:3]=0.2", "maxsumW[c(2,4)]=0.8")
.BoxGroup = c(.Box, .Group)
.CovBudget = c("minB[3:4]=0.1", "maxB[5:6]=0.9")


################################################################################

