
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
# FUNCTION:                    DESCRIPTION:  
#  solveRQuadprog               Calls Goldfarb and Idnani's QP solver
#  solveRDonlp2                 Calls Spelucci's donlp2 solver
# FUNCTION:                    DESCRIPTION:
#  setSolver                    Sets the desired solver
#  setSolver<-                  Sets the desired solver                    
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(PortfolioSolver, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.solverRQuadprog =
function()
{ 
    # Install "Rdonlp2" from - http://arumat.net/Rdonlp2/
    require(Rdonlp2)
    
    # Direct Access:
    data = series = usPortfolioData()
    spec = portfolioSpec()
    constraints = NULL
    setTargetReturn(spec) = mean(as.matrix(series))
    statistics = portfolioStatistics(data, spec)
    data = list(series = series, statistics = statistics)
    solveRQuadprog(data, spec, constraints)
    
    # Frontier Portfolio:
    Data = usPortfolioData() 
    Spec = portfolioSpec()
    setTargetReturn(Spec)<- mean(as.matrix(series))
    Constraints = c(
        "minW[1:8]=0", 
        "minsumW[1:3]=0.3", "minsumW[2:4]=0.1", 
        "maxsumW[6:8]=0.8")
    setSolver(Spec)<-"RQuadprog"
    quadprog = efficientPortfolio(Data, Spec, Constraints) 
    quadprog

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.solverRDonlp2 =
function()
{ 
    # Install "Rdonlp2" from - http://arumat.net/Rdonlp2/
    require(Rdonlp2)

    # Direct Access:
    data = series = usPortfolioData()
    spec = portfolioSpec()
    constraints = NULL
    setTargetReturn(spec) = mean(as.matrix(series))
    statistics = portfolioStatistics(data, spec)
    data = list(series = series, statistics = statistics)
    solveRQuadprog(data, spec, constraints)
    
    # Frontier Portfolio:
    Data = usPortfolioData() 
    Spec = portfolioSpec()
    setTargetReturn(Spec)<- mean(as.matrix(series))
    Constraints = c(
        "minW[1:8]=0", 
        "minsumW[1:3]=0.3", "minsumW[2:4]=0.1", 
        "maxsumW[6:8]=0.8")
    donlp2 = efficientPortfolio(Data, Spec, Constraints)
    donlp2
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit2G.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

