
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
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file 


################################################################################
# FUNCTION:         DESCRIPTION:
#  ZivotWangData     Data sets used for the examples in Zivot/Wang's book
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(ZivotWangData); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.data =
function()
{
    # 31-Jan-1913
    data(CPI.dat)
    print(head(CPI.dat))
    cat("\n")
    
    # 1990-12-31
    data(DowJones30)
    print(head(DowJones30))
    cat("\n")
    
    # 2/2/1984
    data(ford.s)
    print(head(ford.s))
    cat("\n")
    
    # day   sec   price
    data(highFreq3M.df)
    print(head(highFreq3M.df))
    cat("\n")
    
    # 2/2/1984
    data(hp.s)
    print(head(hp.s))
    cat("\n")
    
    # 28-Jan-1919
    data(IP.dat)
    print(head(IP.dat))
    cat("\n")
    
    # 2/28/1976
    data(lexrates.dat)
    print(head(lexrates.dat))
    cat("\n")
    
    # 2000-09-27
    data(msft.dat)
    print(head(msft.dat))
    cat("\n")
    
    # 1871-12-31
    data(shiller.annual)
    print(head(shiller.annual))
    cat("\n")
    
    # 1/31/1871
    data(shiller.dat)
    print(head(shiller.dat))
    cat("\n")
    
    # 31-Jan-1990
    data(singleIndex.dat)
    print(head(singleIndex.dat))
    cat("\n")
    
    # 1959-12-31
    data(varex.ts)
    print(head(varex.ts))
    cat("\n")
    
    # 1-Feb-02
    data(yhoo.df)
    print(head(yhoo.df))
    cat("\n")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    testResult <- runTestFile("C:/Rmetrics/trunk/fBasics/test/runit012D.R")
    printTextProtocol(testResult)
}
  
    
################################################################################

