
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
#  1999 - 2004, Diethelm Wuertz, GPL
#  Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#  info@rmetrics.org
#  www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#  see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#  see Rmetrics's copyright file


################################################################################
# FUNCTION:                 DESCRIPTION:
#  rollFun                   Compute Rolling Function Value
#   rollMean                  Compute Rolling Mean
#   rollVar                   Compute Rolling Variance
#   rollMin                   Compute Rolling Minimum
#   rollMax                   Compute Rolling Maximum
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(RollingAnalysis); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.rollingVector =
function()
{
    # Vector:
    x = 1:10
    
    # .roll.RUnit()
    n = 3
    
    for (na.rm in c(TRUE, FALSE)) {
        
        if (na.rm) x[6] = NA
        cat("\n\nna.rm: ", na.rm)
        cat("\nx: ", x, "\n")
        
        for (trim in c(TRUE, FALSE)) {
        
            cat("\ntrim: ", trim, "\n\n")
            
            # Sum:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = sum)
            print(ans)
            # 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90
            
            # Mean:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = mean)
            #  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
            print(ans)
            
            # Var:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = var)
            #  2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5
            print(ans)
            
            # Min:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = min)
            #  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
            print(ans)
            
            # Max:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = max)
            #  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
            print(ans)
            
        }
    }
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.rollingTimeSeries =
function()
{  
    # Time Series:
    charvec = paste("1999", 10, 11:20, sep = "-")
    ts = timeSeries(data = 1:10, charvec, units = "SERIES", zone = "GMT",
        FinCenter = "GMT") 
    
    # .roll.RUnit()
    n = 3
    
    for (na.rm in c(TRUE, FALSE)) {
        
        x = ts
        if (na.rm) x@Data[6, ] = NA
        cat("\n\nna.rm: ", na.rm)
        print(x)
        
        for (trim in c(TRUE, FALSE)) {
        
            cat("\ntrim: ", trim, "\n\n")
            
            # Sum:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = sum)
            print(ans)
            # 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90
            
            # Mean:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = mean)
            #  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
            print(ans)
            
            # Var:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = var)
            #  2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5
            print(ans)
            
            # Min:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = min)
            #  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
            print(ans)
            
            # Max:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = max)
            #  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
            print(ans)
            
        }
    }
        
    # Return Value:
    return()  
}



# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult = runTestFile("C:/Rmetrics/SVN/trunk/fMultivar/test/runit1C.R")
    printTextProtocol(testResult)
}
   

################################################################################
    
