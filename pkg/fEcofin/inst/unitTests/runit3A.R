
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
# FUNCTION:                     BASIC EXTENSIONS:
#  align                         aligns time series objects by approximation
#  align.default                 align default method
#  attach                        attach a database to the R path
#  attach.default                attach default method
#  log                           log has become a generic function
#  log.default                   log default method
#  round                         round has become a generic function
#  round.default                 round default method
#  sample                        sample has become a generic function
#  sample.default                sample default method
#  sort                          sort has become a generic function
#  sort.default                  sort default method
#  var                           var has become a generic function
#  var.default                   var default method
# FUNCTION:                     ROW AND COLUMN NAMES:
#  "rownames<-"                  rownames<- has become a generic function
#  "rownames<-.default"          rownames<- default method
#  "colnames<-"                  colnames<- has become a generic function
#  "colnames<-.default"          colnames<- default method
# FUNCTION:                     DATE AND TIME SERIES FUNCTIONS:
#  modify                        Modifies a 'timeSeries' object
#  modify.default                Default Method
#  atoms                         Extracts atoms from 'timeSeries' object
#  atoms.default                 Default Method
#  as.POSIXlt                    Converts objects of class POSIXlt
#  as.POSIXlt.default            Default Method
#  as.matrix.ts                  Converts univariate ts to 1-column matrix
#  as.matrix.mts                 Converts multivariate ts to matrix
################################################################################


test.basicExtensions = 
function()
{
    #  align                         aligns time series objects by approximation
    #  align.default                 align default method
    #  attach                        attach a database to the R path
    #  attach.default                attach default method
    #  log                           log has become a generic function
    #  log.default                   log default method
    #  round                         round has become a generic function
    #  round.default                 round default method
    #  sample                        sample has become a generic function
    #  sample.default                sample default method
    #  sort                          sort has become a generic function
    #  sort.default                  sort default method
    #  var                           var has become a generic function
    #  var.default                   var default method
    
    # Return Value:
    return()
}

    
# ------------------------------------------------------------------------------


test.dimNames = 
function()
{
    #  "rownames<-"                  rownames<- has become a generic function
    #  "rownames<-.default"          rownames<- default method
    #  "colnames<-"                  colnames<- has become a generic function
    #  "colnames<-.default"          colnames<- default method

    # Row and Column Names:
    m = matrix(1:8, ncol = 2)
    m
    
    # Set Names:
    colnames(m) = c("A", "B")
    rownames(m) = as.character(1:4)
    m
    
    # Get Names:
    colnames(m)
    rownames(m)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.chronFunctions = 
function()
{
    #  modify                        Modifies a 'timeSeries' object
    #  modify.default                Default Method
    #  atoms                         Extracts atoms from 'timeSeries' object
    #  atoms.default                 Default Method
    #  as.POSIXlt                    Converts objects of class POSIXlt
    #  as.POSIXlt.default            Default Method
    #  as.matrix.ts                  Converts univariate ts to 1-column matrix
    #  as.matrix.mts                 Converts multivariate ts to matrix
    
    # Return Value:
    return()
}       


# ------------------------------------------------------------------------------

    
if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fEcofin/tests/runit3A.R")
    printTextProtocol(testResult)
}   


################################################################################

