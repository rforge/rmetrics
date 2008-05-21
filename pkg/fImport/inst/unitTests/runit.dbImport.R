
# Rmetrics is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# Rmetrics is distributed in the hope that it will be useful,
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
# FUNCTION:          
#  test.dbCategories  
#  test.dbListing 
#  test.dbSeries  
#  test.dbImport           
################################################################################


test.dbCategories <- 
    function()
{  
    print("Not yet implemented")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.dbListing <- 
    function()
{  
    print("Not yet implemented")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------

      
test.dbSeries <- 
    function()
{  
    print("Implemented as Hidden Function")
    
    if (FALSE) {
        
        # db:
        X = .dbSeries(symbols = "DE0009769554")
        print(head(X))
        print(class(X))
        
        # db:
        X = .dbSeries(symbols = c("DE0009769554", "LU0130729220"))
        print(head(X))
        print(class(X))

    }
         
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------
    
       
test.dbImport <- 
    function()
{  
    print("Implemented as Hidden Function")
    
    if (FALSE) {
        
        # db:
        X = .dbImport(
            query = "DE0009769554", 
            file = "tempfile",
            frequency = "daily", 
            save = FALSE, 
            sep = ";", 
            try = TRUE )
            
        print(X)
        print(head(X@data))
        print(class(X@data))
    
    }
         
    # Return Value:
    return()
}

    
################################################################################\

