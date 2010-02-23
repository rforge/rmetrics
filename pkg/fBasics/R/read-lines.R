
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


################################################################################
# FUNCTION:               DESCRIPTION:
#  read.lines              A synonyme function call to readLines
################################################################################


read.lines <-
function(con=stdin(), n=-1L, ok=TRUE, warn=FALSE, encoding="unknown") 
{
    # Description:
    #   A synonyme function call to readLines
    
    # Arguments:
    #   con - a connection object or a character string
    #   n - an integer, the (maximal) number of lines to read. 
    #       Negative values indicate that one should read up 
    #       to the end of input on the connection.
    #   ok - a logical, is it OK to reach the end of the connection 
    #       before n > 0 lines are read? If not, an error will be 
    #       generated.
    #   warn - a logical, warn if a text file is missing a final EOL.
    #       The default is FALSE, note different from function
    #       \code{readLines}.
    #   encoding - encoding to be assumed for input strings. 
    
    # FUNCTION
    
    # Check connection
    if (is.character(con)) {
        con <- file(con, "r")
        on.exit(close(con))
    }
    
    # Return Value:
    .Internal(readLines(con, n, ok, warn, encoding))
}


################################################################################

