
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


###############################################################################
# FUNCTION:               DESCRIPTION:
#  dataSplit               Splits a data matrix from a downloaded file
#  charvecSplit            Splits a charvec vector from a downloaded file
#  stringSplit             Splits a string vector from a downloaded file
###############################################################################


dataSplit <- 
  function (x, split=" ", col=-1) 
{
  # A function implemented by Diethelm Wuertz
  
  # Description:
  #   Splits a data matrix from a downloaded file
  
  # Arguments:
  #   x - a vector of data records to be splitted
  #   split - splitting separator
  #   col - columns to be extracted
  
  # FUNCTION
  
  # Split Function:
  FUN <- function(x, split, col) {
      unlist(strsplit(x, split))[col] }
  
  # Split Data Set:
  data <- unlist(lapply(x, FUN, split = split, col = col))
  data <- gsub("NA", "NaN", data)
  data <- matrix(as.numeric(data), byrow = TRUE, nrow = length(x))
  
  # Return Value:
  data
}


# -----------------------------------------------------------------------------


charvecSplit <-
  function (x, split=" ", col=1, format="%F") 
{
  # A function implemented by Diethelm Wuertz
  
  # Description:
  #   Splits a charvec vector from a downloaded file
  
  # Arguments:
  #   x - a vector of data records to be splitted
  #   split - splitting separator
  #   col - columns to be extracted
  #   format - date format, by default "%F"
  
  # FUNCTION:
  
  # Split Function:
  FUN <- function(x, split, col) {
      unlist(strsplit(x, split))[col] }
  
  # Split Date Character Vector:
  charvec <- unlist(lapply(x, FUN, split = split, col = col))
  charvec <- format(strptime(charvec, format = format))
  
  # Return Value:
  charvec
}


# -----------------------------------------------------------------------------


stringSplit <-
  function (x, split = " ", col = NULL) 
{
  # A function implemented by Diethelm Wuertz
  
  # Description:
  #   Splits a string vector from a downloaded file
  
  # Arguments:
  #   x - a vector of data records to be splitted
  #   split - splitting separator
  #   col - columns to be extracted
  #   format - date format, by default "%F"
  
  # FUNCTION:
  
  # Split Function:
  FUN <- function(x, split, col) {
      unlist(strsplit(x, split))[col] }
  
  # Split String Vector:    
  data <- unlist(lapply(x, FUN, split = split, col = col))
  data <- matrix(data, byrow = TRUE, nrow = length(x))
  
  # eturn Value:
  data
}


###############################################################################

