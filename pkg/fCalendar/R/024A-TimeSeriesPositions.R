
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
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# METHOS               MODIFICATION METHODS:
#  seriesPositions      Extracts positions slot from 'timeSeries' object
#  newPositions<-       Modifies positions of a 'timeSeries' object
#
#  sample.timeSeries    S3: Resamples a 'timeSeries' object in time
#  sort.timeSeries      S3: Sorts reverts a 'timeSeries' object in time
#  rev.timeSeries       S3: Reverts a 'timeSeries' object in time 
#
#  start.timeSeries     S3: Extracts start date of a 'timeSeries' object 
#  end.timeSeries       S3: Extracts end date of a 'timeSeries' object
################################################################################


seriesPositions =
function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts the positions of a 'timeSeries' objects and 
    #   converts them to a 'timeDate' object.
    
    # Arguments:
    #   object - a 'timeSeries' object
    
    # Value:
    #   Returns 'timeSeries' positions as 'timeDate' objects.
    
    # FUNCTION:
        
    # Create 'timeDate' Object:
    ans = timeDate(charvec = object@positions, format = object@format, 
        zone = object@FinCenter, FinCenter = object@FinCenter)   
        
    # Return Value:
    ans   
}


# ------------------------------------------------------------------------------


"newPositions<-" =
function(object, value)
{
    timeSeries(object, value)
}


# ------------------------------------------------------------------------------


sample.timeSeries =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Time sorts a 'timeSeries' object
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
    # Value:
    #   Returns a time resampled object of class 'timeSeries'.
    
    # FUNCTION:
     
    # Data:
    nPOS = length(x@positions)
    index = sample(1:nPOS)
    x[index, ]
    
    # recordIDs:
    DF = x@recordIDs
    DIM = dim(DF)
    if (sum(DIM) > 0) {
        df = rev(DF[, 1])
        if (DIM[2] > 1) 
            for (i in 2:DIM[2]) df = data.frame(df, rev(DF[index, i]))
        colnames(df) <- colnames(DF)
        rownames(df) <- x@positions
        x@recordIDs = df
    }
    
    # Return Value:
    x
}


# ------------------------------------------------------------------------------

    
sort.timeSeries =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Time sorts a 'timeSeries' object
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
    # Value:
    #   Returns a time sorted object of class 'timeSeries'.
    
    # FUNCTION:
    
    # Data:
    POS = x@positions
    index = sort(POS, index.return = TRUE)$ix
    x[index, ]
    
    # recordIDs:
    DF = x@recordIDs
    DIM = dim(DF)
    if (sum(DIM) > 0) {
        df = rev(DF[index, 1])
        if (DIM[2] > 1) 
            for (i in 2:DIM[2]) df = data.frame(df, rev(DF[index, i]))
        colnames(df) <- colnames(DF)
        rownames(df) <- x@positions
        x@recordIDs = df
    }
    
    # Return Value:
    x
}


# ------------------------------------------------------------------------------
  
    
rev.timeSeries =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Time reverts a 'timeSeries' object
    
    # Arguments:
    #   x - a 'timeSeries' object.
    
    # Value:
    #   Returns a time reverted object of class 'timeSeries'.
    
    # FUNCTION:
    
    # Data:
    nPOS = length(x@positions)
    index = nPOS:1
    x[index, ]
    
    # IDs:
    DF = x@recordIDs
    DIM = dim(DF)
    if (sum(DIM) > 0) {
        df = rev(DF[, 1])
        if (DIM[2] > 1) 
            for (i in 2:DIM[2]) df = data.frame(df, rev(DF[, i]))
        colnames(df) <- colnames(DF)
        rownames(df) <- x@positions
        x@recordIDs = df
    }
    
    # Return Value:
    x
} 


# ------------------------------------------------------------------------------


start.timeSeries =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts the first position as a character string
    
    # Arguments:
    #   x - a 'timeSeries' object.
    #   ... - arguments passed to other methods.
    
    # Value:
    #   Returns the first time/date as an object of class 'timeDate'.
  
    # FUNCTION:
    
    # S3 Method:
    ans = start.timeDate(seriesPositions(x))

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


end.timeSeries =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts the last position as a character string
    
    # Arguments:
    #   x - a 'timeSeries' object.
    #   ... - arguments passed to other methods.
    
    # Value:
    #   Returns the last time/date as an object of class 'timeDate'.
    
    # FUNCTION:
    
    # S3 Method:
    ans = end.timeDate(seriesPositions(x))

    # Return Value:
    ans
}


################################################################################

