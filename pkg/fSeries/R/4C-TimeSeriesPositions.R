
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


# fCalendar::4C-TimeSeriesPositions.R
################################################################################
# FUNCTION:                 POSITIONS:
#  seriesPositions           Extracts positions slot from 'timeSeries' object
#  newPositions<-            Modifies positions of a 'timeSeries' object
# METHOD:                   ORDERING:
#  sample.timeSeries         Resamples a 'timeSeries' object in time
#  sort.timeSeries           Sorts reverts a 'timeSeries' object in time
#  rev.timeSeries            Reverts a 'timeSeries' object in time 
#  start.timeSeries          Extracts start date of a 'timeSeries' object 
#  end.timeSeries            Extracts end date of a 'timeSeries' object
################################################################################


################################################################################
# FUNCTION:                 POSITIONS:
#  seriesPositions           Extracts positions slot from 'timeSeries' object
#  newPositions<-            Modifies positions of a 'timeSeries' object


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
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    ans = timeSeries(object, value)
    
    # Return Value:
    ans
}


################################################################################
# METHOD:                   ORDERING:
#  sample.timeSeries         Resamples a 'timeSeries' object in time
#  sort.timeSeries           Sorts reverts a 'timeSeries' object in time
#  rev.timeSeries            Reverts a 'timeSeries' object in time 
#  start.timeSeries          Extracts start date of a 'timeSeries' object 
#  end.timeSeries            Extracts end date of a 'timeSeries' object


.sample.timeSeries =
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
    x = x[index, ]
    
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


sample.timeSeries = 
function (x, ...) 
{ 
    # Index:
    Index = sample(1:length(x@positions))

    # Compose Series:
    x@positions = x@positions[Index] 
    x@Data = as.matrix(x@Data[Index, ])  
    colnames(x@Data) = x@units
    x@recordIDs = as.data.frame(x@recordIDs[Index, ])
    
    # Return value:
    x
}



# ------------------------------------------------------------------------------

    
.sort.timeSeries =
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
    x = x[index, ]
    
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


sort.timeSeries = 
function (x, ...) 
{
    # Index:
    # Index = sort(as.POSIXct(x@positions), index.return = TRUE)$ix
    Index = sort(x@positions, index.return = TRUE)$ix
    
    # Compose Series:
    x@positions = x@positions[Index] 
    x@Data = as.matrix(x@Data[Index, ])  
    colnames(x@Data) = x@units
    x@recordIDs = as.data.frame(x@recordIDs[Index, ])
    
    # Return value:
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
    x = x[index, ]
    
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

