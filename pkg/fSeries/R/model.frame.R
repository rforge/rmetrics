
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
# FUNCTION:                 DESCRIPTION:
#  model.frame.default       Allows to use model.frame for "timeSeries"
################################################################################


model.frame.timeSeries = 
function(formula, data, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracting the “Environment” of a Model Formula
  
    # Arguments:
    #   formula - a model formula
    #   data - a 'timeSeries' object
    
    # Details:
    #   Allows to use model.frame() for "timeSeries" objects.
    
    # Examples:
    #   x = as.timeSeries(data(msft.dat))[1:12, ]
    #   model.frame( ~ High + Low, data = x)
    #   model.frame(Open ~ High + log(Low, base = `base`), data = x) 
   
    # FUNCTION:

    # Create Model Frame:
    Data = data
    data = data.frame(data@Data) 
    Model = stats::model.frame.default(formula, data, ...)
    
    # Convert to timeSeries:
    ans = timeSeries(
        data = as.matrix(Model), 
        charvec = rownames(Data),
        units = colnames(Model),
        format = Data@format,
        FinCenter = Data@FinCenter, 
        recordIDs = Data@recordIDs, 
        title = Data@title, 
        documentation = .description()
        )

    # Return value:
    ans   
}


################################################################################

