#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  ../../COPYING


################################################################################
# FUNCTION:                 DESCRIPTION:
#  model.frame.default       Allows to use model.frame for "timeSeries"
################################################################################


model.frame.timeSeries =
function(formula, data, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracting the Environment of a Model Formula

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

