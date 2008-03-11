################################################################################
# FUNCTION:                 DESCRIPTION:
#  seriesData                Extracts data slot from 'timeSeries' object
#  series                    Extracts data slot from 'timeSeries' object
#  series<-                  Assign new data slot for 'timeSeries' object
################################################################################

seriesData <-
    function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #    Returns the series Data of an ordered data object.

    # Arguments:
    #   object - a 'timeSeries' object

    # Value:
    #    Returns an object of class 'matrix'.

    # FUNCTION:

    # Test:
    if(class(object) != "timeSeries") stop("Object is not a time Series")

    .Deprecated("series", "fSeries")

    # Get Data Slot:
    ans = object@Data

    # Return Value:
    ans
}

# ------------------------------------------------------------------------------

series <-
function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #    Returns the series Data of an ordered data object.

    # Arguments:
    #   object - a 'timeSeries' object

    # Value:
    #    Returns an object of class 'matrix'.

    # FUNCTION:

    # Test:
    stopifnot(inherits(object, "timeSeries"))

    # Get Data Slot:
    ans = object@Data


    # Return Value:
    ans
}


# ------------------------------------------------------------------------------

"series<-" <-
    function(x, value)
{

    stopifnot(inherits(x, "timeSeries"))

    x[seq(NROW(x)), seq(NCOL(x))] <- value
    x
}

################################################################################

