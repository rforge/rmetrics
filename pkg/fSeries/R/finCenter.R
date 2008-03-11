finCenter  <-
function(x)
{
    # A function implemented by

    # Description:
    #

    # Example:
    #
    #

    # FUNCTION:

    stopifnot(inherits(x, "timeSeries"))

    ans <- x@FinCenter
    ans

}

"finCenter<-" <-
function(x, value)
{
    # A function implemented by Yohan Chalabi

    # Description:
    #

    # Example:
    #
    #

    # FUNCTION:

    stopifnot(inherits(x, "timeSeries"))

    # convert to user financial centre
    positions <- timeDate(charvec = time(x), zone = finCenter(x),
                          FinCenter = value)

    time(x) <- positions

    # Return Value:
    x
}
