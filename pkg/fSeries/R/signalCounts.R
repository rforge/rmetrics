signalCounts <-
function(int)
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Descriptions:
    #   Creates the charvec for integer indexed time stamps

    # Arguments:
    #   int - a vector of integers, the counts.

    # FUNCTION:

    # Check that int is an integer
    #   ...

    # Check that all int's are positive ...
    #   ...

    # Format:
    cint = as.character(int)
    ans = format(cint, width = max(nchar(cint)), justify = "right")

    # Return Value:
    ans
}


