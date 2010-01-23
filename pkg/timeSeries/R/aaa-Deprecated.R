

# deprecated functions moved to here ...


# .description
# durationSeries
# midquoteSeries
# spreadSeries


################################################################################


.description <- 
function()
{
    # Note:
    #   # Keep it for older Rmetrics Versions
    
    # FUNCTION:
    
    # Deprecated:
    .Deprecated("description")
    
    # Description String:
    ans = description()
    
    # Return Value:
    ans
}
    

# ------------------------------------------------------------------------------


durationSeries <- 
function(...) 
{
    # Deprecated:
    .Deprecated("returns", "timeSeries")
    
    # Durations
    durations(...)
}


# ------------------------------------------------------------------------------


midquoteSeries =
function(...)
{
    # FUNCTION:
    
    # Deprecated:
    .Deprecated("midquoteSeries")
    
    # Return Value:
    midquotes(...)
}


# ------------------------------------------------------------------------------


spreadSeries =
function(...)
{
    # FUNCTION:
    
    # Deprecated:
    .Deprecated("spreadSeries")
    
    # Return Value:
    spreads(...)
}


################################################################################

    