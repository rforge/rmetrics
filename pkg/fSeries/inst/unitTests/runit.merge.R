test.merge.timeSeries =
function()
{
    # RUnit Test:

    # Time Stamps:
    x = timeSeries()[,1]
    x
    y = timeSeries()
    y
    merge(x, y)

    # Signal Counts:
    x = timeSeries(format = "counts")[,1]
    x
    y = timeSeries(format = "counts")
    y
    merge(x, y)
}
