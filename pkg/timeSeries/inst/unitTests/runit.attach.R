
test.attach.timeSeries =
function()
{
    # RUnit Test:

    # Attach Signal Series
    tS = timeSeries(format = "counts")
    attach(tS)
    SS.1
    detach(tS)

    # Attach timeDate Series:
    tS = timeSeries()
    attach(tS)
    TS.1
    detach(tS)
}
