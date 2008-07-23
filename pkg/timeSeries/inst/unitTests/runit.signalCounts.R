test.signalCounts <-
function()
{
    # RUnit Test:

    int = c(1, 10, 100, 21, 135)
    print(timeSeries:::.signalCounts(sample(int)))

    nc = timeSeries:::.signalCounts(int)
    nc

    ns = sample(nc)
    ns

    sorted = sort(ns)
    sorted
    as.integer(sorted)
    ns

    ordered = order(ns)
    ordered
    ns[ordered]
    as.integer(ns[ordered])

    timeSeries:::.signalCounts(1:12)
    timeSeries:::.signalCounts(sample(1:12))
    timeSeries:::.signalCounts(timeSeries:::.signalCounts(1:12))
}
