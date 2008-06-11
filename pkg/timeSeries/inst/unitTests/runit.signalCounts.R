test.signalCounts <-
function()
{
    # RUnit Test:

    int = c(1, 10, 100, 21, 135)
    print(.signalCounts(sample(int)))

    nc = .signalCounts(int)
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

    .signalCounts(1:12)
    .signalCounts(sample(1:12))
    .signalCounts(.signalCounts(1:12))
}
