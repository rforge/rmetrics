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
#  .endOfPeriodSeries        Returns series back to a given period
#  .endOfPeriodStats         Returns statistics back to a given period
#  .endOfPeriodBenchmarks    Returns benchmarks back to a given period
################################################################################


################################################################################
# FUNCTION:                 DESCRIPTION:
#  .endOfPeriodSeries        Returns series back to a given period
#  .endOfPeriodStats         Returns statistics back to a given period
#  .endOfPeriodBenchmarks    Returns benchmarks back to a given period


.endOfPeriodSeries =
function(x, nYearsBack = c("1y", "2y", "3y", "5y", "10y", "YTD"),
aggregate = c("monthly", "quarterly"), align = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns series back to a given period

    # Arguments:
    #   x - a daily 'timeSeries' object of returns
    #   nYearsBack - a period string. How long back should the series
    #       be extracted?

    # Note:
    #   Add "1m"

    # FUNCTION:

    # Should the series be aligned:
    if (align) x = alignDailySeries(x)

    # Match Arguments:
    nYearsBack = match.arg(nYearsBack)

    # Settings:
    if (nYearsBack == "YTD") yearsBack = 0
    else if (nYearsBack == "1y") yearsBack = 1
    else if (nYearsBack == "2y") yearsBack = 2
    else if (nYearsBack == "3y") yearsBack = 3
    else if (nYearsBack == "5y") yearsBack = 5
    else if (nYearsBack == "10y") yearsBack = 10

    Year = currentYear - yearsBack
    fromDate = timeDate(paste(Year, "-01-01", sep = ""))
    if (yearsBack == 0) {
        toDate = end(x)
    } else {
        toDate = timeDate(paste(currentYear-1, "-12-31", sep = ""))
    }

    # Are there enough Data Points?
    stopifnot(start(x) < fromDate)

    # ReturnValue:
    cut(x, fromDate, toDate)
}


# ------------------------------------------------------------------------------


.endOfPeriodStats =
function(x, nYearsBack = c("1y", "2y", "3y", "5y", "10y", "YTD"),
aggregate = c("monthly", "quarterly"), align = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns series statistics back to a given period

    # Arguments:
    #   x - a daily 'timeSeries' object of returns
    #   nYearsBack - a period string. How long back should the series
    #       be extracted?

    # Note:
    #   Add "1m"

    # FUNCTION:

    # Match Arguments:
    nYearsBack = match.arg(nYearsBack)

    # Should the series be aligned:
    if (align) x = alignDailySeries(x)

    # Series:
    Series = .endOfPeriodSeries(x, nYearsBack = nYearsBack,
        aggregate = aggregate, align = FALSE)

    # Internal Function:
    .cl.vals <- function(x, ci) {
        x = x[!is.na(x)]
        n = length(x)
        if (n <= 1)
            return(c(NA, NA))
        se.mean = sqrt(var(x)/n)
        t.val = qt((1 - ci)/2, n - 1)
        mn = mean(x)
        lcl = mn + se.mean * t.val
        ucl = mn - se.mean * t.val
        c(lcl, ucl)
    }

    # Statistics:
    for (i in 1:ncol(Series))
    {
        # Basic Statistics:
        X = as.vector(Series[, i])
        X.length = length(X)
        X = X[!is.na(X)]
        X.na = X.length - length(X)
        ci = 0.95
        z = c(X.length, X.na, min(X), max(X),
            as.numeric(quantile(X, prob = 0.25, na.rm = TRUE)),
            as.numeric(quantile(X, prob = 0.75, na.rm = TRUE)),
            mean(X), median(X), sum(x), sqrt(var(X)/length(X)),
            .cl.vals(X, ci)[1], .cl.vals(X, ci)[2],
            var(X), sqrt(var(X)), skewness(X), kurtosis(X))
        znames = c("nobs", "NAs", "Minimum", "Maximum", "1. Quartile",
            "3. Quartile", "Mean", "Median", "Sum", "SE Mean",
            "LCL Mean", "UCL Mean", "Variance", "Stdev", "Skewness",
            "Kurtosis")
        stats1 = matrix(z, ncol = 1)
        row.names(stats) = znames

        # Monthly Return Statistics:
        xData = as.vector(x@Data)
        noNegativePeriods = length(xData[xData < 0 ])
        noPositivePeriods = length(xData[xData > 0 ])
        stats1 = rbind(stats1,
            worstPeriod = min(xData),
            negativeValues = noNegativePeriods,
            positiveValues = noPositivePeriods)

        MaximumDrawdown = NA
        TimeUnderWater = NA
        AnnualizedVolatility = NA
        SharpeRatio = NA
        InformationRatio = NA
        ValueAtRisk = NA
        ExpectedShortfall = NA

        # Bind:
        if (i > 1) {
            stats = cbind.data.frame(stats, stats1)
        } else {
            stats = stats1
        }
    }

    # Return Value:
    stats
}



# ------------------------------------------------------------------------------


.endOfPeriodBenchmarks =
function(x, benchmark = ncol(x),
nYearsBack = c("1y", "2y", "3y", "5y", "10y", "YTD"),
aggregate = c("monthly", "quarterly"), align = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns benchmarks back to a given period

    # Arguments:
    #   x - a daily 'timeSeries' object of returns
    #   nYearsBack - a period string. How long back should the series
    #       be extracted?

    # Note:
    #   Add "1m"

    # FUNCTION:

    # Match Arguments:
    nYearsBack = match.arg(nYearsBack)

    # Should the series be aligned:
    if (align) x = alignDailySeries(x)

    # Series:
    Series = .endOfPeriodSeries(x[, -benchmark], nYearsBack = nYearsBack,
        aggregate = aggregate, align = FALSE)
    y = Benchmark = .endOfPeriodSeries(x[, benchmark], nYearsBack = nYearsBack,
        aggregate = aggregate, align = FALSE)

    stats = NULL
    for (i in 1:ncol(Series))
    {
        # Gdet Series:
        x = Series[, i]

        # Compute Statistics:
        stats1 = c(
            TrackingError = NA,
            Alpha = NA,
            Beta = NA,
            CorrelationToBenchmark = NA
        )

        # Bind Results:
        stats = rbind(stats, stats1)
    }

    # Return Value:
    invisible()
}


################################################################################

