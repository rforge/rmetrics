# fBasics: sp500Monthly nyseDaily msftsp500Monthly
# fCalendar: msftsp500Monthly [USDTHB] 
# fSeries: nyseDaily dem2gbpDaily 



.fData.nyseDaily =  
function()
{   # A function implemented by Diethelm Wuertz

    # BMW Data Set:
    myFunction = function(object2x, report) {
        object <<- tkGetDemoData(Data = "nyseDaily", report = report) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = TRUE,
            report = FALSE),
        infoName = "Nyse Daily Data Set" )       
}


# ------------------------------------------------------------------------------


.fData.dem2gbpDaily =  
function()
{   # A function implemented by Diethelm Wuertz

    # BMW Data Set:
    myFunction = function(object2x, report) {
        object <<- tkGetDemoData(Data = "dem2gbp", report = report, 
        FUN = function(x) as.ts(as.vector(t(x))) ) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = TRUE,
            report = FALSE),
        infoName = "DEMGBP Daily Data Set" )       
}


# ------------------------------------------------------------------------------


.fData.bmwDaily =  
function()
{   # A function implemented by Diethelm Wuertz

    # BMW Data Set:
    myFunction = function(object2x, report) {
        object <<- tkGetDemoData(Data = "bmwDaily", report = report) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = TRUE,
            report = FALSE),
        infoName = "BMW Daily Data Set" )       
}


# ------------------------------------------------------------------------------


.fData.sp500Monthly = 
function()
{   # A function implemented by Diethelm Wuertz

    # MSFT and SP%00 Data Sets:
    myFunction = function(object2x, report) {
        object <<- tkGetDemoData(Data = "p500Monthly", report = report) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = TRUE,
            report = FALSE),
        infoName = "SP500 Index Monthly Data" )       
}


# ------------------------------------------------------------------------------


.fData.sp500IndexMonthly = 
function()
{   # A function implemented by Diethelm Wuertz

    # MSFT and SP%00 Data Sets:
    myFunction = function(object2x, report) {
        object <<- tkGetDemoData(Data = "p500IndexMonthly", report = report) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = TRUE,
            report = FALSE),
        infoName = "SP500 Index Monthly Data" )       
}


# ------------------------------------------------------------------------------


.fData.msftsp500Monthly = 
function()
{   # A function implemented by Diethelm Wuertz

    # MSFT and SP%00 Data Sets:
    myFunction = function(object2x, report) {
        object <<- tkGetDemoData(Data = "msftsp500Monthly", report = report) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = TRUE,
            report = FALSE),
        infoName = "msftsp500 Monthly Data Set" )       
}


# ------------------------------------------------------------------------------


.fData.randomDates = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeDate: x = 12 Random Dates in Current Year
    myFunction = function(FinCenter, object2x, report) {
        y = rep(currentYear, 12)
        d = trunc(runif(12, 1, 29))
        m = trunc(runif(12, 1, 13))
        object <<- timeCalendar(y = y, m = m, d = d, FinCenter = FinCenter)
        if (report) tkTitle("12 Random Dates")
        object }
    tkExecute(
        fun = myFunction,
        params = list( 
            FinCenter = "GMT",
            object2x = TRUE,
            report = TRUE),
        infoName = "12 Random Dates in Current Year" )          
}


# ------------------------------------------------------------------------------


.fData.randomTimes = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeDate: x = 12 Random Times in Current Year 
    myFunction = function(FinCenter, object2x, report) {
        y = rep(2005, 12)
        d = trunc(runif(12, 1, 29))
        m = trunc(runif(12, 1, 13))
        h = trunc(runif(12, 0, 24))
        min = trunc(runif(12, 0, 60))
        s = trunc(runif(12, 0, 60))
        object <<- timeCalendar(y = y, m = m, d = d, h = h, 
            min = min, s = s, FinCenter = FinCenter)
        print(report)
        if (report) tkTitle("12 Random Dates")
        object }
    tkExecute(
        fun = myFunction,
        params = list( 
            FinCenter = "GMT",
            object2x = TRUE,
            report = TRUE ),
        infoName = "12 Random Times in Current Year" )          
}


# ##############################################################################


.fCalendar.HighFrequencyData.1 = 
function()
{   # A function implemented by Diethelm Wuertz

    # * Example data.frame: x = FX Reuters USDTHB:
    data(usdthb)
    x <<- tkSaveAsX(data = usdthb, infoName = "FX Reuters USDTHB")
}


################################################################################

