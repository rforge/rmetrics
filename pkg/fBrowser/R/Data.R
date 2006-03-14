

.BasicStatistics.sp500monthly = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Example timeSeries: x = Monthly SP500 Returns
    tkGetData(Data = "sp500Monthly", infoName = "End of Month SP500 Returns")
}


# ------------------------------------------------------------------------------


.fBasics.PlotFunctions.sp500Monthly = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x = Monthly SP500 Returns:
    tkGetData(Data = "sp500Monthly", infoName = "Monthly SP500 Returns")
}


# ------------------------------------------------------------------------------


.fBasics.PlotFunctions.msftsp500Monthly = 
function() 
{   # A function implemented by Diethelm Wuertz

    # * Example timeSeries: x = MSFT|SP500 Returns:
    tkGetData(Data = "msftsp500Monthly", infoName = "Monthly MSFT|SP500 Returns")
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.nyseDaily = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x - Daily NYSE Returns
    tkGetData(Data = "nyseDaily", infoName = "Daily NYSE Returns")
} 


# ------------------------------------------------------------------------------



.fBasics.NormalityTests.sp500Monthly = 
function() 
{   # A function implemented by Diethelm Wuertz

    # * Example timeSeries: - x = Monthly SP500 Returns
    tkGetData(Data = "sp500Monthly", infoName = "Monthly SP500 Returns")
}


# ------------------------------------------------------------------------------



.fBasics.BivariateTests.msftsp500Monthly  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # * Example timeSeries: x = MSFT|SP500 Returns
    tkGetData(Data = "msftsp500Monthly", 
        infoName = "Monthly MSFT|SP500 Returns")
}


# ------------------------------------------------------------------------------


.fBasics.StylizedFacts.nyseDaily = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x = Daily NYSE Returns
    tkGetData(Data = "nyseDaily", infoName = "Daily NYSE Returns")
}


# ------------------------------------------------------------------------------




.fCalendar.TimeDateMethods.dates = 
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


.fCalendar.TimeDateMethods.times = 
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


# ------------------------------------------------------------------------------


.fCalendar.TimeSeriesClass.msftsp500Monthly = 
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


.fCalendar.HighFrequencyData.1 = 
function()
{   # A function implemented by Diethelm Wuertz

    # * Example data.frame: x = FX Reuters USDTHB:
    data(usdthb)
    x <<- tkSaveAsX(data = usdthb, infoName = "FX Reuters USDTHB")
}


# ------------------------------------------------------------------------------


.fSeries.ArmaModelling.nyseDaily =  
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


.fSeries.GarchModelling.dem2gbpDaily =  
function()
{   # A function implemented by Diethelm Wuertz

    # BMW Data Set:
    myFunction = function(object2x, report) {
        object <<- tkGetDemoData(Data = "dem2gbp", report = report) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = TRUE,
            report = FALSE),
        infoName = "DEMGBP Daily Data Set" )       
}



# ------------------------------------------------------------------------------



.fSeries.TimeSeriesTests.1 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Example timeSeries: x - NYSE log Returns
    tkGetData(Data = "nyseDaily", infoName = "Daily NYSE Returns")
}
   

# ------------------------------------------------------------------------------


.fMultivar.BenchmarkAnalysis.1 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x = SP500 Index
    tkGetData(Data = "sp500IndexMonthly", infoName = "Monthly SP500 Index")
}


# ------------------------------------------------------------------------------




.fMultivar.RollingAnalysis.sp500IndexMonthly = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x = SP500 Index
    tkGetData(Data = "sp500IndexMonthly", infoName = "Monthly SP500 Index")
}   


# ------------------------------------------------------------------------------


      
.fExtremes.ExtremesPlots.bmwDaily = 
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


.fExtremes.DataPreprocessing.bmwDaily =  
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


.fExtremes.GEV.bmwDaily = 
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


.fExtremes.GPD.bmwDaily = 
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

