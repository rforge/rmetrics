################################################################################
# About


.aboutRmetrics = 
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   fBasics Popup Menu
    
    # FUNCTION:
    
    # About:
    helpTopic <<- ""
    ans <- tkmessageBox(
        title = "Rmetrics",
        message = paste(
            "Rmetrics 2.2.1 \n",
            "An Environment for \n",
            "Teaching Financial Engineering and Computational Finance \n",
            "(C) 1999-2006, Diethelm Wuertz, GPL"),
        icon = "info", 
        type = "ok")
        
    # Return Value:
    invisible(ans)
}

        
################################################################################
# Rmetrics


.wwwRmetrics = 
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   fBasics Popup Menu
    
    # FUNCTION:
    
    # Rmetrics:
    helpTopic <<- ""
    ans = shell.exec("http://www.rmetrics.org")
    
    # Return Value:
    invisible(ans)
}
    

################################################################################
# HTML Help


.helpStart = 
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   fBasics Popup Menu
    
    # FUNCTION:
    
    # HTML Help:
    helpTopic <<- ""
    ans = help.start()
    
    # Return Value:
    invisible(ans)
}           

        
################################################################################



.miscMenu.lsCmd = function()
{
    # Description:
    #   List Objects Menu
    
    print(ls())
    output = as.character(ls())
    print(output)
    for (i in 1:length(output)) 
        tkinsert(txt, "end", paste(output[i], "\n"))
    tkinsert(txt, "end", "\n")
    
    print(ls(pos = -1))
    output = as.character(ls(pos = -1))
    print(output)
    for (i in 1:length(output)) 
        tkinsert(txt, "end", paste(output[i], "\n"))
    tkinsert(txt, "end", "\n")
}


# ******************************************************************************
# Example Data


.miscMenu.ExampleDataCmd.1 <- 
function(...) 
{
    .miscMenu.ExampleDataCmd(1)
}


# ------------------------------------------------------------------------------


.miscMenu.ExampleDataCmd.2 <- 
function(...) 
{
    .miscMenu.ExampleDataCmd(2)
}


# ------------------------------------------------------------------------------


.miscMenu.ExampleDataCmd.3 <- 
function(...) 
{
    .miscMenu.ExampleDataCmd(3)
}


# ------------------------------------------------------------------------------


.miscMenu.ExampleDataCmd.4 <- 
function(...) 
{
    .miscMenu.ExampleDataCmd(4)
}


# ------------------------------------------------------------------------------


.miscMenu.ExampleDataCmd.5 <- 
function(...) 
{
    .miscMenu.ExampleDataCmd(5)
}


# ------------------------------------------------------------------------------


.miscMenu.ExampleDataCmd.6 <- 
function(...) 
{
    .miscMenu.ExampleDataCmd(6)
}


# ------------------------------------------------------------------------------


.miscMenu.ExampleDataCmd <- 
function(choice) 
{   
    # Description:
    #   Example Data Menu
    
    # Settings:
    what = c(
        "Normal Innovations", 
        "FGN", 
        "ARMA(1,1)", 
        "GARCH(1,1)", 
        "NYSE",
        "MSFT|SP500")
    seriesTitle <<- paste(what[choice], "Series")
        
    # Data:
    if (choice == 1) {
        x <<- rnorm(n = 1000)
    }
    if (choice == 2) {
        x <<- fgnSim()
    }
    if (choice == 3) {
        x <<- armaSim(n = 1000)
    }
    if (choice == 4) {
        x <<- garchSim(n = 1000)
    }
    if (choice == 5) { 
        data(nyseres)
        x <<- nyseres[, 1] 
    }
    if (choice == 6) {
        data(singleIndex.dat)
        x <<- timeSeries(
            data = as.matrix(singleIndex.dat[, 2:3]), 
            charvec = as.character(singleIndex.dat[, 1]),
            units = c("MSFT", "SP500"),
            format = "%d-%b-%Y", 
            FinCenter = "GMT" )
    }
    
    # Info:
    activeDataSet <<- paste("x =", what[choice])
    infoLabelText <<- tclVar(paste("Active Series Data:", activeDataSet))
    tkconfigure(infoLabel, textvariable = infoLabelText)
    tkgrid(infoLabel)
    
    # Output:
    tkinsert(txt, "end", paste(what[choice], "Series\n\n"))
}


################################################################################