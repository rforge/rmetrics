MAJOR        = "0"
MINOR        = "1"
REVISION     = unlist(strsplit("$Revision: 0 $", split=" "))[2]
VERSION      = paste(MAJOR, MINOR, REVISION, sep=".")
VERSION.DATE = "Released 04 Jan 2009"
COPYRIGHT    = "Copyright (C) 2009 rmetrics.org" 


#------------------------------------------------------------------------------- 
#------- MainWindow ------------------------------------------------------------ 
#------------------------------------------------------------------------------- 


showMainWindow <-
function() 
{
	# load glade file
	glade = gladeXMLNew( file.path(gladePath, "rmetrics.glade"), 
	    root='MainWindow')
	mainWindow = glade$getWidget('MainWindow') 
	mainWindow$setTitle("Rmetrics")
	mainWindow$setDefaultSize(650, 400)


	# load button
	closeButton =  glade$getWidget('closeButton')
	newButton   =  glade$getWidget('newButton')
	editButton  =  glade$getWidget('editButton')
	plotButton  =  glade$getWidget('plotButton')
	printButton =  glade$getWidget('printButton')
	gSignalConnect(closeButton, 'clicked', closeApp)
	gSignalConnect(newButton,   'clicked', showNewWindow)
	gSignalConnect(editButton,  'clicked', showEditWindow1)
	gSignalConnect(printButton, 'clicked', showPrintWindow)
	gSignalConnect(plotButton,  'clicked', plotPortfolio)

	# load treeWidget
	treeView <<-  glade$getWidget('treeView')
	model <<-  gtkTreeStoreNew("gchararray")
	treeView$setModel(model)
	model <<-  gtkTreeViewGetModel(treeView)
	# column for holiday names
	renderer = gtkCellRendererTextNew()
	renderer$set(xalign = 0.0)
	col.offset = treeView$insertColumnWithAttributes(-1, "Portfolioname", 
	    renderer, text = 0)
	column = treeView$getColumn(col.offset - 1)
	column$setClickable(TRUE)
	listPortfolios()


	mainWindow$showAll()
}


getSelectedTreeViewEntry <- 
function(treeView) 
{
	model = treeView$getSelection()$getSelected()$model
	iter = treeView$getSelection()$getSelected()$iter
	model$getValue(iter, 0)$value
}


#------------------------------------------------------------------------------- 
#------- list portfolios ------------------------------------------------------- 
#------------------------------------------------------------------------------- 


listPortfolios <-
function() 
{
	model$clear()
	portfolios = as.list( ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv),
	    function(x){class(get(x))}) == "fPORTFOLIO"] )
	for(i in portfolios) {
 		iter <- model$append(NULL)$iter
 		model$set(iter, 0, i)
	}
	treeView$setModel(model)
}


#------------------------------------------------------------------------------- 
#-------- NewWindow ------------------------------------------------------------ 
#------------------------------------------------------------------------------- 


showNewWindow <-
function(input) 
{
	nameEntry = gtkEntry()
	glade <- gladeXMLNew( file.path(gladePath, "new.glade"),  root='NewWindow')
	newWindow=glade$getWidget('NewWindow') 
	newWindow$setTitle("New Portfolio")
	newWindow$setDefaultSize(300, 50)
	cancelButton = glade$getWidget('cancelButton')
	okButton = glade$getWidget('okButton')
	nameEntry <<- glade$getWidget('nameEntry')

	gSignalConnect(cancelButton, 'clicked', destroyWindow )
	gSignalConnect(okButton, 'clicked', showEditWindow0)

	newWindow$showAll()
}



#------------------------------------------------------------------------------- 
#------- EditWindow ------------------------------------------------------------ 
#------------------------------------------------------------------------------- 


showEditWindow0 <- 
function(button) 
{
	portfolioName0 <<- nameEntry$getText()
	button$getParent()$getParent()$getParent()$destroy()
	showEditWindow(portfolioName0)
}


showEditWindow1 <- 
function(button) 
{ 
	if( treeView$getSelection()$countSelectedRows() != 0) {
		treeViewEntry = getSelectedTreeViewEntry( treeView )
		showEditWindow(treeViewEntry)
	}
}

showEditWindow <-
function(portfolioName0) 
{
	portfolioName <<- portfolioName0

	glade <- gladeXMLNew( file.path(gladePath, "edit.glade"), root='EditWindow')
	editWindow=glade$getWidget('EditWindow') 
	editWindow$setTitle("Edit Portfolio")
	editWindow$setDefaultSize(300, 50)
	cancelButton = glade$getWidget('cancelButton')
	okButton = glade$getWidget('okButton')



	comboBoxes = list( "type", "optimize", "estimator", "weights", 
	    "targetReturn", "targetRisk", "solver", "objective", "trace", 
	    "data", "constraints" )

	spinButtons = list("riskFreeRate", "nFrontierPoints")


	for(i in comboBoxes) {
		elementName = paste(i, "ComboBox" , sep="")
		assign(elementName, glade$getWidget(elementName), envir = .GlobalEnv )
	}
	for(i in spinButtons) {
		elementName = paste(i, "SpinButton" , sep="")
		assign(elementName, glade$getWidget(elementName), envir = .GlobalEnv )
	}



	if(exists(portfolioName)) {
		 for(i in comboBoxes) {
			if( i != "objective" && i != "data" && i != "constraints") {
				elementName = paste(i, "ComboBox" , sep="")
				functionName = paste("get", toupper( substr(i,1,1)), 
				    substr(i, 2, nchar(i) ) , sep="")
				getFunc = match.fun(functionName)
				value = getFunc(get(portfolioName))
				i = 0
				get(elementName)$setActive(i)
				while( get(elementName)$getActiveText() != value) {
					i = i+1
					get(elementName)$setActive(i)
					if (get(elementName)$getActive() == -1) {
						print(paste("Could not sync", elementName, 
						    "with portfolio value!") )
						get(elementName)$setActive(0)
						break
					}
				}
			}
		 }
		 for(i in spinButtons) {
			elementName = paste(i, "SpinButton" , sep="")
			functionName = paste("get", toupper( substr(i,1,1)), 
			    substr(i, 2, nchar(i) ) , sep="")
			getFunc = match.fun(functionName)
			value = getFunc(get(portfolioName))
			print(value)
			get(elementName)$setValue(value)
		}
	}
	else {
		for(i in comboBoxes) {
			elementName = paste(i, "ComboBox" , sep="")
			get(elementName)$setActive(0)
		}
	 }

  	gSignalConnect(okButton, 'clicked', createPortfolio )
	gSignalConnect(cancelButton, 'clicked', destroyWindow )



	editWindow$showAll()

}


#------------------------------------------------------------------------------- 
#------- createPortfolio ------------------------------------------------------- 
#------------------------------------------------------------------------------- 


createPortfolio <-
function(input) 
{

	tmpSpec = portfolioSpec()
	setType( tmpSpec )<-typeComboBox$getActiveText()
	setOptimize( tmpSpec )<-optimizeComboBox$getActiveText()
	setEstimator( tmpSpec )<-estimatorComboBox$getActiveText()
	setWeights( tmpSpec )<-weightsComboBox$getActiveText()
	setTargetReturn( tmpSpec )<-targetReturnComboBox$getActiveText()
	setTargetRisk( tmpSpec )<-targetRiskComboBox$getActiveText()
	setRiskFreeRate( tmpSpec )<-riskFreeRateSpinButton$getValueAsInt()
	setNFrontierPoints( tmpSpec )<-nFrontierPointsSpinButton$getValueAsInt()
	setSolver(tmpSpec)<-solverComboBox$getActiveText()
	setObjective(tmpSpec)<-objectiveComboBox$getActiveText()
	setTrace(tmpSpec)<-traceComboBox$getActiveText()
	tmpData = as.timeSeries(dataComboBox$getActiveText())   
	tmpConstraints = constraintsComboBox$getActiveText()
	assign(portfolioName, portfolioFrontier(tmpData, tmpSpec, tmpConstraints ), 
	    envir = .GlobalEnv )

	input$getParent()$getParent()$getParent()$destroy()
	listPortfolios()
	
}


#------------------------------------------------------------------------------- 
#------- PrintWindow ----------------------------------------------------------- 
#------------------------------------------------------------------------------- 


showPrintWindow <-
function(input2) 
{
	glade <- gladeXMLNew( file.path(gladePath, "print.glade"), 
	    root='PrintWindow')
	printWindow=glade$getWidget('PrintWindow') 
	printWindow$setTitle("Print Portfolio")
	printWindow$setDefaultSize(300, 50)

	treeViewEntry = getSelectedTreeViewEntry( treeView )


	printTextView = glade$getWidget("printTextView")
	printTextBuffer = gtkTextBuffer()
	printTextBuffer$setText( getType(get(treeViewEntry)) )
	printTextView$setBuffer( printTextBuffer )


	nameLabel = glade$getWidget("nameLabel")
	nameLabel$setText( paste("Portfolio: ", 
	    getSelectedTreeViewEntry( treeView ) ) )


	printComboBox = glade$getWidget( "printComboBox" )
	# printComboBox.connect("changed", printValues, printComboBox )
	printComboBox$setActive(0)

 	printWindow$showAll()
}


#------------------------------------------------------------------------------- 
#------- slots ----------------------------------------------------------------- 
#------------------------------------------------------------------------------- 


# --- showEdit --- #
destroyWindow <-
function(input) 
{ 
	input$getParent()$getParent()$getParent()$destroy()
}



# --- closeApp --- #
closeApp <-
function(input) 
{ 
	quit()
}



# --- plotPortfolio --- #
plotPortfolio <-
function(input) 
{ 
	if( treeView$getSelection()$countSelectedRows() != 0) {
		treeViewEntry = getSelectedTreeViewEntry( treeView )
		frontierPlot(get(treeViewEntry))
	}
}



#------------------------------------------------------------------------------- 
#-- start ---------------------------------------------------------------------- 
#------------------------------------------------------------------------------- 


getGladePath <- 
function() 
{
	gladePath <<- file.path(.path.package(package="guiPortfolio")[1], "glade")
}


portfolioGui <- 
function() 
{
	getGladePath()
	showMainWindow()
}


################################################################################

