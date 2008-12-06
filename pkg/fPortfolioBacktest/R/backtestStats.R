################################################################################
# PLOT FUNCTIONS:		DESCRIPTION:
# backtestStats			Wrapper function for calculating rolling stats
#
# rollingSigma			Rolling portfolio Sigma risk
# rollingVaR			Rolling Value at Risk
# rollingCVaR			Rolling Conditional Value at Risk
# rollingDar			Rolling Drawdowns at Risk
# rollingCDaR			Rolling Conditional Drawdowns at Risk
# rollingRiskBudgets	Rolling portfolio risk budgets
################################################################################

backtestStats = 
  function(object, stats = "rollingSigma", ...){
	
	# Extract the portfolios into a list:
	portfolios = object$strategyList
	
	# perfrorm stats:
	backtestFun = match.fun(stats)
	ans = backtestFun(portfolios, ...)
	
	# return
	ans
}

# -------------------------------------------------------------------------------------- #


rollingSigma = 
  function(portfolios){
	ans = sapply(portfolios, function(x) getTargetRisk(x)[,"Sigma"])
	dates = sapply(portfolios, function(x) as.character(rev(rownames(getSeries(x)))[1]))
	
	# return
	timeSeries(ans, charvec = dates, units = "Sigma")
}

# -------------------------------------------------------------------------------------- #

rollingVaR = 
 function(portfolios, alpha = 0.05){
	
	# calculate VaR for one portfolio:
  	.var = 
  	  function(x, alpha){
  		R = getSeries(x) %*% t(getWeights(x))
		quantile.default(R, probs = alpha)}
	
	# Calculates VaR for all portfolios:
	ans = sapply(portfolios, FUN = .var, alpha = alpha)
	
	# Extracts the dates:
	dates = sapply(portfolios, function(x) as.character(rev(rownames(getSeries(x)))[1]))

	# Return:
	timeSeries(ans, charvec = dates, units = paste("VaR",alpha,sep = "."))
} 

# -------------------------------------------------------------------------------------- #

rollingCVaR = 
  function(portfolios, alpha = 0.05){
  	
	# Calculate CVaR for one portfolio:
  	.cvar = 
  	  function(x, alpha){
  		R = getSeries(x) %*% t(getWeights(x))
		z = quantile.default(R, probs = alpha)
		mean(as.numeric(R)[R <= z], na.rm = TRUE)
		}
  	
	# Calculate CVaR for all portfolios:  	
	ans = sapply(portfolios, FUN = .cvar, alpha = alpha)
	
	# Extract the dates:
	dates = sapply(portfolios, function(x) as.character(rev(rownames(getSeries(x)))[1]))
	
	# Return:
	timeSeries(ans, charvec = dates, units = paste("CVaR",alpha, sep = "."))
} 

# -------------------------------------------------------------------------------------- #

rollingDaR = 
 function(portfolios, alpha = 0.05){
	
	# calculate VaR for one portfolio:
  	.dar = 
  	  function(x, alpha){
  		R = getSeries(x) %*% t(getWeights(x))
  		dd = drawdowns(as.timeSeries(R)/100)
		quantile.default(dd, probs = alpha)}
	
	# Calculates DaR for all portfolios:
	ans = sapply(portfolios, FUN = .dar, alpha = alpha)
	
	# Extracts the dates:
	dates = sapply(portfolios, function(x) as.character(rev(rownames(getSeries(x)))[1]))

	# Return:
	timeSeries(ans, charvec = dates, units = paste("DaR",alpha, sep = "."))
} 

# -------------------------------------------------------------------------------------- #

rollingCDaR = 
  function(portfolios, alpha = 0.05){
  	
	# Calculate CDaR for one portfolio:
  	.cdar = 
  	  function(x, alpha){
  		R = getSeries(x) %*% t(getWeights(x))
  		dd = drawdowns(as.timeSeries(R)/100)
		z = quantile.default(dd, probs = alpha)
		mean(as.numeric(R)[R <= z], na.rm = TRUE)
		}
  	
	# Calculate CVaR for all portfolios:  	
	ans = sapply(portfolios, FUN = .cdar, alpha = alpha)
	
	# Extract the dates:
	dates = sapply(portfolios, function(x) as.character(rev(rownames(getSeries(x)))[1]))
	
	# Return:
	timeSeries(ans, charvec = dates, units = paste("CDaR", alpha, sep = "."))
} 

# -------------------------------------------------------------------------------------- #

rollingRiskBudgets = 
	function(portfolios){
		
	nPortfolios = length(portfolios)
	assetNames = colnames(getSeries(portfolios[[1]]))
	
	ans = NULL
	for (i in 1:nPortfolios){
		ans = rbind(ans, getCovRiskBudgets(portfolios[[i]]))
		}	
		
	# Extract the dates:
	dates = sapply(portfolios, function(x) as.character(rev(rownames(getSeries(x)))[1]))
	
	# Return:
	timeSeries(ans, charvec = dates, units = assetNames)				
}

# -------------------------------------------------------------------------------------- #

