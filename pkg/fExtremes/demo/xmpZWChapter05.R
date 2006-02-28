

# CHAPTER 5: Modeling Extreme Values
#
#	5.1 Introduction
#	5.2 Modeling Maxima and Worst Cases
#	5.3 Modeling Extremes Over High Thresholds 
#   5.4 Hill's Non-parametric Estimator of Tail Index
#	5.5 References


################################################################################
# Chapter 5.1 - Introduction


	# Load Data - A numeric vector with "times" attribute:
	data(sp.raw)
	class(sp.raw)
	# Convert into a timeSeries object:
	sp.raw.ts = timeSeries(data = sp.raw, 
		charvec = as.character(attr(sp.raw, "times")), 
		units = "SP", tz = "GMT", FinCenter = "GMT")
	class(sp.raw.ts)
	head(sp.raw.ts)
	###
	
	# Create percentage return time series:
	spto87 = getReturns(sp.raw.ts, type = "discrete", percentage = TRUE)
	par(mfrow = c(2, 1), cex = 0.7)
	plot(sp.raw.ts, main = "Daily Closing Prices", ylab = "Price")
	plot(spto87, main = "Daily Percentage Returns", ylab = "Return")


	# Plot gev distributions and densities
	# CDFs:
	z.vals = seq(-5, 5, length = 200)
	cdf.f = ifelse((z.vals > -2), pgev(z.vals, xi = 0.5), 0)
	cdf.w = ifelse((z.vals < 2), pgev(z.vals, xi = -0.5), 1)
	cdf.g = exp(-exp(-z.vals))
	plot(z.vals, cdf.w, type = "l", xlab = "z", ylab = "H(z)")
	lines(z.vals, cdf.f, type = "l", lty = 2)
	lines(z.vals, cdf.g, type = "l", lty = 3)
	legend(-5, 1, legend = c("Weibull H(-0.5, 0, 1)", 
		"Frechet H(0.5, 0, 1)", "Gumbel H(0, 0, 1)"), lty = 1:3)
	# PDFs:
	pdf.f = ifelse((z.vals > -2), dgev(z.vals, xi = 0.5), 0)
	pdf.w = ifelse((z.vals < 2), dgev(z.vals, xi = -0.5), 0)
	pdf.g = exp(-exp(-z.vals))*exp(-z.vals)
	plot(z.vals, pdf.w, type = "l", xlab = "z", ylab = "h(z)")
	lines(z.vals, pdf.f, type = "l", lty = 2)
	lines(z.vals, pdf.g, type = "l", lty = 3)
	legend(-5.25, 0.4, legend = c("Weibull H(-0.5, 0, 1)", 
		"Frechet H(0.5, 0, 1)", "Gumbel H(0, 0, 1)"), lty = 1:3)


	# Analysis of Maxima Data

	
	# Analysis of S&P 500 Daily Returns
	# Jan 5, 1960 - Oct 16, 1987

	
	# Plot -1*(daily returns) upto 1987
	class(spto87)
	plot(-spto87)
	qqPlot(spto87, strip.text = "Daily returns on S&P 500", 
		xlab = "Quantiles of standard normal", 
		ylab = "Quantiles of S&P 500")

	qqnorm(spto87)

	# Compute annual maxima using aggregateSeries
	# and plot descriptive statistics

	annualMax.sp500 = aggregateSeries(-spto87, by = "years", 
	FUN = max)
	Xn = sort(seriesData(annualMax.sp500))
	par(mfrow = c(2, 2))
	plot(annualMax.sp500)
	hist(seriesData(annualMax.sp500), xlab = "Annual maximum")
	plot(Xn, -log(-log(ppoints(Xn))), xlab = "Annual maximum")
	tmp = records(-spto87)
	par(mfrow = c(1, 1))

	# Estimate GEV CDF using annual blocks of daily returns 
	gev.fit.year = gev(-spto87, block = "year")
	class(gev.fit.year)
	names(gev.fit.year)

	# Number of blocks
	gev.fit.year$n

	# plot block maxima
	plot(gev.fit.year$data, 
		main = "Annual block maxima of negative daily returns", 
		ylab = "Mn")

	# MLEs and estimated asymptotic standard errors
	gev.fit.year$par.ests
	gev.fit.year$par.ses

	# 95% CI for xi
	gev.fit.year$par.ests[1]-2*gev.fit.year$par.ses[1]
	gev.fit.year$par.ests[1]+2*gev.fit.year$par.ses[1]

	#
	# plot method: This is better executed from the command line.
	#

	# par(mfrow = c(1, 2))
	# plot(gev.fit.year)

	# follow Coles and do a histogram and density plot of block
	# maxima data using the fitted standardized extreme value
	# data

	# standardized block maxima
	Zn = (seriesData(gev.fit.year$data) - gev.fit.year$par.ests["mu"])/
	gev.fit.year$par.ests["sigma"]
	Zn = sort(Zn)
	# need to figure out how to plot a histogram and density estimate
	# together. Use dgev to compute density values
	gev.density = dgev(Zn, xi = gev.fit.year$par.ests["xi"])
	plot(Zn, gev.density, type = "l")
	hist(Zn)

	# do qq-plot against gumbel for standardized maxima
	# see Embrechts pg 293

	plot(Zn, -log(-log(ppoints(Zn))))

	# compute probability next year's maximum being a new record
	1- pgev(max(gev.fit.year$data), 
	xi = gev.fit.year$par.ests["xi"], 
	mu = gev.fit.year$par.ests["mu"], 
	sigma = gev.fit.year$par.ests["sigma"])

	# estimate 40 year return level
	rlevel.year.40 = rlevel.gev(gev.fit.year, k.blocks = 40)
	class(rlevel.year.40)
	names(rlevel.year.40)
	rlevel.year.40$rlevel

	rlevel.year.40 = rlevel.gev(gev.fit.year, k.blocks = 40, 
	type = "RetLevel")
	names(rlevel.year.40)

	# compute return quantile
	(1-(1/40))^(1/260)

	# Estimate GEV CDF using quarterly blocks of daily returns 
	gev.fit.quarter = gev(-spto87, block = "quarter")
	gev.fit.quarter$n
	gev.fit.quarter$par.ests
	gev.fit.quarter$par.ses

	# 95% CI for xi
	gev.fit.quarter$par.ests[1]-2*gev.fit.quarter$par.ses[1]
	gev.fit.quarter$par.ests[1]+2*gev.fit.quarter$par.ses[1]

	# probability that next quarter's maximum exceeds all previous
	# maxima
	1- pgev(max(gev.fit.quarter$data), 
	xi = gev.fit.quarter$par.ests["xi"], 
	mu = gev.fit.quarter$par.ests["mu"], 
	sigma = gev.fit.quarter$par.ests["sigma"])

	# 40 quarter return level
	rlevel.40.q = rlevel.gev(gev.fit.quarter, k.blocks = 40)

	# 40 year or 160 quarter return level
	rlevel.160.q = rlevel.gev(gev.fit.quarter, k.blocks = 160, 
	type = "RetLevel")
	rlevel.160.q

	# estimate GEV CDF for maximum data (for short losses)
	gev.fit.sa = gev(spto87, "semester")
	names(gev.fit.sa)

	# number of blocks
	gev.fit.sa$n
	# block maxima
	gev.fit.sa$data
	# mles and estimated asymptotic standard errors
	gev.fit.sa$par.ests
	gev.fit.sa$par.ses

	# fit gumbel distribution to annual maxima

	gumbel.fit.year = gumbel(-spto87, block = "year")
	class(gumbel.fit.year)
	names(gumbel.fit.year)
	gumbel.fit.year$par.ests
	gumbel.fit.year$par.ses

	#
	# The following is better executed from the Command line.
	#

	# par(mfrow = c(1, 2))
	# plot(gumbel.fit.year)
	#
	# 1- pgev(max(gumbel.fit.year$data), xi = 0, 
	# mu = gumbel.fit.year$par.ests["mu"], 
	# sigma = gumbel.fit.year$par.ests["sigma"])
	#
	# rlevel.gev(gumbel.fit.year, k.blocks = 40)
	# par(mfrow = c(1, 1))


############################################################################
# Modeling Excesses Over Thresholds
# using the Dainish Fire Loss Data


	plot(danish, main = "Fire Loss Insurance Claims", 
	ylab = "Millions of Danish Krone")

	# plot various GPD values
	# CDFs
	par(mfrow = c(1, 2))
	y.vals = seq(0, 8, length = 200)
	cdf.p = pgpd(y.vals, xi = 0.5)
	cdf.p2 = ifelse((y.vals < 2), pgpd(y.vals, xi = -0.5), 1)
	cdf.e = 1-exp(-y.vals)
	plot(y.vals, cdf.p, type = "l", xlab = "y", ylab = "G(y)", 
	ylim = c(0, 1))
	lines(y.vals, cdf.e, type = "l", lty = 2)
	lines(y.vals, cdf.p2, type = "l", lty = 3)
	legend(1, 0.2, legend = c("Pareto G(0.5, 1)", "Exponential G(0, 1)", 
	"Pareto II G(0.5, 1)"), lty = 1:3)

	# PDFs
	pdf.p = dgpd(y.vals, xi = 0.5)
	pdf.p2 = ifelse((y.vals < 2), dgpd(y.vals, xi = -0.5), 0)
	pdf.e = exp(-y.vals)
	plot(y.vals, pdf.p, type = "l", xlab = "y", ylab = "g(y)", 
	ylim = c(0, 1))
	lines(y.vals, pdf.e, type = "l", lty = 2)
	lines(y.vals, pdf.p2, type = "l", lty = 3)
	legend(2, 1, legend = c("Pareto g(0.5, 1)", "Exponential g(0, 1)", 
	"Pareto II g(-0.5, 1)"), lty = 1:3)
	par(mfrow = c(1, 1))

	# exploratory analysis

	# qq-plots with exponential reference distribution
	par(mfrow = c(1, 2))
	qplot(-spto87, threshold = 1, main = "S&P 500 negative returns")
	qplot(danish, threshold = 10, main = "Danish fire losses")

	# mean excess plots
	par(mfrow = c(1, 2))
	me.sp500 = meplot(-spto87)
	me.dainsh = meplot(danish)
	class(me.sp500)
	colIds(me.sp500)

	# fit gpd to sp500 negative returns with u = 1
	# only plot method is available

	gpd.sp500.1 = gpd(-spto87, threshold = 1)
	class(gpd.sp500.1)
	names(gpd.sp500.1)

	gpd.sp500.1$upper.converged
	gpd.sp500.1$upper.thresh
	gpd.sp500.1$n.upper.exceed
	gpd.sp500.1$p.less.upper.thresh

	gpd.sp500.1$upper.par.ests
	gpd.sp500.1$upper.par.ses

	# This is better executed from the command line.
	# plot(gpd.sp500.1)
	shape(-spto87, end = 600)

	# fit gpd to danish fire insurance data
	gpd.danish.10 = gpd(danish, threshold = 10)
	gpd.danish.10$n.upper.exceed
	gpd.danish.10$p.less.upper.thresh
	gpd.danish.10$upper.par.ests
	gpd.danish.10$upper.par.ses

	par(mfrow = c(1, 2))
	tailplot(gpd.danish.10)
	shape(danish)

	# estimating VaR and ES for sp500 data
	# use quant, gpd.q, gpd.sfall, riskmeasures
	riskmeasures(gpd.sp500.1, c(0.95, 0.99))
	gpd.q(0.99, ci.type = "likelihood")
	gpd.q(0.99, ci.type = "wald")
	gpd.sfall(0.99)
	gpd.sfall(0.99, ci.p = "wald")

	# compute var and es assume normally distributed data
	# make sure to compute confidence intervals
	sp500.mu = mean(-spto87)
	sp500.sd = sqrt(var(-spto87))
	var.95 = sp500.mu + sp500.sd*qnorm(0.95)
	var.99 = sp500.mu + sp500.sd*qnorm(0.99)
	var.95
	var.99

	z95 = (var.95 - sp500.mu)/sp500.sd
	z99 = (var.99 - sp500.mu)/sp500.sd
	es.95 = sp500.mu + sp500.sd*dnorm(z95)/(1-pnorm(z95))
	es.99 = sp500.mu + sp500.sd*dnorm(z99)/(1-pnorm(z99))
	es.95
	es.99

	nobs = nrow(spto87)

	se.var.99 = sqrt((var.95/nobs)*(1+0.5*qnorm(0.99)^2))

	# estimating VaR and ES for danish fire loss data
	riskmeasures(gpd.danish.10, c(0.95, 0.99))

	danish.mu = mean(danish)
	danish.sd = sqrt(var(danish))
	var.95 = danish.mu + danish.sd*qnorm(0.95)
	var.99 = danish.mu + danish.sd*qnorm(0.99)
	var.95
	var.99

	z95 = (var.95 - danish.mu)/danish.sd
	z99 = (var.99 - danish.mu)/danish.sd
	es.95 = danish.mu + danish.sd*dnorm(z95)/(1-pnorm(z95))
	es.99 = danish.mu + danish.sd*dnorm(z99)/(1-pnorm(z99))
	es.95
	es.99

	tailplot(gpd.danish.10)
	gpd.q(0.99, plot = TRUE)
	gpd.sfall(0.99, plot = TRUE)

	quant(danish, p = 0.99)

	 
	# Hill Analysis of sp500 Data
	 

	args(hill)
	hill(-spto87, option = "xi", end = 500)
	hill(s pto87, option = "xi", end = 500)

	 
	# Hill analysis of danish loss data
	 

	class(danish)
	plot(danish)
	hill.danish = hill(danish, option = "xi")
	idx = (
		hill.danish$threshold > =  9.8 & 
		hill.danish$threshold < =  10.2 )
	hill.danish[idx, ]
	hill.danish.q = hill(danish, option = "quantile", p = 0.99, end = 500)


################################################################################

	