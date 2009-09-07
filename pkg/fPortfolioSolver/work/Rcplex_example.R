library("fPortfolio")
library("fPortfolioSolver")

##########################################################################
## 1.) quadratc programming (QP)

## classic Markovitz mean-variance portfolio
## calculate minimum risk given target return

data("LPP2005.RET")
LPP <- LPP2005.RET[, 1:6]

mvSpec <- portfolioSpec()
mvSpec

constraints <- "LongOnly"

mvPf <- minvariancePortfolio(data = LPP,
                             spec = mvSpec,
                             constraints = constraints)
mvPf
weights_quadprog <- getWeights(mvPf)

mvSpecCplex <- portfolioSpec()
setSolver(mvSpecCplex) <- "solveRcplex"


mvPfCplex <- minvariancePortfolio(data = LPP,
                                  spec = mvSpecCplex,
                                  constraints = constraints)
mvPfCplex
weights_cplex <- getWeights(mvPfCplex)

## differences from 4th digit downwards
cbind(weights_cplex, weights_quadprog)

setTargetReturn(mvSpec) <- mean(LPP)
effPfQuadprog <- efficientPortfolio(LPP, mvSpec, constraints)

setTargetReturn(mvSpecCplex) <- mean(LPP)
effPfCplex <- efficientPortfolio(LPP, mvSpecCplex, constraints)

weights_cplex <- getWeights(effPfCplex)
weights_quadprog <- getWeights(effPfQuadprog)

## differences from 4th digit downwards
cbind(weights_cplex, weights_quadprog)

system.time(pfFrQuadprog <- portfolioFrontier(LPP, mvSpec, constraints))
system.time(pfFrCplex <- portfolioFrontier(LPP, mvSpecCplex, constraints))


## bigger problem
library("fEcofin")
data("DowJones30")

DJ <- returns(as.timeSeries(DowJones30), percentage = TRUE)

system.time(pfFrQuadprog <- portfolioFrontier(DJ, mvSpec, constraints))
system.time(pfFrCplex <- portfolioFrontier(DJ, mvSpecCplex, constraints))

## and now a really big data set
load("UBS.Rda")
UBS <- returns(na.omit(X), percentage = TRUE)

system.time(pfFrQuadprog <- portfolioFrontier(UBS, mvSpec, constraints))
system.time(pfFrCplex <- portfolioFrontier(UBS, mvSpecCplex, constraints))

##########################################################################
## 2.) linear programming (LP)

cvarSpec <- portfolioSpec()
setType(cvarSpec) <- "CVAR"
nAssets <- ncol(UBS)
setWeights(cvarSpec) <- rep(1/nAssets, times = nAssets)
setSolver(cvarSpec) <- "solveRglpk"
ewPortfolio <- feasiblePortfolio(data = UBS,
                                 spec = cvarSpec,
                                 constraints = "LongOnly")

minriskSpec <- portfolioSpec()
setType(minriskSpec) <- "CVaR"
setAlpha(minriskSpec) <- 0.05
setTargetReturn(minriskSpec) <- getTargetReturn(ewPortfolio@portfolio)["mean"]

## solve with GLPK

setSolver(minriskSpec) <- "solveRglpk"

timings <- c(CPLEX = NA, GLPK = NA, SYMPHONY = NA)

timings["GLPK"] <- 
system.time(minriskPortfolio <- efficientPortfolio(data = UBS,
                                                   spec = minriskSpec,
                                                   constraints = "LongOnly")
            )["elapsed"]

weights_glpk <- getWeights(minriskPortfolio)
risk_glpk <- getTargetRisk(minriskPortfolio)

## solve with SYMPHONY

setSolver(minriskSpec) <- "solveRsymphony"

timings["SYMPHONY"] <- 
system.time(minriskPortfolio <- efficientPortfolio(data = UBS,
                                                   spec = minriskSpec,
                                                   constraints = "LongOnly")
            )["elapsed"]

weights_symphony <- getWeights(minriskPortfolio)
risk_symphony <- getTargetRisk(minriskPortfolio)

## solve with CPLEX

setSolver(minriskSpec) <- "solveRcplex"

timings["CPLEX"] <- 
system.time(minriskPortfolio <- efficientPortfolio(data = UBS,
                                                   spec = minriskSpec,
                                                   constraints = "LongOnly")
            )["elapsed"]

weights_cplex<- getWeights(minriskPortfolio)
risk_cplex <- getTargetRisk(minriskPortfolio)

cbind(weights_cplex, weights_glpk, weights_symphony)
timings
cbind(risk_cplex, risk_glpk, risk_symphony)
