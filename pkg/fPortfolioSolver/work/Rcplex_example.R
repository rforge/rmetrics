library("fPortfolio")
library("fPortfolioSolver")

data("LPP2005.RET")
LPP <- LPP2005.RET[, 1:6]

mvSpec <- portfolioSpec()
mvSpec

constraints <- "LongOnly"

mvPf <- minvariancePortfolio(data = LPP,
                             spec = mvSpec,
                             constraints = constraints)
mvPf

setSolver(mvSpec) <- "solveRcplex"
mvSpec

mvPfCplex <- minvariancePortfolio(data = LPP,
                                  spec = mvSpec,
                                  constraints = constraints)
mvPfCplex
