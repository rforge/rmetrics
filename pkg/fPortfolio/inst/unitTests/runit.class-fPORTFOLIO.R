
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port:
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION:
#  test.fPORTFOLIO
#  test.portfolioFrontier
#  test.feasiblePortfolio
#  test.feasiblePortfolio.Rdonlp2
#  test.feasiblePortfolio.glpk
#  test.efficientPortfolio
#  test.tangencyPortfolio
#  test.minvariancePortfolio
#  test.minvariancePortfolio.Rdonlp2
#  test.show
#  test.plot.RQuadprog
#  test.plot.Rdonlp2
#  test.plot.Rlpglpk
#  test.weightsSlider
#  test.frontierSlider
################################################################################


test.fPORTFOLIO <- 
    function()
{
    # Class:
    getClass("fPORTFOLIO")

    # Return Value:
    return()
}


################################################################################


test.portfolioFrontier <- 
    function()
{
    # Arguments:
    # portfolioFrontier(data, spec = portfolioSpec(), constraints = NULL,
    #   title = NULL, description = NULL)

    # Load Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))

    # Set Default Specifications - Long Only MV Portfolio
    spec = portfolioSpec()
    setNFrontierPoints(spec) = 10
    print(spec)

    # Calculation of Long Only Minimum Variance Portfolio
    Frontier = portfolioFrontier(data, spec)
    print(Frontier)

    # Return Value:
    return()
}


################################################################################


test.feasiblePortfolio <- 
    function()
{
    # Arguments:
    # feasiblePortfolio(data, spec = portfolioSpec(), constraints = NULL)

    # Get Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))

    # Set Default Specifications - Long Only MV Portfolio
    spec = portfolioSpec()
    setWeights(spec) = rep(1/4, times = 4)
    print(spec)

    # Optimize Long Only Minimum Variance Portfolio:
    ewPortfolio = feasiblePortfolio(data, spec)
    print(ewPortfolio)

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.feasiblePortfolio.Rdonlp2 <- 
    function()
{
    # Arguments:
    # feasiblePortfolio(data, spec = portfolioSpec(), constraints = NULL)

    if (FALSE) {
    
    if (require(Rdonlp2)) {

        # Get Data:
        data = SMALLCAP.RET
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        print(head(data))

        # Set Default Specifications - Long Only MV Portfolio
        spec = portfolioSpec()
        setWeights(spec) = rep(1/4, times = 4)
        setSolver(spec)<-"Rdonlp2"
        print(spec)

        # Constraints:
        constraints = "LongOnly"
        print(constraints)

        # Optimize Long Only Minimum Variance Portfolio:
        ewPortfolio = feasiblePortfolio(data, spec, constraints)
        print(ewPortfolio)

    }
    
    }
    NA

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.feasiblePortfolio.glpk <- 
    function()
{
    # Arguments:
    # feasiblePortfolio(data, spec = portfolioSpec(), constraints = NULL)

    # Get Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))

    # Set Default Specifications - Long Only MV Portfolio
    spec = portfolioSpec()
    setWeights(spec) = rep(1/4, times = 4)
    setSolver(spec) = "solveRglpk"
    print(spec)

    # Constraints:
    constraints = "LongOnly"
    print(constraints)

    # Optimize Long Only Minimum Variance Portfolio:
    ewPortfolio = feasiblePortfolio(data, spec, constraints)
    print(ewPortfolio)

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientPortfolio <- 
    function()
{
    # Arguments:
    # efficientPortfolio(data, spec = portfolioSpec(), constraints = NULL)

    # Load Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))

    # Set Default Specifications - Long Only MV Portfolio
    spec = portfolioSpec()
    setTargetReturn(spec) = mean(colMeans(data))
    print(spec)

    # Constraints:
    constraints = "LongOnly"
    print(constraints)

    # Calculation of Long Only Minimum Variance Portfolio
    meanPortfolio = efficientPortfolio(data, spec, constraints)
    print(meanPortfolio)

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.tangencyPortfolio <- 
    function()
{
    # Arguments:
    # tangencyPortfolio(data, spec = portfolioSpec(), constraints = NULL)

    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))

    # Specifications:
    spec = portfolioSpec()
    print(spec)

    # Portfolio:
    tgPortfolio = tangencyPortfolio(data, spec)
    print(tgPortfolio)

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.minvariancePortfolio <- 
    function()
{
    # Arguments:
    # minvariancePortfolio(data, spec = portfolioSpec(), constraints = NULL)

    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))

    # Specifications:
    spec = portfolioSpec()
    print(spec)

    # Portfolio:
    mvPortfolio = minvariancePortfolio(data, spec)
    print(mvPortfolio)

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.minvariancePortfolio.Rdonlp2 <- 
    function()
{
    # Arguments:
    # minvariancePortfolio(data, spec = portfolioSpec(), constraints = NULL)

    if (FALSE) {
    
    if (require(Rdonlp2)) {

        # Data:
        data = SMALLCAP.RET
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        print(head(data))

        # Specifications:
        spec = portfolioSpec()
        setSolver(spec) = "solveRdonlp2"
        print(spec)

        # Portfolio:
        mvPortfolio = minvariancePortfolio(data, spec)
        print(mvPortfolio)

    }
    
    }
    NA

    # Return Value:
    return()
}


################################################################################


test.show <- 
    function()
{
    # Load Data::
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)

    # Set Default Specifications:
    spec = portfolioSpec()
    setNFrontierPoints(spec) = 10
    spec

    # Calculation of Long Only Minimum Variance Portfolio:
    Frontier = portfolioFrontier(data, spec)
    show(Frontier)

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.plot.RQuadprog <- 
    function()
{
    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]

    # Specifications:
    spec = portfolioSpec()

    # Constraints:
    constraints = "LongOnly"

    # Frontier:
    Frontier = object = portfolioFrontier(data, spec, constraints)

    # Plot:
    par(mfrow = c(1, 1))
    frontierPlot(Frontier, pch = 19)
    minvariancePoints(Frontier, col = "red", pch = 19, cex = 1.5)
    tangencyPoints(Frontier, col = "green")
    tangencyLines(Frontier, col = "green")
    singleAssetPoints(Frontier, col = "red", cex = 1.5)
    equalWeightsPoints(Frontier, col = "blue", pch = 19, cex = 1.5)
    twoAssetsLines(Frontier, col = "grey")
    # .weightsWheel(Frontier)
    monteCarloPoints(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)
    sharpeRatioLines(Frontier, pch = 19, col = "blue")

    # Plot Ask:
    # plot(Frontier, which = "ask")

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.plot.Rdonlp2 <- 
    function()
{
    if (FALSE) {
    
    if (require(Rdonlp2)) {

        # Data:
        Data = SMALLCAP.RET
        Data = Data[, c("BKE", "GG", "GYMB", "KRON")]

        # Specifications:
        Spec = portfolioSpec()
        setSolver(Spec) <- "solveRdonlp2"

        # Constraints:
        Constraints = "LongOnly"

        # Frontier:
        Frontier = portfolioFrontier(Data, Spec, Constraints)

        # Plot:
        par(mfrow = c(1, 1))
        frontierPlot(Frontier, pch = 19)
        minvariancePoints(Frontier, col = "red", pch = 19, cex = 1.5)
        tangencyPoints(Frontier, col = "green")
        tangencyLines(Frontier, col = "green")
        singleAssetPoints(Frontier, col = "red", cex = 1.5)
        equalWeightsPoints(Frontier, col = "blue", pch = 19, cex = 1.5)
        twoAssetsLines(Frontier, col = "grey")
        # .weightsWheel(Frontier)
        monteCarloPoints(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)
        sharpeRatioLines(Frontier, pch = 19, col = "blue")

        # Plot Ask:
        # plot(Frontier, which = "ask")

    }
    
    }
    NA

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.plot.Rlpglpk <- 
    function()
{
    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]

    # Specifications:
    spec = portfolioSpec()
    setType(spec) <- "CVaR"

    # Constraints:
    constraints = "LongOnly"

    # Frontier:
    Frontier = portfolioFrontier(data, spec, constraints)

    # Plot:
    par(mfrow = c(1, 1))
    frontierPlot(Frontier, pch = 19)
    minvariancePoints(Frontier, col = "red", pch = 19, cex = 1.5)
    tangencyPoints(Frontier, col = "green")
    tangencyLines(Frontier, col = "green")
    singleAssetPoints(Frontier, col = "red", cex = 1.5)
    equalWeightsPoints(Frontier, col = "blue", pch = 19, cex = 1.5)
    twoAssetsLines(Frontier, col = "grey")
    # .weightsWheel(Frontier)
    monteCarloPoints(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)
    sharpeRatioLines(Frontier, pch = 19, col = "blue")

    # Plot Ask:
    # plot(Frontier, which = "ask")

    # Return Value:
    return()
}


################################################################################


test.weightsSlider <- 
    function()
{
    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)

    # Specifications:
    spec = portfolioSpec()
    setNFrontierPoints(spec) = 15
    spec

    # Frontier:
    Frontier = portfolioFrontier(data, spec)
    Frontier

    # Slider:
    # weightsSlider(Frontier)
    NA

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.frontierSlider <- 
    function()
{
    # Data:
    data = SMALLCAP.RET
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)

    # Specifications:
    spec = portfolioSpec()
    setNFrontierPoints(spec) = 15
    spec

    # Frontier:
    Frontier = portfolioFrontier(data, spec)
    Frontier

    # Slider:
    # frontierSlider(Frontier)
    NA

    # Return Value:
    return()
}


################################################################################

