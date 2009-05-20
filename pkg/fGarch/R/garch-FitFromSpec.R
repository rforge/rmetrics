
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################
# FUNCTION:               DESCRIPTION:
#  fUGARCHSPEC             Fits the parameters of GARCH process
#  .ugarchSpec             Specifies a univariate GARCH model           
#  .ugarchFit              Fits a univariate GARCH model  
################################################################################


.setfGarchEnv(.llh = 1e99)
.setfGarchEnv(.garchDist = NA)
.setfGarchEnv(.params = NA)
.setfGarchEnv(.series = NA)
.setfGarchEnv(.trace = NA)


# ------------------------------------------------------------------------------


setClass("fUGARCHSPEC", 
    representation(   
        model = "list",
        distribution = "list",
        optimization = "list",
        documentation = "list")  
)


# ------------------------------------------------------------------------------


.ugarchSpec <- 
function( 

    model = list(
        formula = ~ garch(1,1),
        mean = 0,
        include.mean = TRUE,
        delta = 2,
        include.delta = NULL, 
        leverage = NULL,
        init.rec = c("mci", "uev")[1]),

    distribution = list(
        cond.dist = c("norm", "snorm", "ged", "sged", "std", "sstd", 
            "snig", "QMLE")[1], 
        skew = 1,
        include.skew = NULL, 
        shape = 4,
        include.shape = NULL),
        
    optimization = list(
        algorithm = c("nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm")[1],
        hessian = c("ropt", "rcd")[1],
        trace = TRUE,
        control = list(),
        status = NA), 
    
    documentation = list(
        call = match.call(),
        title = NULL, 
        description = NULL )
    )
{
    # Description:
    #   Specifies a garch model to be fitted

    # Example:
    #   .garchSpec())

    # FUNCTION:
    
    # Model Slot:
    Model = list(
        formula = ~ garch(1,1),
        mean = 0,
        include.mean = TRUE,
        delta = 2,
        include.delta = NULL, 
        leverage = NULL,
        init.rec = c("mci", "uev")[1])
    Model[(Names <- names(model))] <- model

    # Distribution Slot:
    Distribution = list(
        cond.dist = c("norm", "snorm", "ged", "sged", "std", "sstd", 
            "snig", "QMLE")[1], 
        skew = 1,
        include.skew = NULL, 
        shape = 4,
        include.shape = NULL)
    Distribution[(Names <- names(distribution))] <- distribution

    # Optimization Slot:
    Optimization = list(
        algorithm = c("nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm")[1],
        hessian = c("ropt", "rcd")[1],
        trace = TRUE,
        control = list(),
        status = NA)
    Optimization[(Names <- names(optimization))] <- optimization
    
    # Documentation Slot:
    Documentation = list(
        call = match.call(),
        title = NULL, 
        description = NULL )
    Documentation[(Names <- names(documentation))] <- documentation

    # Return Value:
    new("fUGARCHSPEC",
        model = Model,
        distribution = Distribution,
        optimization = Optimization,
        documentation = Documentation)
}


# ------------------------------------------------------------------------------


.ugarchFit <- 
function(data, spec = .ugarchSpec())
{
    # Description:
    
    # Arguments:
    
    # Example:   
    
    # FUNCTION:

    # Set Call:
    spec@documentation$call = match.call()

    # Parse Data:
    Name = capture.output(substitute(data))
    if(is.character(data)) {
        eval(parse(text = paste("data(", data, ")")))
        data = eval(parse(text = data))
    }
    data <- as.data.frame(data)

    # Column Names:
    if (isUnivariate(data)) {
        colnames(data) <- "data"
    } else {
        # Check unique column Names:
        uniqueNames = unique(sort(colnames(data)))
        if (is.null(colnames(data))) {
            stop("Column names of data are missing.")
        }
        if (length(colnames(data)) != length(uniqueNames)) {
            stop("Column names of data are not unique.")
        }
    }

    # Handle if we have no left-hand-side for the formula ...
    formula <- spec@model$formula
    #  Note in this case the length of the formula is 2 (else 3):
    if (length(formula) == 3 && isUnivariate(data) ) formula[2] <- NULL
    if (length(formula) == 2) {
        if (isUnivariate(data)) {
            # Missing lhs -- we substitute the data file name as lhs ...
            formula = as.formula(paste("data", paste(formula, collapse = " ")))
        } else {
            stop("Multivariate data inputs require lhs for the formula.")
        }
    }

    # Robust Formula ?
    robust.cvar <- (spec@distribution$cond.dist == "QMLE")

    # Parse Arguments:
    args = .garchArgsParser(formula = formula, data = data, trace = FALSE)
    print(args)

    # Fit:
    ans = .garchFit(
        formula.mean = args$formula.mean,
        formula.var = args$formula.var,
        series = args$series,
        init.rec = spec@model$init.rec, 
        delta = spec@model$delta, 
        skew = spec@model$skew, 
        shape = spec@model$shape, 
        cond.dist = spec@distribution$dist, 
        include.mean = spec@distribution$include.mean,
        include.delta = spec@distribution$include.delta, 
        include.skew  = spec@distribution$include.skew, 
        include.shape = spec@distribution$include.shape, 
        leverage = spec@distribution$leverage,
        trace = spec@optimization$trace,
        algorithm = spec@optimization$algorithm,
        hessian = spec@optimization$hessian,
        robust.cvar = robust.var,
        control = spec@optimization$control,
        title = spec@documentation$title, 
        description = spec@documentation$description)
    ans@call = CALL
    attr(formula, "data") <- paste("data = ", Name, sep = "")
    ans@formula = formula

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------