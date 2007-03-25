

# ------------------------------------------------------------------------------


.builtin =
function(package, detach = TRUE)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Discard Warnings:
    warn = getOption("warn")
    options(warn = -1)
    
    # Capture all the Output to a File.
    file <- file("sink.txt", open = "wt")
    sink(file = file) 
    sink(file = file, type = "message")
    cmd = paste("require(", package, ", quietly = TRUE)")
    isLoaded = try(eval(parse(text = cmd)), silent = TRUE)
    
    # Back to the Console:
    sink(type = "message")
    sink()
    
    # Reset Warnings Option:
    options(warn = warn)

    # Check if the Package can be Loaded:
    if(!isLoaded) stop(paste("Cannot load package", package))
    
    # Return Value:
    invisible(detach)
}


################################################################################


setClass("fCOV", 
    representation(
        call = "call",
        use = "character",
        estimate = "list",
        mu = "numeric",
        Sigma = "matrix",
        title = "character",
        description = "character"
    )  
)


# ------------------------------------------------------------------------------


show.fCOV = 
function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print method for a fCOV object

    # FUNCTION:
    
    # Title:
    cat("\nTitle:\n ")
    cat(object@title, "\n")
    
    # Call:
    cat("\nCall:\n ")
    cat(paste(deparse(object@call), sep = "\n", collapse = "\n"), 
        "\n", sep = "")
    
    # Mean:
    cat("\nMean Vector:\n")
    print(object@mu)
    
    # Mean:
    cat("\nCovariance Matrix:\n")
    print(object@Sigma)
        
    # Description:
    cat("\nDescription:\n ")
    cat(object@description, "\n\n")
        
    # Return Value:
    invisible()
}


setMethod("show", "fCOV", show.fCOV)


# ------------------------------------------------------------------------------


meanCov = 
function(x, title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Convert x into a matrix:
    x = as.matrix(x)
    
    # Add Title and Description:
    if(is.null(title)) title = "Classical Mean and Covariance"
    if(is.null(description)) description = .description()
    
    # Return Value:
    new("fCOV", 
        call = match.call(),
        use = "base",
        estimate = list(),
        mu = colMeans(x),
        Sigma = cov(x),
        title = title,
        description = .description())
}


# ------------------------------------------------------------------------------


.robustCov = 
function(x, use = c("MASS", "rrcov", "robust"), method = c("mve", "mcd"), 
title = NULL, description = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    #   x - any rectangulkar object which can be transformed by the 
    #       function as.matrix() into a "matrix" object.
    #   use - a character string, the name of the package where the
    #       underlying called function is located. Note, "MASS" is 
    #       preloaded in Rmetrics, "rrcov" and "robust", are loaded
    #       when required. Package "robust" will be detached after
    #       usage.
    #   method - a character string denoting the method used to compute
    #       robust mu and Sigma. The choices are:
    #       if use="Mass" - "mcd", "mve", "classical"
    #       if use="rrcov" - "mcd", "mest" "ogk"
    #       if use="robust" - "mcd", "donostah", "M", "pairwiseQC", "pairwiseGK"
    #   ... - optional arguments passed to the underlying functions:
    #       MASS: cov.rob()
    #       rrcov: covMcd(), covMest(), CovOgk()
    #       robust: covRob()
    
    # Note:
    #   You have to install the contributed packages rrcov and robust.
       
    # FUNCTION:
    
    # Settings:
    use = match.arg(use)
    x = as.matrix(x)
    
    # Using Functions from Package "MASS"
    #   Note, this is ower default, MASS is preloaded to Rmetrics ..
    if (use == "MASS") {
        # MASS is loaded by default:
        massCov <- 
        function (x, cor = FALSE, n = dim(x)[1], p = dim(x)[2],
            quantile.used = floor((n + p + 1)/2), 
            method = c("mve", "mcd", "classical"), nsamp = "best", seed) 
        {
            estimate = MASS::cov.rob(x = x, cor = cor, quantile.used = 
                quantile.used, method = method, nsamp = nsamp, seed = seed)
            estimate$package = "MASS::cov.rob"
            estimate$method = method
            estimate
        }
        estimate = massCov(x, ...)     
        if(is.null(title)) {
            method = estimate$method
            if(method == "mcd") title = 
                "Minimum Covariance Determinant Estimator"
            if(method == "mve") title = 
                "Fast Minimum Covariance Determinant Estimator"
            if(method == "classical") title = 
                "Classical Covariance Estimator"
        }
        
    # Using Functions from Package "rrcov"
    } else if (use == "rrcov") {
        # Functions in use:
        #   covMcd(x, cor = FALSE, alpha = 1/2, nsamp = 500, seed = 0, 
        #       print.it = FALSE, use.correction = TRUE, control) 
        #   covMest(x, cor = FALSE, r = 0.45, arp = 0.05, eps = 0.001, 
        #       maxiter = 120, control, t0, S0) 
        #   CovOgk(x, niter = 2, beta = 0.9, control)
        # Requires "rrcov":
        detach = .builtin("rrcov", detach = FALSE)
        rrcovCov <-  
        function(x, method, ...) {
            if (method == "mcd") {
                # Computes a robust multivariate location and scatter estimate 
                #   with a high breakdown point, using the ‘Fast MCD’ (Minimum 
                #   Covariance Determinant) estimator. 
                estimate = rrcov::covMcd(x, ...)
            } else if (method == "mest") {
                # Computes constrained M-Estimates of multivariate location 
                # and scatter based on the translated biweight function 
                # (‘t-biweight’) using a High breakdown point initial 
                # estimate (Minimum Covariance Determinant - ‘Fast MCD’) 
                estimate = rrcov::covMest(x, ...)
            } else if (method == "ogk") {
                # Computes a robust multivariate location and scatter estimate 
                # with a high breakdown point, using the pairwise algorithm 
                # proposed by Marona and Zamar (2002) which in turn is based 
                # on the pairwise robust estimator proposed by Gnanadesikan-
                # Kettenring (1972). 
                ans = CovOgk(x, ...)
                estimate = list()
                estimate$call = ans@call
                estimate$iter = ans@iter
                estimate$crit = ans@crit
                estimate$cov = ans@cov
                estimate$center = ans@center
                estimate$n.obs = ans@n.obs
                estimate$raw.cov = ans@raw.cov
                estimate$raw.center = ans@raw.center
                estimate$raw.mah = ans@raw.mah
                estimate$raw.wt = ans@raw.wt
                estimate$X = ans@X
                estimate$Method = ans@method
            }
            estimate$method = method 
            estimate
        }  
        estimate = rrcovCov(x, method = method, ...)
        estimate$package = "rrcov::covMcd"
        class(estimate) = c("list", "mcd")
        # Detach the Package:
        #   ... we can keep it there are no overwrites
        if (detach) detach("package:rrcov")
        # Add Title:
        if(is.null(title)) {
            estim = estimate$method
            if(estim == "mcd") title = 
                "Minimum Covariance Determinant Estimator"
            if(estim == "mest") title = 
                "Rocke's Constrained M Estimator"
            if(estim == "ogk") title = 
                "Orthogonalized Gnanadesikan-Kettenring Pairwise Estimator"
        }
        
    # Using Functions from Package "robust"
    } else if (use == "robust") {
        # Requires "robust" - has no namespace
        detach = .builtin("robust", detach = TRUE)
        # Argument: "estim" - The choices are ... 
        #   "mcd" for the Fast MCD algorithm of Rousseeuw and Van Driessen, 
        #   "donostah" for the Donoho-Stahel projection based estimator, 
        #   "M" for the constrained M estimator provided by Rocke, 
        #   pairwiseQC" for the orthogonalized quadrant correlation pairwise 
        #       estimator, and 
        #   "pairwiseGK" for the Orthogonalized Gnanadesikan-Kettenring 
        #       pairwise estimator. The default 
        #   "auto" selects from "donostah", "mcd", and "pairwiseQC" with 
        #       the goal of producing a good estimate in a resonable amount 
        #       of time.
        estimate = covRob(x, estim = method, ...)
        estimate$package = "rrcov::covMcd"
        estimate$method = estimate$estim
        class(estimate) = "list"
        # Detach Library:
        #   - Note ther are overwrites !
        if (detach) detach("package:robust")
        # Add Title:
        if(is.null(title)) {
            estim = estimate$estim
            if(estim == "mcd") title = 
                "Minimum Covariance Determinant Estimator"
            if(estim == "donostah") title = 
                "Donostah's Estimator"
            if(estim == "m") title = 
                "Rocke's Constrained M Estimator"
            if(estim == "pairwiseqc") title = 
                "Orthogonalized Quadrant Correlation Pairwise Estimator"
            if(estim == "pairwisegk") title = 
                "Orthogonalized Gnanadesikan-Kettenring Pairwise Estimator"
        }
    }
    
    # Add Description:
    if(is.null(description)) description = .description()
    
    # Return Value:
    new("fCOV", 
        call = match.call(),
        use = use,
        estimate = estimate,
        mu = estimate$center,
        Sigma = estimate$cov,
        title = title,
        description = .description())
}


# ------------------------------------------------------------------------------


if (FALSE) {
    
    # Unit Tests:
    require(fEcofin)
    require(MASS)
      
    # Load data:  
    data(stackloss)
    x = stackloss
    colnames(x) = letters[1:4]
    x

    # Classical Mean-Covariance Estimator:
    meanCov(x)
    
    # Package: "MASS"
    robustCov(x, method = "mve")
    robustCov(x, method = "mcd")
    robustCov(x, method = "classical")
    
    # Package: "rrcov"
    robustCov(x, "rrcov", method = "mcd")
    robustCov(x, "rrcov", method = "mest")
    robustCov(x, "rrcov", method = "ogk")
    
    # Package: "robust"
    robustCov(x, "robust", method = "mcd")
    robustCov(x, "robust", method = "donostah")
    robustCov(x, "robust", method = "M")
    robustCov(x, "robust", method = "pairwiseQC")
    robustCov(x, "robust", method = "pairwiseGK")
    
}


################################################################################ 
# Extractor Functions:

    getCenter
    getCov
        
