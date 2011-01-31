dstable <- function(x, alpha, beta,
		    gamma = 1, delta = 0, pm = 0) {
    .Deprecated(new="stable::dstable()", package="fBasics")
    ans <- stable::dstable(x, alpha=alpha, beta=beta,
			   gamma=gamma, delta=delta, pm=pm)
    ## Attributes -- not desired in 'stable' package
    attr(ans, "control") <-
	cbind.data.frame(dist = "stable", alpha = alpha, beta = beta,
			 gamma = gamma, delta = delta, pm = pm, row.names = "")
    ans
}

pstable <- function(q, alpha, beta, gamma = 1, delta = 0, pm = 0) {
    .Deprecated(new="stable::pstable()", package="fBasics")
    ans <- stable::pstable(q, alpha=alpha, beta=beta,
			   gamma=gamma, delta=delta, pm=pm)
    ## Attributes -- not desired in 'stable' package
    attr(ans, "control") <-
	cbind.data.frame(dist = "stable", alpha = alpha, beta = beta,
			 gamma = gamma, delta = delta, pm = pm, row.names = "")
    ans
}

qstable <- function(p, alpha, beta, gamma = 1, delta = 0, pm = 0) {
    .Deprecated(new="stable::qstable()", package="fBasics")
    ans <- stable::qstable(p, alpha=alpha, beta=beta,
			   gamma=gamma, delta=delta, pm=pm)
    ## Attributes -- not desired in 'stable' package
    attr(ans, "control") <-
	cbind.data.frame(dist = "stable", alpha = alpha, beta = beta,
			 gamma = gamma, delta = delta, pm = pm, row.names = "")
    ans
}

rstable <- function(n, alpha, beta, gamma = 1, delta = 0, pm = 0) {
    .Deprecated(new="stable::rstable()", package="fBasics")
    ans <- stable::rstable(n, alpha=alpha, beta=beta,
			   gamma=gamma, delta=delta, pm=pm)
    ## Attributes -- not desired in 'stable' package
    attr(ans, "control") <-
	cbind.data.frame(dist = "stable", alpha = alpha, beta = beta,
			 gamma = gamma, delta = delta, pm = pm, row.names = "")
    ans
}

stableMode <- function(alpha, beta) {
    .Deprecated(new="stable::stableMode()", package="fBasics")
    ans <- stable::stableMode(alpha, beta)
    ## Attributes -- not desired in 'stable' package
    attr(ans, "control") =
	cbind.data.frame(dist = "stable", alpha = alpha, beta = beta,
			 row.names = "")
    ans
}
