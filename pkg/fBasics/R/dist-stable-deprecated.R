dstable <- function(x, alpha, beta,
		    gamma = 1, delta = 0, pm = 0) {
    .Deprecated(new="stabledist::dstable()", package="fBasics")
    ans <- stabledist::dstable(x, alpha=alpha, beta=beta,
			   gamma=gamma, delta=delta, pm=pm)
    ## Attributes -- not desired in 'stable' package
    attr(ans, "control") <-
	cbind.data.frame(dist = "stable", alpha = alpha, beta = beta,
			 gamma = gamma, delta = delta, pm = pm, row.names = "")
    ans
}

pstable <- function(q, alpha, beta, gamma = 1, delta = 0, pm = 0) {
    .Deprecated(new="stabledist::pstable()", package="fBasics")
    ans <- stabledist::pstable(q, alpha=alpha, beta=beta,
			   gamma=gamma, delta=delta, pm=pm)
    ## Attributes -- not desired in 'stable' package
    attr(ans, "control") <-
	cbind.data.frame(dist = "stable", alpha = alpha, beta = beta,
			 gamma = gamma, delta = delta, pm = pm, row.names = "")
    ans
}

qstable <- function(p, alpha, beta, gamma = 1, delta = 0, pm = 0) {
    .Deprecated(new="stabledist::qstable()", package="fBasics")
    ans <- stabledist::qstable(p, alpha=alpha, beta=beta,
			   gamma=gamma, delta=delta, pm=pm)
    ## Attributes -- not desired in 'stable' package
    attr(ans, "control") <-
	cbind.data.frame(dist = "stable", alpha = alpha, beta = beta,
			 gamma = gamma, delta = delta, pm = pm, row.names = "")
    ans
}

rstable <- function(n, alpha, beta, gamma = 1, delta = 0, pm = 0) {
    .Deprecated(new="stabledist::rstable()", package="fBasics")
    ans <- stabledist::rstable(n, alpha=alpha, beta=beta,
			   gamma=gamma, delta=delta, pm=pm)
    ## Attributes -- not desired in 'stable' package
    attr(ans, "control") <-
	cbind.data.frame(dist = "stable", alpha = alpha, beta = beta,
			 gamma = gamma, delta = delta, pm = pm, row.names = "")
    ans
}

stableMode <- function(alpha, beta) {
    .Deprecated(new="stabledist::stableMode()", package="fBasics")
    ans <- stabledist::stableMode(alpha, beta)
    ## Attributes -- not desired in 'stable' package
    attr(ans, "control") =
	cbind.data.frame(dist = "stable", alpha = alpha, beta = beta,
			 row.names = "")
    ans
}
