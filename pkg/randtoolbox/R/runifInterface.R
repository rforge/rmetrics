set.generator <- function(generator="congru", params=NULL, seed=NULL)
{
	if (generator == "congru") {
		if (is.null(params)) {
			params <- list(generator="congru", mod=2147483647, mult=16807, incr=0)
		}
		if (is.null(seed)) {
			if (!is.null(params$seed)) {
				seed <- params$seed
			} else {
				seed <- floor(2^32 * runif(1))
			}
		}
		params$seed <- seed
		put.state(params)
	} else if (generator == "default") {
		RNGkind("default")
		if (!is.null(seed)) {
			set.seed(seed)
		}
	} else {
		stop("unsupported generator", generator)
	}
	invisible(NULL)
}

put.state <- function(state)
{
	if (state$generator == "congru") {
		.C("set_generator",
			as.integer(1),
			PACKAGE="randtoolbox")
		RNGkind("user")
		.C("put_state_congru",
			as.double(state$mod),
			as.double(state$mult),
			as.double(state$incr),
			as.double(state$seed),
			PACKAGE="randtoolbox")
	} else {
		stop("unsupported generator", state$generator)
	}
	invisible(NULL)
}

get.state <- function()
{
	if (RNGkind(NULL)[1] != "user-supplied") {
		stop("For R base generators, use .Random.seed, not this function")
	}
	generator <- .C("current_generator",
		integer(1),
		PACKAGE="randtoolbox")[[1]]
	if (generator == 1) {
		aux <- .C("get_state_congru",
			mod=double(1),
			mult=double(1),
			incr=double(1),
			seed=double(1),
			PACKAGE="randtoolbox")
		state <- c(list(generator="congru"),aux)
	} else {
		stop("internal error of randtoolbox")
	}
	state
}

