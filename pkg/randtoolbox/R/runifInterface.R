set.generator <- function(name=c("congruRand", "default"), parameters=NULL, seed=NULL, ...,
		only.description=FALSE)
{
	name <- match.arg(name)
	dots <- list(...)
	if (name == "congruRand") {
		if (is.null(parameters)) {
			parameters <- c(mod=dots$mod, mult=dots$mult, incr=dots$incr)
		}
		if (length(parameters) == 0) {
			parameters <- c(mod=2147483647, mult=16807, incr=0)
		}
		if (!identical(names(parameters), c("mod", "mult", "incr"))) {
			param.names <- paste(names(parameters),collapse=" ")
			stop("parameter list \"", param.names, "\" is not correct for congruRand")
		}
		if (is.null(seed)) {
			seed <- floor(as.double(parameters["mod"]) * runif(1))
		}
		state <- c(seed=seed)
		description <- list(name=name, parameters=parameters, state=state)
		if (only.description) {
			return(description)
		} else {
			put.state(description)
		}
	} else if (name == "default") {
		RNGkind("default")
		if (!is.null(seed)) {
			set.seed(seed)
		}
	} else {
		stop("unsupported generator: ", name)
	}
	invisible(NULL)
}

put.state <- function(description)
{
	name <- description$name
	parameters <- description$parameters
	state <- description$state
	if (name == "congruRand") {
		aux <- .C("check_state_congru",
			as.double(parameters["mod"]),
			as.double(parameters["mult"]),
			as.double(parameters["incr"]),
			as.double(state["seed"]),
			err = integer(1),
			PACKAGE="randtoolbox")
		if (aux$err != 0) {
			stop("check congruRand error: ", aux$err)
		}
		.C("set_user_unif_init",
			as.integer(1),
			PACKAGE="randtoolbox")
		RNGkind("user-supplied")
		.C("set_user_unif_rand",
			as.integer(1),
			PACKAGE="randtoolbox")
		.C("put_state_congru",
			as.double(parameters["mod"]),
			as.double(parameters["mult"]),
			as.double(parameters["incr"]),
			as.double(state["seed"]),
			PACKAGE="randtoolbox")
	} else {
		stop("unsupported generator: ", name)
	}
	invisible(NULL)
}

get.state <- function()
{
	if (RNGkind(NULL)[1] != "user-supplied") {
		stop("For R base generators, use .Random.seed, not get.state()")
	}
	generator <- .C("current_generator",
		integer(1),
		PACKAGE="randtoolbox")[[1]]
	if (generator == 1) {
		name <- "congruRand"
		aux <- .C("get_state_congru",
			mod=double(1),
			mult=double(1),
			incr=double(1),
			seed=double(1),
			PACKAGE="randtoolbox")
		parameters <- c(mod=aux$mod, mult=aux$mult, incr=aux$incr)
		state <- c(seed=aux$seed)
	} else {
		stop("internal error of randtoolbox")
	}
	list(name=name, parameters=parameters, state=state)
}

