## 
# @file  runifInterface.R
# @brief R file for runif interface
#
# @author Petr Savicky
#
#
# Copyright (C) 2009, Petr Savicky, Academy of Sciences of the Czech Republic.
# All rights reserved.
#
# The new BSD License is applied to this software.
# Copyright (c) 2009 Petr Savicky. 
# All rights reserved.
#
#      Redistribution and use in source and binary forms, with or without
#      modification, are permitted provided that the following conditions are
#      met:
#      
#          - Redistributions of source code must retain the above copyright
#          notice, this list of conditions and the following disclaimer.
#          - Redistributions in binary form must reproduce the above
#          copyright notice, this list of conditions and the following
#          disclaimer in the documentation and/or other materials provided
#          with the distribution.
#          - Neither the name of the Academy of Sciences of the Czech Republic
#          nor the names of its contributors may be used to endorse or promote 
#          products derived from this software without specific prior written
#          permission.
#     
#      THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#      "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#      LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#      A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#      OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#      SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#      LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#      DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#      THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#      (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#      OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#  
#
#############################################################################
### runif interface
###
###			R functions
### 


set.generator <- function(name=c("congruRand", "WELL", "default"), parameters=NULL, seed=NULL, ...,
		only.dsc=FALSE)
{
	name <- match.arg(name)
	dots <- list(...)
	if (name == "congruRand")
	{
		if (is.null(parameters))
			parameters <- c(mod=dots$mod, mult=dots$mult, incr=dots$incr)
		if (length(parameters) == 0)
			parameters <- c(mod="2147483647", mult="16807", incr="0")
		if (!identical(names(parameters), c("mod", "mult", "incr")))
		{
			param.names <- paste(names(parameters),collapse=" ")
			stop("parameter list \"", param.names, "\" is not correct for congruRand")
		}
		if (is.null(seed))
			seed <- floor(as.double(parameters["mod"]) * runif(1))
		if (is.numeric(parameters))
			parameters <- formatC(parameters, format="f", digits=0)
		if (is.numeric(seed))
			seed <- formatC(seed, format="f", digits=0)
		state <- c(seed=seed)
		description <- list(name=name, parameters=parameters, state=state)
	} else if (name == "WELL")
	{
		if (is.null(parameters))
		{
			order <- as.character(dots$order)
			version <- as.character(dots$version)
			if (is.null(dots$temp))
				dots$temp <- ""
			temp <- as.character(dots$temp)
			if (temp == "temp")
				temp <- "Temp"
			parameters <- c(order=order, version=version, temp=temp)
		}
		if (identical(names(parameters), c("order", "version")))
			parameters <- c(parameters, temp="")
		if (!identical(names(parameters), c("order", "version", "temp")))
		{
			param.names <- paste(names(parameters),collapse=" ")
			cat("parameters required for WELL: order, version, temp\n")
			cat("parameters provided: ", param.names, "\n")
			stop("parameter list is not correct for WELL")
		}
		if (! paste(parameters, collapse="") %in% c("512a", "521a", "521b", "607a", "607b", "800a", "800b", "1024a", "1024b",
			"19937a", "19937aTemp", "19937b", "21701a", "23209a", "23209b", "44497a", "44497aTemp"))
			stop("unsupported parameters for WELL")
		if (is.null(seed))
			seed <- floor(2^31 * runif(1))
		size <- ceiling(as.numeric(parameters["order"])/32)
		state <- .C("initMT2002",
					as.integer(seed),
					as.integer(size),
					integer(size),
					PACKAGE="rngWELL")[[3]]
		description <- list(name=name, parameters=parameters, state=state)
	} else if (name == "default")
	{
		RNGkind("default")
		if (!is.null(seed))
			set.seed(seed)
		return(invisible(NULL))
	} else
		stop("unsupported generator: ", name)
	if (only.dsc)
		return(description)
	put.description(description)
	invisible(NULL)
}

put.description <- function(description)
{
	name <- description$name
	parameters <- description$parameters
	state <- description$state
	if (name == "congruRand")
	{
		aux <- .C("put_state_congru",
			parameters,
			state,
			err = integer(1),
			PACKAGE="randtoolbox")
		if (aux$err != 0)
			stop("check congruRand error: ", aux$err)
		if (RNGkind()[1] != "user-supplied")
		{
			.C("set_noop", PACKAGE="randtoolbox")
			RNGkind("user-supplied")
			aux <- .C("put_state_congru",
				parameters,
				state,
				err = integer(1),
				PACKAGE="randtoolbox")
			if (aux$err != 0)
				stop("check congruRand error: ", aux$err)
		}
	} else if (name == "WELL")
	{
		.C("set_noop", PACKAGE="randtoolbox")
		RNGkind("user-supplied")
		.C("putRngWELL",
			as.integer(parameters["order"]),
			match(parameters["version"], c("a", "b")),
			as.integer(parameters["temp"] == "Temp"),
			as.integer(state),
			PACKAGE="rngWELL")
	} else 
		stop("unsupported generator: ", name)
	invisible(NULL)
}

get.description <- function()
{
	if (RNGkind(NULL)[1] != "user-supplied")
		stop("For R base generators, use .Random.seed, not get.description()")
	generator <- .C("current_generator",
		integer(1),
		PACKAGE="randtoolbox")[[1]]
	if (generator == 1)
	{
		name <- "congruRand"
		outspace <- "18446744073709551616" # 2^64
		aux <- .C("get_state_congru",
			parameters=rep(outspace, times=3),
			seed=outspace,
			PACKAGE="randtoolbox")
		parameters <- aux$parameters
		seed <- aux$seed
		state <- c(seed=aux$seed)
		if(parameters[1] == "4294967296" && parameters[2] == "1664525" && parameters[3] == "1013904223")
			literature <- "Knuth - Lewis"
		else if(parameters[1] == "281474976710656" && parameters[2] == "31167285" && parameters[3] == "1")
			literature <- "Lavaux - Jenssens"
		else if(parameters[1] == "18446744073709551616" && parameters[2] == "636412233846793005" && parameters[3] == "1")
			literature <- "Haynes"
		else if(parameters[1] == "4294967296" && parameters[2] == "69069" && parameters[3] == "0") 
			literature <- "Marsaglia"
		else if(parameters[1] == "4294967295" && parameters[2] == "16807" && parameters[3] == "0") 
			literature <- "Park - Miller"
		else 
			literature <- "Unknown"
	} else if (generator == 2)
	{
		name <- "WELL"
		tmp <- .C("getRngWELL",
			order = integer(1),
			version = integer(1),
			temp = integer(1),
			state = integer(2000),
			PACKAGE="rngWELL")
		order <- as.character(tmp$order)
		version <- letters[tmp$version]
		temp <- if (tmp$temp == 1) "Temp" else ""
		parameters <- c(order=order, version=version, temp=temp)
		size <- ceiling(tmp$order/32)
		state <- tmp$state[1:size]
		literature <- "Panneton - L'Ecuyer - Matsumoto"
	} else
		stop("internal error of randtoolbox")
	list(name=name, authors=literature, parameters=parameters, state=state)
}

