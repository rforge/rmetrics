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
			parameters <- c(mod="2147483647", mult="16807", incr="0")
		}
		if (!identical(names(parameters), c("mod", "mult", "incr"))) {
			param.names <- paste(names(parameters),collapse=" ")
			stop("parameter list \"", param.names, "\" is not correct for congruRand")
		}
		if (is.null(seed)) {
			seed <- floor(as.double(parameters["mod"]) * runif(1))
		}
		if (is.numeric(parameters)) {
			parameters <- formatC(parameters, format="f", digits=0)
		}
		if (is.numeric(seed)) {
			seed <- formatC(seed, format="f", digits=0)
		}
		state <- c(seed=seed)
		description <- list(name=name, parameters=parameters, state=state)
		if (only.description) {
			return(description)
		} else {
			put.description(description)
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

put.description <- function(description)
{
	name <- description$name
	parameters <- description$parameters
	state <- description$state
	if (name == "congruRand") {
		aux <- .C("put_state_congru",
			parameters,
			state,
			err = integer(1),
			PACKAGE="randtoolbox")
		if (aux$err != 0)
			stop("check congruRand error: ", aux$err)
		.C("set_generator",
			as.integer(1),
			PACKAGE="randtoolbox")
		if (RNGkind()[1] != "user-supplied") {
			RNGkind("user-supplied")
			aux <- .C("put_state_congru",
				parameters,
				state,
				err = integer(1),
				PACKAGE="randtoolbox")
			if (aux$err != 0)
				stop("check congruRand error: ", aux$err)
		}
	} else {
		stop("unsupported generator: ", name)
	}
	invisible(NULL)
}

get.description <- function()
{
	if (RNGkind(NULL)[1] != "user-supplied") {
		stop("For R base generators, use .Random.seed, not get.state()")
	}
	generator <- .C("current_generator",
		integer(1),
		PACKAGE="randtoolbox")[[1]]
	if (generator == 1) {
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
	} else {
		stop("internal error of randtoolbox")
	}
	list(name=name, authors=literature, parameters=parameters, state=state)
}

