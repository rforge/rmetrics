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


setGenerator.WELL <- function(parameters=NULL, seed=NULL, ...)
{
	dots <- list(...)
	if (is.null(parameters))
	{
		if (is.null(dots$order)) dots$order <- ""
		if (is.null(dots$version)) dots$version <- ""
		if (is.null(dots$init)) dots$init <- "1"
		if (dots$order != "" & nchar(dots$version) != 1) {
			stop("unsupported parameters order=", dots$order, ", version=", dots$version, " for WELL")
		}
		version.name <- paste(dots$order, dots$version, sep="")
		order <- substr(version.name, 1, nchar(version.name) - 1)
		version <- substr(version.name, nchar(version.name), nchar(version.name))
		parameters <- c(order=order, version=version, init=dots$init)
	}
	if (!identical(names(parameters), c("order", "version", "init")))
	{
		param.names <- paste(names(parameters), collapse=" ")
		cat("parameters required for WELL: order, version\n")
		cat("parameters provided: ", param.names, "\n")
		stop("parameter list is not correct for WELL")
	}
	if (! paste(parameters["order"], parameters["version"], sep="") %in% c("512a", "1024a",
		"19937a", "19937c", "44497a", "44497b"))
		stop("unsupported parameters order=", parameters["order"], ", version=", parameters["version"], " for WELL")
	if (is.null(seed))
		seed <- floor(2^31 * runif(1))
	#size <- ceiling(as.numeric(parameters["order"])/32)
	.C("set_noop", PACKAGE="rngWELLOriginal")
	RNGkind("user-supplied")
	.C("set_generator",
		as.integer(parameters["order"]),
		match(parameters["version"], c("a", "b", "c"), nomatch=0),
		as.integer(parameters["init"]),
		PACKAGE="rngWELLOriginal")
	set.seed(seed)
	invisible(NULL)
}

