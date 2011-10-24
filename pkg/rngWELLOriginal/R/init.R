.onLoad <- function(lib, pkg)
{
	library.dynam("rngWELLOriginal", pkg, lib)
}

.onUnload <- function(lib) 
{
	library.dynam.unload("rngWELLOriginal", lib)
}

