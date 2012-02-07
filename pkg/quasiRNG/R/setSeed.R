### set the seed ###

setSeed <- function(seed)
	invisible( .Call("doSetSeed", seed) )


