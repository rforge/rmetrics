version.randtoolbox <- function()
{
	.C("version_randtoolbox", character(1))[[1]]
}

