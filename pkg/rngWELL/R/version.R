 #############################################################################
 #   Copyright (c) 2009 Christophe Dutang and Petr Savicky                   #
 #                                                                           #
 #   This code can be used freely for personal, academic, or                 #
 #   non-commercial purposes. For commercial purposes, please contact        #
 #   P. L'Ecuyer at: lecuyer@iro.UMontreal.ca                                #
 #   (at your option) any later version.                                     #
 #                                                                           #
 #                                                                           #
 #############################################################################
### version function
###
###			R functions
### 

version.rngWELL <- function()
{
	.C("version_rngWELL", character(1))[[1]]
}

