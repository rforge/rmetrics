#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  ../../COPYING


################################################################################


.First.lib =
function(lib, pkg)
{
    # Startup Mesage and Desription:
    MSG <- if(getRversion() >= "2.5") packageStartupMessage else message
    dsc <- packageDescription(pkg)
    if(interactive() || getOption("verbose")) {
        # not in test scripts
        MSG(sprintf("Rmetrics Package %s (%s) loaded.", pkg, dsc$Version))
    }

    # Load dll:
    # library.dynam("timeSeries", pkg, lib)

    # see ?cbind2
    # Currently, a call 'methods:::bind_activation(TRUE)'
    methods:::bind_activation(TRUE)
}

.onUnload <- function(libpath) methods:::bind_activation(FALSE)

if(!exists("Sys.setenv", mode = "function")) # pre R-2.5.0, use "old form"
    Sys.setenv <- Sys.putenv

################################################################################

