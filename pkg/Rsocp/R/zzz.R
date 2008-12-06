

################################################################################
 
    
.First.lib =  
function(lib, pkg)
{   
    # Startup Mesage and Desription:
    MSG <- if(getRversion() >= "2.5") packageStartupMessage else message
    dsc <- packageDescription(pkg)
    if(interactive() || getOption("verbose")) { 
        # not in test scripts
        MSG(sprintf("R Package %s (%s) loaded.", pkg, dsc$Version))
    }

    # Load dll:
    library.dynam("Rsocp", pkg, lib) 
}


################################################################################

