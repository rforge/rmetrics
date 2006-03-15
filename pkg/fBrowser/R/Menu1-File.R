
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file 
    
    
################################################################################
# File Popup Menu


.File.PopupMenu =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   File Popup Menu
    
    # FUNCTION:
    
    # New Toolbar Menu:
    fileMenu = newToolbarMenu()
  
    # Add Menu:
    Label = "List Active Data Set"
    subLabel = 
        NULL
    Command = 
        ".listActiveDataSet"
    addToolbarMenu(fileMenu, Label, subLabel, Command)     
    
    # Add Menu:
    Label = "List Objects"
    subLabel = 
        NULL
    Command = 
        ".listObjects"
    addToolbarMenu(fileMenu, Label, subLabel, Command)  
    
    # Add Menu:
    Label = "________________________________"
    subLabel = 
        NULL
    Command = 
        "tkSeparator"
    addToolbarMenu(fileMenu, Label, subLabel, Command) 
    
    # Add Menu:
    Label = "List Rmetrics Data Files"
    subLabel = 
        NULL
    Command = 
        ".listRmetricsData"
    addToolbarMenu(fileMenu, Label, subLabel, Command)  
    
    # Add Menu:
    Label = "Load Rmetrics Data File"
    subLabel = 
        NULL
    Command = 
        ".loadRmetricsData"
    addToolbarMenu(fileMenu, Label, subLabel, Command)  
    
    # Add Menu:
    Label = "________________________________"
    subLabel = 
        NULL
    Command = 
        "tkSeparator"
    addToolbarMenu(fileMenu, Label, subLabel, Command) 
    
    # Add Menu:
    Label = "Install Package() ..."
    subLabel = 
        NULL
    Command = 
        ".menuInstallPackages"
    addToolbarMenu(fileMenu, Label, subLabel, Command)  
    
    # Add Menu:
    Label = "Update Package() ..."
    subLabel = 
        NULL
    Command = 
        ".menuUpdatePackages"
    addToolbarMenu(fileMenu, Label, subLabel, Command)  
   
    # Add Menu:
    Label = "________________________________"
    subLabel = 
        NULL
    Command = 
        "tkSeparator"
    addToolbarMenu(fileMenu, Label, subLabel, Command) 
    
    # Add Menu:          
    Label = "Run a Rmetrics Demo"
    subLabel = c(
        "from fBasics",
        "from fCalendar", 
        "from fSeries", 
        "from fMultivar", 
        "from fExtremes", 
        "from fOptions")
    Command = c(
        "xmpfBasics",
        "xmpfCalendar",
        "xmpfSeries",
        "xmpfMultivar",
        "xmpfExtremes",
        "xmpfOptions")
    addToolbarMenu(fileMenu, Label, subLabel, Command)     
        
    # Add Menu:
    Label = "________________________________"
    subLabel = 
        NULL
    Command = 
        "tkSeparator"
    addToolbarMenu(fileMenu, Label, subLabel, Command) 
    
    # Add Menu:
    Label = "Quit"
    subLabel = 
        NULL
    Command = 
        ".quitBrowser"
    addToolbarMenu(fileMenu, Label, subLabel, Command) 
    
    # Cascade fileMenu:
    cascadeToolbarMenu(Menu = fileMenu, Label = "File")        
}


################################################################################

