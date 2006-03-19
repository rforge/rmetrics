myFirstPopup = function() {    
    # New Popup Menu:
      myMenu = tkNewPopup()
    # Add Menu – 1st Entry: 
      tkAddPopupMenu(myMenu,
		  Label = "Generate Normal RVs", 
		  Command = "rnormCmd" )         
    # Add Menu 2nd Entry with Submenus:       
      tkAddPopupMenu(myMenu , 
         Label = "Generate RVs", 
         subLabel c("Normal", "Student-t"), 
         Command c("rnormCmd", "rtCmd") )  
    # Cascade fileMenu:
      tkCascadePopup(Menu = myMenu, Label = "myMenu")        
}
