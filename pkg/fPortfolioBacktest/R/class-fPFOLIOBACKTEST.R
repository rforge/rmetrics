
################################################################################
# FUNCTION:                     		  DESCRIPTION:
#  "fPFOLIOBACKTEST"               S4 Portfolio Backtest Class
################################################################################


# fPFOLIOBACKTEST CLASS:

setClass("fPFOLIOBACKTEST",
   
    # Description:
    #   Represens S4 fPFOLIOBACKTEST Class
     
    representation(
        windows = "list",
        strategy = "list",
        smoother = "list",
        messages = "list")
    )

################################################################################

   