

### readme.txt ###


# ------------------------------------------------------------------------------
# RMETRICS PACKAGE LIST:


     PACKAGE:               DEPENDS:           
     
     Rmetrics               fUtilities, fEcofin, fCalendar, fSeries, fImport, 
                            fBasics, fArma, fGarch, fNonlinear, fUnitRoots, 
                            fTrading, fMultivar, fRegression, fExtremes, 
                            fCopulae, fOptions, fExoticOptions, fAsianOptions, 
                            fAssets, fPortfolio
                        
    01.  fUtilities
    02.  fEcofin            fUtilities
    
    03.  fCalendar          methods, MASS, fEcofin, fUtilities
    04.  fSeries            MASS, robustbase, fCalendar
    05.  fImport            fSeries
    06.  fBasics            fImport
    
    07.  fArma              fBasics
    08.  fGarch             fBasics, fArma
    09.  fNonlinear         fBasics
    10.  fUnitRoots         urca, fBasics
    
    11.  fTrading           fBasics
    12.  fMultivar          methods, sn, fBasics
    13.  fRegression        methods, mgcv, nnet, polspline, fTrading, fMultivar
    
    14.  fExtremes          fBasics, fTrading
    15.  fCopulae           adapt, fBasics, fMultivar
    
    16.  fOptions           fBasics
    17.  fExoticOptions     fOptions
    18.  fAsianOptions      fOptions
    
    19.  fAssets            methods, robustbase, MASS, fBasics, fRegression
    20.  fPortfolio         quadprog, lpSolve, MASS, fAssets


# ------------------------------------------------------------------------------
# PACKAGES IN USE:


    BASE:
        base, datasets, graphics, grDevices, grid, methods, profile,
        Recommended, splines, stats, stats4, tcltk, tools, utils
    
    BASE/RECOMMENDED:
        mgcv
        VR: class, MASS, nnet, spatial 
    
    CRAN:
        akima [suggested]
        adapt
        lpSolve
        polspline
        quadprog
        robustbase
        urca
        
    OTHER:
        Rdonlp2 [suggested]

        
# ------------------------------------------------------------------------------
# HIGH PRIORITY PACKAGES:   


    03-fCalendar   04-fSeries               JSS article on fCalendar 
    07-fArma       08-fGarch                JSS article on fGarch
    14-fExtrems    15-fCopulae
    19-fAssets     20-fPortfolio                    
                                               
                        
# ------------------------------------------------------------------------------
# PACKAGES UNDER DEVELOPMENT


    fBrowser       
    fBonds    
    fTickdata
    
    
# ------------------------------------------------------------------------------
# WORTH TO THINK ABOUT IT:


    disentangle man/.Rd and R/.R script files 
    add man/fPackage-package.Rd files
    
    add NAMESPACE for all Packages
