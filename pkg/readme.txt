

# ------------------------------------------------------------------------------
#   Packages available from CRAN: (still under development)

     PACKAGE:           DEPENDS:           
     
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

21.  Rmetrics           fUtilities, fEcofin, fCalendar, fSeries, fImport, 
                        fBasics, fArma, fGarch, fNonlinear, fUnitRoots, 
                        fTrading, fMultivar, fRegression, fExtremes, fCopulae, 
                        fOptions, fExoticOptions, fAsianOptions, fAssets, 
                        fPortfolio


# ------------------------------------------------------------------------------
#   Packages still under development and available from this Respository:


    fBrowser       
    fBonds    
    fTickdata
    
    Rdonlp2
    akima
