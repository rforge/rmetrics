\name{fCopulae-package}


\alias{fCopulae-package}
\alias{fCopulae}


\docType{package}


\title{Modelling Copulae and Dependence Structures}


\description{

  The Rmetrics \code{fCopulae} package is a  collection of functions to 
  manage, to investigate and to analyze bivariate financial returns by  
  Copulae. Included are the families of Archemedean, Elliptical, 
  Extreme Value, and Empirical Copulae.
  
}


\details{

    \tabular{ll}{
        Package:    \tab fCopulae\cr
        Type:       \tab Package\cr
        Version:    \tab R 3.0.1\cr
        Date:       \tab 2014\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2014 Rmetrics Assiciation\cr
        URL:        \tab \url{https://www.rmetrics.org}
  }
  

}


\section{1 Introduction}{

  The package \code{fCoplae} was written to explore and investigate
  bivariate copulae and dependence structures.
  
}


\section{2 Archimedean Copulae}{

  This chapter contains functions for analysing and modeling Archemedean
  copulae.
  
  
  \emph{Archimedean Copula Density, Probability and Random Numbers:}
  
  \preformatted{
  darchmCopula               Computes Archimedean copula density
  parchmCopula               Computes Archimedean copula probability 
  rarchmCopula               Generates Archimedean copula random variates 
  }
  
  For the Gumbel Copula we have a fast implementation.
  
  \preformatted{
  rgumbelCopula              Generates fast gumbel random variates
  dgumbelCopula              Computes bivariate Gumbel copula density
  pgumbelCopula              Computes bivariate Gumbel copula probability
  }
  
  
  \emph{Archimedean Copula Dependency Structure:}
  
  \preformatted{
  archmTau                   Returns Kendall's tau for Archemedean copulae
  archmRho                   Returns Spearman's rho for Archemedean copulae
  }
  
  \preformatted{
  archmTailCoeff             Computes tail dependence for Archimedean copulae
  archmTailPlot              Plots Archimedean tail dependence function
  }
  
  
  \emph{Archimedean Copula Generator:}
  
  \preformatted{
  archmList                  Returns list of implemented Archimedean copulae
  archmParam                 Sets Default parameters for an Archimedean copula
  archmRange                 Returns the range of valid alpha values
  archmCheck                 Checks if alpha is in the valid range
  }
  
  \preformatted{
  Phi                        Computes Archimedean Phi, inverse and derivatives
  PhiSlider                  Displays interactively generator function
  Kfunc                      Computes Archimedean Density Kc and its Inverse
  KfuncSlider                Displays interactively the density and concordance
  }
  
  
  \emph{Archemedean Copula Modeling:}
  
  \preformatted{
  archmCopulaSim             Simulates bivariate elliptical copula
  archmCopulaFit             Fits the paramter of an elliptical copula
  }
  
  
  \emph{Archemedean Copula Slider:}
  
  \preformatted{
  darchmSlider               Displays interactively archimedean density
  parchmSlider               Displays interactively Archimedean probability 
  rarchmSlider               Displays interactively Archimedean probability
  }
  
}


\section{3 Elliptical Copulae}{

  This chapter contains functions for analysing and modeling elliptical
  copulae.
  
  
  \emph{Elliptical Copula Density, Probability and Random Numbers:}
  
  \preformatted{
  dellipticalCopula          Computes elliptical copula density
  pellipticalCopula          Computes elliptical copula probability
  rellipticalCopula          Generates elliptical copula variates
  }
  
  
  \emph{Elliptical Copula Slider:}
  
  \preformatted{
  dellipticalSlider          Generates interactive plots of density
  pellipticalSlider          Generates interactive plots of probability
  rellipticalSlider          Generates interactive plots of random variates
  }
  
  \emph{Elliptical Copula Dependency Structures:}
  
  \preformatted{
  ellipticalTau              Computes Kendall's tau for elliptical copulae
  ellipticalRho              Computes Spearman's rho for elliptical copulae
  }
  
  \preformatted{
  ellipticalTailCoeff        Computes tail dependence for elliptical copulae
  ellipticalTailPlot         Plots tail dependence function
  }
  
  \emph{Elliptical Copula Generator:}
  
  \preformatted{
  ellipticalList             Returns list of implemented Elliptical copulae
  ellipticalParam            Sets default parameters for an elliptical copula
  ellipticalRange            Returns the range of valid rho values
  ellipticalCheck            Checks if rho is in the valid range
  }
  
  \preformatted{
  gfunc                      Generator function for elliptical distributions
  gfuncSlider                Slider for generator, density and probability
  }
  
  \emph{Elliptical Copula Modeling:}
  \preformatted{
  ellipticalCopulaSim        Simulates bivariate elliptical copula
  ellipticalCopulaFit        Fits the paramter of an elliptical copula
  }

}


\section{4 Extreme Value Copulae}{

  This chapter contains functions for analysing and modeling extreme
  value copulae.
  
  \emph{Extremem Value Copula Density, Probability and Random Numbers:}
  
  \preformatted{
  devCopula                  Computes extreme value copula density
  pevCopula                  Computes extreme value copula probability
  revCopula                  Generates extreme value copula random variates
  }
  
  \preformatted{
  devSlider                  Displays interactively plots of density
  pevSlider                  Displays interactively plots of probability
  revSlider                  isplays interactively plots of random variates
  }
  
  
  \emph{Extreme Value Copula Dependeny Structures:}
  
  \preformatted{
  evTau                      Returns Kendall's tau for extreme value copulae
  evRho                      Returns Spearman's rho for extreme value copulae
  }

  \preformatted{
  evTailCoeff                Computes tail dependence for extreme value copulae
  evTailCoeffSlider          Plots extreme value tail dependence function
  }
  
  
  \emph{Extreme Value Copula Generator:}
  
  \preformatted{
  evList                    Returns list of implemented extreme value copulae
  evParam                   Sets Default parameters for an extreme value copula
  evCheck                   Checks if parameters are in the valid range
  evRange                   Returns the range of valid parameter values
  }
  
  \preformatted{
  Afunc                     Computes Dependence function
  AfuncSlider               Displays interactively dependence function
  }
  
  
  \emph{Extreme Value Copula Modeling:}
  
  \preformatted{
  evCopulaSim                Simulates bivariate extreme value copula
  evCopulaFit                Fits the paramter of an extreme value copula
  }

}


\section{5 Empirical Copula.}{

  This chapter contains functions for analysing and modeling empirical
  copulae.
  
  \emph{Empirical Copulae Density and Probability:}
  
  \preformatted{
  pempiricalCopula           Computes empirical copula probability
  dempiricalCopula           Computes empirical copula density
  }
  
}


\section{About Rmetrics:}{

    The \code{fCopulae} Rmetrics package is written for educational 
    support in teaching "Computational Finance and Financial Engineering" 
    and licensed under the GPL.
    
}


\keyword{package}


