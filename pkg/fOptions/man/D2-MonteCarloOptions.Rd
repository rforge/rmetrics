\name{MonteCarloOptions}

\alias{MonteCarloOptions}

\alias{wienerMCPath}
\alias{plainVanillaMCPayoff}
\alias{arithmeticAsianMCPayoff}
\alias{MonteCarloOption}


\title{Monte Carlo Valuation of Options}


\description{

    A collection and description of functions to valuate 
    options by Monte Carlo methods. The functions include 
    beside the main Monte Carlo Simulator, example functions 
    to generate Monte Carlo price paths and to compute 
    Monte Carlo price payoffs.
    \cr
    
    The functions are:

    \tabular{ll}{
	\code{sobolInnovations} \tab Example for scrambled Sobol innovations, \cr
	\code{wienerPath} \tab Example for a Wiener price path, \cr
	\code{plainVanillaPayoff} \tab Example for the plain vanilla option's payoff, \cr
	\code{arithmeticAsianPayoff} \tab Example for the arithmetic Asian option's payoff, \cr
	\code{MonteCarloOption} \tab Monte Carlo Simulator for options. }
    
}


\usage{
MonteCarloOption(delta.t, pathLength, mcSteps, mcLoops, init = TRUE, 
    innovations.gen, path.gen, payoff.calc, antithetic = TRUE, 
    standardization = FALSE, trace = TRUE, \dots)
}

\arguments{
  
    \item{antithetic}{
        a logical flag, should antithetic variates be used? 
        By default TRUE.
        }
    \item{delta.t}{
        the time step interval measured as a fraction of one
        year, by default one day, i.e. \code{delta.t=1/360}.
        }
    \item{init}{
        a logical flag, should the random number generator be 
        initialized? By default TRUE.
        }
    \item{innovations.gen}{
        a user defined function to generate the innovations, this can 
        be the normal random number generator \code{rnorm.pseudo} with 
        mean zero and variance one. For the usage of low discrepancy
        sequences alternativey \code{rnorm.halton} and \code{rnorm.sobol}
        can be called. The generator must deliver a normalized
        matrix of innovations with dimension given by the number of
        Monte Carlo steps and the path length. The first three
        arguments of the generator are the the number of Monte Carlo 
        steps \code{mcSteps}, the path length \code{pathLength} and the
        initialization flag \code{init}. Optional arguments can be passed
        through the argument \code{\dots}, e.g. the type of scrambling 
        for low discrepancy numbers.
        }
    \item{mcLoops, mcSteps}{
        the number of Monte Carlo loops and Monte Carlo Steps.
        In total \code{mcLoops*mcSteps} samples are included in
        one MC simulation.
        }
    \item{path.gen}{
        the user defined function to generate the price path.
        As the only input argument serves the matrix of innovations,
        the option parameters must be available as global variables.
        }
    \item{pathLength}{
        the length of the price path. This may be calculated
        as \code{floor(Time/delta.t)}, where \code{Time} denotes the 
        time to maturation measured in years.
        }
    \item{payoff.calc}{
        a user defined function to calculate the payoff of the option.
        As the only input argument serves the path matrix as returned
        by the path generator. The option parameters must be available 
        as global variables.
        }
    \item{standardization}{
        a logical flag, should the innovations for one loop be
        standardized? By default TRUE.
        }
    \item{trace}{
        a logical flag, should the Monte Carlo simulation be traced?
        By default TRUE.
        }
    \item{\dots}{
        additional arguments passed to the innovations generator.
        }   

}


\value{
  
    \emph{The user defined innovation generator}
    \cr
    returns a numeric matrix of (random) innovations to build the Monte 
    Carlo Paths.
    \cr
    
    \emph{The user defined path generator}
    \cr
    returns a numeric matrix of the Monte Carlo paths for the calculation
    of the option's payoffs.
    To be more precise, as an example the function returns for a Wiener
    process the matrix 
    \code{(b-sigma*sigma/2)*delta.t + sigma*sqrt(delta.t)*innovations}, 
    where the first term corresponds to the drift and the second to the 
    volatility.
    \cr
    
    
    \emph{The user defined payoff calculator},
    \cr
    returns the vector of the option's payoffs calculated from the generated
    paths. As an example this becomes for an arithmetic Asian call option
    with a Wiener Monte Carlo path \code{payoff = exp(-r*Time)*max(SM-X, 0)} 
    where code{SM = mean(S*exp(cumsum(path)))} and \code{path} denotes
    the MC price paths.
    \cr
    
    
    \bold{MonteCarloOption:}
    \cr  
    returns a vector with the option prices for each Monte Carlo loop.
    
}


\details{
    
    \bold{The Innovations:}
    \cr\cr
    The innovations must created by the user defined innovation generator.
    The Generator has to return a numeric matrix of (random) innovations 
    of size \code{mcSteps} times the \code{pathLength}. The example
    section shows how to write sa function for scrambled Quasi Monte Carlo
    Sobol numbers. The package comes with three generators
    \code{rnorm.pseudo}, \code{rnorm.halton} and \code{rnorm.sobol}
    which can easily be used for simulations. 
    \cr
    
    \bold{The Price Paths:}
    \cr\cr
    The user must provide a function which generates the price paths.
    In the example section the function \code{wienerPath} creates a 
    Wiener Monte Carlo path from random innovations.
    The Wiener price path requires as input \code{b}, the annualized 
    cost-of-carry rate, and \code{sigma}, the annualized volatility of 
    the underlying security, to compute the drift and variance of the 
    path, these variables must be globally defined.
    \cr
    
    \bold{The Payoff Function:}
    \cr\cr
    The user must also provide a function which computes the payoff 
    value of the option. The example sections show how to write 
    payoff calculators for the plain vanilla option and for the 
    arithmetic Asian Option. As the only input argument the path matrix 
    is required. Again, the option parameters must be globally available.
    \cr
    
    \bold{The Monte Carlo Simulator:}
    \cr\cr   
    The simulator is the heart of the Monte Carlo valuation process. 
    This simulator performs \code{mcLoops} Monte Carlo loops each with 
    \code{mcSteps} Monte Carlo steps. In each loop the following steps
    are done: first the innovation matrix is created from the specified
    innovation generator (usually build from the normal pseudo random 
    number or low discrepancy generators), then anththetic innovations 
    are added if desired (by default \code{anththetic=TRUE}), then the 
    innovations can be standardized within each loop (by default 
    \code{standardization=FALSE}), and finally the average payoff of 
    all samples in the loop is computed. The simulation can be traced 
    loop by loop setting the argument \code{trace=TRUE}.
     
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\references{
Birge J.R. (1994);  
    \emph{Quasi-Monte Carlo Approaches to Option Pricing}, 
    Department of Industrial and Operations Engineering, 
    Technical Report 94--19, University of Michigan.

Boyle P. (1977);
    \emph{Options: A Monte Carlo approach},
    Journal of Finance, 32, 323--338.

Glasserman P. (2004); 
    \emph{Monte Carlo Methods in Financial Engineering},
    Springer-Verlag New York, Inc., 596 pp.

Jaeckel P. (2002); 
    \emph{Monte Carlo Methods in Finance}, 
    John Wiley and Sons Ltd, 222 pp. 

}


\examples{
## SOURCE("fOptions.D2-MonteCarloOptions")
   
## How to perform a Monte Carlo Simulation?
   
## First Step:
   xmpOptions("\nStart: Sobol Generator > ")
   # Write a function to generate the option's innovations. 
   # Use scrambled normal Sobol numbers:
   sobolInnovations = function(mcSteps, pathLength, init, ...) {
     # Create Normal Sobol Innovations:
     innovations = rnorm.sobol(mcSteps, pathLength, init, ...)
     # Return Value:
     innovations }
    
## Second Step: 
   xmpOptions("\nNext: Wiener Path > ")
   # Write a function to generate the option's price paths.  
   # Use a Wiener path:
   wienerPath = function(eps) { 
     # Note, the option parameters must be globally defined!
     # Generate the Paths:
     path = (b-sigma*sigma/2)*delta.t + sigma*sqrt(delta.t)*eps
     # Return Value:
     path }
      
## Third Step: 
   # Write a function for the option's payoff
   
   # Example 1: use the payoff for a plain Vanilla Call or Put:
   xmpOptions("\nNext: Plain Vanilla Payoff > ")
   plainVanillaPayoff = function(path) { 
     # Note, the option parameters must be globally defined!
     # Compute the Call/Put Payoff Value:
     ST = S*exp(sum(path))
     if (TypeFlag == "c") payoff = exp(-r*Time)*max(ST-X, 0)
     if (TypeFlag == "p") payoff = exp(-r*Time)*max(0, X-ST)
     # Return Value:
     payoff }
   
   # Example 2: use the payoff for an arithmetic Asian Call or Put:
   xmpOptions("\nNext: Arithmetic Asian Payoff > ")
   arithmeticAsianPayoff = function(path) { 
     # Note, the option parameters must be globally defined!
     # Compute the Call/Put Payoff Value:
     SM = mean(S*exp(cumsum(path)))
     if (TypeFlag == "c") payoff = exp(-r*Time)*max(SM-X, 0)
     if (TypeFlag == "p") payoff = exp(-r*Time)*max(0, X-SM)
     # Return Value:
     payoff }

## Final Step: 
   xmpOptions("\nNext: Set Option Parameters > ")
   # Set Global Parameters for the plain Vanilla / arithmetic Asian Options:
   TypeFlag <<- "c"; S <<- 100; X <<- 100
   Time <<- 1/12; sigma <<- 0.4; r <<- 0.10; b <<- 0.1
   
   # Do the Asian Simulation with scrambled random numbers:
   xmpOptions("\nNext: Monte Carlo Simulation > ")
   mc = MonteCarloOption(delta.t = 1/360, pathLength = 30, mcSteps = 5000, 
     mcLoops = 50, init = TRUE, innovations.gen = sobolInnovations, 
     path.gen = wienerPath, payoff.calc = arithmeticAsianPayoff, 
     antithetic = TRUE, standardization = FALSE, trace = TRUE, 
     scrambling = 2, seed = 4711)
   
   # Plot the MC Iteration Path:
   xmpOptions("\nNext: Output Results > ")
   par(mfrow = c(1, 1))
   mcPrice = cumsum(mc)/(1:length(mc))
   plot(mcPrice, type = "l", main = "Arithmetic Asian Option", 
     xlab = "Monte Carlo Loops", ylab = "Option Price")
   
   # Compare with Turnbull-Wakeman Approximation:
   TW = TurnbullWakemanAsianApproxOption(TypeFlag = "c", S = 100, SA = 100, 
     X = 100, Time = 1/12, time = 1/12, tau = 0 , r = 0.1, b = 0.1, 
     sigma = 0.4)$price
   print(TW)
   abline(h = TW, col = 2)
}


\keyword{programming}

