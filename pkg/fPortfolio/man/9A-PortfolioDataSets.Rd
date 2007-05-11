\name{PortfolioDataSets}

\alias{PortfolioDataSets}

\alias{vanIndices}

\alias{altInvest}
\alias{annualInvest}
\alias{assetsCorr}
\alias{berndtInvest}
\alias{jobstCov}

\alias{largecap.ts}
\alias{microcap.ts}
\alias{midcap.ts}
\alias{smallcap.ts}
\alias{midcapD.ts}
\alias{returns.three.ts}

\alias{SWXLP}


\title{fPortfolio Data Sets}


\description{

    Data sets used in the examples of the 'fPortfolio'
    package.

}


\format{

    The \emph{Van Hedge Fund Indices} were one of the first performance 
    benchmarks based on a large and representative sample of hedge fund 
    returns. Published from 1995 until now, the Van Indices reflect the 
    average  performance of hedge funds back to 1988. The Indices are 
    produced on a monthly basis. They represent the average performance 
    of hedge funds around the world and therefore tracks the performance 
    of the overall hedge fund universe. 
    
    All Index returns are based on hedge funds returns that are net of 
    fees. They are simple non-dollar-weighted averages. Indexes for 
    different time periods may be based on different funds, depending 
    on the hedge funds reporting to VAN at the time. The database, used 
    in construction of the Indices, contains information on approximately 
    5,300 hedge funds.

    The Van Global Hedge Fund Index is reported on a percentage-change 
    basis. The Index was set at an initial value of 1,000 as of its 
    January 1, 1988 inception.

    The csv data file consists of 16 columns, the first with the date
    entries formatted as "\%m/\%d/\%Y", followed by 14 columns with the
    Index, and the last column with the Global Hedge Fund Index.
    
    The Indexes are:
    
    AggressiveGrowth \cr             
    DistressedSecurities \cr      
    EmergingMarkets \cr               
    FundofFunds \cr                   
    Income \cr                        
    Macro \cr                         
    MarketNeutralArbitrage \cr        
    MarketNeutralHedging \cr
    MarketTiming \cr                  
    Opportunistic \cr                 
    SeveralStrategies \cr             
    ShortSelling \cr                  
    SpecialSituations \cr             
    Value \cr                         
    VanGlobalHedgeFundIndex    
    
}


\source{

    Van Hedge Fund Advisors International, Inc. \cr
    \emph{http://hedgefund.com/}
    
}


\examples{
## -
}


\keyword{data}

