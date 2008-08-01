\name{PortfolioData}

\alias{PortfolioData}

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
\alias{LPP2005REC}
\alias{SPISECTOR}

\alias{equityFunds}


\title{fPortfolio Data Sets}


\description{

    Data sets used in the examples of the 'fPortfolio'
    package.
    \cr
    
    The data sets are:
    
    \tabular{ll}{   
    \code{altInvest} \tab Monthly Alternative Investment Data Set, \cr
    \code{annualInvest} \tab Annual Investment Data Set, \cr
    \code{assetsCorr} \tab US Asset Correlation Matrix, \cr
    \code{berndtInvest} \tab Berndt's Investment Data Set, \cr
    \code{jobstCov} \tab Covariance Matrix of 30 Stocks, \cr 
    %
    \code{largecap.ts} \tab Monthly US Largecap Equities, \cr
    \code{microcap.ts} \tab Monthly US Microcap Equities, \cr
    \code{midcap.ts} \tab Monthly US Midcap Equities, \cr
    \code{smallcap.ts} \tab Monthly US Smallcap Equities, \cr
    \code{midcapD.ts} \tab Daily US Midcap Equities, \cr
    \code{returns.three.ts} \tab Returns, \cr
    %
    \code{SWXLP} \tab Swiss Pension Fund LPP-2000, \cr
    \code{LPP2005REC} \tab Swiss Pension Fund LPP-2005, \cr
    \code{SPISECTOR} \tab SPI Sector Indices. \cr
    %
    \code{equityFunds} \tab Equity Funds. }

}


\details{

    \bold{Monthly Alternative Investment Data Set:}\cr
    \cr
    \code{altInvest} 
    is a monthly investment data set comparing equity and bond returns
        with an alternative investment in hedge funds. 
    The data set ranges from 1996-07-01 to 2003-11-01. 
    The columns are named: FUSEX, FBIDX, VANUS.
    \cr   
    FUSEX is the "Fidelity Spartan US Equity Index Inv" Fund.
    The investment seeks to provide investment results that correspond 
    to the total return performance of common stocks publicly traded 
    in the US. The fund normally invests at least 80\% of assets in 
    common stocks included in the Standard and Poor's 500 index, which 
    broadly represents the performance of common stocks publicly traded 
    in the US. In addition, the fund lends securities to earn 
    income [Google]. 
    \cr  
    FBIDX is the "Fidelity US Bond Index" Fund.
    The investment seeks to provide investment results that correspond 
    to the total return of the bonds in the Lehman Brothers Aggregate 
    Bond index. The fund normally invests at least 80\% of total assets 
    in bonds included in the Lehman Brothers Aggregate Bond index. It 
    uses statistical sampling techniques based on duration, maturity, 
    interest rate sensitivity, security structure, and credit 
    quality [Google].\cr
    
    VANUS is the Van Global Hedge Fund Index.
    \cr
    
    
    \bold{Annual Investment Data Set:}\cr
    \cr
    \code{annualInvest}
    is a monthly investment data set.\cr
    The data set ranges from 1973-12-31 to 1994-12-31.\cr
    The columns are named: TBills3m, LongBonds, SP500, Wilshire5000, NASDAQ,
        LehmanBonds, EAFE, Gold.\cr
    Source:\cr
    
    
    \bold{US Asset Correlation Matrix:}\cr
    \cr
    \code{assetsCorr} 
    is an upper correlation matrix of US investments.\cr
    The columns are named: LargeStocksUS, SmallStocksUS, CorporateBondsUS,
        TreasuryBondsUS, RealEstateUS, StocksCanada, StocksUK, StocksGermany,
        StocksSwitzerland, StocksEmergingMarkets.\cr
    Source:\cr
         
    
    \bold{Berndt's Investment Data Set:}\cr
    \cr
    \code{berndtInvest} 
    is a monthly data set of US equities including market risk free 
        returns.\cr
    The data set ranges from 1978-01-01 to 1987-12-01.
    The columns are named: CITCRP, CONED, CONTIL, DATGEN, DEC, DELTA, 
        GENMIL, GERBER, IBM, MARKET, MOBIL, PANAM, PSNH, TANDY, TEXACO, 
        WEYER, RKFREE.\cr
    Source:\cr

    
    \bold{Covariance Matrix of 30 Stocks:}\cr
    \cr
    \code{jobstCov} 
    is a covariance data set of 30 Stocks.\cr
    The columns are named: Stock01, ..., Stock30.\cr
    Source:\cr
    
    
    \bold{US Largecap Equities:}\cr
    \cr
    \code{largecap.ts} 
    is a monthly data set of US largecap equities.\cr
    The data set ranges from 1997-01-31 to 2001-12-31.\cr
    The columns are named: AMAT, AMGN, CAT, DD, G, GENZ, GM, HON, KR, LLTC, 
        MSFT, ORCL, PG, PHA, SO, TXN, UTX, WM, WYE, YHOO, market, t90.\cr
    Source:\cr
    
        
    \bold{US Microcap Equities:}\cr
    \cr
    \code{microcap.ts} 
    is a monthly data set of US microcap equities.\cr
    The data set ranges from 1997-01-31 to 2001-12-31.\cr
    The columns are named: GHI, PBCI, BY, SPNC, ZIF, NHC, NXR, HMSY, MPR, 
        PRCP, EVST, XLA, WSTL, GAIT, JOUT, IIVI, DMCO, FNIS, IOMT, FBCI, 
        market, t90. \cr
    Source:\cr
           
    
    \bold{US Midcap Equities:}\cr
    \cr
    \code{midcap.ts} 
    is a monthly data set of US midcap equities.\cr
    The data set ranges from 1997-01-31 to 2001-12-31.\cr
    The columns are named: MAT, EMN, LEG, AAPL, UTR, HB, BNK, APA, LNCR, 
        BMET, DBD, FAST, AF, CPWR, EC, SNV, HSY, TXT, APCC, LXK, market, 
        t90.\cr
    Source:\cr
    
    
    \bold{US Smallcap Equities:}\cr
    \cr
    \code{smallcap.ts} 
    is a monthly data set of US smallcap equities.\cr
    The data set ranges from 1997-01-31 to 2001-12-31.\cr
    The columns are named: MODI, MGF, MEE, FCEL, OII, SEB, RML, AEOS, BRC, 
        CTC, TNL, IBC, KWD, TOPP, RARE, HAR, BKE, GG, GYMB, KRON, market, 
        t90.\cr
    Source:\cr
    
           
    \bold{Daily US Midcap Equities:}\cr
    \cr
    \code{midcapD.ts} 
    is a daily data set of US midcap equities.\cr
    The data set ranges from 2000-01-03 to 2001-12-31.\cr
    The columns are named: LSCC, CSGS, EC, NYB, ALTR, APH, CLS, NET, SBUX, 
        AYE, ASBC, SBL, PCZ, OSI, TRP, ROH, SU, MTD, RAD, GUC, market. \cr
    Source:\cr

        
    \bold{Monthly Returns:}\cr
    \cr
    \code{returns.three.ts} 
    is a monthly data set of returns.\cr
    The data set ranges from 1991-02-28 to 1995-12-29.\cr
    The columns are named: RAL, GMH, IVX.\cr
    Source:\cr
    
    
    \bold{Swiss Pension Fund LPP-2000:}\cr
    \cr
    \code{SWXLP} 
    is a daily data set of the Swiss pension fund benchmark LPP-2000.\cr
    The data set ranges from 2000-01-03 to 2007-05-08.\cr
    The columns are named: SBI, SPI, SII, LP25, LP40, LP60.\cr
    Source:\cr
    
        
    \bold{Swiss Pension Fund LPP-2005:}\cr
    \cr
    \code{LPP2005REC} 
    is a daily data set of the Swiss pension fund benchmark LPP-2005.\cr
    The data set ranges from 2005-11-01 to 2007-04-11.\cr
    The columns are named: 
        SBI, SPI, SII, LMI, MPI, ALT, LPP25, LPP40, LPP60. \cr
    Source:\cr
    
    
    \bold{Swiss Performance Index Subsectors:}\cr
    \cr
    \code{SPISECTOR} 
    is a daily data set of the Swiss performance index subsectors.\cr
    The data set ranges from 1999-12-31 to 2008-06-18.\cr
    The columns are named: 
        SPI, BASICMAT, INDUSTRY, CONGOODS, HLTHCARE, CONSERV, TELECOM,
        UTILITIES, FINANCE, TECH. \cr
    Source:\cr
    
    
    \bold{Equity Funds:}\cr
    \cr
    \code{equityFunds} 
    is a daily data set of Equity Funds.\cr
    The data set ranges from 2002-01-01 to 2007-05-31.\cr
    The columns are named: EASTEU, LATAM, CHINA, INDIA, ENERGY, MINING, 
        GOLD, WATER.\cr
    Source:\cr

        
    \bold{Van Hedge Fund Indices:}\cr
    \cr
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


    Source: With permission from Van Hedge Fund Advisors International, Inc. \cr
    \emph{http://hedgefund.com/}
    
}


\format{
    
    Time series files are in CSV Excel spreadsheet format. 
    The delimiter is a semicolon. 
    
}


\examples{
## -
}


\keyword{data}

