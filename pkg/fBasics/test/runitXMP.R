

    require(fBasics)
    require(fCalendar)
    require(RUnit)
    
    # Economic and Finanacial Markets
    TRY = try(source("library/fBasics/demo/xmpDWChapter011.R"))
    checkTrue(class(TRY) != "try-error")
    
    # Financial Time Series Data
    TRY = try(source("library/fBasics/demo/xmpDWChapter012.R"))
    checkTrue(class(TRY) != "try-error")
    
    # Distribution Functions in Finance
    TRY = try(source("library/fBasics/demo/xmpDWChapter013.R"))
    checkTrue(class(TRY) != "try-error")
    
    # Stylized Facts, Structures and Dependencies
    TRY = try(source("library/fBasics/demo/xmpDWChapter014.R"))
    checkTrue(class(TRY) != "try-error")
    
    # Probability Theory and Hypothesis Tests
    TRY = try(source("library/fBasics/demo/xmpDWChapter015.R"))
    checkTrue(class(TRY) != "try-error")
    
    # Zivot Wang Chapter 1
    TRY = try(source("library/fBasics/demo/xmpZWChapter01.R"))
    checkTrue(class(TRY) != "try-error")
    
    