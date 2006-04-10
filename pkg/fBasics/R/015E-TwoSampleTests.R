
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
# FUNCTION:             DISTRIBUTIONAL TESTS:
#  ks2Test               Two sample Kolmogorov-Smirnov test
# FUNCTION:             LOCATION TESTS:
#  .locationTest         Location test suite
#  tTest                 Unpaired t test for differences in mean
#  kw2Test               Kruskal-Wallis test for differences in locations  
# FUNCTION:             VARIANCE TESTS:
#  .varianceTest         Variance test suite
#  varfTest              F test for differences in variances
#  bartlett2Test         Bartlett's test for differences in variances
#  fligner2Test          Fligner-Killeen test for differences in variances
# FUNCTION:             SCALE TESTS:
#  .scaleTest            Scale test suite
#  ansariTest            Ansari-Bradley test for differences in scale
#  moodTest              Mood test for differences in scale
# FUNCTION:             CORRELATION TESTS:
#  .correlationTest      Correlation test suite
#  pearsonTest           Pearson product moment correlation coefficient
#  kendallTest           Kendall's tau correlation test
#  spearmanTest          Spearman's rho correlation test
################################################################################


################################################################################
# Distribution Tests:


ks2Test = 
function(x, y, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Two-sample Kolmogorov-Smirnov test.
    
    # Arguments:
    #   x - a numeric vector of data values.
    #   description - a brief description of the project of type 
    #       character.
    #   title - a character string which allows for a project title.
    
    # Note:
    #   A function partly copied from "stats"
    
    # FUNCTION:
    
    # Call:
    call = match.call()
    
    # Test:
    test = list()
    
    # Data Set Name:
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    test$data.name = DNAME
    
    # Check Type:
    if (class(x) == "zoo") {
        x = as.timeSeries(x)
    }
    if (class(x) == "timeSeries") {
        if (dim(x)[2] > 1) stop("x must be an univariate time series")
        x = as.vector(x)
    }
    if (class(y) == "zoo") {
        y = as.timeSeries(y)
    }
    if (class(y) == "timeSeries") {
        if (dim(y)[2] > 1) stop("y must be an univariate time series")
        y = as.vector(y)
    }
    
    # Compute Test:
    if (!exists("ks.test")) {
        two.sided = ks.gof(x = x, y = y, alternative = "two.sided")
        exact     = list(); exact$p.value = NA;   exact$statistic = NA
        less      = list(); less$p.value = NA;    less$statistic = NA
        greater   = list(); greater$p.value = NA; greater$statistic = NA
    } else { 
        two.sided = ks.test(x = x, y = y, alternative = "two.sided")
        exact     = ks.test(x = x, y = y, exact = TRUE, alternative = "two.sided")
        less      = ks.test(x = x, y = y, alternative = "less")
        greater   = ks.test(x = x, y = y, alternative = "greater")
    }

    # P Value:
    PVAL = c(
        two.sided$p.value, 
        exact$p.value, 
        less$p.value, 
        greater$p.value)
    names(PVAL) = c(
        "Alternative       Two-Sided", 
        "Alternative Exact Two-Sided",
        "Alternative            Less", 
        "Alternative         Greater")
    test$p.value = PVAL
    
    # Statistic:
    STATISTIC = c(
        two.sided$statistic, 
        less$statistic, 
        greater$statistic)
    names(STATISTIC) = c(
        "D | Two Sided", 
        "   D^- | Less", 
        "D^+ | Greater")
    test$statistic = STATISTIC
    
    # Add:
    if (is.null(title)) title = "Kolmogorov-Smirnov Two Sample Test"
    if (is.null(description)) description = date()
    
    # Return Value:
    new("fHTEST",     
        call = call,
        data = list(x = x, y = y), 
        test = test,
        title = as.character(title), 
        description = as.character(description) ) 
}


# ******************************************************************************
# Location Tests:


.locationTest =
function(x, y, method = c("t", "kruskal2"), 
title = NULL, description = NULL) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Correlation Tests
       
    # FUNCTION:
    
    # Test:
    if (method[1] == "t") {
        ans = tTest(x, y, title = title, description = description) 
    }
    if (method[1] == "kruskal2Test") {
        ans = kw2Test(x, y, title = title, description = description) 
    }  
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


tTest = 
function(x, y, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #    Tests if two population means are equal. 
    
    # Arguments:
    #   x, y - two numeric vector of data values or time series objects
    #   description - a brief description of the porject of type character.
    #   title - a character string which allows for a project title.
    
    # Note:
    #   The function 't.test' comes in R and SPlus with the same
    #   arguments, so this function 'tTest' can be used also under 
    #   SPlus.
    
    # FUNCTION:
    
    # Call:
    call = match.call()
    
    # Test:
    test = list()
    
    # Data Set Name:
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    test$data.name = DNAME
    
    # Check Type:
    if (class(x) == "zoo") {
        x = as.timeSeries(x)
    }
    if (class(x) == "timeSeries") {
        if (dim(x)[2] > 1) stop("x must be an univariate time series")
        x = as.vector(x)
    }
    if (class(y) == "zoo") {
        y = as.timeSeries(y)
    }
    if (class(y) == "timeSeries") {
        if (dim(y)[2] > 1) stop("y must be an univariate time series")
        y = as.vector(y)
    }
    
    # Asymptotic Test:
    two.sided = t.test(x = x, y = y, alternative = "two.sided",
        mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
    less = t.test(x = x, y = y, alternative = "less",
        mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
    greater = t.test(x = x, y = y, alternative = "greater",
        mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
    
    # Assume Equal Variances:    
    two.sided.equal = t.test(x = x, y = y, alternative = "two.sided",
        mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
    less.equal = t.test(x = x, y = y, alternative = "less",
        mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
    greater.equal = t.test(x = x, y = y, alternative = "greater",
        mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
        
    # Sample Estimates:
    PARAMETER = c(length(x), length(y), 0)
    names(PARAMETER) = c(
        "x Observations", 
        "y Observations",
        "mu")
    test$parameter = PARAMETER
    
    # Sample Estimates:
    ESTIMATE = c(two.sided$estimate, var(x), var(y))
    names(ESTIMATE) = c("Mean of x", "Mean of y", "Var  of x", "Var  of y")
    test$estimate = ESTIMATE
    
    # P Values:
    PVAL = c(
        two.sided$p.value, 
        less$p.value, 
        greater$p.value, 
        two.sided.equal$p.value,
        less.equal$p.value,
        greater.equal$p.value)
    names(PVAL) = c(
        "Alternative Two-Sided", 
        "Alternative      Less",
        "Alternative   Greater", 
        "Alternative Two-Sided | Equal Var", 
        "Alternative      Less | Equal Var",
        "Alternative   Greater | Equal Var")
    test$p.value = PVAL   
    
    # Statistic:  
    STATISTIC = c(
        two.sided$statistic, 
        two.sided.equal$statistic)
    names(STATISTIC) = c(
        "            T", 
        "T | Equal Var")
    test$statistic = STATISTIC
    
    # Confidence Intervals:
    CONF.INT = cbind(
        a = two.sided$conf.int, 
        b = less$conf.int, 
        c = greater$conf.int, 
        d = two.sided.equal$conf.int, 
        e = less.equal$conf.int, 
        f = greater.equal$conf.int)
    # For Splus compatibility use named a CONF.INT
    # and dimnames instead of colnames!
    dimnames(CONF.INT)[[2]] = c(
        "Two-Sided", 
        "     Less", 
        "  Greater", 
        "Two-Sided | Equal Var", 
        "     Less | Equal Var", 
        "  Greater | Equal Var")
    test$conf.int = CONF.INT     
      
    # Add:
    if (is.null(title)) title = "t Test"
    if (is.null(description)) description = date()
    
    # Return Value:
    new("fHTEST",     
        call = call,
        data = list(x = x, y = y), 
        test = test,
        title = as.character(title), 
        description = as.character(description) ) 
}


# ------------------------------------------------------------------------------ 


kw2Test = 
function(x, y, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Performs a Kruskal-Wallis rank sum test of the null that 
    #   the location parameters of the distribution of x are the 
    #   same in each group (sample). The alternative is that they 
    #   differ in at least one. 
    
    # Arguments:
    #   x, y - two numeric vector of data values or time series objects
    #   description - a brief description of the porject of type character.
    #   title - a character string which allows for a project title.
    
    # Note:
    #   A function linked to "stats"
    
    # FUNCTION:
    
    # Call:
    call = match.call()
    
    # Test:
    test = list()
    
    # Data Set Name:
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    test$data.name = DNAME
   
    # Check Type:
    if (class(x) == "zoo") {
        x = as.timeSeries(x)
    }
    if (class(x) == "timeSeries") {
        if (dim(x)[2] > 1) stop("x must be an univariate time series")
        x = as.vector(x)
    }
    if (class(y) == "zoo") {
        y = as.timeSeries(y)
    }
    if (class(y) == "timeSeries") {
        if (dim(y)[2] > 1) stop("y must be an univariate time series")
        y = as.vector(y)
    }
    
    # Sample Estimates:
    ESTIMATE = c(mean(x), mean(y), var(x), var(y))
    names(ESTIMATE) = c("Mean of x", "Mean of y", "Var  of x", "Var  of y")
    test$estimate = ESTIMATE
    
    # Parameter:
    PARAMETER = c(length(x), length(y))
    names(PARAMETER) = c(
        "x Observations", 
        "y Observations")
    test$parameter = PARAMETER
    
    # Operate on Lists:
    x = list(x = x, y = y)
    if (length(x) < 2) stop("x must be a list with at least 2 elements")
    k = length(x)
    l = sapply(x, "length")
    g = factor(rep(1 : k, l))
    x = unlist(x)
   
    # Test:
    n = length(x)
    if (n < 2) stop("not enough observations")
    r = rank(x)
    TIES = table(x)
    
    # Statistic:
    STATISTIC = sum(tapply(r, g, "sum")^2 / tapply(r, g, "length"))
    STATISTIC = ((12 * STATISTIC / (n * (n + 1)) - 3 * (n + 1)) /
        (1 - sum(TIES^3 - TIES) / (n^3 - n)))
    names(STATISTIC) = "KW chi-squared"
    test$statistic = STATISTIC
    
    # P Value:
    PVAL = 1 - pchisq(STATISTIC, 1)
    names(PVAL) = ""
    test$p.value = PVAL
    
    # Add:
    if(is.null(title)) title = "Kruskal-Wallis Two Sample Test"
    if(is.null(description)) description = date()  
    
    # Return Value:
    new("fHTEST",     
        call = call,
        data = list(x = x, y = y), 
        test = test,
        title = as.character(title), 
        description = as.character(description) )
}


# ******************************************************************************
# Variance Tests:


.varianceTest =
function(x, y, method = c("varf", "bartlett2", "fligner2"), 
title = NULL, description = NULL) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Correlation Tests
       
    # FUNCTION:
    
    # Test:
    if (method[1] == "varf") {
        ans = varfTest(x, y, title = title, description = description) 
    }
    if (method[1] == "bartlett2") {
        ans = bartlett2Test(x, y, title = title, description = description) 
    }  
    if (method[1] == "fligner2") {
        ans = fligner2Test(x, y, title = title, description = description) 
    } 
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


varfTest = 
function(x, y, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Performs an F test to compare the variances of two samples 
    #   from normal populations. 

    # Arguments:
    #   x, y - a numeric vector of data values.
    #   description - a brief description of the porject of type 
    #       character.
    #   title - a character string which allows for a project title.
    
    # Notes:
    #   A modified copy originally from R's ctest package Version 1.8.1
    
    # FUNCTION:
    
    # Call:
    call = match.call()
    
    # Test:
    test = list()
    
    # Data Set Name:
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    test$data.name = DNAME
    
    # Check Type:
    if (class(x) == "zoo") {
        x = as.timeSeries(x)
    }
    if (class(x) == "timeSeries") {
        if (dim(x)[2] > 1) stop("x must be an univariate time series")
        x = as.vector(x)
    }
    if (class(y) == "zoo") {
        y = as.timeSeries(y)
    }
    if (class(y) == "timeSeries") {
        if (dim(y)[2] > 1) stop("y must be an univariate time series")
        y = as.vector(y)
    }
    
    # Estimate - Hypothesized Equal Variance:
    ratio = 1
    DF.x = length(x) - 1
    DF.y = length(y) - 1
    VAR.x = var(x)
    VAR.y = var(y)
    ESTIMATE = VAR.x / VAR.y / ratio
    names(ESTIMATE) = "Ratio of Variances"
    test$estimate = ESTIMATE
   
    # Parameter:
    PARAMETER = c(ratio, DF.x, DF.y)
    names(PARAMETER) = c(
        "Hypothesized Ratio", 
        "Numerator   df", 
        "Denumerator df")
    test$parameter = PARAMETER
    
    # Statistic:
    STATISTIC = ESTIMATE / ratio
    names(STATISTIC) = "F"
    test$statistic = STATISTIC
    
    # P Value:
    p = pf(STATISTIC, DF.x, DF.y)
    PVAL = c(
        two.sided = 2 * min(p, 1 - p), 
        less = p, 
        greater = 1 - p)
    names(PVAL) = c(
        "Alternative Two-Sided", 
        "Alternative      Less",
        "Alternative   Greater")
    test$p.value = PVAL
    
    # Confidence Interval:
    conf.level = 0.95
    B = (1 - conf.level) / 2
    two.sided = c(ESTIMATE/qf(1-B, DF.x, DF.y), ESTIMATE/qf(B, DF.x, DF.y))
    less = c(0, ESTIMATE/qf(1-conf.level, DF.x, DF.y)) 
    greater = c(ESTIMATE/qf(conf.level, DF.x, DF.y), Inf) 
    CONF.INT = cbind(
        a = two.sided, 
        b = less, 
        c = greater)
    dimnames(CONF.INT)[[2]] = c(
        "Two-Sided", 
        "     Less", 
        "  Greater")
    test$conf.int = CONF.INT
   
    # Add:
    if(is.null(title)) title = "F Test of Variances"
    if(is.null(description)) description = date()
        
    # Return Value:
    new("fHTEST",     
        call = call,
        data = list(x = x, y = y), 
        test = test,
        title = as.character(title), 
        description = as.character(description) )
}


# ------------------------------------------------------------------------------


bartlett2Test = 
function(x, y, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Bartlett's test for differences in variances
    
    # Arguments:
    #   x - a numeric vector of data values.
    
    # Note:
    #   # A function linked to "stats"

    # FUNCTION:
    
    # Call:
    call = match.call()
    
    # Test:
    test = list()
    
    # Data Set Name:
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    test$data.name = DNAME
    
    # Check Type:
    if (class(x) == "zoo") {
        x = as.timeSeries(x)
    }
    if (class(x) == "timeSeries") {
        if (dim(x)[2] > 1) stop("x must be an univariate time series")
        x = as.vector(x)
    }
    if (class(y) == "zoo") {
        y = as.timeSeries(y)
    }
    if (class(y) == "timeSeries") {
        if (dim(y)[2] > 1) stop("y must be an univariate time series")
        y = as.vector(y)
    }
    
    # Settings:
    x = list(x = x, y = y)
    k = length(x)
    n = sapply(x, "length") - 1
    v = sapply(x, "var")
    n.total = sum(n)
    v.total = sum(n * v) / n.total
    
    # Statistic:
    STATISTIC = ((n.total * log(v.total) - sum(n * log(v))) /
        (1 + (sum(1 / n) - 1 / n.total) / (3 * (k - 1))))
    names(STATISTIC) = "Bartlett's Chi-squared"
    test$statistic = STATISTIC
    
    # P Value:
    PVAL = 1 - pchisq(STATISTIC, 1)    
    names(PVAL) = ""
    test$p.value = PVAL  
    
    # Add:
    if(is.null(title)) title = "Bartlett Test for Homogeneity of Variances"
    if(is.null(description)) description = date()  
    
    # Return Value:
    new("fHTEST",     
        call = call,
        data = list(x = x, y = y), 
        test = test,
        title = as.character(title), 
        description = as.character(description) )
}


# ------------------------------------------------------------------------------
   

fligner2Test = 
function(x, y, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fligner-Killeen's rank based test for homogeneity of variances
 
    # Arguments:
    #   x - a numeric vector of data values.
    
    # Note:
    #   A function linked to "stats"
    
    # FUNCTION:
    
    # Call:
    call = match.call()
    
    # Test:
    test = list()
    
    # Data Set Name:
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    test$data.name = DNAME
    
    # Check Type:
    if (class(x) == "zoo") {
        x = as.timeSeries(x)
    }
    if (class(x) == "timeSeries") {
        if (dim(x)[2] > 1) stop("x must be an univariate time series")
        x = as.vector(x)
    }
    if (class(y) == "zoo") {
        y = as.timeSeries(y)
    }
    if (class(y) == "timeSeries") {
        if (dim(y)[2] > 1) stop("y must be an univariate time series")
        y = as.vector(y)
    }
    
    # Settings:
    x = list(x = x, y = y)
    k = length(x)
    l = sapply(x, "length")
    g = factor(rep(1 : k, l))
    x = unlist(x)
   
    # Statistic:
    n = length(x)
    x = unlist(tapply(x, g, function(u) u - median(u)))
    a = qnorm((1 + rank(abs(x)) / (n + 1)) / 2)
    STATISTIC = sum(tapply(a, g, "sum")^2 / tapply(a, g, "length"))
    STATISTIC = (STATISTIC - n * mean(a)^2) / var(a)
    names(STATISTIC) = "FK:med chi-squared"
    test$statistic = STATISTIC

    # P Value:
    PVAL = 1 - pchisq(STATISTIC, 1)
    names(PVAL) = ""
    test$p.value = PVAL
    
    # Add:
    if(is.null(title)) title = "Fligner-Killeen Test for Homogeneity of Variances"
    if(is.null(description)) description = date()  
    
    # Return Value:
    new("fHTEST",     
        call = call,
        data = list(x = x, y = y), 
        test = test,
        title = as.character(title), 
        description = as.character(description) )
}


# ******************************************************************************
# Scale Tests:


.scaleTest =
function(x, y, method = c("ansari", "mood"), 
title = NULL, description = NULL) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Correlation Tests
       
    # FUNCTION:
    
    # Test:
    if (method[1] == "ansari") {
        ans = ansariTest(x, y, title = title, description = description) 
    }
    if (method[1] == "mood") {
        ans = moodTest(x, y, title = title, description = description) 
    }  
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


ansariTest = 
function(x, y, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Ansari-Bradley's test for differences in scale
    
    # Arguments:
    #   x - a numeric vector of data values.
    #   description - a brief description of the porject of type 
    #       character.
    #   title - a character string which allows for a project title.
    
    # Note:
    #   A function copied from "stats" for SPlus compatibility
    
    # FUNCTION:
    
    # Call:
    call = match.call()
    
    # Test:
    test = list()
    
    # Data Set Name:
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    test$data.name = DNAME
     
    # Check Type:
    if (class(x) == "zoo") {
        x = as.timeSeries(x)
    }
    if (class(x) == "timeSeries") {
        if (dim(x)[2] > 1) stop("x must be an univariate time series")
        x = as.vector(x)
    }
    if (class(y) == "zoo") {
        y = as.timeSeries(y)
    }
    if (class(y) == "timeSeries") {
        if (dim(y)[2] > 1) stop("y must be an univariate time series")
        y = as.vector(y)
    }
    
    # Test:
    two.sided = .ansariTest(x = x, y = y, alternative = "two.sided",
        exact = FALSE, conf.int = TRUE, conf.level = 0.95)
    less = .ansariTest(x = x, y = y, alternative = "less",
        exact = FALSE, conf.int = TRUE, conf.level = 0.95)
    greater = .ansariTest(x = x, y = y, alternative = "greater",
        exact = FALSE, conf.int = TRUE, conf.level = 0.95)
        
    two.sided.exact = .ansariTest(x = x, y = y, alternative = "two.sided",
        exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    less.exact = .ansariTest(x = x, y = y, alternative = "less",
        exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    greater.exact = .ansariTest(x = x, y = y, alternative = "greater",
        exact = TRUE, conf.int = TRUE, conf.level = 0.95)

    # Statistic:
    STATISTIC = c(two.sided$statistic)
    names(STATISTIC) = "AB"
    test$statistic = STATISTIC
    
    # P Values:
    PVAL = c(
        two.sided$p.value, 
        two.sided.exact$p.value,
        less$p.value, 
        less.exact$p.value,
        greater$p.value, 
        greater.exact$p.value)
    names(PVAL) = c(
        "Alternative Two-Sided        ", 
        "Alternative Two-Sided | Exact",
        "Alternative      Less        ", 
        "Alternative      Less | Exact",
        "Alternative   Greater        ", 
        "Alternative   Greater | Exact")
    test$p.values = PVAL
    
    # Confidence Levels:
    CONF.INT = cbind(
        a = two.sided$conf.int, 
        b = two.sided.exact$conf.int,
        c = less$conf.int, 
        d = less.exact$conf.int,
        e = greater$conf.int, 
        f = greater.exact$conf.int)
    # For Splus compatibility use named a CONF.INT
    # and dimnames instead of colnames!
    dimnames(CONF.INT)[[2]] = c(
        "Two-Sided | Asymptotic ", 
        "Two-Sided |      Exact ", 
        "Less      | Asymptotic ", 
        "Less      |      Exact ",
        "Greater   | Asymptotic ", 
        "Greater   |      Exact ")
    test$conf.int = CONF.INT
    
    # Add:
    if(is.null(title)) title = "Ansari-Bradley Test for Scale"
    if(is.null(description)) description = date()

    # Return Value:
    new("fHTEST",     
        call = call,
        data = list(x = x, y = y), 
        test = test,
        title = as.character(title), 
        description = as.character(description) )
}


# ------------------------------------------------------------------------------


dansariw = 
function(x = NULL, m, n = m)
{   # A function Implemented by Diethelm Wuertz
    
    # Arguments:
    #   x - if x is null, then all available density-values are
    #       returned, the names of the vector belong to the
    #       allowed x values.
    
    # Example:
    #   dansariw(m = 3, n = 4)
    
    # FUNCTION:
    
    astart = 0
    L1 = 1 + floor(m *n/2)
    A1 = A2 = A3 = rep(-99, times = L1)
    IFAULT = 0
    lower = floor((m+1)^2/4)
    upper = lower + floor(m*n/2)
    Q = lower:upper
    
    # Density:
    result = .Fortran("asgscale", as.integer(m), as.integer(n), 
        as.double(astart), as.double(A1), as.integer(L1), as.double(A2), 
        as.double(A3), as.integer(IFAULT), PACKAGE = "fBasics")
        
    # Result:
    ans = result[[4]]/choose(m+n, n)
    if (is.null(x)) {
        names(ans) = as.character(Q)
    } else {
        x = as.integer(x)
        d = rep(0, times = length(x))   
        for (i in 1:length(x)) {
            if (x[i] >= upper) {
                d[i] = 1
            } else {
                if (x[i] >= lower) d[i] = ans[x[i]+1-lower]
            }
        } 
        ans = d
        names(ans) = as.character(x)
    }
    
    # Return Value:
    ans

}


# ------------------------------------------------------------------------------


if (class(version) != "Sversion") {
.pansariw = 
function(p, m, n) 
{   # A function Implemented by Diethelm Wuertz

    # Internal R Function:
    .C("pansari", as.integer(length(q)), p = as.double(q),
        as.integer(m), as.integer(n), PACKAGE = "stats")$p 
}}


# ------------------------------------------------------------------------------


pansariw =
function(q = NULL, m, n = m)
{   # A function Implemented by Diethelm Wuertz
    
    # Arguments:
    #   q - if q is null, then all available p-values are returned, 
    #       the names of the vector belong to the allowed q values
    
    # Example:
    #   pansariw(m = 3, n = 4)
    
    # Note:
    #   There exists an undocumented C function in R:
    #   .pansari = function(q, m, n) {
    #       .C("pansari", as.integer(length(q)), p = as.double(q),
    #           as.integer(m), as.integer(n), PACKAGE = "stats")$p }
    
    
    # FUNCTION:
    
    # Settings:
    astart = 0
    L1 = 1 + floor(m *n/2)
    A1 = A2 = A3 = rep(-99, times = L1)
    IFAULT = 0
    lower = floor((m+1)^2/4)
    upper = lower + floor(m*n/2)
    Q = lower:upper
    
    # p-values:
    result = .Fortran("wprob", as.integer(m), as.integer(n), 
        as.double(astart), as.double(A1), as.integer(L1), 
        as.double(A2), as.double(A3), as.integer(IFAULT), 
        PACKAGE = "fBasics")
        
    # Result:
    ans = result[[4]]
    if (is.null(q)) {
        names(ans) = as.character(Q)
    } else {
        q = as.integer(q)
        p = rep(0, times = length(q))   
        for (i in 1:length(q)) {
            if (q[i] >= upper) {
                p[i] = 1
            } else {
                if (q[i] >= lower) p[i] = ans[q[i]+1-lower]
            }
        } 
        ans = p
        names(ans) = as.character(q)
    }
    
    # Return Value:
    ans
}
    

# ------------------------------------------------------------------------------


if (class(version) != "Sversion") {
.qansariw = 
function(p, m, n) 
{   # A function Implemented by Diethelm Wuertz

    # Internal R Function:
    .C("qansari", as.integer(length(p)), q = as.double(p),
        as.integer(m), as.integer(n), PACKAGE = "stats")$q 
}}
    

# ------------------------------------------------------------------------------


qansariw =
function(p, m, n = m)
{   # A function Implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Arguments:
    #   p - if p is null, then all available quantiles are returned, 
    #       the names of the vector belong to the allowed p values
    
    # Example:
    #   qansariw(pansariw(m = 3, n = 4), m = 3, n = 4)
    #   qansariw((0:10)/10,  m = 3, n = 4)
    
    # Note:
    #   There exists an undocumented C function in R:
    #   .qansari = function(p, m, n) {
    #       .C("qansari", as.integer(length(p)), q = as.double(p),
    #           as.integer(m), as.integer(n), PACKAGE = "stats")$q }
    
    
    # Settings:
    P = pansariw(q = NULL, m = m, n = n)
    q = 0 * p 
    
    # Quantiles:
    for ( i in 1:length(p) ) {
        index = sign(P-p[i])
        q[i] = as.integer(names(index[index >= 0])[1])
    }
    ans = q
    
    # Return Value:
    ans 
}


# ------------------------------------------------------------------------------


.ansariTest =
function(x, y, alternative = c("two.sided", "less", "greater"),
exact = TRUE, conf.int = FALSE, conf.level = 0.95, ...)
{
    # A modifed copy from R 1.9.1
    
    # Data Set Name:
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    
    # DW 2005-02-18
    cint = NA
    ESTIMATE = NA
    ###
    
    alternative = match.arg(alternative)
    if (conf.int) {
        if (!((length(conf.level) == 1) && is.finite(conf.level)
             && (conf.level > 0) && (conf.level < 1)))
            stop("conf.level must be a single number between 0 and 1")
    }

    # x = x[complete.cases(x)]
    # y = y[complete.cases(y)]
    
    m = length(x)
    if (m < 1) stop("not enough x observations")
    n = length(y)
    if (n < 1) stop("not enough y observations")
    
    # DW - Made Global:
    N <<- m + n
    EVEN <<- ((N %% 2) == 0)

    r = rank(c(x, y))
    STATISTIC = sum(pmin(r, N - r + 1)[seq(along = x)])
    TIES = (length(r) != length(unique(r)))

    # DW - Always select:
    # if (is.null(exact)) exact = ((m < 50) && (n < 50))

    if (exact && !TIES) {
        PVAL = switch(alternative,
           two.sided = {
               if (STATISTIC > ((m + 1)^2 %/% 4 + ((m * n) %/% 2) / 2))
                   p = 1 - pansariw(STATISTIC - 1, m, n)
               else
                   p = pansariw(STATISTIC, m, n)
               min(2 * p, 1) },
           less = 1 - pansariw(STATISTIC - 1, m, n),
           greater = pansariw(STATISTIC, m, n))
        if (conf.int) {
            alpha = 1 - conf.level
            x = sort(x)
            y = sort(y)
            
            ab <<- function(sig) {
                rab = rank(c(x/sig, y))
                sum(pmin(rab, N - rab + 1)[seq(along = x)])
            }
            ratio = outer(x,y,"/")
            aratio = ratio[ratio >= 0]
            sigma = sort(aratio)

            cci <<- function(alpha) {
                u = absigma - qansariw(alpha/2,  m, n)
                l = absigma - qansariw(1 - alpha/2, m, n)
                # Check if the statistic exceeds both quantiles first.
                uci = NULL
                lci = NULL
                if (length(u[u >= 0]) == 0 || length(l[l > 0]) == 0) {
                  warning(paste("Samples differ in location: Cannot",
                                "compute confidence set, returning NA"))
                  return(c(NA, NA))
                }
                if (is.null(uci)) {
                  u[u < 0] = NA
                  uci = min(sigma[which(u == min(u, na.rm = TRUE))])
                }
                if (is.null(lci)) {
                  l[l <= 0] = NA
                  lci = max(sigma[which(l == min(l, na.rm = TRUE))])
                }
                # The process of the statistics does not need to be
                # monotone in sigma: check this and interchange quantiles.
                if (uci > lci) {
                  l = absigma - qansariw(alpha/2,  m, n)
                  u = absigma - qansariw(1 - alpha/2, m, n)
                  u[u < 0] = NA
                  uci = min(sigma[which(u == min(u, na.rm = TRUE))])
                  l[l <= 0] = NA
                  lci = max(sigma[which(l == min(l, na.rm = TRUE))])
                }
                c(uci, lci)
            }

            cint = if (length(sigma) < 1) {
                warning("Cannot compute confidence set, returning NA")
                c(NA, NA)
            } else {
                # Compute statistics directly: 
                absigma = sapply(sigma + c(diff(sigma)/2,
                    sigma[length(sigma)]*1.01), ab)
                switch(alternative, 
                    two.sided = { cci(alpha) }, 
                    greater = { c(cci(alpha*2)[1], Inf) }, 
                    less= { c(0, cci(alpha*2)[2]) })
            }
            attr(cint, "conf.level") = conf.level
            u = absigma - qansariw(0.5, m, n)
            sgr = sigma[u <= 0]
            if (length(sgr) == 0) {
                sgr = NA
            } else {
                sgr = max(sgr)
            }
            sle = sigma[u > 0]
            if (length(sle) == 0) {
                sle = NA
            } else {
                sle = min(sle)
            }
            ESTIMATE = mean(c(sle, sgr))
        }
    } else {
        EVEN = ((N %% 2) == 0)
        normalize <<- function(s, r, TIES, m = length(x), n=length(y)) {
            z = if (EVEN) s-m*(N+2)/4 else s-m*(N+1)^2/(4*N)
            if (!TIES) {
                SIGMA = 
                if (EVEN) sqrt((m*n *(N+2)*(N-2))/(48*(N-1)))
                else sqrt((m*n*(N+1)*(3+N^2))/(48*N^2))
            } else {
                r = rle(sort(pmin(r, N - r + 1)))
                SIGMA = 
                if (EVEN) sqrt(m*n*(16*sum(r$l*r$v^2)-N*(N+2)^2)/(16*N*(N-1)))
                else sqrt(m*n*(16*N*sum(r$l*r$v^2)-(N+1)^4)/(16*N^2*(N-1)))
            }
            z / SIGMA
        }
        p = pnorm(normalize(STATISTIC, r, TIES))
        PVAL = switch(alternative, 
            two.sided = 2 * min(p, 1 - p),
            less = 1 - p, 
            greater = p)

        if (conf.int && !exact) {
            alpha = 1 - conf.level
            ab <<- function(sig, zq) {
                r = rank(c(x / sig, y))
                s = sum(pmin(r, N -r + 1)[seq(along = x)])
                TIES = (length(r) != length(unique(r)))
                normalize(s, r, TIES, length(x), length(y)) - zq
            }
            # Use uniroot here. Compute the range of sigma first.
            srangepos = NULL
            srangeneg = NULL
            if (any(x[x > 0]) && any(y[y > 0]))
                srangepos <-
                    c(min(x[x > 0], na.rm = TRUE)/max(y[y > 0], na.rm = TRUE),
                      max(x[x > 0], na.rm = TRUE)/min(y[y > 0], na.rm = TRUE))
            if (any(x[x <= 0]) && any(y[y < 0]))
                srangeneg <-
                    c(min(x[x <= 0], na.rm = TRUE)/max(y[y < 0], na.rm = TRUE),
                      max(x[x <= 0], na.rm = TRUE)/min(y[y < 0], na.rm = TRUE))
            if (any(is.infinite(c(srangepos, srangeneg)))) {
                warning(paste("Cannot compute asymptotic confidence",
                    "set or estimator"))
                conf.int = FALSE
            } else {
                ccia <<- function(alpha) {
                    # Check if the statistic exceeds both quantiles first.
                    statu = ab(srange[1], zq = qnorm(alpha/2))
                    statl = ab(srange[2], zq = qnorm(alpha/2, lower.tail = FALSE))
                    if (statu > 0 || statl < 0) {
                        warning(paste("Samples differ in location:",
                            "Cannot compute confidence set,",
                            "returning NA"))
                        return(c(NA, NA))
                    }
                    u = uniroot(ab, srange, tol = 1.0e-4,
                        zq = qnorm(alpha/2))$root
                    l = uniroot(ab, srange, tol = 1.0e-4,
                        zq = qnorm(alpha/2, lower.tail = FALSE))$root
                    # The process of the statistics does not need to be
                    # monotone: sort is ok here.
                    sort(c(u, l))
                }
                srange <<- range(c(srangepos, srangeneg), na.rm = FALSE)
                cint = switch(alternative, 
                    two.sided = { ccia(alpha) }, 
                    greater = { c(ccia(alpha*2)[1], Inf) }, 
                    less = { c(0, ccia(alpha*2)[2]) })
                attr(cint, "conf.level") = conf.level
                ## Check if the statistic exceeds both quantiles first.
                statu = ab(srange[1], zq = 0)
                statl = ab(srange[2], zq = 0)
                if (statu > 0 || statl < 0) {
                    ESTIMATE = NA
                    warning("Cannot compute estimate, returning NA")
                } else
                    ESTIMATE = uniroot(ab, srange, tol = 1.0e-4, zq = 0)$root
            }
        }
        if (exact && TIES) {
            warning("Cannot compute exact p-value with ties")
            if (conf.int)
                warning(paste("Cannot compute exact confidence",
                    "intervals with ties"))
        }
    }

    # Result:
    names(STATISTIC) = "AB"
    RVAL = list(statistic = STATISTIC, p.value = PVAL,
        null.value = c("Ratio of Scales" = 1), alternative = alternative,
        method = "Ansari-Bradley Test", data.name = DNAME)
        
    if (conf.int)
        RVAL = c(RVAL, list(conf.int = cint,
            estimate = c("Ratio of Scales" = ESTIMATE)))
            
    # Return Value:
    class(RVAL) = "list"
    RVAL
}


# ------------------------------------------------------------------------------


moodTest = 
function(x, y, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Performs Mood's two-sample test for a difference in 
    #   scale parameters. 
    
    # Arguments:
    #   x, y - a numeric vector of data values.
    #   description - a brief description of the porject of type 
    #       character.
    #   title - a character string which allows for a project title.
    
    # Notes:
    #   A modified copy originally from R's ctest package Version 1.8.1
    
    # FUNCTION:
    
    # Call:
    call = match.call()
    
    # Test:
    test = list()
    
    # Data Set Name:
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    test$data.name = DNAME
    
    # Check Type:
    if (class(x) == "zoo") {
        x = as.timeSeries(x)
    }
    if (class(x) == "timeSeries") {
        if (dim(x)[2] > 1) stop("x must be an univariate time series")
        x = as.vector(x)
    }
    if (class(y) == "zoo") {
        y = as.timeSeries(y)
    }
    if (class(y) == "timeSeries") {
        if (dim(y)[2] > 1) stop("y must be an univariate time series")
        y = as.vector(y)
    }

    # Check Data:
    m = length(x)
    n = length(y)
    s = m + n
    if (s < 3) stop("not enough observations")
  
    # Statistic:
    r = rank(c(x, y)) 
    # From R:
    # z = ((sum((r[1:length(x)] - (s + 1) / 2)^2) - m * (s^2 - 1) / 12)
    #      / sqrt(m * n * (s + 1) * (s + 2) * (s - 2) / 180) )
    # To run also under S-Plus use ...      
    a = sum( (r[1:length(x)]-0.5*(s+1))^2 ) - m*(s*s-1)/12
    b = sqrt(as.numeric(m) * n * (s + 1) * (s + 2) * (s - 2) / 180) 
    STATISTIC = a/b
    names(STATISTIC) = "Z"
    test$statistic = STATISTIC
    
    # P Values:
    p = pnorm(STATISTIC)
    PVAL = c(2 * min(p, 1 - p), p, 1 - p)
    names(PVAL) = c(
        "Alternative Two-Sided", 
        "Alternative      Less",
        "Alternative   Greater")
    test$p.value = PVAL

    # Add:
    if(is.null(title)) title = "Mood Two-Sample Test of Scale"
    if(is.null(description)) description = date()

    # Return Value:
    new("fHTEST",     
        call = call,
        data = list(x = x, y = y), 
        test = test,
        title = as.character(title), 
        description = as.character(description) )
}


# ******************************************************************************
# Correlation Tests:


.correlationTest =
function(x, y, method = c("pearson", "kendall", "spearman"), 
title = NULL, description = NULL) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Correlation Tests
       
    # FUNCTION:
    
    # Test:
    if (method[1] == "pearson") {
        ans = pearsonTest(x, y, title = title, description = description) 
    }
    if (method[1] == "kendall") {
        ans = kendallTest(x, y, title = title, description = description) 
    }  
    if (method[1] == "spearman") {
       ans = spearmanTest(x, y, title = title, description = description)
    }
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


pearsonTest = 
function(x, y, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   A test for association between paired samples
    
    # Arguments:
    #   x - a numeric vector of data values.
    #   description - a brief description of the porject of type 
    #       character.
    #   title - a character string which allows for a project title.
    
    # Note:
    #   # A function linked to "stats"
    
    # FUNCTION:
    
    # Call:
    call = match.call()
    
    # Test:
    test = list()
    
    # Data Set Name:
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    test$data.name = DNAME
    
    # Check Type:
    if (class(x) == "zoo") {
        x = as.timeSeries(x)
    }
    if (class(x) == "timeSeries") {
        if (dim(x)[2] > 1) stop("x must be an univariate time series")
        x = as.vector(x)
    }
    if (class(y) == "zoo") {
        y = as.timeSeries(y)
    }
    if (class(y) == "timeSeries") {
        if (dim(y)[2] > 1) stop("y must be an univariate time series")
        y = as.vector(y)
    }
    
    # Test:
    two.sided = cor.test(x = x, y = y, alternative = "two.sided",  
        method = "pearson")
    less = cor.test(x = x, y = y, alternative = "less",  
        method = "pearson")
    greater = cor.test(x = x, y = y, alternative = "greater",  
        method = "pearson")  
        
    # Sample Estimates:   
    ESTIMATE = two.sided$estimate
    names(ESTIMATE) = "Correlation"
    test$estimate = ESTIMATE
    
    # Parameter
    DF = two.sided$parameter
    names(DF) = "Degrees of Freedom"
    test$parameter = DF
    
    # P Values:
    PVAL = c(
        two.sided$p.value, 
        less$p.value, 
        greater$p.value)
    names(PVAL) = c(
        "Alternative Two-Sided", 
        "Alternative      Less", 
        "Alternative   Greater")
    test$p.value = PVAL     
    
    # Confidences Levels:
    if (!is.null(two.sided$conf.int)) {
        CONF.INT = cbind(
            a = two.sided$conf.int, 
            b = less$conf.int, 
            c = greater$conf.int)
        dimnames(CONF.INT)[[2]] = c(
            "Two-Sided", 
            "     Less", 
            "  Greater")
        test$conf.int = CONF.INT  
    }  
    
    # Statistic:    
    STATISTIC = two.sided$statistic
    names(STATISTIC) = "t"
    test$statistic = STATISTIC
        
    # Add:
    if (is.null(title)) title = "Pearson's Correlation Test"
    if (is.null(description)) description = date()
    
    # Return Value:
    new("fHTEST",     
        call = call,
        data = list(x = x, y = y), 
        test = test,
        title = as.character(title), 
        description = as.character(description) ) 
}


# ------------------------------------------------------------------------------


kendallTest = 
function(x, y, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   A test for association between paired samples
    
    # Arguments:
    #   x - a numeric vector of data values.
    #   description - a brief description of the porject of type 
    #       character.
    #   title - a character string which allows for a project title.
    
    # Note:
    #   # A function linked to "stats"
    
    # FUNCTION:
    
    # Call:
    call = match.call()
    
    # Test:
    test = list()
       
    # Data Set Name:
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    test$data.name = DNAME
    
    # Check Type:
    if (class(x) == "zoo") {
        x = as.timeSeries(x)
    }
    if (class(x) == "timeSeries") {
        if (dim(x)[2] > 1) stop("x must be an univariate time series")
        x = as.vector(x)
    }
    if (class(y) == "zoo") {
        y = as.timeSeries(y)
    }
    if (class(y) == "timeSeries") {
        if (dim(y)[2] > 1) stop("y must be an univariate time series")
        y = as.vector(y)
    }
    
    # Test:
    two.sided = cor.test(x = x, y = y, alternative = "two.sided",  
        method = "kendall")
    less = cor.test(x = x, y = y, alternative = "less",  
        method = "kendall")
    greater = cor.test(x = x, y = y, alternative = "greater",  
        method = "kendall") 
        
    # Exact Test:
    if (class(version) != "Sversion") {
        two.sided.exact = cor.test(x = x, y = y, exact = TRUE, 
            alternative = "two.sided",  method = "kendall")
        less.exact = cor.test(x = x, y = y, exact = TRUE, 
            alternative = "less", method = "kendall")
        greater.exact = cor.test(x = x, y = y, exact = TRUE, 
            alternative = "greater",  method = "kendall")
    } else {
        two.sided.exact = list()
        two.sided.exact$p.value = two.sided.exact$statistic = NA
        less.exact = list()
        less.exact$p.value = less.exact$statistic = NA
        greater.exact = list()
        greater.exact$p.value = greater.exact$statistic = NA 
    }
            
    # Sample Estimates:
    ESTIMATE = two.sided$estimate
    names(ESTIMATE) = "tau"
    test$estimate = ESTIMATE
    
    # P Values:
    PVAL = c(
        two.sided$p.value, 
        two.sided.exact$p.value,
        less$p.value, 
        less.exact$p.value, 
        greater$p.value, 
        greater.exact$p.value)
    if (is.na(two.sided.exact$p.value)) {
        names(PVAL) = c(
            "Alternative Two-Sided", 
            "Alternative Two-Sided | Exact",
            "Alternative      Less", 
            "Alternative      Less | Exact", 
            "Alternative   Greater", 
            "Alternative   Greater | Exact")
    } else {
        names(PVAL) = c(
            "Alternative         Two-Sided", 
            "Alternative Two-Sided | Exact",
            "Alternative              Less", 
            "Alternative      Less | Exact", 
            "Alternative           Greater", 
            "Alternative   Greater | Exact")
    }
    test$p.value = PVAL     
    
    # Statistic:
    # STATISTIC = c(
    #   two.sided$statistic, two.sided.exact$statistic,
    #   less$statistic, less.exact$statistic,
    #   greater$statistic, greater.exact$statistic)
    STATISTIC = c(
        two.sided$statistic, 
        two.sided.exact$statistic)
    # names(STATISTIC) = c(
    #   "z | Two-Sided", "T | Two-Sided | Exact",
    #   "z | Less", "T | Less | Exact",
    #   "z | Greater", "T | Greater | Exact")
    names(STATISTIC) = c(
        "z", 
        "T | Exact")
    test$statistic = STATISTIC
        
    # Add:
    if (is.null(title)) title = "Kendall's tau Correlation Test"
    if (is.null(description)) description = date()
    
    # Return Value:
    new("fHTEST",     
        call = call,
        data = list(x = x, y = y), 
        test = test,
        title = as.character(title), 
        description = as.character(description) ) 
}


# ------------------------------------------------------------------------------


spearmanTest = 
function(x, y, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   A test for association between paired samples
    
    # Arguments:
    #   x - a numeric vector of data values.
    #   description - a brief description of the porject of type 
    #       character.
    #   title - a character string which allows for a project title.
    
    # Note:
    #   # A function linked to "stats"
    
    # FUNCTION:
    
    # Call:
    call = match.call()
    
    # Test:
    test = list()
    
        
    # Data Set Name:
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    test$data.name = DNAME
    
    # Check Type:
    if (class(x) == "zoo") {
        x = as.timeSeries(x)
    }
    if (class(x) == "timeSeries") {
        if (dim(x)[2] > 1) stop("x must be an univariate time series")
        x = as.vector(x)
    }
    if (class(y) == "zoo") {
        y = as.timeSeries(y)
    }
    if (class(y) == "timeSeries") {
        if (dim(y)[2] > 1) stop("y must be an univariate time series")
        y = as.vector(y)
    }
    
    # Test:
    two.sided = cor.test(x = x, y = y, alternative = "two.sided",  
        method = "spearman")
    less = cor.test(x = x, y = y, alternative = "less",  
        method = "spearman")
    greater = cor.test(x = x, y = y, alternative = "greater",  
        method = "spearman")
        
    # Sample Estimates:
    ESTIMATE = two.sided$estimate
    names(ESTIMATE) = "rho"
    test$estimate = ESTIMATE
    
    # P Values:
    PVAL = c(
        two.sided$p.value, 
        less$p.value, 
        greater$p.value)
    names(PVAL) = c(
        "Alternative Two-Sided", 
        "Alternative      Less", 
        "Alternative   Greater")
    test$p.value = PVAL   
    
    # Statistic:  
    STATISTIC = two.sided$statistic
    names(STATISTIC) = "S"
    test$statistic = STATISTIC
      
    # Add:
    if (is.null(title)) title = "Spearman's rho Correlation Test"
    if (is.null(description)) description = date()
    
    # Return Value:
    new("fHTEST",     
        call = call,
        data = list(x = x, y = y), 
        test = test,
        title = as.character(title), 
        description = as.character(description) ) 
}


################################################################################

