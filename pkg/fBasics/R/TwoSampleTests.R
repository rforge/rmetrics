
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             DISTRIBUTIONAL TESTS:
#  ks2Test               Performs a two sample Kolmogorov-Smirnov test
# FUNCTION:             LOCATION TESTS:
#  locationTest          Performs locations tests on two samples
#  .tTest                Unpaired t test for differences in mean
#  .kw2Test              Kruskal-Wallis test for differences in locations  
# FUNCTION:             VARIANCE TESTS:
#  varianceTest          Performs variance tests on two samples
#  .varfTest             F test for differences in variances
#  .bartlett2Test        Bartlett's test for differences in variances
#  .fligner2Test         Fligner-Killeen test for differences in variances
# FUNCTION:             SCALE TESTS:
#  scaleTest             Performs scale tests on two samples
#  .ansariTest           Ansari-Bradley test for differences in scale
#  .moodTest             Mood test for differences in scale
# FUNCTION:             CORRELATION TESTS:
#  correlationTest       Performs correlation tests on two samples
#  .pearsonTest          Pearson product moment correlation coefficient
#  .kendallTest          Kendall's tau correlation test
#  .spearmanTest         Spearman's rho correlation test
################################################################################


################################################################################
# FUNCTION:             DISTRIBUTIONAL TESTS:
#  ks2Test               Performs a two sample Kolmogorov-Smirnov test


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
    
    # Convert Type:
    x = as.vector(x)
    y = as.vector(y)
    
    # Compute Test: 
    two.sided = ks.test(x = x, y = y, alternative = "two.sided")
    exact     = ks.test(x = x, y = y, exact = TRUE, alternative = "two.sided")
    less      = ks.test(x = x, y = y, alternative = "less")
    greater   = ks.test(x = x, y = y, alternative = "greater")

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


################################################################################
# FUNCTION:             LOCATION TESTS:
#  locationTest          Performs locations tests on two samples
#  .tTest                Unpaired t test for differences in mean
#  .kw2Test              Kruskal-Wallis test for differences in locations  


locationTest =
function(x, y, method = c("t", "kw2"), 
title = NULL, description = NULL) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Correlation Tests
  
    # FUNCTION:
    
    # Test:
    method = match.arg(method)
    if (method == "t") {
        ans = .tTest(x, y, title = title, description = description) 
    }
    if (method == "kw2") {
        ans = .kw2Test(x, y, title = title, description = description) 
    }  
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.tTest = 
function(x, y, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #    Tests if two population means are equal. 
    
    # Arguments:
    #   x, y - two numeric vector of data values or time series objects
    #   description - a brief description of the porject of type character.
    #   title - a character string which allows for a project title.

    # FUNCTION:
    
    # Call:
    call = match.call()
    
    # Test:
    test = list()
    
    # Data Set Name:
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    test$data.name = DNAME
    
    # Convert Type:
    x = as.vector(x)
    y = as.vector(y)
    
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


.kw2Test = 
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
   
    # Convert Type:
    x = as.vector(x)
    y = as.vector(y)
    
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


################################################################################
# FUNCTION:             VARIANCE TESTS:
#  varianceTest          Performs variance tests on two samples
#  .varfTest             F test for differences in variances
#  .bartlett2Test        Bartlett's test for differences in variances
#  .fligner2Test         Fligner-Killeen test for differences in variances


varianceTest =
function(x, y, method = c("varf", "bartlett", "fligner"), 
title = NULL, description = NULL) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Correlation Tests
       
    # FUNCTION:
    
    # Test:
    method = match.arg(method)
    if (method == "varf") {
        ans = .varfTest(x, y, title = title, description = description) 
    }
    if (method == "bartlett") {
        ans = .bartlett2Test(x, y, title = title, description = description) 
    }  
    if (method == "fligner") {
        ans = .fligner2Test(x, y, title = title, description = description) 
    } 
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.varfTest = 
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
    
    # Convert Type:
    x = as.vector(x)
    y = as.vector(y)
    
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


.bartlett2Test = 
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
    
    # Convert Type:
    x = as.vector(x)
    y = as.vector(y)
    
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
  

.fligner2Test = 
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
    
    # Convert Type:
    x = as.vector(x)
    y = as.vector(y)
    
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


################################################################################
# FUNCTION:             SCALE TESTS:
#  scaleTest             Performs scale tests on two samples
#  .ansariTest           Ansari-Bradley test for differences in scale
#  .moodTest             Mood test for differences in scale


scaleTest =
function(x, y, method = c("ansari", "mood"), 
title = NULL, description = NULL) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Scale Tests
    
    # FUNCTION:
    
    # Test:
    method = match.arg(method)
    if (method == "ansari") {
        ans = .ansariTest(x, y, title = title, description = description) 
    }
    if (method == "mood") {
        ans = .moodTest(x, y, title = title, description = description) 
    }  
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.ansariTest = 
function(x, y, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Ansari-Bradley's test for differences in scale
    
    # Arguments:
    #   x - a numeric vector of data values.
    #   description - a brief description of the porject of type 
    #       character.
    #   title - a character string which allows for a project title.
  
    # FUNCTION:
    
    # Call:
    call = match.call()
    
    # Test:
    test = list()
    
    # Data Set Name:
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    test$data.name = DNAME
     
    # Convert Type:
    x = as.vector(x)
    y = as.vector(y)
    
    # Test:
    two.sided = .ansari2Test(x = x, y = y, alternative = "two.sided",
        exact = FALSE, conf.int = TRUE, conf.level = 0.95)
    less = .ansari2Test(x = x, y = y, alternative = "less",
        exact = FALSE, conf.int = TRUE, conf.level = 0.95)
    greater = .ansari2Test(x = x, y = y, alternative = "greater",
        exact = FALSE, conf.int = TRUE, conf.level = 0.95)
        
    two.sided.exact = .ansari2Test(x = x, y = y, alternative = "two.sided",
        exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    less.exact = .ansari2Test(x = x, y = y, alternative = "less",
        exact = TRUE, conf.int = TRUE, conf.level = 0.95)
    greater.exact = .ansari2Test(x = x, y = y, alternative = "greater",
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


.ansari2Test =
function(x, y, alternative = c("two.sided", "less", "greater"),
exact = TRUE, conf.int = FALSE, conf.level = 0.95, ...)
{
    # Arguments:
    #   x - numeric vector of data values. 
    #   y - numeric vector of data values. 
    #   alternative - indicates the alternative hypothesis and must 
    #       be one of "two.sided", "greater" or "less". You can specify 
    #       just the initial letter. 
    #   exact - a logical indicating whether an exact p-value should 
    #       be computed. 
    #   conf.int - a logical,indicating whether a confidence interval 
    #       should be computed. 
    #   conf.level - confidence level of the interval. 

    # FUNCTION:
    
    # Return Value:
    ansari.test(x, y, alternative, exact, conf.int, conf.level, ...)     
}


# ------------------------------------------------------------------------------


.moodTest = 
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
    
    # Convert Type:
    x = as.vector(x)
    y = as.vector(y)

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


################################################################################
# FUNCTION:             CORRELATION TESTS:
#  correlationTest       Performs correlation tests on two samples
#  .pearsonTest          Pearson product moment correlation coefficient
#  .kendallTest          Kendall's tau correlation test
#  .spearmanTest         Spearman's rho correlation test


correlationTest =
function(x, y, method = c("pearson", "kendall", "spearman"), 
title = NULL, description = NULL) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Correlation Tests
        
    # FUNCTION:
    
    # Test:
    method = match.arg(method)
    if (method[1] == "pearson") {
        ans = .pearsonTest(x, y, title = title, description = description) 
    }
    if (method[1] == "kendall") {
        ans = .kendallTest(x, y, title = title, description = description) 
    }  
    if (method[1] == "spearman") {
       ans = .spearmanTest(x, y, title = title, description = description)
    }
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.pearsonTest = 
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
    #   A function linked to "stats"
  
    # FUNCTION:
    
    # Call:
    call = match.call()
    
    # Test:
    test = list()
    
    # Data Set Name:
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    test$data.name = DNAME
    
    # Convert Type:
    x = as.vector(x)
    y = as.vector(y)
    stopifnot(length(x) == length(y))
    
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


.kendallTest = 
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
    #   A function linked to "stats"

    # FUNCTION:
    
    # Call:
    call = match.call()
    
    # Test:
    test = list()
       
    # Data Set Name:
    DNAME = paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    test$data.name = DNAME
    
    # Convert Type:
    x = as.vector(x)
    y = as.vector(y)
    stopifnot(length(x) == length(y))
    
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


.spearmanTest = 
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
    
    # Convert Type:
    x = as.vector(x)
    y = as.vector(y)
    stopifnot(length(x) == length(y))
    
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

