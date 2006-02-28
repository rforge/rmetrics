# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# A copy of the GNU General Public License is available via WWW at
# http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
# writing to the Free Software Foundation, Inc., 59 Temple Place,
# Suite 330, Boston, MA  02111-1307  USA. 

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
# PART I: 
# FUNCTIONS:          FRACTIONAL BROWNIAN MOTION:
#  fbmSim              Generates fractional Brownian motion
#  .fbmSim.mvn          Numerical approximation of the stochastic integral
#  .fbmSim.chol         Choleki's decomposition of the covariance matrix
#  .fbmSim.lev          Method of Levinson
#  .fbmSim.circ         method of Wood and Chan
#  .fbmSim.wave         Wavelet synthesis
#  .convol              Internal Convolution
# FUNCTIONS:          FRACTIONAL GAUSSIAN NOISE:
#  fgnSim              Generates fractional Gaussian noise
#  .fgnSim.durbin       Durbin's Method
#  .fgnSim.paxson       Paxson's Method
#  .fgnSim.beran        Beran's Method
#  farimaSim           Generates FARIMA time series process
################################################################################


################################################################################
# PART II: Reimplemented functions from
#   Beran's SPlus Scripts
# FUNCTIONS:          DESCRIPTION:
#  farimaTrueacf       Returns FARMA true autocorrelation function
#  farimaTruefft       Returns FARMA true fast Fourier transform
#  .ckFARIMA0           Returns FARMA true autocorrelation function
#  .gkFARIMA0           Returns FARMA true fast Fourier transform
#  .simFARIMA0          Simulates Time Series
#  fgnTrueacf          Returns FGN true autocorrelation function
#  fgnTruefft          Returns FGN true fast Fourier transform
#  .ckFGN0              Returns FGN true autocorrelation function
#  .gkFGN0              Returns FGN true fast Fourier Transform
#  .simFGN0             Simulates time series
# WHITTLE:
#  whittleFit         Whittle Estimator
#  .CetaFGN             Internal Functions ...
#  .CetaARIMA 
#  .Qeta 
#  .fspecFGN 
#  .ffourier.FGN.est 
#  .FGN.spectrum 
#  .FGN.B.est.adjust 
#  .FGN.B.est 
#  .fspecARIMA 
#  .per 
#  .Qmin2 
#  .whittle 
################################################################################


################################################################################
# PART III: Reimplemented functions from
#   Taqqu M.S, Teverovsky V, Willinger W.
#   Estimators for Long-Range Dependence: An Empirical Study
#   Fractals, Vol 3, No. 4, 785-788, 1995
# FUNCTIONS:          HURST EXPONENT:
#  fHURST              S4 Class Representation
#   print.fHURST        S3 Print Method
#   plot.fHURST         S3 Plot Method
#  aggvarFit           3.1 Aggregated variance method
#  diffvarFit          3.2 Differenced aggregated variance method
#  absvalFit           3.3 Absolute values (moments) method
#  higuchiFit          3.4 Higuchi's method
#  pengFit             3.5 peng's or Residuals of Regression method
#  rsFit               3.6 R/S method
#  perFit              3.7 Periodogram and cumulated periodogram method
#  boxperFit           3.8 Boxed (modified) peridogram method
#  whittleFit          3.9 Whittle estimator -> PART II
################################################################################


################################################################################
# PART IV: Wavelet Estimator
# FUNCTIONS:          DESCRIPTION:
#  waveletFit          Wavelet Estimator
#  .accessD 
#  .wd 
#  .filter.select
#  .first.last  
################################################################################


################################################################################
# PART V: Statistical Tests and Slider
# FUNCTIONS:          DESCRIPTION:
#  .beranTest          Not yet ready for usage ...
#  .rsTest             Not yet ready for usage ...
#  .vsTest             Not yet ready for usage ...
# FUNCTION:			  SLIDER:
#  hurstSlider         Hurst Slider
################################################################################


################################################################################
# PART I: Fractional Time Series Simulation


# ******************************************************************************
# Fractional Brownian Motion


fbmSim = 
function(n = 100, H = 0.7, method = c("mvn", "chol", "lev", "circ", "wave"),
waveJ = 7, doplot = TRUE, fgn = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Simulation of fractional Brownian motion by five different methods
    
    # Arguments: 
    #   n : length of the desired sample 
    #   H : self-similarity parameter          
    #   doplot : = TRUE ----> plot path of fBm

    # Value: 
    #   Simulation of a standard fractional Brownian motion
    
    # Details:
    #   The underlying functions were ported from SPlus code written
    #   by J.F. Couerjolly. They are documented in the reference given
    #   below.
    
    # Reference:
    #   Couerjolly J.F.,
    #       Simulation and Identification of the Fractional Brownian 
    #       Motion: A Bibliographical and Comparative Study,
    #       Journal of Statistical Software 5, 2000
    
    # FUNCTION:
    
    # Initialization:
    method = method[1]
    fun = paste(".fbmSim.", method, sep = "")
    funFBM = match.fun(fun)
    
    # Simulate:
    if (method == "wave") {
        ans = funFBM(n, H, waveJ, doplot, fgn)
    } else {
        ans = funFBM(n, H, doplot, fgn)
    }
        
    # Return Value:
    ans
}


# ******************************************************************************


.fbmSim.mvn = 
function(n = 1000, H = 0.7, doplot = TRUE, fgn = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Arguments: 
    #   n : length of the desired sample 
    #   H : self-similarity parameter          
    #   doplot : = TRUE ----> plot path of fBm

    # Value: 
    #   Simulation of a standard fractional Brownian motion
    #   at times { 0, 1/n,..., n-1/n }
    #   by numerical approximation of stochastic integral

    # Author: 
    #    Coeurjolly 06/2000 of the original SPlus port

    # Reference:
    #   Mandelbrot B. and Van Ness, 
    #       Fractional brownian motions, 
    #       fractional noises and applications, SIAM Review, 10, n.4, 1968.
    #   Couerjolly J.F.,
    #       Simulation and Identification of the Fractional Brownian motion:
    #       A Bibliographical and Comparative Study,
    #       Journal of Statistical Software 5, 2000
    
    # FUNCTION:
    
    # Initialization:
    dB1 = rnorm(n)
    borne = trunc(n^1.5)
    dB2 = rnorm(borne)
    fBm = rep(0, n)
    CH = sqrt(gamma(2 * H + 1) * sin(pi * H))/gamma(H + 1/2)    ##

    # Main Program:
    ind1 = (1:n)^(H - 1/2)
    ind2 = (1:(borne + n))^(H - 1/2)
    for(i in (2:n)) {
        I1 = dB1[(i - 1):1] * ind1[1:(i - 1)]
        I2 = (ind2[(i + 1):(i + borne)] - ind2[1:borne]) * dB2
        fBm[i] = sum(I1) + sum(I2)
    }
    fBm = fBm * n^( - H) * CH
    fBm[1] = 0
    
    # Result:
    ans = drop(fBm)
    Title = "mvnFBM Path"
    if (fgn) {
        ans = c(fBm[1], diff(fBm))
        Title = "mvnFGN Path"
    }
    
    # Plot of fBm   
    if (doplot) {
        time = 1:n
        Nchar = as.character(n)
        Nleg = paste("N=", Nchar, sep = "")
        Hchar = as.character(round(H, 3))
        Hleg = paste(", H=", Hchar, sep = "")
        NHleg = paste(c(Nleg, Hleg), collapse = "")
        leg = paste(c(Title, NHleg), collapse = " - ")
        plot(time, ans, type = "l", main = leg, col = "steelblue")
        grid()
    }
    
    # Return Value:
    ans = as.ts(ans)
    attr(ans, "control") <- c(method = "mvn", H = H)
    ans
}


# ------------------------------------------------------------------------------


.fbmSim.wave = 
function(n = 1000, H = 0.7, J = 7, doplot = TRUE, fgn = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Arguments: 
    #   n : length of the desired sample
    #   H : self-similarity parameter          
    #   J : resolution
    #   doplot : = TRUE ----> plot of path of fBm

    # Value: 
    #   Simulation of a standard fractional Brownian motion
    #   at times { 0, 1/n,..., n-1/n } by wavelet synthesis

    # Author: 
    #    Coeurjolly 06/2000 of the original SPlus port

    # Reference:  
    #   Abry P. and Sellan F., 
    #       The wavelet-based synthesis
    #       for fractional Brownian motion, Applied and computational
    #       harmonic analysis, 1996 + Matlab scripts from P. Abry 
    #   Couerjolly J.F.,
    #       Simulation and Identification of the Fractional Brownian motion:
    #       A Bibliographical and Comparative Study,
    #       Journal of Statistical Software 5, 2000
    
    # FUNCTION:

    # Daubechies filter of length 20
    Db20 = c(0.026670057901000001, 0.188176800078)
    Db20 = c(Db20, 0.52720118932000004, 0.688459039454)
    Db20 = c(Db20, 0.28117234366100002, -0.24984642432699999)
    Db20 = c(Db20, -0.19594627437699999, 0.127369340336)
    Db20 = c(Db20, 0.093057364604000006, -0.071394147165999997)
    Db20 = c(Db20, -0.029457536821999999, 0.033212674058999997)
    Db20 = c(Db20, 0.0036065535670000001, -0.010733175483)
    Db20 = c(Db20, 0.001395351747, 0.0019924052950000002)
    Db20 = c(Db20, -0.00068585669500000003, -0.000116466855)
    Db20 = c(Db20, 9.3588670000000005e-05, -1.3264203000000001e-05)
    secu = 2 * length(Db20) 

    # Quadrature mirror filters of Db20
    Db20qmf = (-1)^(0:19) * Db20
    Db20qmf = Db20qmf[20:1]
    nqmf = -18  
    
    # Truncated fractional coefficients appearing in fractional integration
    # of the multiresolution analysis
    prec = 0.0060000000000000001
    hmoy = c(1, 1)
    s = H + 1/2
    d = H - 1/2
    if (H == 1/2) {
        ckbeta = c(1, 0)
        ckalpha = c(1, 0)
    } else {
        # Truncature at order prec
        ckalpha = 1
        ckbeta = 1
        ka = 2
        kb = 2
        while(abs(ckalpha[ka - 1]) > prec) {
            g = gamma(1 + d)/gamma(ka)/gamma(d + 2 - ka)
            if (is.na(g))
                g = 0
            ckalpha = c(ckalpha, g)
            ka = ka + 1
        }
        while(abs(ckbeta[kb - 1]) > prec) {
            g = gamma(kb - 1 + d)/gamma(kb)/gamma(d)
            if (is.na(g))
                g = 0
            ckbeta = c(ckbeta, g)
            kb = kb + 1
        }
    }
    lckbeta = length(ckbeta)
    lckalpha = length(ckalpha)  ##

    # Number of starting points
    nbmax = max(length(ckbeta), length(ckalpha))
    nb0 = n/(2^(J)) + 2 * secu  ##

    # Sequence fs1:
    fs1 = .convol(ckalpha, Db20)
    fs1 = .convol(fs1, hmoy)
    fs1 = 2^( - s) * fs1
    fs1 = fs1 * sqrt(2) # 

    # Sequence gs1:
    gs12 = .convol(ckbeta, Db20qmf)
    gs1 = cumsum(gs12)
    gs1 = 2^(s) * gs1
    gs1 = gs1 * sqrt(2) ##

    # Initialization:
    nb1 = nb0 + nbmax
    bb = rnorm(nb1)
    b1 = .convol(bb, ckbeta)
    bh = cumsum(b1)
    bh = bh[c(nbmax:(nb0 + nbmax - 1))]
    appro = bh
    tappro = length(appro)  ##

    # Useful function:
    dilatation = function(vect) {   
        # dilates one time vector vect
        ldil = 2 * length(vect) - 1
        dil = rep(0, ldil)
        dil[seq(1, ldil, by = 2)] = vect
        drop(dil)
    }

    # Synthese's algorithm:
    for(j in 0:(J - 1)) {
        appro = dilatation(appro)
        appro = .convol(appro, fs1)
        appro = appro[1:(2 * tappro)]
        detail = rnorm(tappro) * 2^(j/2) * 4^( - s) * 2^( - j * s)
        detail = dilatation(detail)
        detail = .convol(detail, gs1)
        detail = detail[( - nqmf + 1):( - nqmf + 2 * tappro)]
        appro = appro + detail
        tappro = length(appro)
    }
    debut = (tappro - n)/2
    fBm = appro[c((debut + 1):(debut + n))]
    fBm = fBm - fBm[1]
    fGn = c(fBm[1], diff(fBm))
    fGn = fGn * 2^(J * H) * n^( - H)    # path on [0,1]
    fBm = cumsum(fGn)
    fBm[1] = 0  
    
    # Result:
    ans = drop(fBm)
    Title = "waveFBM Path"
    if (fgn) {
        ans = c(fBm[1], diff(fBm))
        Title = "waveFGN Path"
    }
    
    # Plot of fBM/FGN:
    if (doplot) {
        time = 1:n
        Nchar = as.character(n)
        Nleg = paste("N=", Nchar, sep = "")
        Hchar = as.character(round(H, 3))
        Hleg = paste(", H=", Hchar, sep = "")
        NHleg = paste(c(Nleg, Hleg), collapse = "")
        leg = paste(c(Title, NHleg), collapse = " - ")
        plot(time, ans, type = "l", main = leg, col = "steelblue")
        grid()
    }
    
    # Return Value:
    ans = as.ts(ans)
    attr(ans, "control") <- c(method = "wave", H = H)
    ans
}


# ------------------------------------------------------------------------------


.convol = 
function(x, y)
{   # A function implemented by Diethelm Wuertz

    # Arguments: 
    #   x,y : vectors
    
    # Value: 
    #   convolution of vectors x and y

    # Author: 
    #    Coeurjolly 06/2000 of the original SPlus port

    # FUNCTION:
    
    if (missing(x) | missing(y)) {
        break 
    } else {
        a = c(x, rep(0, (length(y) - 1)))
        b = c(y, rep(0, (length(x) - 1)))
        a = fft(a, inverse = F)
        b = fft(b, inverse = F)
        conv = a * b
        conv = Re(fft(conv, inverse = TRUE))
        conv = conv/length(conv)
        drop(conv)
    }
}


# ------------------------------------------------------------------------------


.fbmSim.chol = 
function(n = 1000, H = 0.7, doplot = TRUE, fgn = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Arguments: 
    #   n : length of the desired sample
    #   H : self-similarity parameter          
    #   doplot : = TRUE ----> plot path of fBm

    # Value: 
    #   Simulation of a standard fractional Brownian motion
    #   at times { 0, 1/n,..., n-1/n }
    #   by Choleki's decomposition of the covariance matrix of the fBm

    # Author: 
    #   Coeurjolly 06/2000 of the original SPlus port
    
    # Reference:
    #   Couerjolly J.F.,
    #       Simulation and Identification of the Fractional Brownian motion:
    #       A Bibliographical and Comparative Study,
    #       Journal of Statistical Software 5, 2000
      
    # FUNCTION:
    
    # Construction of covariance matrix of fBm  
    H2 = 2 * H
    matcov = matrix(0, n - 1, n - 1)
    for(i in (1:(n - 1))) {
        j = i:(n - 1)
        r = 0.5 * (abs(i)^H2 + abs(j)^H2 - abs(j - i)^H2)
        r = r/n^H2
        matcov[i, j] = r
        matcov[j, i] = matcov[i, j]
    }
    L = chol(matcov)
    Z = rnorm(n - 1)
    fBm = t(L) %*% Z
    fBm = c(0, fBm)
    
    # Result:
    ans = drop(fBm)
    Title = "cholFBM Path"
    if (fgn) {
        ans = c(fBm[1], diff(fBm))
        Title = "cholFGN Path"
    }

    # Plot of fBm:
    if (doplot) {
        time = 1:n
        Nchar = as.character(n)
        Nleg = paste("N=", Nchar, sep = "")
        Hchar = as.character(round(H, 3))
        Hleg = paste(", H=", Hchar, sep = "")
        NHleg = paste(c(Nleg, Hleg), collapse = "")
        leg = paste(c(Title, NHleg), collapse = " - ")
        plot(time, ans, type = "l", main = leg, col = "steelblue")
        grid()
    }
    
    # Return Value:
    ans = as.ts(ans)
    attr(ans, "control") <- c(method = "chol", H = H)
    ans
}


# ------------------------------------------------------------------------------


.fbmSim.lev = 
function(n = 1000, H = 0.7, doplot = TRUE, fgn = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Arguments: 
    #   n : length of the desired sample 
    #   H : self-similarity parameter          
    #   plotfBm :  =1 ---> plot path of fBm

    # Value: 
    #   Simulation of a standard fractional Brownian motion
    #   at times { 0, 1/n,..., n-1/n } by Levinson's method
    
    # Author: 
    #   Coeurjolly 06/2000 of the original SPlus port

    # Reference:
    #   Peltier R.F., 
    #       Processus stochastiques fractals avec
    #       applications en finance, these de doctorat, p.42, 28.12.1997
    #   Couerjolly J.F.,
    #       Simulation and Identification of the Fractional Brownian motion:
    #       A Bibliographical and Comparative Study,
    #       Journal of Statistical Software 5, 2000
    
    # FUNCTION:
    
    # Covariances of fGn:
    k = 0:(n - 1)
    H2 = 2 * H
    r = (abs((k - 1)/n)^H2 - 2 * (k/n)^H2 + ((k + 1)/n)^H2)/2

    # Initialization of algorithm:
    y = rnorm(n)
    fGn = rep(0, n)
    v1 = r
    v2 = c(0, r[c(2:n)], 0)
    k =  - v2[2]
    aa = sqrt(r[1]) # 

    # Levinson's algorithm:
    for(j in (2:n)) {
        aa = aa * sqrt(1 - k * k)
        v = k * v2[c(j:n)] + v1[c((j - 1):(n - 1))]
        v2[c(j:n)] = v2[c(j:n)] + k * v1[c((j - 1):(n - 1))]
        v1[c(j:n)] = v
        bb = y[j]/aa
        fGn[c(j:n)] = fGn[c(j:n)] + bb * v1[c(j:n)]
        k =  - v2[j + 1]/(aa * aa)
    }
    fBm = cumsum(fGn)
    fBm[1] = 0  
    
    # Result:
    ans = drop(fBm)
    Title = "levFBM Path"
    if (fgn) {
        ans = c(fBm[1], diff(fBm))
        Title = "levFGN Path"
    }

    # Plot of fBm:
    if (doplot) {
        time = 1:n
        Nchar = as.character(n)
        Nleg = paste("N=", Nchar, sep = "")
        Hchar = as.character(round(H, 3))
        Hleg = paste(", H=", Hchar, sep = "")
        NHleg = paste(c(Nleg, Hleg), collapse = "")
        leg = paste(c(Title, NHleg), collapse = " - ")
        plot(time, ans, type = "l", main = leg, col = "steelblue")
        grid()
    }
    
    # Return Value:
    ans = as.ts(ans)
    attr(ans, "control") <- c(method = "lev", H = H)
    ans
}


# ------------------------------------------------------------------------------


.fbmSim.circ = 
function(n = 100, H = 0.7, doplot = TRUE, fgn = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Arguments: 
    #   n : length of the desired sample
    #   H : self-similarity parameter          
    #   doplot : = TRUE ---> plot path of fBm

    # Value: 
    #   Simulation of a standard fractional Brownian motion
    #   at times { 0, 1/n,..., n-1/n } by Wood-Chan's method

    # Author: 
    #    Coeurjolly 06/2000 of the original SPlus port

    # Reference:
    #   Wood A. and Chan G., 
    #       Simulation of stationnary Gaussian processes, 
    #       Journal of computational and grahical statistics, Vol.3, 1994. 
    #   Couerjolly J.F.,
    #       Simulation and Identification of the Fractional Brownian motion:
    #       A Bibliographical and Comparative Study,
    #       Journal of Statistical Software 5, 2000   

    # FUNCTION:

    # First line of the circulant matrix, C, built via covariances of fGn 
    lineC = function(n, H, m) {
        k = 0:(m - 1)
        H2 = 2 * H
        v = (abs((k - 1)/n)^H2 - 2 * (k/n)^H2 + ((k + 1)/n)^H2)/2
        ind = c(0:(m/2 - 1), m/2, (m/2 - 1):1)
        v = v[ind + 1]
        drop(v)
    }

    # Next power of two > n:
    m = 2
    repeat {
        m = 2 * m
        if (m >= (n - 1)) break
    }
    stockm = m  ##

    # Research of the power of two (<2^18) such that C is definite positive:
    repeat {
        m = 2 * m
        eigenvalC = lineC(n, H, m)
        eigenvalC = fft(c(eigenvalC), inverse = FALSE)
        ### DW: That doesn't work on a complex vectors !
        ### if ((all(eigenvalC > 0)) | (m > 2^17)) break
        ### We use:
        if ((all(Re(eigenvalC) > 0)) | (m > 2^17)) break
    }
    if (m > 2^17) {
        cat("----> exact method, impossible!!", fill = TRUE)
        cat("----> can't find m such that C is definite positive", fill = TRUE)
        break
    } else {
        # Simulation of W=(Q)^t Z, where Z leads N(0,I_m)
        # and   (Q)_{jk} = m^(-1/2) exp(-2i pi jk/m):
        ar = rnorm(m/2 + 1)
        ai = rnorm(m/2 + 1)
        ar[1] = sqrt(2) * ar[1]
        ar[(m/2 + 1)] = sqrt(2) * ar[(m/2 + 1)]
        ai[1] = 0
        ai[(m/2 + 1)] = 0
        ar = c(ar[c(1:(m/2 + 1))], ar[c((m/2):2)])
        aic =  - ai
        ai = c(ai[c(1:(m/2 + 1))], aic[c((m/2):2)])
        W = complex(real = ar, imaginary = ai)  ##

        # Reconstruction of the fGn:
        W = (sqrt(eigenvalC)) * W
        fGn = fft(W, inverse = F)
        fGn = (1/(sqrt(2 * m))) * fGn
        fGn = Re(fGn[c(1:n)])
        fBm = cumsum(fGn)
        fBm[1] = 0  
        
        # Result:
        ans = drop(fBm)
        Title = "circFBM Path"
        if (fgn) {
            ans = c(fBm[1], diff(fBm))
            Title = "circFGN Path"
        }
    
        # Plot of fBm:
        if (doplot) {
            time = 1:n
            Nchar = as.character(n)
            Nleg = paste("N=", Nchar, sep = "")
            Hchar = as.character(round(H, 3))
            Hleg = paste(", H=", Hchar, sep = "")
            NHleg = paste(c(Nleg, Hleg), collapse = "")
            leg = paste(c(Title, NHleg), collapse = " - ")
            plot(time, ans, type = "l", main = leg, col = "steelblue")
            grid()
        }
    }
    
    # Return Value:
    ans = as.ts(ans)
    attr(ans, "control") <- c(method = "circ", H = H)
    ans
}


# ******************************************************************************
# Fractional Gaussian Noise


fgnSim = 
function(n = 1000, H = 0.7, method = c("beran", "durbin", "paxson"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a series of fractional Gaussian Noise
    
    # Arguments:
    #   n - length of desired series.
    #   H - the Hurst parameter.
    #   sigma - the standard deviation of the innovations used.
    
    # Details:
    #   FGN time series simulation. The FGN sequences use 
    #   functions going back to Taqqu et al., Paxson and 
    #   Beran (with Maechler's modifications from StatLib).

    # FUNCTION:
    
    # Settings:
    mean = 0 
    std = 1
    method = method[1]
    ans = NA
    
    # Generate Sequence:
    if (method == "beran")  
        ans = .fgnSim.beran (n = n, H = H, mean = mean, std = std)
    if (method == "durbin") 
        ans = .fgnSim.durbin(n = n, H = H, mean = mean, std = std)
    if (method == "paxson") 
        ans = .fgnSim.paxson(n = n, H = H, mean = mean, std = std)
    if (is.na(ans[1])) stop("No valid method selected.")
    
    # Result:
    ans = as.ts(ans)
    attr(ans, "control") <- c(method = method, H = H)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------
    

.fgnSim.durbin = 
function(n, H, mean, std) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Function to simulate a FGN  sequence, using the Durbin-Levinson 
    #   coefficients, along the original C Function of Vadim Teverovsky
    #   Original Cdurbin.C, double loop removed - now single loop in R!
    #   This makes the function quite fast.
    
    # Settings:
    n = n + 1
    h = H
    sigma = std
    normal = rnorm(n+1)
    sigma2 = sigma*sigma/2
    acov0 = 2*sigma2
    acov = vee = phi1 = phi2 = output = rep(0, n)
    I = 1:n
    acov = sigma2 *( (I+1)^(2*h) - 2*I^(2*h) + abs(I-1)^(2*h) )
    phi1[1] = acov[1]/acov0
    phi2[1] = phi1[1]
    vee0 = acov0
    vee[1] = vee0 * (1 - phi1[1]^2)
    output[1] = sqrt(vee0) * normal[1]      
    
    # Durbin-Levinson:
    for (i in 2:n){
        phi1[i] = acov[i]
        J = 1:(i-1)
        phi1[i] = phi1[i] - sum(phi2[J]*acov[i-J])
        phi1[i] = phi1[i]/vee[i-1]
        vee[i] = vee[i-1]*(1-phi1[i]^2)
        output[i] = sqrt(vee[i-1]) * normal[i]
        phi1[J] = phi2[J] - phi1[i]*phi2[i-J]
        output[i] = output[i] + sum(phi2[J] * output[i-J])
        phi2[1:i] =  phi1[1:i]
    }     
    
    # Result:
    ans = sigma*output[-1] + mean 
    attr(ans, "control") <- c(method = "durbin", H = H)
    
    # Return value:
    ans
}


# ------------------------------------------------------------------------------


.fgnSim.paxson = 
function(n, H, mean, std) 
{    
    # Description:
    #   Generates a FGN sequence by Paxson's FFT-Algorithm       
    
    # Details:
    #   This file contains a function for synthesizing approximate 
    #   fractional Gaussian noise.  
    #   * Note that the mean and variance of the returned points is 
    #     irrelevant, because any linear transformation applied to the 
    #     points preserves their correlational structure (and hence 
    #     their approximation to fractional Gaussian noise); and by 
    #     applying a linear transformation you can transform the 
    #     points to have any mean and variance you want.
    #   * If you're using the sample paths for simulating network 
    #     arrival counts, you'll want them to all be non-negative.  
    #     Hopefully you have some notion of the mean and variance 
    #     of your desired traffic, and can apply the corresponding
    #     transformation. If, after transforming, a fair number of 
    #     the points are still negative, then perhaps your traffic 
    #     is not well-modeled using fractional Gaussian noise.  
    #     You might instead consider using an exponential transformation.  
    #   * All of this is discussed in the technical report:
    #     Fast Approximation of Self-Similar Network Traffic,
    #     Vern Paxson, technical report LBL-36750/UC-405, April 1995.
    #     URL:ftp://ftp.ee.lbl.gov/papers/fast-approx-selfsim.ps.Z
    
    # Value:
    #   FGN vector of length n.
    
    # FUNCTION:
    
    # Returns a Fourier-generated sample path of a "self similar" process,
    # consisting of n points and Hurst parameter H (n should be even).
    n = n/2
    lambda = ((1:n)*pi)/n
    
    # Approximate ideal power spectrum:
    d = -2*H - 1
    dprime = -2*H
    a = function(lambda,k) 2*k*pi+lambda
    b = function(lambda,k) 2*k*pi-lambda
    a1 = a(lambda,1); b1 = b(lambda,1)
    a2 = a(lambda,2); b2 = b(lambda,2)
    a3 = a(lambda,3); b3 = b(lambda,3)
    a4 = a(lambda,4); b4 = b(lambda,4)
    FGNBest = a1^d+b1^d+a2^d+b2^d+a3^d+b3^d + 
        (a3^dprime+b3^dprime + a4^dprime+b4^ dprime)/(8*pi*H) 
    f = 2 * sin(pi*H) * gamma(2*H+1) * (1-cos(lambda)) * 
        (lambda^(-2*H-1) + FGNBest )
    
    # Adjust for estimating power:
    # spectrum via periodogram.
    f = f * rexp(n)
    
    # Construct corresponding complex numbers with random phase.
    z = complex(modulus = sqrt(f), argument = 2*pi*runif(n))
    
    # Last element should have zero phase:
    z[n] = abs(z[n])
    
    # Expand z to correspond to a Fourier:
    # transform of a real-valued signal.
    zprime = c(0, z, Conj(rev(z)[-1]))
    
    # Inverse FFT gives sample path:
    z = Re(fft(zprime, inv = TRUE))     
    
    # Standardize:
    z = (z-mean(z))/sqrt(var(z))
    ans = std*z + mean 
    attr(ans, "control") <- c(method = "paxson", H = H)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.fgnSim.beran = 
function(n, H = 0.7, mean = 0, std = 1) 
{ 
    # Description:
    #   Generates a FGN sequence by Beran's FFT-Algorithm  
    
    # Value:
    #   FGN vector of length n.
    
    # FUNCTION:
    
    # Generate Sequence:
    z = rnorm(2*n)
    zr = z[c(1:n)]
    zi = z[c((n+1):(2*n))]
    zic = -zi
    zi[1] = 0
    zr[1] = zr[1]*sqrt(2)
    zi[n] = 0
    zr[n] = zr[n]*sqrt(2)
    zr = c(zr[c(1:n)], zr[c((n-1):2)])
    zi = c(zi[c(1:n)], zic[c((n-1):2)])
    z = complex(real = zr,imaginary = zi)
    
    # .gkFGN0:
    k = 0:(n-1)
    gammak = (abs(k-1)**(2*H)-2*abs(k)**(2*H)+abs(k+1)**(2*H))/2 
    ind = c(0:(n - 2), (n - 1), (n - 2):1)
    .gkFGN0 = fft(c(gammak[ind+1]), inverse = TRUE) 
    gksqrt = Re(.gkFGN0)
    if (all(gksqrt > 0)) {
        gksqrt = sqrt(gksqrt)
        z = z*gksqrt
        z = fft(z, inverse = TRUE)
        z = 0.5*(n-1)**(-0.5)*z
        z = Re(z[c(1:n)]) 
    } else {
        gksqrt = 0*gksqrt
        stop("Re(gk)-vector not positive") 
    }
    
    # Standardize:
    # (z-mean(z))/sqrt(var(z))
    ans = std*drop(z) + mean 
    attr(ans, "control") <- c(method = "beran", H = H)
    
    # Return Value:
    ans
}


# ******************************************************************************


farimaSim = 
function(n = 1000, model = list(ar = c(0.5, -0.5), d = 0.3, ma = 0.1),
method = c("freq", "time"), ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Simulates a FARMA Time Series Process
    
    # Note:
    #   Splus-Like argument list
    
    # Example:
    #   armaSim(model = list(ar = c(0.5, -0.5), d = 0.2, ma = 0.1))
    #   armaSim(model = list(d = 0.2, ma = 0))
    #   armaSim(model = list(d = 0.2))
    
    # FUNCTION:
    
    # Settings:
    innov = NULL
    n.start = 100
    start.innov = NULL
    rand.gen = rnorm
    
    # Simulate:
    if (!is.list(model)) 
        stop("model must be list")
    if (is.null(innov)) 
        innov = rand.gen(n, ...)
    n = length(innov) 
    if (is.null(start.innov)) 
        start.innov = rand.gen(n, ...) 
    n.start = length(start.innov)

    # AR PART:
    p = length(model$ar)
    if (p == 1 && model$ar == 0) 
        p = 0
    if (p) { 
        minroots = min(Mod(polyroot(c(1, -model$ar))))
        if (minroots <= 1) stop("ar part of model is not stationary") 
    }
    
    # MA PART:
    q = length(model$ma)
    if (q == 1 && model$ma == 0) 
        q = 0
    if (n.start < p + q) 
        stop("burn-in must be as long as ar + ma")
    
    # DIFFERENCING:
    ## if (model$d < 0) stop("d must be positive ") 
    dd = length(model$d)    
    if (dd) { 
        # FRACDIFF if "dd" is a non-integer value:
        d = model$d
        if (d != round(d) ) { 
            TSMODEL = "FRACDIFF" 
        } else { 
            TSMODEL = "ARIMA" } 
    } else {
        d = 0 
        TSMODEL = "ARIMA" 
    } 
    
    # ARMA:
    if (TSMODEL == "ARIMA") {
        stop("d is a short range model")
    }
        
    if (TSMODEL == "FRACDIFF") {
        if (p == 0) model$ar = 0
        if (q == 0) model$ma = 0
        mu = 0
        # Use Fortran Routine from R's contributed fracdiff package:
        # This is a BUILTIN function ...
        x = .Fortran("fdsim", as.integer(n), as.integer(p), as.integer(q), 
            as.double(model$ar), as.double(model$ma), as.double(model$d), 
            as.double(mu), as.double(rnorm(n + q)), x = double(n + q), 
            as.double(.Machine$double.xmin), as.double(.Machine$double.xmax), 
            as.double(.Machine$double.neg.eps), as.double(.Machine$double.eps), 
            PACKAGE = "fSeries")$x[1:n] 
    }
               
    # Return Value:
    ans = as.ts(x)
    attr(ans, "control") <- c(method = method, model = unlist(model))
    ans
}
  

################################################################################
# PART II:
# DESCRIPTION:
#   The functions are from the appendix of J. Beran "Statistics for 
#   long-memory processes", Chapman and Hall 1984
# LICENSE:
#   Permission is hereby given to StatLib to redistribute this software.
#   The software can be freely used for non-commercial purposes, and can
#   be freely distributed for non-commercial purposes only.
# AUTHORS:
#   Jan Beran <jberan@iris.rz.uni-konstanz.de>
#   Modified: Martin Maechler <maechler@stat.math.ethz.ch>
#   Modified: Diethelm Wuertz <wuertz@itp.phys.ethz.ch> for this R-Port


.ckFGN0 = 
function(n, H) 
{   # A function implemented by Diethelm Wuertz

    # Description:  
    #   Computes covariances of a fractional Gaussian  process
    
    # Arguments: 
    #   n = length of time series
    #   H = self-similarity parameter
    
    # Value: 
    #   Covariances upto lag n-1
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler, Date: Sep 95.
    
    # FUNCTION:
    k = 0:(n-1)
    H2 = 2 * H
    ans = drop((abs(k-1)**H2-2*abs(k)**H2+abs(k+1)**H2)/2)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


fgnTrueacf = 
function(n, H)
{
    .ckFGN0(n = n, H = H)
}


# ------------------------------------------------------------------------------


.gkFGN0 = 
function(n, H) 
{   # A function implemented by Diethelm Wuertz

    # Description: 
    #   Calculates gk = fft of V=(r(0),...,r(n-2),
    #   r(n-1), r(n-2),...,r(1), where r=the autocovariances
    #   of a fractional Gaussian process with variance 1
    
    # Arguments:
    #   n = length of time series
    #   H = self-similarity parameter
    
    # Value: 
    #   gk = Fourier transform of V at Fourier frequencies
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler, Date: Sep 95.
        
    # FUNCTION:
    
    # FFT:
    gammak = .ckFGN0(n, H)
    ind = c(0:(n - 2), (n - 1), (n - 2):1)
    gk = gammak[ind+1]
    ans = drop(fft(c(gk), inverse = TRUE))
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


fgnTruefft = 
function(n, H)
{
    .gkFGN0(n = n, H = H)
}


# ------------------------------------------------------------------------------


.simFGN0 = 
function(n, H) 
{   # A function implemented by Diethelm Wuertz

    # Description: 
    #   Simulates a series X(1),...,X(n) of a fractional Gaussian process
    
    # Arguments:
    #   n = length of time series
    #   H = self-similarity parameter
    
    # Value: 
    #   simulated series X(1),...,X(n)
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler, Date: Sep 95.
    
    # FUNCTION:
    
    # Simulation:
    z = rnorm(2*n)
    zr = z[c(1:n)]
    zi = z[c((n+1):(2*n))]
    zic = -zi
    zi[1] = 0
    zr[1] = zr[1]*sqrt(2)
    zi[n] = 0
    zr[n] = zr[n]*sqrt(2)
    zr = c(zr[c(1:n)],zr[c((n-1):2)])
    zi = c(zi[c(1:n)],zic[c((n-1):2)])
    z = complex(real = zr,imaginary = zi)
    gksqrt = Re(.gkFGN0(n, H))
    if (all(gksqrt > 0)) {
        gksqrt = sqrt(gksqrt)
        z = z*gksqrt
        z = fft(z, inverse = TRUE)
        z = 0.5*(n-1)**(-0.5)*z
        ans = drop(Re(z[c(1:n)])) 
    } else {
        gksqrt = 0*gksqrt
        cat("Re(gk)-vector not positive") }
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.ckFARIMA0 = 
function(n, H) 
{
    # Description: 
    #   Computes the covariances of a fractional ARIMA(0,d,0) process
    
    # Arguments:
    #   n = length of time series
    #   H = self-similarity parameter
    
    # Value:
    #   Covariances up to lag n-1
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler, Date: Sep 95.
    
    # FUNCTION:
    
    # Covariances:
    # result = (0:(n-1))
    # k = 1:(n-1)
    # d = H - 0.5
    # result[1] = gamma(1-2*d)/gamma(1-d)**2
    # result[k+1] = result[1]*(k**(2*H-2))*gamma(1-d)/gamma(d)
    # result[k+1] = result[1]*gamma(k+d)* gamma(1-d)/(gamma(k-d+1)*gamma(d))
    # ans = drop(result)      
    
    # Covariances:
    res = numeric(n)
    d = H - 0.5
    g1d = gamma(1 - d)
    gd = pi/(sin(pi * d) * g1d)
    res[1] = gamma(1 - 2 * d)/g1d^2
    k = 1:min(50, n - 1)
    res[k + 1] = res[1] * gamma(k + d) * g1d/(gamma(k - d + 1) * gd)
    if (n > 51) {
        k <- 51:(n - 1)
        res[k + 1] <- res[1] * g1d/gd * k^(2 * H - 2)
    }
    
    # Return Value:
    res
}


# ------------------------------------------------------------------------------


farimaTrueacf = 
function(n, H)
{
    .ckFARIMA0(n = n, H = H)
}


# ------------------------------------------------------------------------------


.gkFARIMA0 = 
function(n, H) 
{
    # Description: 
    #   Calculates  gk=fft of V=(r(0),...,r(n-2),r(n-1),r(n-2),...,r(1)),
    #   where r = the autocovariances of a fractional ARIMA with innovation 
    #   variance 0
    
    # Arguments: 
    #   n = length of time series
    #   H = self-similarity parameter
    
    # Value: 
    #   gk = Fourier transform of V at the Fourier frequencies
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler, Date: Sep 95.
    
    # FUNCTION:
    
    # FFT:
    gammak = .ckFARIMA0(n,H)
    ind = c(0:(n - 2), (n - 1), (n - 2):1)
    gk = gammak[ind+1]
    ans = drop(fft(c(gk), inverse = TRUE))
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


farimaTruefft = 
function(n, H)
{
    .gkFARIMA0(n = n, H = H)
}


# ------------------------------------------------------------------------------


.simFARIMA0 = 
function(n, H) 
{
    # Description: 
    #   Simulates a series X(1),...,X(n) of a fractional ARIMA(0,d,0) 
    #   process (d=H-1/2)
    
    # Arguments: 
    #   n = length of time series
    #   H = self-similarity parameter
    
    # Value: 
    #   Simulated series X(1),...,X(n)
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler, Date: Sep 95.
    
    # FUNCTION:
    
    # Simulate:
    z = rnorm(2*n)
    zr = z[c(1:n)]
    zi = z[c((n+1):(2*n))]
    zic = -zi
    zi[1] = 0
    zr[1] = zr[1]*sqrt(2)
    zi[n] = 0
    zr[n] = zr[n]*sqrt(2)
    zr = c(zr[c(1:n)], zr[c((n-1):2)])
    zi = c(zi[c(1:n)], zic[c((n-1):2)])
    z = complex(real = zr, imaginary = zi)
    cat("n = ", n, "h = ", H)
    gksqrt = Re(.gkFARIMA0(n, H))
    if (all(gksqrt > 0)) {
        gksqrt = sqrt(gksqrt)
        z = z*gksqrt
        z = fft(z, inverse = TRUE)
        z = 0.5*(n-1)**(-0.5)*z
        ans = drop(Re(z[c(1:n)])) 
    } else {
        gksqrt = 0*gksqrt
        stop("Re(gk)-vector not positive") 
    }
    
    # Return Value:
    ans
}


# ******************************************************************************
# Whittle Estimator


whittleFit = 
function(x, order = c(1, 1), subseries = 1, method = c("fgn", "farma"), 
trace = FALSE, spec = FALSE, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Minimizes an approximate log-likelihood function applied to the
    #   spectral density to obtain an estimate of the parameters of a 
    #   process.

    # Details:
    #   Function and programs for the calculation of Whittle's estimator 
    #   and the goodness of fit statistic as defined in Beran (1992). The 
    #   models are fractional Gaussian noise or fractional Arima. The data 
    #   series may be divided into subseries for which the parameters are 
    #   fitted separately. 
    # 
    #   There are several options for using the Whittle estimator. Some are 
    #   described below.  
    #   1.  One can optionally subdivide the series into "subseries".
    #   3.  One can output the periodogram by "spec". 
    #   4.  One can "trace" intermediate minimization results. 
    #   5.  The "model" can be either farma or fgn. 
    #   6.  If the model is farma, the "order" has to be specified. 
    #   7.  The starting value of H for the minimization procedure is "h". 
    #   8.  "ar" and "ma" are starting values of the time series coefficients. 
    #       (Length of vectors should be the same as p and q). 
    
    # FUNCTION:
    
    # Settings:
    data = list(x = x)
    x = as.vector(x)
    
    # Start Values:
    h = 0.7
    ar = rep(0.5, length = order[1]) / order[1]
    ma = rep(0.5, length = order[2]) / order[2] 
    
    # Estimate:
    if (trace) cat("Iteration Path:\n")
    result = .whittle(xinput = x, nsub = subseries, model = method[1], 
        pp = order[1], qq = order[2], h = h, ar = ar, ma = ma, out = trace, 
        spec = spec)[[1]]
        
    # Add:
    if (is.null(title)) title = "Hurst Exponent from Whittle Estimator"
    if (is.null(description)) description = as.character(date())
        
    # Return Value:
    new("fHURST", 
        call = match.call(),
        method = paste(method[1], "whittle"),
        hurst = result,
        parameter = list(subseries = subseries, order = order,
            h = h, ar = ar, ma = ma),        
        data = data,
        fit = result,
        plot = list(doplot = FALSE),
        title = title,
        description = description
        )   
}


# ------------------------------------------------------------------------------
# Functions to make this function independent from Beran's code 


.CetaFGN = 
function(eta) 
{   # A function implemented by Diethelm Wuertz

    # Description: 
    #   Computes covariance matrix of hat{eta} for fGn
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler Sep 95, Diethelm Wuertz 2004
    
    # FUNCTION:

    # Settings:
    M = length(eta)
    
    # Size of steps in Riemann sum: 2*pi/m
    m = 10000
    mhalfm = trunc((m-1)/2)
    
    # Size of delta for numerical calculation of derivative
    delta = 1.0e-9
    
    # Partial derivatives of log f (at each Fourier frequency)
    lf = matrix(1, ncol = M, nrow = mhalfm)
    f0 = .fspecFGN(eta,m)$fspec
    for (j in (1:M)) {
        etaj = eta
        etaj[j] = etaj[j] + delta
        fj = .fspecFGN(etaj, m)$fspec
        lf[,j] = log(fj/f0)/delta }
    
        # Calculate D:
    Djl = matrix(1,ncol = M, nrow = M)
    for (j in (1:M)) {
        for(l in (1:M)) {
            Djl[j,l] = 2*2*pi/m*sum(lf[,j]*lf[,l]) 
        } 
    }
    ans = drop(matrix(4*pi*solve(Djl), ncol = M, nrow = M, byrow = TRUE))
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.CetaARIMA = 
function(eta, p, q) 
{   # A function implemented by Diethelm Wuertz

    # Description:  
    #   Computes ovariance matrix of hat{eta} for fractional ARIMA
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler, Diethelm Wuertz 2004
    
    # FUNCTION:
    
    # Settings:
    M = length(eta)
    
    # Size of steps in Riemann sum: 2*pi/m:
    m = 10000
    mhalfm = trunc((m-1)/2)
    
    # Size of delta for numerical calculation of derivative:
    delta = 1.0e-9
    # partial derivatives of log f (at each Fourier frequency)
    lf = matrix(1, ncol = M, nrow = mhalfm)
    f0 = .fspecARIMA(eta, p, q, m)$fspec
    for (j in (1:M)) {
        etaj = eta
        etaj[j] = etaj[j]+delta
        fj = .fspecARIMA(etaj, p, q, m)$fspec
        lf[,j] = log(fj/f0)/delta 
    }
    
    # Calculate D:
    Djl = matrix(1,ncol = M, nrow = M)
    for (j in (1:M)) {
        for (l in (1:M)) {
            Djl[j,l] = 2*2*pi/m*sum(lf[,j]*lf[,l]) 
        } 
    }
    ans = drop(matrix(4*pi*solve(Djl),ncol = M, nrow = M, byrow = TRUE))
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.Qeta = 
function(eta) 
{   # A function implemented by Diethelm Wuertz

    # Description: 
    #   Calculation of A, B and Tn = A/B**2
    #   where A = 2pi/n sum 2*[I(lambda = j)/f(lambda = j)],
    #         B = 2pi/n sum 2*[I(lambda = j)/f(lambda = j)]**2  and
    #   the sum is taken over all Fourier frequencies
    #   lambda = j = 2pi*j/n (j=1,...,(n-1)/2.
    #   f is the spectral density of fractional Gaussian
    #   noise or fractional ARIMA(p,d,q) with self-similarity parameter H = h.
    #   cov(X(t),X(t+k))=integral(exp(iuk)f(u)du)
    
    # Arguments: 
    #   h
    #   (n,nhalfm = trunc[(n-1)/2] and the
    #   nhalfm-dimensional  GLOBAL vector `yper' must be defined.)
    
    # Value: 
    #   list(n=n,h=h,A=A,B=B,Tn=Tn,z=z,pval=pval, theta1=theta1,fspec=fspec)
    #   Tn is the goodness of fit test statistic
    #   Tn=A/B**2 defined in Beran (1992),
    #   z is the standardized test statistic,
    #   pval the corresponding p-value P(w>z).
    #   theta1 is the scale parameter such that
    #   f=theta1*fspec and integral(log[fspec]) = 0.
    
    # Note: 
    #   yper[1] must be the periodogram I(lambda = 1) at
    #   the frequency 2pi/n (i.e. not the frequency zero !).
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler Sep. 95, Diethelm Wuertz 2004
    
    # FUNCTION:
    
    # Settings:
    h = eta[1]
    if (imodel == 1) {
        fspec = .fspecFGN(eta,n)
        theta1 = fspec$theta1
        fspec = fspec$fspec 
    } else {
        fspec = .fspecARIMA(eta,p,q,n)
        theta1 = fspec$theta1
        fspec = fspec$fspec 
    }
    yf = yper/fspec
    yfyf = yf**2
    A = 2*(2*pi/n)*sum(yfyf)
    B = 2*(2*pi/n)*sum(yf)
    Tn = A/(B**2)
    z = sqrt(n)*(pi*Tn-1)/sqrt(2)
    pval = 1-pnorm(z)
    theta1 = B/(2*pi)
    fspec = fspec
    Qresult = list(n = n, h = h, eta = eta, A = A,B = B, Tn = Tn, 
        z = z, pval = pval, theta1 = theta1, fspec = fspec)
    ans = drop(Qresult)
    
    # Return value:
    ans
}


# ------------------------------------------------------------------------------


.fspecFGN = 
function(eta, m) 
{   # A function implemented by Diethelm Wuertz

    # Description: 
    #   Calculation of the spectral density f of normalized fractional 
    #   Gaussian noise with self-similarity parameter H=h at the 
    #   Fourier frequencies 2*pi*j/m (j=1,...,(m-1)).
    
    # Arguments: 
    #   m = sample size
    #   h = self-similarity parameter
    
    # Value: 
    #   list(fspec = fspec, theta1 = theta1)
    
    # Note:
    #   1. cov(X(t),X(t+k)) = integral[exp(iuk)f(u)du]
    #   2. f = theta1*fspec and integral[log(fspec)] = 0.
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler, Diethelm Wuertz 2004
    
    # FUNCTION:
    
    # Settings:
    
    # Taqqu: This Implementation is more efficient than Beran's:
    fspec = .ffourier.FGN.est(eta, m)
    logfspec = log(fspec)
    fint = 2/(m)*sum(logfspec)
    theta1 = exp(fint)
    fspec = fspec/theta1
    ans = drop(list(fspec = fspec, theta1 = theta1))
    
    # Return Value:
    ans
}      


# ------------------------------------------------------------------------------


.ffourier.FGN.est = 
function(H, n) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler, Diethelm Wuertz 2004
    
    # FUNCTION:
    
    # Spectrum:
    ans = .FGN.spectrum((2 * pi * (1:((n - 1)/2)))/n, H)/pi/2 
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------

    
.FGN.spectrum = 
function(lambda, H) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler, Diethelm Wuertz 2004
    
    # FUNCTION:
    
    # Settings:
    ans = 2*sin(pi*H)*gamma(2*H+1)*(1-cos(lambda))*(lambda^(-2*H-1) +
        .FGN.B.est.adjust(lambda, H)) 
    ans
}


# ------------------------------------------------------------------------------
        

.FGN.B.est.adjust = 
function(lambda, H) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler, Diethelm Wuertz 2004
    
    # FUNCTION:
    
    # Settings:
    B = .FGN.B.est(lambda, H)
    ans = (1.0002-0.000134*lambda) * (B-2^(-7.65*H-7.4)) 
    ans
}
    

# ------------------------------------------------------------------------------

    
.FGN.B.est = 
function(lambda, H) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler, Diethelm Wuertz 2004
    
    # FUNCTION:
    
    # Settings:
    d = -(2*H+1)
    dprime = -2*H
    a = function(lambda, k) { 2 * k * pi+lambda }
        b = function(lambda, k) { 2 * k * pi-lambda }
        a1 = a(lambda, 1); b1 = b(lambda, 1)
        a2 = a(lambda, 2); b2 = b(lambda, 2)
    a3 = a(lambda, 3)
    b3 = b(lambda, 3)
    a4 = a(lambda, 4)
    b4 = b(lambda, 4)
    ans = a1^d+b1^d+a2^d+b2^d+a3^d+b3^d+
        (a3^dprime+b3^dprime+a4^dprime+b4^dprime)/(8*pi*H) 
    ans
}


# ------------------------------------------------------------------------------
    

.fspecARIMA = 
function(eta, p, q, m) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler, Diethelm Wuertz 2004
    
    # FUNCTION:
    
    # Settings:
    h = eta[1]
    phi = c()
    psi = c() 
    mhalfm = trunc((m-1)/2)
    x = 2*pi/m*(1:mhalfm)
    # Calculation of f at Fourier frequencies   
    far = (1:mhalfm)/(1:mhalfm)
    fma = (1:mhalfm)/(1:mhalfm)
    if (p > 0) {
        phi = cbind(eta[2:(p+1)])
        cosar = cos(cbind(x) %*% rbind(1:p))
        sinar = sin(cbind(x) %*% rbind(1:p))
        Rar = cosar %*% phi
        Iar = sinar %*% phi
        far = (1-Rar)**2 + Iar**2 }
    if (q > 0) {
        psi = cbind(eta[(p+2):(p+q+1)])
        cosar = cos(cbind(x) %*% rbind(1:q))
        sinar = sin(cbind(x) %*% rbind(1:q))
        Rar = cosar %*% psi
        Iar = sinar %*% psi
        fma = (1+Rar)**2 + Iar**2 }
    fspec = fma/far*sqrt((1-cos(x))**2 + sin(x)**2)**(1-2*h)
    theta1 = 1/(2*pi)
    ans = list(fspec = fspec, theta1 = theta1)
    ans
}


# ------------------------------------------------------------------------------
        
            
.per = 
function(z)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler, Diethelm Wuertz 2004
    
    # FUNCTION:
    
    # Settings:
    n = length(z)
    ans = (Mod(fft(z))**2/(2*pi*n))[1:(n %/% 2 + 1)] 
    
    # Return Value:
    ans
}
    

# ------------------------------------------------------------------------------

    
.Qmin2 = 
function(etatry) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Compute:  
    ans = .Qeta(etatry)$B
    assign("bBb", ans, pos = 1)
    
    # Return Value:
    ans 
}


# ------------------------------------------------------------------------------
# Internal Function: WHITTLE ESTIMATOR:
    

.whittle = 
function(xinput, nsub = 1, model = c("farma", "fgn"), pp = 1, 
qq = 1, h = 0.5, ar = c(0.5), ma = c(0.5), out = TRUE, spec = FALSE) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function
    
    # Author: 
    #   Jan Beran; modified: Martin Maechler, Diethelm Wuertz 2004
    
    # FUNCTION:
    
    # Settings:
    model = model[1]
    assign("out", out, pos = 1)
    nmax = length(xinput)
    startend = c(1, nmax)
    istart = startend[1]
    iend = startend[2]
    nloop = nsub
    assign("n", trunc((iend - istart+1)/nloop), pos = 1)
    nhalfm = trunc((n - 1)/2)
    if (model == "farma") 
        assign("imodel", 2, pos = 1)
    if (model == "fgn") 
        assign("imodel", 1, pos = 1)
    assign("p", 0, pos = 1)
    assign("q", 0, pos = 1)
    if (imodel == 2) {
        assign("p", pp, pos = 1); assign("q", qq, pos = 1) 
    } else {
        assign("p", 0, pos=1); assign("q", 0, pos = 1) }
    eta = c(h)
    if (p > 0) eta[2:(p+1)] = ar 
    if (q > 0) eta[(p+2):(p+q+1)] = ma 
    M = length(eta) #loop
    thetavector = c()
    i0 = istart
    flax = vector("list", nloop)    
    
    # Necessary to make nsub/nloop work.  VT.
    for (iloop in (1:nloop)) {
        
        h = max(0.2, min(h, 0.9))
        eta[1] = h
        i1 = i0+n - 1
        y = xinput[i0:i1]   
        
        # Standardize Data:
        vary = var(y)
        y = (y - mean(y))/sqrt(var(y))  
        
        # Periodogram of the Data:
        if (spec) {
            assign("yper", .per(y)[2:(nhalfm+1)], w = 1)
        } else {
            assign("yper", .per(y)[2:(nhalfm+1)], pos = 1)
        }
        s = 2*(1-h)
        etatry = eta    
        
        # Modified to make optim not give incorrect result.  VT
        if (imodel == 1) {
            result = optim(par = etatry, fn = .Qmin2, 
                method = "L-BFGS-B", lower = 0, upper = 0.999) 
        } else {
            result = optim(par = etatry, fn = .Qmin2) 
        }
        eta = result$par
        sturno = result$message
        theta1 = .Qeta(eta)$theta1
        theta = c(theta1, eta)
        thetavector = c(thetavector, theta) 
        
        # Calculate goodness of fit statistic
        Qresult = .Qeta(eta)    #output
        M = length(eta)
        if (imodel == 1) {
            SD = .CetaFGN(eta)
            SD = matrix(SD, ncol = M, nrow = M, byrow = TRUE) / n 
        } else {
            # Changed to eliminate crashing in solve.qr in CetaARIMA.  VT
            cat("M =", M, "\n")
            if (M > 2) {
                for (i in 3:M) {
                    for (j in 2:(i-1)) {
                        temp = eta[i]+eta[j]
                        if (abs(temp) < 0.0001) {
                            cat("Problem with estimating confidence intervals,",
                                "parameter ", i, "and  parameter ", j, 
                                "are the same, eliminating.\n")
                            eta = eta[ - i]
                            eta = eta[ - j]
                            M = M - 2
                            p = p - 1
                            q = q - 1 
                        } 
                    } 
                } 
            }
            SD = .CetaARIMA(eta, p, q)
            SD = matrix(SD, ncol = M, nrow = M, byrow = TRUE)/n 
        }   
        Hlow = eta[1] - 1.96 * sqrt(SD[1, 1])
        Hup = eta[1]+1.96 * sqrt(SD[1, 1])
        if (out) {
            cat("theta =", theta, fill = TRUE)
            cat("H =", eta[1], fill = TRUE)
            cat("95%-CI for H: [", Hlow, ",", Hup, "]", fill = TRUE) 
        }
        
        # Changing of the signs of the moving average parameters
        # in order to respect the sign of the Splus convention
        if (q > 0) eta[(p+2):(p+q+1)] = -eta[(p+2):(p+q+1)]
        etalow = c()
        etaup = c()
        for (i in (1:length(eta))) {
            etalow = c(etalow, eta[i] - 1.96 * sqrt(SD[i, i]))
            etaup = c(etaup, eta[i]+1.96 * sqrt(SD[i, i])) 
        }
        if (out) {
            cat("95%-CI:", fill = TRUE)
            print(cbind(etalow, etaup), fill = TRUE) 
        }
        if (spec) {
            cat("Periodogram is in yper", fill = TRUE)
            assign("fest", Qresult$theta1 * Qresult$fspec, w = 1)
            cat("Spectral density is in fest", fill = TRUE) 
        }
        flax[[iloop]] = list()      
        flax[[iloop]]$par = eta
        flax[[iloop]]$sigma2 = bBb * var(xinput)
        flax[[iloop]]$conv.type = sturno
        remove("bBb", pos = 1)  
            
        # Next subseries:
        i0 = i0+n   
        
        # Changing of the signs of the moving average parameters:
        if(q > 0) eta[(p+2):(p+q+1)] = -eta[(p+2):(p+q+1)]
    } # end of nloop
    
    
    # Return:
    return(flax)
    
    # Clean up:
    remove("n", pos = 1)
    remove("p", pos = 1)
    remove("q", pos = 1)
    remove("imodel", pos = 1)
    remove("out", pos = 1)
    if(spec == FALSE) remove("yper", pos = 1) 
}


# ##############################################################################
# PART III: Reimplemented Functions from Taqqu


setClass("fHURST", 
    representation(
        call = "call",
        method = "character",
        hurst = "list",
        parameter = "list",
        data = "list",
        fit = "list",
        plot = "list",
        title = "character",
        description = "character")  
)   



# ------------------------------------------------------------------------------


show.fHURST = 
function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Prints a fHURST Object
    
    # FUNCTION:
    
    # Setting:
    x = object
    doplot = TRUE
    
    # Title:
    cat("\nTitle:\n ", x@title, "\n", sep = "")
    
    # Call:
    cat("\nCall:\n ")
    cat(paste(deparse(x@call), sep = "\n", collapse = "\n"), "\n", sep = "")
    
    # Method:
    cat("\nMethod:\n ", x@method, "\n", sep = "")
    
    # Hurst Exponent:
    cat("\nHurst Exponent:\n")
    H = as.numeric(unlist(x@hurst)[1:2])
    names(H) = names(unlist(x@hurst)[1:2])
    output = capture.output(H)
    cat(paste(" ", output), sep = "\n")
    
    # Hurst Exponent Diagnostic:
    if (!is.null(x@hurst$diag)) {
        cat("\nHurst Exponent Diagnostic:\n ")
        print(x@hurst$diag[2, ])
    }
    
    # Parameter Settings:
    cat("\nParameter Settings:\n")
    parms = unlist(x@parameter)
    integer.parms = as.integer(parms)
    names(integer.parms) = names(parms)
    # output = capture.output(integer.parms)
    # cat(paste(" ", output), sep = "\n")
    print(integer.parms)
    
    # Description:
    cat("\nDescription:\n ", x@description, sep = "")   
    cat("\n\n")
    
    # Plot:
    fit = object
    if (x@plot$doplot) {
        labels = TRUE
        if (labels) {
            xlab = fit@plot$xlab
            ylab = fit@plot$ylab
            H = as.character(round(fit@hurst$H, digits = 4))
            main = paste(fit@method, "\n H =", H)
            M = fit@plot$m[fit@plot$weights == 1]
            min.M = as.character(min(M))
            max.M = as.character(max(M))
            M = as.character(length(M))
            gridlines = TRUE 
        } else {    
            xlab = ""
            ylab = ""
            main = "" 
            gridlines = FALSE 
        }   
        # Plot:
        x = c(0, log10(fit@plot$m))
        y = c(0, log10(fit@plot$data))
        wt = fit@plot$weights
        plot(x, y, type = "n", xlab = xlab, ylab = ylab)
        title(main = main)
        if (gridlines) grid()
        x = x[-1]
        y = y[-1]
        points(x[wt == 1], y[wt == 1], pch = 19, cex = fit@plot$cex)
        points(x[wt == 0], y[wt == 0], pch = 3, cex = fit@plot$cex)
        # y = mx + c
        m = fit@fit$coeff[[2]]
        a = fit@fit$coeff[[1]]
        x.fit = x[wt == 1]
        y.fit = m * x.fit + a
        lines(x.fit, y.fit, lwd = 2)
        if (is.numeric(fit@plot$abline))
            abline(fit@plot$abline[1], fit@plot$abline[2], lty = 3)
    }
        
    # Return Value:
    invisible()
}


setMethod("show", "fHURST", show.fHURST)


# ******************************************************************************
# Aggregated Variance - [Taqqu 3.1] 


aggvarFit = 
function(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),
doplot = FALSE, trace = FALSE, title = NULL, description = NULL)
{   # A functions implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    #   x - a numeric vector, a 'timeSeries' object, or any other
    #       object which can be transformed into a vector by the
    #       function 'as.vector'. 
    #   levels - the number of aggregation levels
    #   minnpts - the minimum block size
    #   cut.off - the lower and upper cut off for the fit
    
    # Value:
    #   Returns a list with the folllowing elements:
    #   data - a data frame with blocksizes M, aggregated variances
    #       'AGGVAR', and weights for the fit ,'wt', the numeric
    #       values of 'beta' and the Hurst exponent 'H'.
        
    # FUNCTION:
    
    # Settings:
    call = match.call()
    data = list(x = x)
    x = as.vector(x)
    n = length(x)
    increment = (log10(n/minnpts))/levels   
    M = floor(10^((1:levels)*increment))
    M = M[M > 0]
    
    # Create Data:
    AGGVAR = NULL
    for (m in M) {
        nCols = n %/% m 
        X = matrix(x[1:(m*nCols)], byrow = FALSE, ncol = nCols) 
        STATS = var(colMeans(X))
        AGGVAR = c( AGGVAR, STATS )
        if (trace) cat("\n\tm = \t", m, "\tAggVar = \t", STATS  )
    }
    if(trace) cat("\n")
    
    # Fit:
    wt = trunc((sign((M-cut.off[1])*(cut.off[2]-M))+1)/2)
    fit = lsfit(log10(M), log10(AGGVAR), wt)
    fitH = lsfit(log10(M), 0.5*log10(AGGVAR*M*M), wt)
    fitH$wt = NULL
    diag = as.data.frame(ls.print(fitH, print.it = FALSE)[[2]][[1]])
    beta = fit$coef[[2]]
    H = (beta + 2) / 2
    
    # Return Value:
    plot = list(m = M, data = AGGVAR, weights = wt, 
        abline = c(0, -1), cex = 0.7, doplot = doplot,
        xlab = "log10(m)", ylab = "log10(variances)")
    
    # Add:
    if (is.null(title)) 
        title = "Hurst Exponent from Aggregated Variances"
    if (is.null(description)) 
        description = as.character(date())
    
    # Return Value:
    new("fHURST", 
        call = call,
        method = "Aggregated Variance Method",
        hurst = list(H = H, beta = beta, diag = diag),
        parameter = list(n = n, levels = levels, minnpts = minnpts,
            cut.off = cut.off),        
        data = data,
        fit = fit,
        plot = plot,
        title = title,
        description = description
        )   
}


# ******************************************************************************
# Differenced Aggregated Variance - [Taqqu 3.2]


diffvarFit =
function(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5),
doplot = FALSE, trace = FALSE, title = NULL, description = NULL)
{   # A functions implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    #   x - a numeric vector, a 'timeSeries' object, or any other
    #       object which can be transformed into a vector by the
    #       function 'as.vector'. 
    #   levels - the number of aggregation levels
    #   minnpts - the minimum block size
    #   cut.off - the lower and upper cut off for the fit
    
    # Value:
    #   Returns a list with the folllowing elements:
    #   data - a data frame with blocksizes M, differenced aggregated 
    #       variances 'DIFFVAR', and weights for the fit ,'wt', the 
    #       numeric values of 'beta' and the Hurst exponent 'H'.
    
    # FUNCTION:
    
    # Settings:
    call = match.call()
    n = length(as.vector(x))
    data = list(x = x)
    x = as.vector(x)
    
    # Compute Aggregated Variances:
    ans = aggvarFit(x, levels, minnpts, cut.off)   
    
    # Create Differenced Data:  
    DIFFVAR = -diff(ans@plot$data)
    
    # What M's to use?
    # M = ( ans@plot$data[-levels, 1] + ans@plot$data[-1] ) / 2
    # M = sqrt ( ans@plot$data[-levels] * ans@plot$data[-1] )
    # M = ans@plot$data[-1]
    M = ans@plot$m[-levels]
    
    # Remove negative and zero values:
    M = M[DIFFVAR > 0]
    wt = (ans@plot$weights[-levels])[DIFFVAR > 0]
    DIFFVAR = DIFFVAR[DIFFVAR > 0]
    
    if (trace) {
        for ( i in 1:length(M) ) 
            cat("\n\tm = \t", M[i], "\tDiffVar = \t", DIFFVAR[i]  )
        cat("\n")
    }
    
    # Fit:
    fit = lsfit(log10(M), log10(DIFFVAR), wt)
    fitH = lsfit(log10(M), 0.5*log10(DIFFVAR*M*M), wt)
    fitH$wt = NULL
    diag = as.data.frame(ls.print(fitH, print.it = FALSE)[[2]][[1]])
    beta = fit$coef[[2]]
    H = (beta + 2) / 2
    
    # Return Value:
    plot = list(m = M, data = DIFFVAR, weights = wt, 
        abline = FALSE, cex = 0.7, doplot = doplot,
        xlab = "log10(m)", ylab = "log10(variances)")
      
    # Add:
    if (is.null(title)) 
        title = "Hurst Exponent from Differenced Aggregated Variances"
    if (is.null(description)) 
        description = as.character(date())
    
    # Return Value:
    new("fHURST", 
        call = call,
        method = "Differenced Aggregated Variance",
        hurst = list(H = H, beta = beta, diag = diag),
        parameter = list(n = n, levels = levels, minnpts = minnpts,
            cut.off = cut.off),        
        data = data,
        fit = fit,
        plot = plot,
        title = title,
        description = description
        )   
}


# ******************************************************************************
# Absolute Value/Moments Method - [Taqqu 3.3]


absvalFit = 
function(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5), moment = 1,
doplot = FALSE, trace = FALSE, title = NULL, description = NULL)
{   # A functions implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    #   x - a numeric vector, a 'timeSeries' object, or any other
    #       object which can be transformed into a vector by the
    #       function 'as.vector'. 
    #   levels - the number of aggregation levels
    #   minnpts - the minimum block size
    #   cut.off - the lower and upper cut off for the fit
    
    # Value:
    #   Returns a list with the folllowing elements:
    #   data - a data frame with blocksizes M, differenced aggregated 
    #       variances 'DIFFVAR', and weights for the fit ,'wt', the 
    #       numeric values of 'beta' and the Hurst exponent 'H'.
    
    # FUNCTION:
    
    # Settings:
    call = match.call()
    data = list(x = x)
    x = as.vector(x)
    n = length(x)
    increment = (log10(n/minnpts))/levels   
    M = floor(10^((1:levels)*increment))
    
    # Compute Absolute Moments:
    ABSVAL = NULL
    for (m in M) {
        nCols = n %/% m 
        X = matrix(x[1:(m*nCols)], byrow = FALSE, ncol = nCols) 
        Y = colMeans(X)
        MEAN = mean(Y)
        STATS = sum( (abs(Y-MEAN))^moment ) / (length(Y) - 1)
        ABSVAL = c( ABSVAL, STATS )
        if (trace) cat("\n\tm = \t", m, "\tAbsVal = \t", STATS  )
    }
    if(trace) cat("\n")
    
    
    # Fit:
    wt = trunc((sign((M-cut.off[1])*(cut.off[2]-M))+1)/2)
    fit = lsfit(log10(M), log10(ABSVAL), wt)
    fitH = lsfit(log10(M), log10(ABSVAL*M^moment)/moment, wt)
    fitH$wt = NULL
    diag = as.data.frame(ls.print(fitH, print.it = FALSE)[[2]][[1]])
    beta = fit$coef[[2]]
    H = beta/moment + 1
    
    # Return Value:
    plot = list(m = M, data = ABSVAL, weights = wt, 
        abline = c(0, -0.5), cex = 0.7, doplot = doplot,
        xlab = "log10(m)", ylab = "log10(variances)")
    
    # Add:
    if (is.null(title)) 
        title = "Hurst Exponent from Absolute Values"
    if (is.null(description)) 
        description = as.character(date())  
        
    # Return Value:
    new("fHURST", 
        call = call,
        method = paste("Absolute Moment - No.", as.character(moment)),
        hurst = list(H = H, beta = beta, diag = diag),
        parameter = list(n = n, levels = levels, minnpts = minnpts,
            cut.off = cut.off, moment = moment),        
        data = data,
        fit = fit,
        plot = plot,
        title = title,
        description = description
        )   
}


# ******************************************************************************
# Higuchi Method / Fratal Dimension Method - [Taqqu 3.4]


higuchiFit = 
function(x, levels = 50, minnpts = 2, cut.off = 10^c(0.7, 2.5), 
doplot = FALSE, trace = FALSE, title = NULL, description = NULL)
{   # A functions implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    #   x - a numeric vector, a 'timeSeries' object, or any other
    #       object which can be transformed into a vector by the
    #       function 'as.vector'. 
    #   levels - the number of aggregation levels
    #   minnpts - the minimum block size
    #   cut.off - the lower and upper cut off for the fit
    
    # Value:
    #   Returns a list with the folllowing elements:
    #   data - a data frame with blocksizes M, differenced aggregated 
    #       variances 'DIFFVAR', and weights for the fit ,'wt', the 
    #       numeric values of 'beta' and the Hurst exponent 'H'.
    
    # FUNCTION:
    
    # Settings:
    call = match.call()
    data = list(x = x)
    x = as.vector(x)
    y = cumsum(x)
    n = length(x)
    increment = (log10(n/minnpts))/levels   
    M = floor(10^((1:levels)*increment))
      
    # Higuchi Method:    
    if (trace) cat("\nHiguchi Iteration Path:")
    HIGUCHI = NULL
    for ( m in M ) {
        k.max = max(floor((n-(1:m))/m) )
        X = matrix(rep(0, length = m*k.max), byrow = FALSE, ncol = k.max)
        for ( i in 1:m ) {
            for ( k in 1:(floor((n-i)/m)) ) {
                X[i, k] = abs(y[1+k*m] - y[i+(k-1)*m]) / floor((n-i)/m)
            }
        }        
        Y = sum(X) * (n-1) / m^3
        STATS = Y / n
        HIGUCHI = c( HIGUCHI, STATS )
        if (trace) cat("\n\tm = \t", m, "\tHiguchi = \t", STATS  )  
    }
    if (trace) cat("\n")
    
    # Fit:
    wt = trunc((sign((M-cut.off[1])*(cut.off[2]-M))+1)/2)
    fit = lsfit(log10(M), log10(HIGUCHI), wt)
    fitH = lsfit(log10(M), log10(HIGUCHI*M*M), wt)
    fitH$wt = NULL
    diag = as.data.frame(ls.print(fitH, print.it = FALSE)[[2]][[1]])
    beta = fit$coef[[2]]
    H = beta + 2
    
    # Return Value:
    plot = list(m = M, data = HIGUCHI, weights = wt, 
        abline = c(0, -0.5), cex = 0.7, doplot = doplot,
        xlab = "log10(m)", ylab = "log10(curve length)")
    
    # Add:
    if (is.null(title)) 
        title = "Hurst Exponent from Higuchi Method"
    if (is.null(description)) 
        description = as.character(date())  
        
    # Return Value:
    new("fHURST", 
        call = call,
        method = "Higuchi Method",
        hurst = list(H = H, beta = beta, diag = diag),
        parameter = list(n = n, levels = levels, minnpts = minnpts,
            cut.off = cut.off),        
        data = data,
        fit = fit,
        plot = plot,
        title = title,
        description = description
        )   
}   


# ******************************************************************************
# Peng's Method / Variance of Residuals - [Taqqu 3.5]


pengFit = 
function(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5), 
method = c("mean", "median"), 
doplot = FALSE, trace = FALSE, title = NULL, description = NULL)
{   # A functions implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    #   x - a numeric vector, a 'timeSeries' object, or any other
    #       object which can be transformed into a vector by the
    #       function 'as.vector'. 
    #   levels - the number of aggregation levels
    #   minnpts - the minimum block size
    #   cut.off - the lower and upper cut off for the fit
    
    # Value:
    #   Returns a list with the folllowing elements:
    #   data - a data frame with blocksizes M, differenced aggregated 
    #       variances 'DIFFVAR', and weights for the fit ,'wt', the 
    #       numeric values of 'beta' and the Hurst exponent 'H'.
    
    # FUNCTION:
    
    # Settings:
    call = match.call()
    data = list(x = x)
    x = as.vector(x)
    n = length(x)
    increment = (log10(n/minnpts))/levels   
    M = floor(10^((1:levels)*increment))
    M = M[M>2]
    
    # Averaging Function:
    stats = match.fun(method[1])
    
    # Peng's Method:
    PENG = NULL
    for (m in M) {
        nCols = n %/% m 
        X = matrix(x[1:(m*nCols)], byrow = FALSE, ncol = nCols)
        Y = colCumsums(X)
        V = NULL
        t = cbind(1, as.matrix(1:m))       
        for (i in 1:nCols ) {        
            y = Y[, i]
            nrx = nry = NROW(t)
            ncx = NCOL(t)
            ncy = NCOL(y)
            # Fast lsfit - Have a look on function 'lsfit'
            # This is the essential line from function 'lsfit'
            res = .Fortran("dqrls", 
                qr = t, n = nrx, p = ncx, 
                y = as.matrix(y), ny = ncy, tol = 1e-7, 
                coefficients = mat.or.vec(ncx, ncy), 
                residuals = mat.or.vec(nrx, ncy), 
                effects = mat.or.vec(nrx, ncy), rank = integer(1), 
                pivot = as.integer(1:ncx), qraux = double(ncx), 
                work = double(2 * ncx), 
                PACKAGE = "base")$residuals
            V = c(V, var(res))
        }
        STATS = stats(V)
        PENG = c(PENG, STATS)
        if (trace) cat("\n\tm = \t", m, "\tPENG = \t", STATS  )  
    }
    if (trace) cat("\n")
    
    # Fit:
    wt = trunc((sign((M-cut.off[1])*(cut.off[2]-M))+1)/2)
    fit = lsfit(log10(M), log10(PENG), wt)
    fitH = lsfit(log10(M), log10(PENG)/2, wt)
    fitH$wt = NULL
    diag = as.data.frame(ls.print(fitH, print.it = FALSE)[[2]][[1]])
    beta = fit$coef[[2]]
    H = beta/2
    
    # Return Value:
    plot = list(m = M, data = PENG, weights = wt, 
        abline = FALSE, cex = 0.7, doplot = doplot,
        xlab = "log10(m)", ylab = "log10(var[residuals])")
    
    # Add:
    if (is.null(title)) 
        title = "Hurst Exponent from Peng Method"
    if (is.null(description)) 
        description = as.character(date())  
        
    # Return Value:
    new("fHURST", 
        call = call,
        method = "Peng Method",
        hurst = list(H = H, beta = beta, diag = diag),
        parameter = list(n = n, levels = levels, minnpts = minnpts,
            cut.off = cut.off),        
        data = data,
        fit = fit,
        plot = plot,
        title = title,
        description = description
        )   
}   

    
# ******************************************************************************
# R/S Statistic Method - [Taqqu 3.6]


rsFit = 
function(x, levels = 50, minnpts = 3, cut.off = 10^c(0.7, 2.5), 
doplot = FALSE, trace = FALSE, title = NULL, description = NULL)
{   # A functions implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    #   x - a numeric vector, a 'timeSeries' object, or any other
    #       object which can be transformed into a vector by the
    #       function 'as.vector'. 
    #   levels - the number of aggregation levels
    #   minnpts - the minimum block size
    #   cut.off - the lower and upper cut off for the fit
    
    # Value:
    #   Returns a list with the folllowing elements:
    #   data - a data frame with blocksizes M, differenced aggregated 
    #       variances 'DIFFVAR', and weights for the fit ,'wt', the 
    #       numeric values of 'beta' and the Hurst exponent 'H'.
    
    # FUNCTION:
    
    # Settings:
    call = match.call()
    data = list(x = x)
    x = as.vector(x)
    n = length(x)
    increment = (log10(n/minnpts))/levels   
    M = floor(10^((1:levels)*increment))
    M = M[M > 1]
    
    # R/S Method:
    Y = cumsum(x)
    Y2 = cumsum(x*x)
    RS = NULL
    for (m in M) {  
        S = sqrt(Y2[m]/m - (Y[m]/m)^2)      
        Z = Y[1:m]-(1:m)*Y[m]/m   
        STATS = (max(Z) - min(Z))/S  
        RS = c(RS, STATS)
        if (trace) cat("\n\tm = \t", m, "\tR/S = \t", STATS  )  
    }
    if (trace) cat("\n")
    
    # Fit:
    wt = trunc((sign((M-cut.off[1])*(cut.off[2]-M))+1)/2)
    fit = lsfit(log10(M), log10(RS), wt)
    fitH = fit
    fitH$wt = NULL
    diag = as.data.frame(ls.print(fitH, print.it = FALSE)[[2]][[1]])
    beta = fit$coef[[2]]
    H = beta
    
    # Plot Values:
    plot = list(m = M, data = RS, weights = wt, 
        abline = FALSE, cex = 0.7, doplot = doplot,
        xlab = "log10(d)", ylab = "log10(r/s)")
    
    # Add:
    if (is.null(title)) 
        title = "Hurst Exponent from R/S Method"
    if (is.null(description)) 
        description = as.character(date())  
        
    # Return Value:
    new("fHURST", 
        call = call,
        method = "R/S Method",
        hurst = list(H = H, beta = beta, diag = diag),
        parameter = list(n = n, levels = levels, minnpts = minnpts,
            cut.off = cut.off),        
        data = data,
        fit = fit,
        plot = plot,
        title = title,
        description = description
        )   
}  



# ******************************************************************************
# Periodogram Method - [Taqqu 3.7]


perFit = 
function(x, cut.off = 0.10,
method = c("per", "cumper"), doplot = FALSE, title = NULL, description = NULL)
{   # A functions implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    #   x - a numeric vector, a 'timeSeries' object, or any other
    #       object which can be transformed into a vector by the
    #       function 'as.vector'. 
    #   levels - the number of aggregation levels
    #   minnpts - the minimum block size
    #   cut.off - the lower and upper cut off for the fit
    
    # Value:
    #   Returns a list with the folllowing elements:
    #   data - a data frame with blocksizes M, differenced aggregated 
    #       variances 'DIFFVAR', and weights for the fit ,'wt', the 
    #       numeric values of 'beta' and the Hurst exponent 'H'.
    
    # FUNCTION:
    
    # Settings:
    call = match.call()
    data = list(x = x)
    x = as.vector(x)
    n = length(x)
    FFT = Mod(fft(x))^2/(2*pi*n)
    pgram = FFT[1:(n %/% 2+1)]
    N = length(pgram)  
    
    # Periodogram Method:
    if (method[1] == "per") {
        Method = "Periodogram Method"
        X = (pi/n)*c(2:((n*cut.off)))
        Y = pgram[2:((n*cut.off))]
        fit = lsfit(x = log10(X), y = log10(Y))     
        fitH = lsfit(log10(X), log10(X/Y)/2)
        diag = as.data.frame(ls.print(fitH, print.it = FALSE)[[2]][[1]])      
        beta = fit$coef[[2]]   
        H = (1-beta)/2 
        U = (pi/n)*(1:n)
        V = FFT 
    }
    
    # Cumulated Periodogram Method:
    if (method[1] == "cumper") {    
        Method = "Cumulated Periodogram Method"
        PGRAM = cumsum(pgram[2:n])
        U = (pi/n)*c(1:(n-1))
        V = PGRAM[1:(n-1)] 
        X = (pi/n)*c(1:(((n-1)*cut.off)))
        Y = PGRAM[1:(((n-1)*cut.off))]
        fit = lsfit(x = log10(X), y = log10(Y))
        fitH = lsfit(log10(X), log10(X*X/Y)/2)
        diag = as.data.frame(ls.print(fitH, print.it = FALSE)[[2]][[1]])    
        beta = fit$coef[[2]]
        H = (2-beta)/2
        U = (pi/n)*(1:n)
        V = cumsum(FFT) 
    }
    
    # Plot Values:
    plot = list(m = U, data = V, weights = rep(1, times = n), 
        abline = FALSE, cex = 0.25, doplot = doplot,
        xlab = "log10(frequency)", ylab = "log10(periodogram)")
    
    # Add:
    if (is.null(title)) 
        title = "Hurst Exponent from Periodgram Method"
    if (is.null(description)) 
        description = as.character(date())  
        
    # Return Value:
    new("fHURST", 
        call = call,
        method = Method,
        hurst = list(H = H, beta = beta, diag = diag),
        parameter = list(n = n, cut.off = 100*cut.off),        
        data = data,
        fit = fit,
        plot = plot,
        title = title,
        description = description
        )    
}


# ******************************************************************************
# Boxed (Modified) Periodogram Method - [Taqqu 3.8]


boxperFit = 
function(x, nbox = 100, cut.off = 0.10, 
doplot = FALSE, trace = FALSE, title = NULL, description = NULL) 
{   # A functions implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    #   x - a numeric vector, a 'timeSeries' object, or any other
    #       object which can be transformed into a vector by the
    #       function 'as.vector'. 
    
    # Value:
    #   Returns a list with the folllowing elements:
    #   data - a data frame with blocksizes M, differenced aggregated 
    #       variances 'DIFFVAR', and weights for the fit ,'wt', the 
    #       numeric values of 'beta' and the Hurst exponent 'H'.
    
    # FUNCTION:
    
    # Settings:
    call = match.call()
    data = list(x = x)
    x = as.vector(x)
    len = length(x)
    pgram = (Mod(fft(x))^2/(2*pi*len)) [1:(len %/% 2+1)]
    n = length(pgram)
    
    # Calculate fractions from percentage:
    per1 = cut.off
    per2 = 1 - per1
    m = log10(per2 * n) / nbox
    
    # Do the boxes (except for beginning few points):
    padj = z = NULL
    for (i in 1:nbox) {
        m1 = floor(10^(m * i - m) + per1 * n)
        m2 = floor(10^(m * i) + per1 * n)
        padj[i] = sum(pgram[m1:m2])/(m2 - m1 + 1)
        z[i] = log10((pi * (m2 + m1))/(2 * n)) 
    }
    
    # x|y points:
    X = c( 0, log10((pi/n) * (2:floor(per1 * n))) )
    Y = c( 0, log10(pgram[2:floor(per1 * n)]) )
    i = (floor(per1 * n) + 1):(floor(per1 * n) + nbox)   
    X = c(X, z[i - floor(per1 * n)] )
    Y = c(Y, log10(padj[i - floor(per1 * n)]) )
       
    # Fit:
    XN = 10^X
    YN = 10^Y
    fit = lsfit(log10(XN), log10(YN))
    fitH = lsfit(log10(XN), log10(XN/YN)/2)
    diag = as.data.frame(ls.print(fitH, print.it = FALSE)[[2]][[1]])   
    beta = fit$coef[[2]]
    H = (1-beta)/2
    
    # Plot Values:
    plot = list(m = XN, data = YN, weights = rep(1, times = length(XN)), 
        abline = FALSE, cex = 0.5, doplot = doplot, 
        xlab = "log10(frequency)", ylab = "log10(periodogram)") 
    
    # Add:
    if (is.null(title)) 
        title = "Hurst Exponent from Boxed Periodgram Method"
    if (is.null(description)) 
        description = as.character(date())  
        
    # Return Value:
    new("fHURST", 
        call = call,
        method = "Boxed Periodogram",
        hurst = list(H = H, beta = beta, diag = diag),
        parameter = list(n = n, nbox = nbox, cut.off = cut.off),        
        data = data,
        fit = fit,
        plot = plot,
        title = title,
        description = description
        )    
}

   
# ******************************************************************************
# Whittle Estimator -> PART II - [Taqqu 3.8]



################################################################################
# PART IV: Wavelet Estimator


waveletFit = 
function(x, length = NULL, order = 2, octave = c(2, 8), 
doplot = FALSE, title = NULL, description = NULL)    
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Function to do the Wavelet estimator of H.
    
    # Arguments:
    #   x - Data set.
    #   length - Length of data to be used (must be power of 2) 
    #       if NULL, the previous power will be used
    #   octave - Beginning and ending octave for estimation.

    # Details:
    #   This method computes the Discrete Wavelet Transform, averages the 
    #   squares of the coefficients of the transform, and then performs a 
    #   linear regression on the logarithm of the average, versus the log 
    #   of j, the scale parameter of the transform. The result should be 
    #   directly proportional to H.
    #   There are several options available for using this method: method.
    #   1.  The length of the data must be entered (power of 2).
    #   2.  c(j1, j2) are the beginning and ending octaves for the estimation.
    #   3.  'order' is the order of the wavelet. (2 default)
    #   5.  Calls functions from R's Wavelet package. ( wd, accessD ).
    #   6.  Inside function, a bound.effect is used in the estimation to 
    #       avoid boundary effects on the coefficients.

    # Authors:
    #   Based on work by Ardry and Flandrin.
    #   Originally written by Vadim Teverovsky 1997.   
    
    # Notes:
    #   Calls functions from R's package 'wavethresh'
    
    # FUNCTION:
    
    # Settings:
    N = order
    call = match.call()
    data = list(x = x)
    x = as.vector(x)
    j1 = octave[1]
    j2 = octave[2]
    if(is.null(length)) length = 2^floor(log(length(x))/log(2))
    noctave = log(length, base = 2) - 1
    bound.effect = ceiling(log(2*N, base = 2))     
    
    # Calculate:
    transform = .wd(x[1:(length)], filter = N)
    statistic = rep(0, noctave)
    if (j2 > noctave - bound.effect) {
        # cat("Upper bound too high, resetting to ", noctave-bound.effect, "\n")
        j2 = noctave - bound.effect 
        octave[2] = j2
    }       
    for (j in 1:(noctave - bound.effect)) {
        statistic[j] = log(mean((.accessD(transform, 
            lev = (noctave+1-j))[N:(2^(noctave+1-j)-N)])^2), base = 2) 
    }
    
    # Fit:
    X = 10^c(j1:j2)
    Y = 10^statistic[j1:j2]
    fit = lsfit(log10(X), log10(Y))
    fitH = lsfit(log10(X), log10(Y*X)/2)
    diag = as.data.frame(ls.print(fitH, print.it = FALSE)[[2]][[1]])   
    beta = fit$coef[[2]]
    H = (beta+1)/2
    
    # Plot Values:
    plot = list(
        m = 10^c(1:(noctave - bound.effect)), 
        data = 10^statistic[1:(noctave - bound.effect)], 
        weights = rep(1, times = length(X)), 
        abline = FALSE, cex = 0.5, doplot = doplot, 
        xlab = "Octave", ylab = "Statistic") 
        
    # Add Slope:    
    # abline(fit$coef[[1]], fit$coef[[2]])}
    
    # Add:
    if (is.null(title)) title = "Hurst Exponent from Wavelet Estimator"
    if (is.null(description)) description = as.character(date())
    
    # Return Value:
    new("fHURST", 
        call = call,
        method = "Wavelet Method",
        hurst = list(H = H, beta = beta, diag = diag),
        parameter = list(length = length, order = order, octave = octave),        
        data = data,
        plot = plot,
        fit = fit,
        title = title,
        description = description
        )    
}


# ------------------------------------------------------------------------------


# Following auxilliary code copied from:


# Package: wavethresh
# Version: 2.2-8
# Note:    ----- Version also in wvrelease() in ./R/release.R
# Date: 2004-03-08
# Author: Guy Nason <G.P.Nason@Bristol.ac.uk>
#   of R-port: Arne Kovac (1997) and Martin Maechler (1999)
# Maintainer: Martin Maechler <maechler@stat.math.ethz.ch>
# Title: Software to perform wavelet statistics and transforms.
# Description: Software to perform 1-d and 2-d wavelet statistics and transforms
# Depends: R (>= 1.4)
# License: GPL version 2 or later


.accessD = 
function(wd.obj, level, boundary = FALSE)
{   # A function copied from R-package wavethresh

    # FUNCTION:
    
    # Settings:
    ctmp = class(wd.obj)
    if (is.null(ctmp) || all(ctmp != "wd"))
        stop("argument `wd.obj' is not of class \"wd\"")
    if (level < 0) 
        stop("Must have a positive level")
    else if(level > wd.obj$nlevels - 1)
        stop("`level' must be less than resolution (= nlevels)")
    level = level + 1
    first.last.d = wd.obj$fl.dbase$first.last.d
    first.level = first.last.d[level, 1]
    last.level  = first.last.d[level, 2]
    off.l = first.last.d[level, 3]
    
    if (boundary) {
        n = last.level - first.level + 1
        wd.obj$D[(off.l + 1):(off.l + n)]
    } else {
        n = 2^(level - 1)
        wd.obj$D[(off.l + 1 - first.level):(off.l + n - first.level)]
    }
}


# ------------------------------------------------------------------------------


.wd = 
function(data, filter.number = 2, family = c("DaubExPhase", "DaubLeAsymm"),
bc = c("periodic", "symmetric"), verbose = getOption("verbose"))
{   # A function copied from R-package wavethresh

    # FUNCTION:
    
    # Settings:
    if (verbose) cat("Argument checking...")
    family = match.arg(family)
    bc = match.arg(bc)
    DataLength = length(data)           

    # Check that we have a power of 2 data elements
    nlevels = log(DataLength)/log(2)
    if(round(nlevels) != nlevels)
        stop("The length of data is not a power of 2")

    # Select the appropriate filter
    if (verbose) cat("...done\nFilter...")
    filter = .filter.select(filter.number = filter.number, family = family)

    # Build the first/last database
    if (verbose) cat("...selected\nFirst/last database...")
    fl.dbase = .first.last(LengthH = length(filter$H), DataLength =
        DataLength, bc = bc) 

    # Put in the data
    C = rep(0, fl.dbase$ntotal)
    C[1:DataLength] = data              
    error = if (verbose) 1 else 0
    if (verbose) cat("built\nObject code...")

    # Compute the decomposition
    if(verbose) cat("Decomposing...\n")
    nbc = switch(bc, periodic = 1, symmetric = 2)
    wavelet.decomposition = .C("wavedecomp",
        C = as.double(C),
        LengthC = as.integer(fl.dbase$ntotal),
        D = double(fl.dbase$ntotal.d),
        LengthD = as.integer(fl.dbase$ntotal.d),
        H = as.double(filter$H),
        LengthH = as.integer(length(filter$H)),
        nlevels = as.integer(nlevels),
        firstC = as.integer(fl.dbase$first.last.c[, 1]),
        lastC = as.integer(fl.dbase$first.last.c[, 2]),
        offsetC = as.integer(fl.dbase$first.last.c[, 3]),
        firstD = as.integer(fl.dbase$first.last.d[, 1]),
        lastD = as.integer(fl.dbase$first.last.d[, 2]),
        offsetD = as.integer(fl.dbase$first.last.d[, 3]),
        nbc = as.integer(nbc),
        error = as.integer(error),
        PACKAGE = "fSeries")
           
    if (verbose) cat("done\n")
    error = wavelet.decomposition$error
    if (error != 0) stop(paste("Error", error, " occured in wavedecomp"))

    # Result:
    l = list(C = wavelet.decomposition$C, D = wavelet.decomposition$D,
        nlevels = wavelet.decomposition$nlevels, fl.dbase = fl.dbase,
        filter = filter, bc = bc)
    class(l) = "wd"
    
    # Return Value:
    l
}


# ------------------------------------------------------------------------------
 

.filter.select = 
function(filter.number, family = c("DaubExPhase", "DaubLeAsymm"),
constant = 1)
{   # A function copied from R-package wavethresh

    # FUNCTION:
    
    # Settings:
    family = match.arg(family)# one of the two, maybe abbrev. in call
    if ((filter.number = as.integer(filter.number)) <= 0)
        stop("invalid `filter.number'")
    if (family == "DaubExPhase") {

    # The following wavelet coefficients are taken from 
    #   Daubechies, I (1988) 
    #   Orthonormal Bases of Wavelets
    #   Communications on Pure and Applied Mathematics. Page 980
    # or 
    #   Ten Lectures on Wavelets, Daubechies, I, 1992
    #   CBMS-NSF Regional Conference Series, page 195, Table 6.1
    #   Comment from that table reads:
    #   "The filter coefficients for the compactly supported wavelets
    #   with extremal phase and highest number of vanishing moments
    #   compatible with their support width".
    filter.name = 
        switch(filter.number,
            { 
            # filter.number  -- 1 --
            # This is for the Haar basis. (not in Daubechies).
            H = rep(1/sqrt(2), 2)
            "Haar wavelet"
            },
            { # filter.number  -- 2 --
            H = c(0.48296291314500001,   0.83651630373800001, 
                  0.22414386804200001, 
                 -0.12940952255099999)
            "Daub cmpct on ext. phase N=2"
            },
            { # filter.number  -- 3 --
            H = c(0.33267055294999998,   0.80689150931099995,  
                  0.45987750211799999,  -0.13501102001000001, 
                 -0.085441273882,        0.035226291882000001) 
           "Daub cmpct on ext. phase N=3"
           },
           { # filter.number  -- 4 --
           H = c(0.23037781330900001,    0.71484657055300005,  
                 0.63088076793000003,   -0.027983769417,        
                -0.18703481171899999,    0.030841381836,   
                 0.032883011667000001,  -0.010597401785) 
           "Daub cmpct on ext. phase N=4"
           },
           { # filter.number  -- 5 --
           H = c(0.160102397974,         0.60382926979700002,  
                 0.72430852843799998,    0.138428145901,      
                -0.242294887066,        -0.032244869585000002,
                 0.07757149384,         -0.006241490213,      
                -0.012580751999,         0.003335725285)
           "Daub cmpct on ext. phase N=5"
           },
           { # filter.number  -- 6 --
           H = c(0.11154074335,          0.49462389039799998,  
                 0.751133908021,         0.31525035170900001,   
                -0.22626469396500001,   -0.12976686756700001, 
                 0.097501605586999995,   0.02752286553,       
                -0.031582039318000001,   0.000553842201,         
                 0.004777257511,        -0.001077301085)  
           "Daub cmpct on ext. phase N=6"
           },
           {  # filter.number -- 7 --
           H = c(0.077852054084999997,   0.396539319482,       
                 0.72913209084599995,    0.469782287405,        
                -0.14390600392899999,   -0.22403618499399999, 
                 0.071309219267,         0.080612609151000006,
                -0.038029936935000001,  -0.016574541631,         
                 0.012550998556,         0.000429577973,   
                -0.001801640704,         0.0003537138)     
           "Daub cmpct on ext. phase N=7"
           },
           { # filter.number  -- 8 --
           H = c(0.054415842243000001,   0.31287159091400002,  
                 0.67563073629699999,    0.58535468365400001,   
                -0.015829105256,        -0.28401554296199999, 
                 0.000472484574,         0.12874742662,         
                -0.017369301002000001,  -0.044088253931,         
                 0.013981027917,         0.008746094047,  
                -0.004870352993,        -0.000391740373,         
                 0.000675449406,        -0.000117476784) 
           "Daub cmpct on ext. phase N=8"
           },
           { # filter.number  -- 9 --
           H = c(0.038077947363999998,   0.24383467461300001, 
                 0.60482312369000002,    0.65728807805099998,    
                 0.13319738582499999,   -0.293273783279,  
                -0.096840783222999993,   0.14854074933799999,  
                 0.030725681479000001,  -0.067632829061000002,   
                 0.000250947115,         0.022361662124,   
                -0.004723204758,        -0.004281503682,       
                 0.001847646883,         0.000230385764,         
                -0.000251963189,         3.934732e-05)     
           "Daub cmpct on ext. phase N=9"
           },
           { # filter.number -- 10 --
           H = c(0.026670057901000001,   0.188176800078,        
                 0.52720118893199996,    0.688459039454,         
                 0.28117234366100002,   -0.24984642432699999,
                -0.19594627437699999,    0.127369340336,         
                 0.093057364604000006,  -0.071394147165999997,  
                -0.029457536822,         0.033212674058999997, 
                 0.003606553567,        -0.010733175483,        
                 0.001395351747,         0.001992405295,        
                -0.000685856695,        -0.000116466855,  
                 9.358867e-05,          -1.3264203e-05)   
           "Daub cmpct on ext. phase N=10"
           }
           ) #  switch ( filter.number )
    if (is.null(filter.name))
        stop(paste("Unknown filter number (not in {1:10}) for", family,
               "(Daubechies wavelets with extremal phase...)"))

    } else { 
        # if(family == "DaubLeAsymm")
        # The following wavelet coefficients are taken from
        # Ten Lectures on Wavelets, Daubechies, I, 1992
        # CBMS-NSF Regional Conference Series, page 198, Table 6.3
        # Comment from that table reads:
        #   "The low pass filter coefficients for the "least-asymmetric"
        #   compactly supported wavelets with maximum number of
        #   vanishing moments, for N = 4 to 10
        filter.name = 
            switch(filter.number,
               NULL, NULL, NULL, 
               { # filter.number  -- 4 --
               H = c(-0.107148901418, -0.041910965125,   0.703739068656,   
                     1.136658243408,   0.421234534204,  -0.140317624179,  
                    -0.017824701442,   0.045570345896)   
               "Daub cmpct on least asymm N=4"
               },
               { # filter.number  -- 5 --
               H = c(0.038654795955,   0.041746864422,  -0.055344186117,  
                     0.281990696854,   1.023052966894,   0.89658164838,    
                     0.023478923136,  -0.247951362613,  -0.029842499869,  
                     0.027632152958)   
               "Daub cmpct on least asymm N=5"
               },
               { # filter.number  -- 6 --
               H = c(0.021784700327,   0.004936612372,  -0.166863215412,  
                    -0.068323121587,   0.694457972958,   1.113892783926,   
                     0.477904371333,  -0.102724969862,  -0.029783751299,  
                     0.06325056266,    0.002499922093,  -0.011031867509)  
               "Daub cmpct on least asymm N=6"
               },
               { # filter.number  -- 7 --
               H = c(0.003792658534,  -0.001481225915,  -0.017870431651,  
                     0.043155452582,   0.096014767936,  -0.070078291222,  
                     0.024665659489,   0.758162601964,   1.085782709814,   
                     0.408183939725,  -0.198056706807,  -0.152463871896,  
                     0.005671342686,   0.014521394762)   
               "Daub cmpct on least asymm N=7"
               },
               { # filter.number  -- 8 --
               H = c(0.002672793393,  -0.0004283943,    -0.021145686528,   
                     0.005386388754,   0.069490465911,  -0.038493521263,   
                    -0.073462508761,   0.515398670374,   1.099106630537,    
                     0.68074534719,   -0.086653615406,  -0.202648655286,  
                     0.010758611751,   0.044823623042,  -0.000766690896,   
                    -0.004783458512)   
               "Daub cmpct on least asymm N=8"
               },
               { # filter.number  -- 9 --
               H = c(0.001512487309,  -0.000669141509,  -0.014515578553, 
                     0.012528896242,   0.087791251554,  -0.02578644593,  
                    -0.270893783503,   0.049882830959,   0.873048407349,  
                     1.015259790832,   0.337658923602,  -0.077172161097,  
                     0.000825140929,   0.042744433602,  -0.016303351226,  
                    -0.018769396836,   0.000876502539,   0.001981193736)   
               "Daub cmpct on least asymm N=9"
               },
               { # filter.number  -- 10 --
               H = c(0.001089170447,   0.00013524502,   -0.01222064263,   
                    -0.002072363923,   0.064950924579,   0.016418869426,  
                    -0.225558972234,  -0.100240215031,   0.667071338154,  
                     1.0882515305,     0.542813011213,  -0.050256540092,  
                    -0.045240772218,   0.07070356755,    0.008152816799,   
                    -0.028786231926,  -0.001137535314,   0.006495728375,   
                     8.0661204e-05,   -0.000649589896)  
               "Daub cmpct on least asymm N=10"
               }
           ) # switch ( filter.number )

    if(is.null(filter.name))
        stop(paste("Unknown filter number (not in {4:10}) for", family,
            "\n (Daubechies wavelets with least asymmetry...)"))
    H = H/sqrt(2)
    } # ""DaubLeAsymm" family

    H = H/constant
    
    # Return Value:
    list(H = H, name = filter.name, family = family,
        filter.number = filter.number)
}


# ------------------------------------------------------------------------------


.first.last = 
function(LengthH, DataLength, bc = c("periodic", "symmetric"))
{   # A function copied from R-package wavethresh

    # FUNCTION:
    
    # Settings:
    bc = match.arg(bc)
    levels = log(DataLength)/log(2)
    first.last.c = matrix(0, nrow = levels + 1, ncol = 3,
        dimnames = list(NULL, c("First", "Last", "Offset")))
    first.last.d = matrix(0, nrow = levels, ncol = 3,
        dimnames = list(NULL, c("First", "Last", "Offset")))
    if (bc == "periodic") {
        # Periodic boundary correction
        first.last.c[, 1] = rep(0, levels + 1)
        first.last.c[, 2] = 2^(0:levels) - 1
        first.last.c[, 3] = rev(c(0, cumsum(rev(1 + first.last.c[, 2])
                             )[1:levels]))
        first.last.d[, 1] = rep(0, levels)
        first.last.d[, 2] = 2^(0:(levels - 1)) - 1
        first.last.d[, 3] = rev(c(0, cumsum(rev(1 + first.last.d[, 2])
            )[1:(levels - 1)]))
        ntotal = 2 * DataLength - 1
        ntotal.d = DataLength - 1
    } else { 
        # (bc == "symmetric")
        # Symmetric boundary reflection
        first.last.c[levels + 1, 1] = 0
        first.last.c[levels + 1, 2] = DataLength - 1
        first.last.c[levels + 1, 3] = 0
        ntotal = first.last.c[levels + 1, 2] - first.last.c[levels + 1, 1] + 1
        ntotal.d = 0
        for (i in levels:1) {
            first.last.c[i, 1] = trunc(0.5 * (1 - LengthH +
                first.last.c[i + 1, 1]))
            first.last.c[i, 2] = trunc(0.5 * first.last.c[i + 1, 2])
            first.last.c[i, 3] = first.last.c[i + 1, 3] +
            first.last.c[i + 1, 2] - first.last.c[i + 1, 1] + 1
            first.last.d[i, 1] = trunc(0.5 * (first.last.c[i + 1, 1] - 1))
            first.last.d[i, 2] = trunc(0.5 * (first.last.c[i + 1, 2] + 
                LengthH - 2))
            if(i != levels) {
                first.last.d[i, 3] = first.last.d[i + 1, 3] +
                    first.last.d[i + 1, 2] - first.last.d[i + 1, 1] + 1
            }
            ntotal = ntotal + first.last.c[i, 2] - first.last.c[i, 1] + 1
            ntotal.d = ntotal.d + first.last.d[i, 2] - first.last.d[i, 1] + 1
        }
    }
    names(ntotal) = NULL
    names(ntotal.d) = NULL
    
    # Return Value:
    list(first.last.c = first.last.c, ntotal = ntotal,
        first.last.d = first.last.d, ntotal.d = ntotal.d)
}


################################################################################
# Statistical Tests:


.beranTest =
function()
{   # A function implemented by Diethelm Wuertz

    # Result:
    ans = NA
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.rsTest = 
function(x, q) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the statistic of the modified R/S test 
 
    # Arguments:
    #   x - time series 
    #   q - number of lags included for calculation of covariances 

    # Details:
    #   significance level: 0.050,  0.10 
    #   critical value:     1.747,  1.62 
    
    # Notes:
    #   This functions uses partly code from Christoph Helwig
    #   presented in the R-help list, 2004.

    # References: 
    #   Lo (1991), Long-term Memory in Stock Market Prices, 
    #   Econometrica 59, 1279--1313  

    xbar = mean(x) 
    N = length(x) 
    r = max(cumsum(x-xbar)) - min(cumsum(x-xbar)) 
    
    covar = NULL 
    for (i in 1:q) { 
        covar = c(covar, sum((x[1:(N-i)]-xbar)*(x[(1+i):N]-xbar))) 
    } 
    
    if (q > 0) {
        s = sum((x-xbar)^2)/N + sum((1-(1:q)/(q+1))*covar)*2/N 
    } else {
        s = sum((x-xbar)^2)/N 
    }
    
    rs = r/(sqrt(s)*sqrt(N)) 
    method = "R/S Test for Long Memory" 
    names(rs) = "R/S Statistic" 
    names(q) = "Bandwidth q" 
    
    # Result:
    ans = structure(list(statistic = rs, parameter = q, method = method, 
        data.name = deparse(substitute(x))), class = "htest") 
        
    # Return Value:
    ans
} 



# ------------------------------------------------------------------------------
 

.vsTest = 
function(x, q) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Calculates the statistic of the modified V/S test 
    
    # Arguments:
    #   x - time series 
    #   q - number of lags included for calculation of covariances 
     
    # Details:
    #   significance level: 0.01,   0.05,   0.10 
    #   critical value:     0.2685, 0.1869, 0.1518 
    
    # Notes:
    #   This functions uses partly code from Christoph Helwig
    #   presented in the R-help list, 2004.
    
    # References: 
    #   Giraitis, Kokoszka und Leipus (2000), Rescaled variance 
    #   and related tests for long memory in volatility and levels 

    xbar = mean(x) 
    N = length(x) 
    v = sum((cumsum(x-xbar))^2) - (sum(cumsum(x-xbar)))^2/N 
    covar = NULL 
    
    for (i in 1:q) { 
        covar = c(covar, 
        sum((x[1:(N-i)]-xbar)*(x[(1+i):N]-xbar))) 
    } 
    
    if (q > 0) {
        s = sum((x-xbar)^2)/N + sum((1-(1:q)/(q+1))*covar)*2/N 
    } else {
        s = sum((x-xbar)^2)/N 
    }
    
    vs = v/(s*N^2) 
    method = "V/S Test for Long Memory" 
    names(vs) = "V/S Statistic" 
    names(q) = "Bandwidth q" 
    
    # Result:
    ans = structure(list(statistic = vs, parameter = q, method = method, 
        data.name = deparse(substitute(x))), class = "htest")
        
    # Return Value:
    ans 
}


################################################################################
# Slider:


hurstSlider =
function(x = fgnSim())
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Displays interactively Hurst exponent estimates
    
    # Arguments:
    #   x - a numerical vector or any other object which can
    #       transformed into a numeric vector.
    
    # FUNCTION:
    
    # Transform and Save Series:
    .xHurst <<- as.vector(x)   
    
    # Graphic Frame:
    par(mfrow = c(1, 1), cex = 0.7)
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        method = .sliderMenu(no = 1)
        levels = .sliderMenu(no = 2)
        minnpts = .sliderMenu(no = 3)
        lower  = .sliderMenu(no = 4)
        range = .sliderMenu(no = 5)
        
        # Plot:
        description = paste("Method", method, as.character(date()))
        if (method == 1) ans = aggvarFit(x = .xHurst, levels = levels, 
            minnpts = minnpts, cut.off = 10^c(lower, lower+range), 
            doplot = TRUE, description = description)
            
        if (method == 2) ans = diffvarFit(x = .xHurst, levels = levels, 
            minnpts = minnpts, cut.off = 10^c(lower, lower+range), 
            doplot = TRUE, description = description)
            
        if (method == 3) ans = absvalFit(x = .xHurst, levels = levels, 
            minnpts = minnpts, cut.off = 10^c(lower, lower+range), 
            doplot = TRUE, description = description)
            
        if (method == 4) ans = higuchiFit(x = .xHurst, levels = levels, 
            minnpts = minnpts, cut.off = 10^c(lower, lower+range), 
            doplot = TRUE, description = description)
            
        if (method == 5) ans = pengFit(x = .xHurst, levels = levels, 
            minnpts = minnpts, cut.off = 10^c(lower, lower+range), 
            method = "mean", doplot = TRUE, description = description)
            
        if (method == 6) ans = rsFit(x = .xHurst, levels = levels, 
            minnpts = minnpts, cut.off = 10^c(lower, lower+range), 
            doplot = TRUE, description = description)
            
        if (method >= 7) ans = perFit(x = .xHurst, cut.off = lower, 
            doplot = TRUE, description = description)
        
        # Add Legend:
        show(ans)
        if (method == 7) {
            mtext(text = paste(
                "cut.off = ", lower,
                sep = ""), line = -1.5, side = 3, cex = 0.8)
        } else {
            mtext(text = paste(
                "levels = ", levels, " | ",
                "minnpts = ", minnpts, " | ",
                "cut.off = 10^[", lower, ", ", lower+range, "]",
                sep = ""), line = -1.5, side = 3, cex = 0.8)
        }
        what = paste("Method:",
          "1:aggvar | 2:diffvar | 3:absval | 4:higuchi | 5:peng | 6:rs | 7:per")
        mtext(what, side = 4, cex = 0.55, line = 0.9, adj = 0, 
            col = "steelblue")
                   
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
        names       = c("method", "levels", "minnpts", "lower", "range"),
        minima      = c(       1,       10,         1,     0.1,     0.1),
        maxima      = c(       7,      200,        10,     1.5,     3.0), 
        resolutions = c(       1,        5,         1,     0.1,     0.1),
        starts      = c(       1,       50,         3,     0.7,     1.8)) 
}


################################################################################

