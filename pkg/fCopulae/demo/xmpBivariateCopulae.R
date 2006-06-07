
#   *** This list is not yet complete ***


# ------------------------------------------------------------------------------

.unitTest.mCopula = 
function()
{
    n = 100
    uv = .gridCoord(x = (0:n)/n)
    z = round(matrix(.pfrechetCopula(uv$x, uv$y, "m"), n+1), 3)
    par(mfrow = c(2, 2)) 
    contour(z=z, main = "M Copula")
    persp(z=z, theta = -40, phi =30)
    
    n = 10
    uv = .gridCoord(x = (0:n)/n)
    z = round(matrix(.pfrechetCopula(uv$x, uv$y, "m"), n+1), 3)
    z
}


# ------------------------------------------------------------------------------


.unitTest.piCopula = 
function()
{
    n = 100
    uv = .gridCoord(x = (0:n)/n)
    z = round(matrix(.pfrechetCopula(uv$x, uv$y, "pi"), n+1), 3)
    par(mfrow = c(2, 2)) 
    contour(z=z, main = "Pi Copula")
    persp(z=z, theta = -40, phi =30)
    
    n = 10
    uv = .gridCoord(x = (0:n)/n)
    z = round(matrix(.pfrechetCopula(uv$x, uv$y, "pi"), n+1), 3)
    z
}


# ------------------------------------------------------------------------------


.unitTest.wCopula = 
function()
{
    n = 100
    uv = .gridCoord(x = (0:n)/n)
    z = round(matrix(.pfrechetCopula(uv$x, uv$y, "w"), n+1), 3)
    par(mfrow = c(2, 2)) 
    contour(z=z, main = "W Copula")
    persp(z=z, theta = -40, phi =30)
    
    n = 10
    uv = .gridCoord(x = (0:n)/n)
    z = round(matrix(.pfrechetCopula(uv$x, uv$y, "w"), n+1), 3)
    z
}


# ------------------------------------------------------------------------------





# ------------------------------------------------------------------------------


.unitTest.A =
function()
{
    # type = c("gumbel", "galambos", "husler.reis", "tawn", "bb5") )    
    
    x = (0:100)/100
        
    plot(x = c(0,1), y = c(0,1), type = "n")
    for (param in round(exp((0:10)/5), 1)) {
        A = .A(x, param = param, type = "gumbel")
        lines(x, A, type = "l")
    }
    
    plot(x = c(0,1), y = c(-1,1), type = "n")
    for (param in round(exp((0:10)/5), 1)) {
        A = .A(x, param = param, type = "gumbelII")
        lines(x, A, type = "l")
    }

    plot(x = c(0,1), y = c(-1,1), type = "n")
    for (param in round(exp((0:10)/5), 1)) {
        A = .A(x, param = param, type = "galambos")
        lines(x, A, type = "l")
    }
    
}
