

.gamData = 
function() 
{   # A function implemented by Diethelm Wuertz

    # GAM - Example Data:
    # Example Data: x = 0.7*sin(x1) + 0.3*exp(x2) + eps
    # fit = gam(formula = x ~ s(x1) + s(x2), data = x)
    set.seed(4711)
    x1 = rnorm(1000)
    x2 = rnorm(1000)
    y = sin(x1) + exp(x2) 
    eps = 0.1 * rnorm(1000)
    y = y + eps
    data.frame(cbind(x = y, x1 = x1, x2 = x2))
}

gamData = .gamData()

