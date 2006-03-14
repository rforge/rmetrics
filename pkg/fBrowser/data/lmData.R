

.lmData = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # LM - Example Data:
    # Example Data: x = 0.7*x1 + 0.3*x2 + eps
    set.seed(4711)
    x1 = rnorm(1000)
    x2 = rnorm(1000)
    y = 0.7 * x1 + 0.3 * x2
    eps = 0.1 * rnorm(1000)
    y = y + eps
    data.frame(x = y, x1 = x1, x2 = x2) 
}

lmData = .lmData()

