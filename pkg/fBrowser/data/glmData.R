

# A function implemented by Diethelm Wuertz
# GLM / BINOMIAL / LOGIT - Example Data:
# Example Data: x = 10*sin(x1) + exp(x2) + eps
# family = binomial(link = logit)
# glm(formula = x ~ x1 + x2, family = family, data = x)
set.seed(4711)
x1 = rnorm(1000)
x2 = rnorm(1000)
eps = 0.1 * rnorm(1000)
y = 0.7 * x1 + 0.3 * x2 + eps
y = 1 / ( 1 + exp(-y) )
glmData = data.frame(x = y, x1 = x1, x2 = x2)
rm("x1", "x2", "eps", "y")

