library(randtoolbox)

param <- rbind(
	c(512, "a"),
	c(521, "a"),
	c(521, "b"),
	c(607, "a"),
	c(607, "b"),
	c(800, "a"),
	c(800, "b"),
	c(1024, "a"),
	c(1024, "b"),
	c(19937, "a"),
	c(19937, "b"),
	c(19937, "c"),
	c(21701, "a"),
	c(23209, "a"),
	c(23209, "b"),
	c(44497, "a"),
	c(44497, "b"))

dimnames(param) <- list(NULL, c("order", "version"))

result1 <- rep(NA, times=nrow(param))
result2 <- rep(NA, times=nrow(param))

seed <- floor(2^31*runif(1))
cat("using seed", seed, "for test of the output of WELL RNG\n")

m <- 100
cat("generating sequences of the length", m, "from each generator\n")

for (i in 1:nrow(param))
{
	cat(i, "")
	order <- param[i, 1]
	version <- param[i, 2]
	generator <- paste(order, version, sep="")
	set.generator("WELL", order=order, version=version, seed=seed)
	s0 <- getWELLState()
	x <- runif(m)
	out <- rngWELLScriptR(m, s0, generator, includeState=TRUE)
	s1 <- getWELLState()
	result1[i] <- all(x == out$x)
	result2[i] <- all(s1 == out$state)
}

cat("\n\n")
print(cbind(data.frame(param), result1=result1, result2=result2))

stopifnot(all(result1, result2))

