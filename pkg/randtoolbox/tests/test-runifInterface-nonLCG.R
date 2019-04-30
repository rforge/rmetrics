library(randtoolbox)
RNGkind()


#set WELL19937a
set.generator("WELL", version="19937a", seed=12345)
runif(5)


storedState <- get.description()
x <- runif(10)

y <- runif(100)

#Restore the generator from storedState and regenerate the same numbers
put.description(storedState)
all(x == runif(10))


# generate the same random numbers as in Matlab
set.generator("MersenneTwister", initialization="init2002", resolution=53, seed=12345)
runif(5)
# [1] 0.9296161 0.3163756 0.1839188 0.2045603 0.5677250
# Matlab commands rand('twister', 12345); rand(1, 5) generate the same numbers,
# which in short format are   0.9296    0.3164    0.1839    0.2046    0.5677


storedState <- get.description()
