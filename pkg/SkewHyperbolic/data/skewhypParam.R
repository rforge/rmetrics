### small shape
skewhypSmallShape <-
    expand.grid(mu = 0, delta = 1, beta = c(0,2), nu = c(1,5))

### large shape
skewhypLargeShape <-
    expand.grid(mu = 0, delta = 1, beta = c(-5,0,1,2,5),
                nu = c(1,2,5,10,20))

### small param
skewhypSmallParam <-
    expand.grid(mu = c(0,2), delta = c(1,5), beta = c(0,2), nu = c(1,5))

### large param
skewhypLargeParam <-
    expand.grid(mu = c(-2,0,1,2), delta = c(0.5,1,2,5),
                beta = c(-5,0,1,2,5), nu = c(1,2,5,10,20))


