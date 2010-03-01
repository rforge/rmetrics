millsR <- function(y, log = FALSE)
{
    if (log) {
        millsR <- pnorm(y, lower.tail = FALSE, log.p = TRUE) -
                  dnorm(y, log = TRUE)
    } else {
        millsR <- (1 - pnorm(y))/dnorm(y)
    }
    return(millsR)
}
