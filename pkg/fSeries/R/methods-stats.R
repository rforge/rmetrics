# experimental

members <- c("dnorm", "dcauchy", "dt")

# template definition
template <-  c("{",
               "finCenter <- XXX@FinCenter",
               "XXX <- as.matrix(XXX)",
               "ans <- callGeneric()",
               "if (is.matrix(ans))",
               "    ans <- as.timeSeries(ans, finCenter, finCenter)",
               "ans",
               "}")

# set the Methods in a loop
for (f in members) {
    def <- function()NULL
    formals(def) <- formals(f)
    bodyText <- gsub("XXX", names(formals(f))[1], template)
    body(def) <- parse(text = bodyText)
    setMethod(f, "timeSeries", def)
}
