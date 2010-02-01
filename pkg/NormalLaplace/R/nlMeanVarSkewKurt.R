###### mean function ########################################################
nlMean <- function (mu = 0, sigma = 1, alpha = 1, beta = 1,
                  param = c(mu,sigma,alpha,beta),log=FALSE) 
{
    mu  <-  param[1]
    sigma  <-  param[2]
    alpha  <-  param[3]
    beta  <-  param[4]

    nlMean <- mu + 1/alpha-1/beta

    return(nlMean)

}


###### variance function ########################################################
nlVar <- function (mu = 0, sigma = 1, alpha = 1, beta = 1,
                  param = c(mu,sigma,alpha,beta),log=FALSE) 
{
    mu  <-  param[1]
    sigma  <-  param[2]
    alpha  <-  param[3]
    beta  <-  param[4]
    
    nlVar <- sigma^2+1/(alpha^2)+1/(beta^2)
    return(nlVar)
}


####### skewness function ##################################################
nlSkew <- function (mu = 0, sigma = 1, alpha = 1, beta = 1,
                  param = c(mu,sigma,alpha,beta),log=FALSE) 
{
    mu  <-  param[1]
    sigma  <-  param[2]
    alpha  <-  param[3]
    beta  <-  param[4]

    k2<-nlVar(param)
    k3<-2/(alpha^3)-2/(beta^3)

    nlSkew<-k3/(k2^(3/2))

    return(nlSkew)

}


###### kurtosis function ####################################################

nlKurt <- function (mu = 0, sigma = 1, alpha = 1, beta = 1,
                  param = c(mu,sigma,alpha,beta),log=FALSE) 
{
    mu  <-  param[1]
    sigma  <-  param[2]
    alpha  <-  param[3]
    beta  <-  param[4]
    
    k2<-nlVar(param)
    k4<-6/(alpha^4)+6/(beta^4)
    nlKurt<-k4/k2^2

    return(nlKurt)

}
