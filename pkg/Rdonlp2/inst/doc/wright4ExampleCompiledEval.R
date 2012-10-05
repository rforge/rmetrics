require(Rdonlp2)

start = c(1, 1, 1, 1, 1)
eqFun.bound = c(2+3*sqrt(2), -2+2*sqrt(2), 2)

#------------------------------
# Normal package usage
#------------------------------

fun <- function(x){
  (x[1]-1)^2+(x[1]-x[2])^2+(x[2]-x[3])^3+(x[3]-x[4])^4+(x[4]-x[5])^4}

eqFun = list(
    function(x) x[1]+x[2]*x[2]+x[3]*x[3]*x[3],
    function(x) x[2]-x[3]*x[3]+x[4],
    function(x) x[1]*x[5] ) 

ans.donlp <- donlp2(start, fun, par.lower = rep(-Inf, length(start)), 
    par.upper = rep(+Inf, length(start)), nlin=eqFun, nlin.lower=eqFun.bound, nlin.upper=eqFun.bound)

ans.donlp

fun(ans.donlp$par)

#--------------------------------------------------------------------------
# Usage with an external pointer. For convenience, inline and Rcpp are used
#--------------------------------------------------------------------------

require(inline)


inc <- 'SEXP wright4(SEXP xs) {
    Rcpp::NumericVector x(xs);
    
    int mode = x[0];
    int fun_id = x[1];
    
    if(mode == 0) { 
    if(fun_id == 0) {
    return Rcpp::wrap(pow((x[2]-1),2) + pow((x[2]-x[3]),2) + pow(x[3]-x[4],3) + pow(x[4] - x[5], 4) + pow(x[5] - x[6], 4));
    } else if(fun_id==1) {
    return Rcpp::wrap(x[2]+x[3]*x[3]+x[4]*x[4]*x[4]);
    } else if(fun_id==2) {
    return Rcpp::wrap(x[3]-x[4]*x[4]+x[5]);
    } else if(fun_id==3) {
    return Rcpp::wrap(x[2]*x[6]);
    }
    } else if(mode==1) {
    // gradients are not implemented
    };
    
    return R_NilValue; 
    
    }
    
    '

src.xptr <- '
    typedef SEXP (*funcPtr)(SEXP);
    return(XPtr<funcPtr>(new funcPtr(&wright4)));
    '
create_xptr <- cxxfunction(signature(), body=src.xptr, inc=inc, plugin="Rcpp")

ans.donlpC <- donlp2(start, create_xptr(), par.lower = rep(-Inf, length(start)), 
    par.upper = rep(+Inf, length(start)), nlin.lower=eqFun.bound, nlin.upper=eqFun.bound)

ans.donlpC

fun(ans.donlpC$par)
