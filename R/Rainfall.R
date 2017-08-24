#' Rainfall series
#' @title Rainfall series
#' @description This function simulates rainfall series as a stochastic variable, by using marked Poisson process.
#' @param ndays Number of days
#' @param lambda The frequency of rainfall events (day^-1)
#' @param alpha The mean of rainfall event (cm day^-1)
#' @return rainfall series
#' @examples RainPoisson(ndays = 60, lambda = 0.1, alpha = 0.95)
#' @export RainPoisson
#' @return


RainPoisson = function(ndays, lambda, alpha){

    rain = rep(NA, ndays)
    for(i in 1:ndays){
        n1 = runif(1, min = 0, max = 1)
        if(n1 < lambda){
            n2 = runif(1, min = 0, max = 1)
            rain[i] = (1/alpha)*log(1/(1-n2))
        }
        else{rain[i] = 0}
    }

    return(rain)
}
