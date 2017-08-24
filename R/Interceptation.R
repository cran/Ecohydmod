#' Interceptation
#' @title Canopy interceptation
#' @description This function calculates the amount of rain intercepted in the canopy.
#' @param R Rainfall
#' @param Rstar The maximum amount which the canopy intercepts
#' @return canopy interceptation
#' @examples CInt_f(R = 10, Rstar = 3)
#' @export CInt_f
#' @return


CInt_f = function(R, Rstar){
    if(R <= Rstar){itcp = R}
    else{itcp = Rstar}
    return(itcp)
}

