#' Leakage function based on the soil moisture
#' @title Leakage
#' @description This function calculates the leakage based on the soil moisture, soil water retantion curve and the soil hydraulic conductivity.
#' @param s Soil moisture
#' @param Ks Soil saturated hydraulic conductivity
#' @param b The exponent of the water retention curve
#' @return leakage
#' @examples Lk_f(s = 0.25, Ks = 2000, b = 4.38)
#' @export Lk_f
#' @return


Lk_f = function(s, Ks, b){
    lk = Ks * s^(2*b + 3)
    return(lk)
}

