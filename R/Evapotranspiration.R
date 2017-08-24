#' Evapotranspiration function based on the soil moisture
#' @title Evapotranspiration
#' @description This function calculates the evapotranspiration based on the soil moisture, soil water retantion curve and vegetation properties.
#' @param s Soil moisture
#' @param Emax Maximum evapotranspiration rate
#' @param Ew Minimum evapotranspiration rate
#' @param sh Soil moisture at hidroscopic point
#' @param sw Soil moisture at wilting point
#' @param sstar Soil moisture below field capacity point
#' @return evapotranspiration
#' @examples Et_f(s = 0.25, Emax = 5, Ew = 0.5, sh = 0.01, sw = 0.15, sstar = 0.40)
#' @export Et_f
#' @return


Et_f = function(s, Emax, Ew, sh, sw, sstar){
    if(s > sh & s <= sw){et = Ew * ((s - sh)/(sw - sh))}
    else if(s > sw & s <= sstar){et = Ew + (Emax - Ew) * ((s - sw)/(sstar - sw))}
    else if(s > sstar & s <= 1.0){et = Emax}
    else{et = NA}
    return(et)
}

