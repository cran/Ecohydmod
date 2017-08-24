#' NDVI simulation
#' @title NDVI simulation
#' @description This function simulates the NDVI based on soil moisture and vegetation parameters. Numerical solution.
#' @param s A vector with soil moisture
#' @param sw Soil moisture at wilting point
#' @param sstar Soil moisture below field capacity point
#' @param kA Constant of assimilation
#' @param kR Constant of respiration
#' @param Nmax Maximum NDVI of the vegetation
#' @param Nmin Minimum NDVI of the vegetation
#' @param N0 Initial condiction of NDVI. If it is missing, the average of Nmax and Nmin will be used
#' @return NDVI series
#' @examples rain = 10 * RainPoisson(ndays = 365, lambda = 0.05, alpha = 0.60)
#' s = swb_f(R = rain, Rstar = 3, Emax = 5, Ew = 0.5, Ks = 2000, b = 4.38, Zr = 400,
#' n = 0.5, sh = 0.01, sw = 0.10, sstar = 0.25, s0 = 0.10, nsteps = 48, gr = T)[,3]
#' NDVI = SimNDVI(s, sw = 0.10, sstar = 0.35, kA = 0.064, kR = 0.011,
#' Nmax = 0.93, Nmin = 0.26, N0 = 0.5)
#' @export SimNDVI
#' @return


SimNDVI = function(s, sw, sstar, kA, kR, Nmax, Nmin, N0){

    # Optional arguments
    if(missing(N0)){N0 = (Nmax+Nmin)/2; warning('N0 is missing, it was signed as (Nmax+Nmin)/2')}
    else if(N0<=Nmin){N0 = (Nmax+Nmin)/2; warning('N0 is very low, it was signed as (Nmax+Nmin)/2')}
    else{N0 = N0}

    reg = lm(c(0,1)~c(sw, sstar))
    cfs = coefficients(reg)
    a = as.numeric(cfs[2])
    b = as.numeric(cfs[1])

    A = function(s, sw, sstar){
        if(s <= sw){asm = 0}
        else if(s > sstar){asm = 1}
        else{asm = a*s+b}
        return(asm)
    }

    nr = length(s)
    ndvi = rep(NA, nr)

    for(i in 1:nr){
        N1 = (kA*A(s[i], sw, sstar)*(Nmax - N0)) - (kR*(N0 - Nmin))
        ndvi[i] = N0 + N1
        N0 = ndvi[i]
    }
    return(ndvi)
}

