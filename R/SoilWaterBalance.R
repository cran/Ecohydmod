#' Soil water balance
#' @title Soil water balance
#' @description This function calculates the daily soil water balance and its components based on the rainfall, soil properties and vegetation properties.
#' @param R Daily rainfall, which should be a vector.
#' @param Rstar The maximum amount which the canopy intercepts
#' @param Emax Maximum evapotranspiration rate
#' @param Ew Minimum evapotranspiration rate
#' @param Ks Soil saturated hydraulic conductivity
#' @param b The exponent of the water retention curve
#' @param Zr Root depth
#' @param n The soil porosity
#' @param sh Soil moisture at hidroscopic point
#' @param sw Soil moisture at wilting point
#' @param sstar Soil moisture below field capacity point
#' @param nsteps Number of steps/division for the numerical solution
#' @param s0 Initial soil moisture to start the simulation. If it is missing, s0 is signed equal to sh.
#' @param gr Logical argument to show graphics of results. Default is FALSE
#' @return soil water balance components
#' @examples rain = 10 * RainPoisson(ndays = 365, lambda = 0.05, alpha = 0.60)
#' swb_f(R = rain, Rstar = 3, Emax = 5, Ew = 0.5, Ks = 2000, b = 4.38, Zr = 400,
#' n = 0.5, sh = 0.01, sw = 0.10, sstar = 0.25, s0 = 0.10, nsteps = 48)
#' @export swb_f
#' @return


swb_f = function(R, Rstar, Emax, Ew, Ks, b, Zr, n, sh, sw, sstar, nsteps, s0, gr){

    # Optional arguments
    if(missing(s0)){s0 = (sh+sw)/2; warning('s0 is missing, it was signed as (sh+sw)/2')}
    else if(s0<=sh){s0 = (sh+sw)/2; warning('s0 is very low, it was signed as (sh+sw)/2')}
    else{s0 = s0}

    if(missing(gr)){gr = FALSE}
    else{gr = TRUE}

    nr = length(R)
    dtp = nsteps
    dt = 1/nsteps

    # Days
    days = rep(1:nr, each = dtp)

    # Rainfall
    mrain = matrix(data = R, ncol = nr)
    mrain0 = matrix(data = rep(0, (dtp-1)*nr), ncol = nr)
    mrain = rbind(mrain, mrain0)
    rain = as.numeric(mrain)

    # Numeric solution
    CIntRes = rep(NA, length(days))
    ETRes = rep(NA, length(days))
    LkRes = rep(NA, length(days))
    sRes = rep(NA, length(days))
    QRes = rep(NA, length(days))

    for(i in 1:length(days)){
        CIntRes[i] = CInt_f(R = rain[i], Rstar)
        s1 = s0 + (rain[i] - CIntRes[i])/(n*Zr)
        if(s1 > 1.0){QRes[i] = (s1 - 1.0)*n*Zr; s2 = 1.0}
        else{QRes[i]=0; s2 = s1}

        ETRes[i] = Et_f(s2, Emax, Ew, sh, sw, sstar)*dt
        LkRes[i] = Lk_f(s2, Ks, b)*dt
        s0 = s2 - (ETRes[i] + LkRes[i])/(n*Zr)
        sRes[i] = s0
        print(paste('Running ', round((100*(i/length(days))), digits = 2), '%', sep=''))
    }

    sday = tapply(sRes, days, mean)
    Etday = tapply(ETRes, days, sum)
    Lkday = tapply(LkRes, days, sum)
    Qday = tapply(QRes, days, sum)

    mres = matrix(NA, nrow = nr, ncol = 6)
    mres[,1] = 1:nr; mres[,2] = R; mres[,3] = sday; mres[,4] = Etday; mres[,5] = Lkday; mres[,6] = Qday
    colnames(mres) = c('Days', 'Rainfall', 'soil_moisture', 'ET', 'Lk', 'Runoff')

    # Graphical results
    if(gr==FALSE){return(mres)}
    else{
        par(mfrow = c(4,1), mar = c(5,5,1,1), las = 1)
        plot(ifelse(mres[,2]>0, mres[,2], NA), type='h', col = 'blue', lwd=5, ylab = 'Rain')
        plot(mres[,3], type='l', col = 'blue', lwd=2, ylab = 's')
        plot(mres[,4], type='l', col = 'blue', lwd=2, ylab = 'ET')
        plot(mres[,5], type='l', col = 'blue', lwd=2, ylab = 'Lk')
        layout(1)
        return(mres)
        }
}

