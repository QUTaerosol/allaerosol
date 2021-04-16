
##Lognormal fitting script - 
##Most of the detail regarding how the script works is described in the FitLognormal function

##Input data
#sd ##This is the dNdlogDp for the combined distributions combined distribution sd
#Dp ##This is a vector of the diameters (it's required that length(Dp) == ncol(sd))
#date ##This is a vector of dates (it's required that length(date) == nrow(sd))
#TotConc ##This is a vector of Total Concentrations (it's required that length(date) == nrow(sd))

##Define functions for lognormal fitting
nlsParam <- function(nmodes, p.approx, s.approx, mu.approx){
  if (nmodes == 2){
    form <- 
      "y ~ (a/(log10(b)*sqrt(2*pi))) * exp(-((log10(x)-log10(c))^2)/(2*(log10(b)^2))) + 
    (d/(log10(e)*sqrt(2*pi))) * exp(-((log10(x)-log10(f))^2)/(2*(log10(e)^2)))"
    
    start = list(a=p.approx[1], b=s.approx[1], c=mu.approx[1],
                 d=p.approx[2], e=s.approx[2], f=mu.approx[2])
    
  } else if (nmodes == 1){
    form <- 
      "y ~ (a/(log10(b)*sqrt(2*pi))) * exp(-((log10(x)-log10(c))^2)/(2*(log10(b)^2)))"
    
    start = list(a=p.approx[1], b=s.approx[1], c=mu.approx[1])
    
  } else if (nmodes == 3){
    form <- "y ~ (a/(log10(b)*sqrt(2*pi))) * exp(-((log10(x)-log10(c))^2)/(2*(log10(b))^2)) + 
    (d/(log10(e)*sqrt(2*pi))) * exp(-((log10(x)-log10(f))^2)/(2*(log10(e))^2)) +
    (h/(log10(i)*sqrt(2*pi))) * exp(-((log10(x)-log10(j))^2)/(2*(log10(i))^2))"
    
    start = list(a=p.approx[1], b=s.approx[1], c=mu.approx[1],
                 d=p.approx[2], e=s.approx[2], f=mu.approx[2],
                 h=p.approx[3], i=s.approx[3], j=mu.approx[3])
    
  } else if (nmodes == 4){
    form <- 
      "y ~ (a/(log10(b)*sqrt(2*pi))) * exp(-((log10(x)-log10(c))^2)/(2*(log10(b)^2))) + 
    (d/(log10(e)*sqrt(2*pi))) * exp(-((log10(x)-log10(f))^2)/(2*(log10(e)^2))) +
    (h/(log10(i)*sqrt(2*pi))) * exp(-((log10(x)-log10(j))^2)/(2*(log10(i)^2))) +
    (k/(log10(l)*sqrt(2*pi))) * exp(-((log10(x)-log10(m))^2)/(2*(log10(l)^2)))"
    
    start = list(a=p.approx[1], b=s.approx[1], c=mu.approx[1],
                 d=p.approx[2], e=s.approx[2], f=mu.approx[2],
                 h=p.approx[3], i=s.approx[3], j=mu.approx[3],
                 k=p.approx[4], l=s.approx[4], m=mu.approx[4])
    
  } else if (nmodes == 5){
    form <- 
      "y ~ (a/(log10(b)*sqrt(2*pi))) * exp(-((log10(x)-log10(c))^2)/(2*(log10(b)^2))) + 
    (d/(log10(e)*sqrt(2*pi))) * exp(-((log10(x)-log10(f))^2)/(2*(log10(e)^2))) +
    (h/(log10(i)*sqrt(2*pi))) * exp(-((log10(x)-log10(j))^2)/(2*(log10(i)^2))) +
    (k/(log10(l)*sqrt(2*pi))) * exp(-((log10(x)-log10(m))^2)/(2*(log10(l)^2))) +
    (n/(log10(o)*sqrt(2*pi))) * exp(-((log10(x)-log10(p))^2)/(2*(log10(o)^2)))"
    
    start = list(a=p.approx[1], b=s.approx[1], c=mu.approx[1],
                 d=p.approx[2], e=s.approx[2], f=mu.approx[2],
                 h=p.approx[3], i=s.approx[3], j=mu.approx[3],
                 k=p.approx[4], l=s.approx[4], m=mu.approx[4],
                 n=p.approx[4], o=s.approx[4], p=mu.approx[4])
  }
  
  return(list(form, start))
}
FitLS <- function(Dp, dNdlogDp, name, date, p.approx, s.approx, mu.approx, low.lim, up.lim){
  library(minpack.lm) ##nlsLM
  library(gtools)
  ##Extract x and y values for bls fitting of lognormal modes
  
  x <- as.numeric(Dp)
  y <- as.numeric(dNdlogDp)
  ##Extract formula and intial value vectors for nls fitting of lognormal modes
  nmodes <- length(mu.approx)
  Params <- nlsParam(nmodes, p.approx, s.approx, mu.approx)
  form <- as.formula(Params[[1]])
  strt <- Params[[2]]
  ##Define upper and lower limits for each parameter
  lower <- low.lim #rep(low.lim, nmodes)
  upper <- up.lim #rep(up.lim, nmodes)
  ##Fit lognormal modes using nlsLM
  fit.sd <- nlsLM(formula = as.formula(form),
                  start = strt,
                  control = nls.control(tol = 1E-5, minFactor = 1/1024, maxiter = 500),
                  trace = FALSE, lower = lower, upper = upper)
  ##Extract mean, sd and proportion for each mode
  mn <- coef(fit.sd)[3*(1:nmodes)]
  sd <- coef(fit.sd)[3*(1:nmodes)-1]
  N <- coef(fit.sd)[3*1:(nmodes)-2]  #/mean(data$TotConc)  #/sum(coef(fit.sd)[3*(1:nmodes)-2])
  
  ##Create series of data frames - with columnns corresponding to each mode- and rows corresponding to input data - Dp
  mn_df <- data.frame(matrix(mn, ncol = length(mn), nrow = length(Dp), byrow = T))
  sd_df <- data.frame(matrix(sd, ncol = length(mn), nrow = length(Dp), byrow = T))
  N_df <- data.frame(matrix(N, ncol = length(mn), nrow = length(Dp), byrow = T))
  Dp <- data.frame(matrix(Dp, ncol = length(mn), nrow = length(Dp)))
  
  ##Compute lognormal distributions for above matrices
  modes_dNdlogD <- (N_df/(log10(sd_df)*sqrt(2*pi)))*exp(-((log10(Dp)-log10(mn_df))^2)/(2*log10(sd_df)^2))
  
  flag <- 0
  
  ##Flag fits where one mode is never at higher conc than all of the others
  colMax <- apply(modes_dNdlogD, 1, function(x) which(x == max(x)))
  if(length(unique(colMax)) < nmodes){
    flag <- 1
  }
  
  ##Sum rows to get a total distribution
  modes_dNdlogD$Total <- rowSums(modes_dNdlogD)
  ##Add measured data 
  modes_dNdlogD$dia <- Dp
  ##Compute reconstruction error
  Err <- sum(abs(modes_dNdlogD$Total - dNdlogDp), na.rm = T) / sum(abs(dNdlogDp), na.rm = T)
  ##Compute max error
  ind <- which.max(abs(modes_dNdlogD$Total - dNdlogDp))
  Err_max <- max(abs(modes_dNdlogD$Total - dNdlogDp), na.rm = T) / dNdlogDp[ind]
  ##Get proportions for each mode at a number of sizes
  mix50nm <-  (N/(log10(sd)*sqrt(2*pi)))*exp(-((log10(50)-log10(mn))^2)/(2*log10(sd)^2))/
    sum((N/(log10(sd)*sqrt(2*pi)))*exp(-((log10(50)-log10(mn))^2)/(2*log10(sd)^2)))
  mix100nm <-  (N/(log10(sd)*sqrt(2*pi)))*exp(-((log10(100)-log10(mn))^2)/(2*log10(sd)^2))/
    sum((N/(log10(sd)*sqrt(2*pi)))*exp(-((log10(100)-log10(mn))^2)/(2*log10(sd)^2)))
  mix150nm <-  (N/(log10(sd)*sqrt(2*pi)))*exp(-((log10(150)-log10(mn))^2)/(2*log10(sd)^2))/
    sum((N/(log10(sd)*sqrt(2*pi)))*exp(-((log10(150)-log10(mn))^2)/(2*log10(sd)^2)))
  dat_out <<- data.frame(sample = name, mn = mn, sd = sd, N = N, mix50nm = mix50nm, mix100nm = mix100nm, 
                    mix150nm = mix150nm, BIC = BIC(fit.sd), AIC = AIC(fit.sd), Err = Err, 
                    Err_max = Err_max, name = name, flag = flag, mode = NA, iteration = NA, nmodes = NA, date = date)
  return(dat_out)
}
FitSSA <- function(Dp, dNdlogDp, ssa = dat, samp = sample[i]){
  ###First fit the SSA mode
  x <- Dp[Dp >= 500]
  y <- dNdlogDp[Dp >= 500]
  form <- 
    "y ~ (a/(log10(b)*sqrt(2*pi))) * exp(-((log10(x)-log10(c))^2)/(2*(log10(b)^2)))"
  strt = list(a=runif(1, 0, 1000), b=runif(1, 2.5, 3), c = runif(1, 140, 260))
  ##Define upper and lower range for params
  upperSSA = c(t(data.frame(p = 1000, s = 3, mu= 260)))
  lowerSSA = c(t(data.frame(p = 0, s = 2.5, mu = 140)))
  ##Fit lognormal modes using nlsLM
  fit.sd <- NA
  while(length(fit.sd) == 1){
    fit.sd <- nlsLM(formula = as.formula(form),
                    start = strt,
                    control = nls.control(tol = 1E-5, minFactor = 1/1024, maxiter = 500),
                    trace = FALSE, lower = lowerSSA, upper = upperSSA)
  }
  mn <- coef(fit.sd)[3]
  sd <- coef(fit.sd)[2]
  N <- coef(fit.sd)[1]
  ##Subtract SSA mode from observations
  SSA_dNdlogDp <- (N/(log10(sd)*sqrt(2*pi)))*exp(-((log10(Dp)-log10(mn))^2)/(2*log10(sd)^2))
  dNdlogDp <- dNdlogDp - SSA_dNdlogDp
  #dNdlogDp[dNdlogDp < 0] <- 0
  ##Store the SSA variables
  #ssa <- dat
  ssa[1,] <- NA
  ssa$sample <- samp; ssa$name <- samp; ssa$mn <- mn; ssa$sd <- sd; ssa$N <- N; ssa$mode <- 6
  return(list(dNdlogDp, ssa))
}
FitLognormal <- function(date, sd, Dp, TotConc, nmodes, upper, lower){
  ##This function fits up to 6 lognormal distributions to a combined SMPS/APs/OPC dataset
  ##sd is an array of the measured dNdlogDp data, date is a vector dates corresponding to each sample, Dp is a vector of measurd diameters and TotConc is a vector of total concentration
      #sd has dimensions no. of samples x no. of diameters 
      #date has length = no. of samples
      #Dp has length = no. of diameters
      #TotConc has length = no. of diameters
      #nmodes is an integer indicating the number of lognormal modes to fit to the distribution
      #upper and lower are named data.frames with variables p, sd and mn, corresponding with upper/lower constraints for these variables for each lognormal mode.
            ##p is the number fraction of the lognormal distributions i.e. lognormal distributions/total conc.
            ##sd is the geometric std dev
            ##mn is the geo mean diameter in nm
      #th number of rows of upper and lower correspond to the number of modes.
      
  ##Up to 5 lognormal distributions are fitted.
  ##The function using non-linear least square fits to the data (nlsLM)
  ##It applies a randon set of initial conditions (constrained by the upper an lower limits for each parameters) and performs 1000 fits to each size distribution
  ##The error in each of these fits is computed via the Bayesian Information Criterion (BIC).
  
  ##FitSSA - is used to fit marine distributions - as in Modini et al. 2015, but is not used here.
  ##FitLS runs the non-linear least square fitting and in turn calls nlsParam - which provides the fitting formula and parameters.

  sample <- unique(sd$sample)
  ##Define an dataframe for fitting results
  out <- data.frame(sample = NA, mn = NA, sd = NA, N = NA, mix50nm = NA,
                    mix100nm = NA, mix150nm = NA, BIC = NA, AIC = NA, Err = NA, Err_max = NA, name = NA, 
                    flag = NA, mode = NA, iteration = NA, nmodes = NA, 
                    date = as.POSIXct("1/1/2000 01:00", format = "%d/%m/%Y %H:%M", tz = "UTC"))[-1,]
  dat <- temp <- out
  ##Define the number of iterations to run for each sample
  it <- 100

  ##Loop through samples and fit distributions
  prog <- 0
  for (i in 1:nrow(sd)){
    if(round(i/nrow(sd),1) > prog){
      prog <- round(i/nrow(sd),1)
      print(prog)
    }
    dat <- dat[-c(1:nrow(dat)),]
    ##Define the Dp and dNdlogDp
    dNdlogDp <- as.numeric(sd[i,])
    
    #SSA <- FitSSA(Dp, dNdlogDp, ssa = dat, samp = i)
    #dNdlogDp <- SSA[[1]]
    #ssa <- SSA[[2]]
    
    ###Fit the residual distribution
    for (n in 1:it){
      #nmodes <- sample(2:5)[1]
      ##Create a set of random lognormal parameters - bound to the ranges below
      if (nmodes == 1){
        mode <- data.frame(p.approx = c(runif(1, lower$p[1], upper$p[1])),
                           s.approx = c(runif(1, lower$s[1], upper$s[1])),
                           mu.approx = c(runif(1, lower$mu[1], upper$mu[1])))
      } else if (nmodes == 2){
        mode <- data.frame(p.approx = c(runif(1, lower$p[1], upper$p[1]), runif(1, lower$p[2], upper$p[2])),
                           s.approx = c(runif(1, lower$s[1], upper$s[1]), runif(1, lower$s[2], upper$s[2])),
                           mu.approx = c(runif(1, lower$mu[1], upper$mu[1]), runif(1, lower$mu[2], upper$mu[2])))
      } else if (nmodes == 3){
        mode <- data.frame(p.approx = c(runif(1, lower$p[1], upper$p[1]), runif(1, lower$p[2], upper$p[2]), runif(1, lower$p[3], upper$p[3])),
                           s.approx = c(runif(1, lower$s[1], upper$s[1]), runif(1, lower$s[2], upper$s[2]), runif(1, lower$s[3], upper$s[3])),
                           mu.approx = c(runif(1, lower$mu[1], upper$mu[1]), runif(1, lower$mu[2], upper$mu[2]), runif(1, lower$mu[3], upper$mu[3])))
      } else if (nmodes == 4){
        mode <- data.frame(p.approx = c(runif(1, lower$p[1], upper$p[1]), runif(1, lower$p[2], upper$p[2]), runif(1, lower$p[3], upper$p[3]), runif(1, lower$p[4], upper$p[4])),
                           s.approx = c(runif(1, lower$s[1], upper$s[1]), runif(1, lower$s[2], upper$s[2]), runif(1, lower$s[3], upper$s[3]), runif(1, lower$s[4], upper$s[4])),
                           mu.approx = c(runif(1, lower$mu[1], upper$mu[1]), runif(1, lower$mu[2], upper$mu[2]), runif(1, lower$mu[3], upper$mu[3]), runif(1, lower$mu[4], upper$mu[4])))
      }else if (nmodes == 5){
        mode <- data.frame(p.approx = c(runif(1, lower$p[1], upper$p[1]), runif(1, lower$p[2], upper$p[2]), runif(1, lower$p[3], upper$p[3]), runif(1, lower$p[4], upper$p[4]), runif(1, lower$p[5], upper$p[5])),
                           s.approx = c(runif(1, lower$s[1], upper$s[1]), runif(1, lower$s[2], upper$s[2]), runif(1, lower$s[3], upper$s[3]), runif(1, lower$s[4], upper$s[4]), runif(1, lower$s[5], upper$s[5])),
                           mu.approx = c(runif(1, lower$mu[1], upper$mu[1]), runif(1, lower$mu[2], upper$mu[2]), runif(1, lower$mu[3], upper$mu[3]), runif(1, lower$mu[4], upper$mu[4]), runif(1, lower$mu[5], upper$mu[5])))
      }
      ##Extract initial values
      p.approx <- mode$p.approx * mean(TotConc[i], na.rm = T)
      s.approx <- mode$s.approx
      mu.approx = mode$mu.approx
      ##Compute number concs for p range
      upper_in <- upper
      lower_in <- lower
      upper_in$p <- upper$p *  mean(TotConc[i], na.rm = T)
      lower_in$p <- lower$p *  mean(TotConc[i], na.rm = T)
      ##Put in format readable by nlsLM
      low.lim <- c(t(lower_in))
      up.lim <- c(t(upper_in))
      ##Fit data
      temp <- try(FitLS(Dp = Dp, dNdlogDp = dNdlogDp, name = i, date = date[i], p.approx = p.approx, s.approx = s.approx, 
                        mu.approx = mu.approx, low.lim = low.lim, up.lim = up.lim), silent = T)
      if(length(temp) > 1){
        ##Add iteration #
        temp$iteration <- n
        temp$nmodes <- nmodes
        ##Add mode # according to mean
        temp$mode <- rank(temp$mn, ties.method = "first")
        ##Sort by mode
        temp <- temp[order(temp$mode),]
        ##Bind output
        dat <- rbind(dat, temp)
      }
    }
    if(nrow(dat) > 0){
      ##Extract samples with "not acceptable" fit - one mode engulfs another
      dat$BIC[which(dat$flag == 1)] <- NA
      dat$AIC[which(dat$flag == 1)] <- NA
      dat$Err_max[which(dat$flag == 1)] <- NA
      rank <- data.frame(it = unique(dat$iteration),
                         BIC = 1 * rank(aggregate(dat$BIC, by = list(dat$iteration), FUN = mean, na.rm = T)$x),
                         ERRrank = 0 * rank(aggregate(dat$Err_max, by = list(dat$iteration), FUN = mean, na.rm = T)$x))
      rank$Tot <- rowSums(rank[,c(2:ncol(rank))])
      ##Extract the iteration that matches the obs most closely
      best <- rank$it[which(rank$Tot == min(rank$Tot))]
      ##Create output df
      dat <- dat[which(dat$iteration == best),]
      #dat$date <- date[i]
      #ssa$date <- sd$date[i]
      out <- rbind(out, dat)
    }
    #print(out)
  }
  return(out)
}
##Run lognormal fitting functions
#sd_lognormal <- FitLognormal(sd = sd)
##Save data
#saveRDS(sd_lognormal, file = "SD_LognormalParams.rds")
#write.table(sd_lognormal, file = "SD_LognormalParams.csv", sep = ",", col.names=T, row.names= F)
