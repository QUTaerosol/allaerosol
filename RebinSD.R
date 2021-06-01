Rebin_SD <- function(Dp, dNdlogDp, Dp.int, dlogDp.int){
    #### This function takes an existing size distribution (Dp and dNdlogDp) and rebins the data to a new set of diameters (Dp.int). It also takes the dlogDp for the new set of diameters as input (this could be computed within the function as an upgrade)
    ###The function returns dNdlogDp.int, the new dNdlogDp values associated with the diameters passed to the functino (Dp.int)
    ##The function can handle multiple different instruments e.g. AMPS and APS data combined together.
    dlogDp <- diff(log10(Dp))
    dlogDp <- c(dlogDp, dlogDp[length(dlogDp)])
 ##I have found some instances where the dlogDp is too high or low
 ##This is where the SMPS ends and the APS starts for example.
    ind <- which(dlogDp < 0 | dlogDp > 0.09)
 ##Replace these with the subsequent dlogDp value.
    dlogDp[ind] <- dlogDp[ind + 1]
 ##Create upper and lower bound for original bins
    upper <- log10(Dp) + dlogDp/2
    lower <- log10(Dp) - dlogDp/2
 ##Create upper and lower bound for new bins
    upper.int <- log10(Dp.int) + dlogDp.int/2
    lower.int <- log10(Dp.int) - dlogDp.int/2
    
 ##Initialise a matrix of the fraction of the original bin in the new bins
  ##frac.mat has the same number of rows as the new/rebinned size distribution data
    ##and the same number of cols as the original size distributions data.
    frac.mat <- matrix(0, nrow = length(Dp.int), ncol = length(Dp))
 ##Make a matrix of the dNdlogDp in data
    dNdlogDp.mat <- matrix(as.numeric(dNdlogDp),nrow = length(Dp.int), ncol = length(Dp), byrow = T)
 ##Cycle through and fill frac.mat
    for (rw in 1:nrow(frac.mat)){
        frac.mat[rw, which(lower > lower.int[rw] & upper < upper.int[rw])] <- 1
        lw.bound <- which(upper > lower.int[rw] & lower < lower.int[rw])
        frac.mat[rw, lw.bound] <- (upper[lw.bound] - lower.int[rw])/(upper[lw.bound] - lower[lw.bound])
        up.bound <- which(lower < upper.int[rw] & upper > upper.int[rw])
        frac.mat[rw, up.bound] <-  (upper.int[rw] - lower[up.bound])/(upper[up.bound] - lower[up.bound])
        frac.mat[rw, which(is.na(dNdlogDp))] <- 0
    }
  ##Normalise the frac.matrix
    frac.mat <- frac.mat/matrix(rowSums(frac.mat, na.rm = T), nrow = nrow(frac.mat), ncol = ncol(frac.mat))
   ##Mulitply the frac.mat with the dNdlogDp matrix and sum each row
    dNdlogDp.int <- rowSums(dNdlogDp.mat * frac.mat, na.rm = T)
    return(dNdlogDp.int)
}