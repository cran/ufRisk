
arfilt <- function(ar = 0, ma = 0, d = 0, k = 50) {
    p <- length(ar[ar != 0])
    q <- length(ma[ma != 0])
    if (p == 0) {
        ar <- 0
    }
    if (q == 0) {
        ma <- 0
    }

    ar_coef <- c(1, ar, rep(0, k - p))
    arma_coef <- (1:(k + 1)) * 0
    arma_coef[1] <- 1

    if (p | q > 0) {
        for (i in 2:(k + 1)) {
            if ((i - q) < 1) {
                arma_coef[i] <- sum(ma[1:(q - abs(i - q) - 1)] * arma_coef[(i - 1):1]) - ar_coef[i]
            } else {
                arma_coef[i] <- sum(ma[1:q] * arma_coef[(i - 1):(i - q)]) - ar_coef[i]
            }
        }
    } else {
        arma_coef + 1
    }

    d_coef <- choose(d, 0:k) * ((-1)^(0:k))

    coef_all <- (1:(k + 1)) * 0
    for (j in 1:(k + 1)) {
        coef_all[j] <- sum(d_coef[1:j] * arma_coef[j:1])
    }
    return(-coef_all[-1])
}


