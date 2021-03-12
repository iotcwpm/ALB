# utilities.R - DESC
# /utilities.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# dm {{{


dm <- function(x, k, alternative = c("two.sided", "less", "greater")) {

  # STOP if NAs
  if(any(is.na(x)))
    stop()

  # STOP if dim no c(1,n,1,1,1,1)
  if(any(dim(x)[-2] > 1))
    stop()
  
  alternative <- match.arg(alternative)

  #
  n <- dim(x)[2]
  cv <- acf(x, lag.max=k, type="covariance", plot=FALSE)$acf[,,1]
  eps <- 1.0e-8
  vr <- max(eps, sum(c(cv[1], 2 * cv[-1])) / n)
  statistic <- mean(x) / sqrt(vr)

  if (alternative == "two.sided")
    pval <- 2 * pnorm(-abs(statistic))
  else if (alternative == "less")
    pval <- pnorm(statistic)
  else if (alternative == "greater")
    pval <- pnorm(statistic, lower.tail = FALSE)

  return(structure(list(statistic = statistic, parameter = k,
    alternative = alternative, p.value = pval)))
}

# }}}

library(forecast)

# masep(observations, estimates, lag) {{{

masep <- function(obs, hat, h=1) {

  # CHECK minimum number of values (lag + 1)
  if (length(obs) < (h+1))
    return(NULL)

  # CALCULATE naive prediction
  naive <- c(rep(NA, h), obs)[seq(length(obs))]

  # LOG ratios
  t1 <- log(obs / hat)
  t2 <- log(naive / hat)

  # RESIDUALS
  rsdl1 <- t1[!is.na(t1 * t2)]
  rsdl2 <- t2[!is.na(t1 * t2)]
  
  mase <- sum(abs(rsdl1)) / sum(abs(rsdl2))
  pvalue <- unname(dm.test(rsdl1, rsdl2, alternative="greater")$p.value)
  
  return(data.table(mase=mase, pvalue=pvalue))
} # }}}

