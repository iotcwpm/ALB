# model_base.R - DESC
# /model_base.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(mse)

method(oem) <- sampling.oem

res <- list()

# USE one CPUE only

observations(oem)$idx <- observations(oem)$idx[1]
deviances(oem)$idx <- deviances(oem)$idx[1]

# CONTROL

control <- mpCtrl(list(
  hcr = mseCtrl(method=cpue.hcr,
    args=list(k1=0.1, k2=0.2, k3=0.1, k4=0.2, target=0.1)),
  est = mseCtrl(method=cpue.ind)))


# --- (1) tunegrid: args=list(k1=0.1, k2=0.2, k3=0.1, k4=0.2, target=0.1))

# grid, n = 1875

grid <- expand.grid(list(
  k1=seq(0.1, 2, length=5),
  k2=seq(0.1, 2, length=5),
  k3=seq(0.1, 2, length=5),
  k4=seq(0.1, 2, length=5),
  target=seq(0.1, 2, length=3)
))

gridtune <- tunegrid(om, oem, control, metric=mets, statistic=stats['S6'],
  grid=grid, args=mseargs, years=list(2030:2039), iters=1:100,
  tracking=c("cpue.slope", "cpue.mean"))

save(gridtune, file="model/cpue.RData", compress="xz")

# FIND hot spots

# COMPUTE performance S8



# --- (2) TUNE over target

# TUNE for 50% green

system.time(
tun <- tunebisect(om, oem=oem, control=control, args=mseargs,
  metrics=list(SB=ssb), statistic=statistics["S6"], years=2030:2035,
  tune=list(target=c(0.01, 5)), prob=0.5, tol=0.02, maxit=12,
  tracking=c("cpue.slope", "cpue.mean"), parallel=TRUE)
)

res$tun05green <- tun

save(res, file="model/cpue.RData", compress="xz")

# TUNE for 60% green

system.time(
tun <- tunebisect(om, oem=oem, control=control, args=mseargs,
  metrics=list(SB=ssb), statistic=statistics["S6"], years=2030:2035,
  tune=list(target=c(0.01, 5)), prob=0.6, tol=0.02, maxit=12,
  tracking=c("cpue.slope", "cpue.mean"), parallel=TRUE)
)

res$tun06green <- tun

save(res, file="model/cpue.RData", compress="xz")

# TUNE for 70% green

system.time(
tun <- tunebisect(om, oem=oem, control=control, args=mseargs,
  metrics=list(SB=ssb), statistic=statistics["S6"], years=2030:2035,
  tune=list(target=c(0.01, 5)), prob=0.7, tol=0.02, maxit=12,
  tracking=c("cpue.slope", "cpue.mean"), parallel=TRUE)
)

res$tun07green <- tun

save(res, file="model/cpue.RData", compress="xz")

