# model_tune.R - DESC
# /model_tune.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


source("model.R")

library(doParallel)
registerDoParallel(25)

# load statistics
data(statistics)

# TPERIOD, last OM year + 11:15
tperiod <- list(2034:2039)

tune <- list()


# --- TUNE(trigger) perfect.sa + fixedF.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=fixedF.hcr,
    args=list(ftrg=0.20))))

tune$perf_fixf_05_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(ftrg=c(0.05, 0.40)), prob=0.5, tol=0.02, maxit=12, parallel=TRUE)

tune$perf_fixf_06_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(ftrg=c(0.05, 0.40)), prob=0.6, tol=0.02, maxit=12, parallel=TRUE)

tune$perf_fixf_07_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(ftrg=c(0.05, 0.40)), prob=0.7, tol=0.02, maxit=12, parallel=TRUE)

save(tune, file="model/tune.Rdata", compress="xz")


# --- TUNE(trigger) perfect.sa + hockeystick.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0.10, trigger=0.40, target=mean(refpts(om)$MSY) * 0.90,
      metric=relmets$SB0, output="catch", dlow=0.85, dupp=1.15))))

tune$perf_hcst_05_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(trigger=c(0.05, 0.95)), prob=0.5, tol=0.02, maxit=12, parallel=TRUE)

tune$perf_hcst_06_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(trigger=c(0.35, 0.55)), prob=0.6, tol=0.02, maxit=12, parallel=TRUE)

tune$perf_hcst_07_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(trigger=c(0.35, 0.55)), prob=0.7, tol=0.02, maxit=12, parallel=TRUE)

save(tune, file="model/tune.Rdata", compress="xz")


# --- TUNE(target) perfect.sa + hockeystick.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0.10, trigger=0.40, target=mean(refpts(om)$MSY) * 0.90,
      metric=relmets$SB0, output="catch", dlow=0.85, dupp=1.15))))

tune$perf_hcst_05_targ <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(target=c(0.5e4, 8.5e4)), prob=0.5, tol=0.02, maxit=12, parallel=TRUE)

tune$perf_hcst_06_targ <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(target=c(0.5e4, 8.5e4)), prob=0.6, tol=0.02, maxit=12, parallel=TRUE)

tune$perf_hcst_07_targ <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(target=c(0.5e4, 8.5e4)), prob=0.7, tol=0.02, maxit=12, parallel=TRUE)

save(tune, file="model/tune.Rdata", compress="xz")


# --- TUNE(gamma) perfect.sa + trend.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=1, nyears=5, metric=relmets$SB0))))

tune$perf_tren_05_gamma <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(target=c(0.5e4, 8.5e4)), prob=0.5, tol=0.02, maxit=12, parallel=TRUE)

tune$perf_tren_06_gamma <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(target=c(0.5e4, 8.5e4)), prob=0.6, tol=0.02, maxit=12, parallel=TRUE)

tune$perf_tren_07_gamma <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(target=c(0.5e4, 8.5e4)), prob=0.7, tol=0.02, maxit=12, parallel=TRUE)

save(tune, file="model/tune.Rdata", compress="xz")


# --- TUNE(trigger) jabba.sa + hockeystick.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0.10, trigger=0.40, target=mean(refpts(om)$MSY) * 0.90,
      metric=relmets$BMSY, output="catch", dlow=0.85, dupp=1.15))))

tune$jabba_hcst_05_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(trigger=c(0.05, 0.95)), prob=0.5, tol=0.02, maxit=12, parallel=TRUE)

tune$jabba_hcst_06_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(trigger=c(0.35, 0.55)), prob=0.6, tol=0.02, maxit=12, parallel=TRUE)

tune$jabba_hcst_07_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(trigger=c(0.35, 0.55)), prob=0.7, tol=0.02, maxit=12, parallel=TRUE)

save(tune, file="model/tune.Rdata", compress="xz")


# --- TUNE(target) jabba.sa + hockeystick.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0.10, trigger=0.40, target=mean(refpts(om)$MSY) * 0.90,
      metric=relmets$BMSY, output="catch", dlow=0.85, dupp=1.15))))

tune$jabba_hcst_05_targ <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(target=c(0.5e4, 8.5e4)), prob=0.5, tol=0.02, maxit=12, parallel=TRUE)

tune$jabba_hcst_06_targ <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(target=c(0.5e4, 8.5e4)), prob=0.6, tol=0.02, maxit=12, parallel=TRUE)

tune$jabba_hcst_07_targ <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(target=c(0.5e4, 8.5e4)), prob=0.7, tol=0.02, maxit=12, parallel=TRUE)

save(tune, file="model/tune.Rdata", compress="xz")


# --- TUNE(gamma) jabba.sa + trend.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=1, nyears=5, metric=relmets$SB0))))

tune$jabba_tren_05_gamma <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(target=c(0.5e4, 8.5e4)), prob=0.5, tol=0.02, maxit=12, parallel=TRUE)

tune$jabba_tren_06_gamma <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(target=c(0.5e4, 8.5e4)), prob=0.6, tol=0.02, maxit=12, parallel=TRUE)

tune$jabba_tren_07_gamma <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(target=c(0.5e4, 8.5e4)), prob=0.7, tol=0.02, maxit=12, parallel=TRUE)

save(tune, file="model/tune.Rdata", compress="xz")



