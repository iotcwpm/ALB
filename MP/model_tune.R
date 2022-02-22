# model_tune.R - DESC
# /model_tune.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


source("model.R")

library(doParallel)
registerDoParallel(2)

# load statistics
data(statistics)

# TPERIOD, last OM year + 11:15
tperiod <- list(2030:2034)


# --- TUNE(trigger) perfect.sa + fixedF.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=fixedF.hcr,
    args=list(ftrg=0.20))))

perf_fixf_05_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(ftrg=c(0.05, 0.40)), prob=0.5, tol=0.02, maxit=12, parallel=TRUE)

perf_fixf_06_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(ftrg=c(0.05, 0.40)), prob=0.6, tol=0.02, maxit=12, parallel=TRUE)

perf_fixf_07_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(ftrg=c(0.05, 0.40)), prob=0.7, tol=0.02, maxit=12, parallel=TRUE)


# --- TUNE(trigger) perfect.sa + hockeystick.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0.10, trigger=0.40, target=mean(refpts(om)$MSY) * 0.90,
      metric=relmets$SB0, output="catch", dlow=0.85, dupp=1.15))))

perf_hcst_05_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(trigger=c(0.05, 0.95)), prob=0.5, tol=0.02, maxit=12, parallel=TRUE)

perf_hcst_05_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(target=c(0.5e4, 8.5e4)), prob=0.5, tol=0.02, maxit=12, parallel=TRUE)

perf_hcst_06_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(trigger=c(0.35, 0.55)), prob=0.6, tol=0.02, maxit=12, parallel=TRUE)

perf_hcst_07_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(trigger=c(0.35, 0.55)), prob=0.7, tol=0.02, maxit=12, parallel=TRUE)




fut <- fwd(om, control=fwdControl(year=2020:2040, quant="fbar", value=0.2))

performance(fut, metrics=mets[c("SB", "F")],
  statistic=statistics['green'], years=tperiod)[, mean(data)]



control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0.10, trigger=0.40, target=mean(refpts(om)$MSY) * 0.90,
      metric=relmets$BMSY, output="catch", dlow=0.85, dupp=1.15))))







# --- TUNE perfect.sa + trend.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=trend.hcr,
a    args=list(k1=1.5, k2=3, gamma=0.85, nyears=5, metric=stock))))

ti <- system.time(
tune_perSA_stHCR <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=list(SB=function(x) unitSums(ssb(x)), F=function(x) unitMeans(fbar(x))),
  statistic=statistics["green"], years=tperiod, tune=list(gamma=c(0.5, 1.2)),
  prob=0.6, tol=0.02, maxit=12, parallel=TRUE)
)



# --- jabba.sa + trend.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa,
    args=list(model.type="Fox", idx.se=c(0.2, 0.2))),
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=0.85, nyears=5, metric=stock))))

ti <- system.time(
tune_jabSA_stHCR <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=list(SB=function(x) unitSums(ssb(x)), F=function(x) unitMeans(fbar(x))),
  statistic=statistics["green"], years=tperiod, tune=list(gamma=c(0.85, 1)),
  prob=0.6, tol=0.01, maxit=12, parallel=TRUE)
)

save(tune_jabSA_stHCR, file="model/tune.Rdata")

# perfect.sa + trend.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=0.85, nyears=5, metric=stock))))

ti <- system.time(
tune_perSA_stHCR <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=list(SB=function(x) unitSums(ssb(x)), F=function(x) unitMeans(fbar(x))),
  statistic=statistics["green"], years=tperiod, tune=list(gamma=c(0.5, 1.2)),
  prob=0.6, tol=0.02, maxit=12, parallel=TRUE)
)

save(tune_jabSA_stHCR, tune_perSA_stHCR, file="model/tune.Rdata")







