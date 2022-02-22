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
tperiod <- list(2030:2034)


# --- DEBUG
idx <- sample(seq(500), 5)
om <- iter(om, idx)
oem <- iter(oem, idx)

tes <- mp(om, oem, ctrl=control, args=mseargs)

performance(tune_perSA_stHCR$min,
  metrics=mets[c("SB", "F")], statistic=statistics[1:5], years=tperiod)
# ---

# estimator + hcr + param + objective (est_hcr_par_obj)

# --- TUNE(trigger) perfect.sa + hockeystick.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0.10, trigger=0.40, target=35000,
      metric=relmets$SBMSY, output="catch", dlow=0.85, dupp=1.15))))

tu_perf_hcst_05 <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(trigger=c(0.35, 0.55)), prob=0.5, tol=0.02, maxit=12, parallel=TRUE)

tu_perf_hcst_06 <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(trigger=c(0.35, 0.55)), prob=0.6, tol=0.02, maxit=12, parallel=TRUE)

tu_perf_hcst_07 <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(trigger=c(0.35, 0.55)), prob=0.7, tol=0.02, maxit=12, parallel=TRUE)










# --- TUNE perfect.sa + trend.hcr

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







