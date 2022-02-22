# model_corners.R - DESC
# ALB/OM/model_corners.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ioalbmse)
load_all('../ioalbmse')

library(r4ss)
library(ss3diags)

source("utilities.R")

library(doParallel)
registerDoParallel(2)

# TODO COMBINE into CCD

# --- RUN corners

# SETUP runs for grid corners

full <- list(
  M = seq(0.20, 0.35, length = 4),
  sigmaR = seq(0.4, 0.8, length = 3),
  steepness = seq(0.7, 0.9, length = 3),
  cpues = c(14, 12),
  lfreq = c(1e-2, 0.1, 1),
  llq = c(1, 1.01)
)

corners <- lapply(full, function(x) c(x[1], x[length(x)]))

grid <- setioalbgrid(corners, dir = "model/corners",
  base = "data/PSLFwt/CPUE_SouthWest", name = "abt", write=FALSE) # NOTE

# PREPARE retros

lapply(file.path("model/corners", grid$id), prepareRetro)

# RUN models & retros

# ls | parallel -j21 --bar --progress '(cd {}; ss_3.30.15 && packss3run; cd retro; for d in ./*/ ; do (cd "$d" && ss_3.30.15 && packss3run); done)'

# parallel --jobs 35 --progress 'cd {} && ss_3.30.15' ::: *

dirs <- list.dirs('model/corners', rec=FALSE)

# LOAD results (results df, FLStock, FLSR)

corners <- loadOMS(subdirs=dirs, grid=grid)

save(corners, grid, file="model/corners/load.RData", compress="xz")

# LOAD retros

retros <- foreach(x=setNames(dirs, nm=seq(length(dirs)))) %dopar% {
  
  rdirs <- setNames(c(x, as.list(list.dirs(file.path(x, "retro"),
    recursive=FALSE))), nm=seq(0, 5))

  rretro <- lapply(rdirs, readOutputss3)

  return(SSsummarize(rretro, verbose=FALSE))
}

save(retros, grid, file="model/corners/retros.RData", compress="xz")


# --- DIAGNOSTICS & METRICS

load("model/corners/load.RData")
load("model/corners/retros.RData")

res <- corners$results

# 1. FIND unrealistic values (SSB_Virgin > 1e7 t, SSB_status > 3)

res[, del1:=SSB_Virgin > 1e6 | SSB_status > 3]


# 2. CHECK convergence < 1e-4

res[, del2:=Convergence_Level > 1e-4]

# 3. COMPUTE retrospective Mohn's rho

mrhos <- lapply(retros, SSmohnsrho, startyr=2013, verbose=FALSE)

# ADD to res
res[, mrho:=unlist(lapply(mrhos, '[[', 'AFSC_Hurtado_SSB'))]

# 4. COMPUTE measure of process error

res[, Recr_sigma := c(sqrt(yearVars(residuals(corners$sr)[, ac(1975:2015)])))]

# 5. COMPUTE hcxval prediction skill

ssmases <- rbindlist(foreach(x=retros) %dopar% {
  ma <- rbindlist(lapply(1:4, function(y) SSmase(x, Season=y, verbose=FALSE)))
  ma[!is.na(MASE), .(mase=mean((MAE.PR * n.eval) / (MAE.base * n.eval), na.rm=TRUE)),
    by=.(Index, Season)]
  }, idcol='iter')

mases <- rbindlist(foreach(i=seq(length(retros))) %dopar% {
  
  x <- data.table(retros[[i]]$indices)
  j <- res[iter == i, cpues]
  y <- rbindlist(list(
    x[Fleet == j & Seas == 1 & imodel == 2 & Yr == 2017],
    x[Fleet == j & Seas == 1 & imodel == 3 & Yr == 2016],
    x[Fleet == j & Seas == 1 & imodel == 4 & Yr == 2015],
    x[Fleet == j & Seas == 1 & imodel == 5 & Yr == 2014],
    x[Fleet == j & Seas == 1 & imodel == 6 & Yr == 2013]))

  return(cbind(y[1, .(Fleet, Fleet_name, Seas)], masep(y$Obs, y$Exp)))
  }, idcol="iter")

res <- merge(res, mases[, .(iter, mase, pvalue)], by="iter")

# ID runs with mase(LLCPUE3_S01) > 1

res[, del3:=mase > 1]

# 6. ACF for recruitment and SSB

# CREATE acfs table

ssbs <- lapply(corners$output, extractSSB)
ssbs[["0"]] <- extractSSB(readOutputss3("model/base"))

acfs <- lapply(ssbs, function(x)
  acf(c(x), lag.max=30, plot=FALSE))
acfs <- lapply(acfs, function(x)
  data.table(lag=c(x$lag), acf=c(x$acf)))
acfs <- rbindlist(acfs, idcol="iter")

# 7. COMPUTE ccf(rec, ssb)

recs <- lapply(corners$output, extractRec)
recs[["0"]] <- extractRec(readOutputss3("model/base"))

ccfs <- Map(function(x, y) ccf(c(x), c(y), plot=FALSE, lag.max=20),
  recs, ssbs)

ccfs <- rbindlist(lapply(ccfs, function(x) data.table(lag=c(x$lag), acf=c(x$acf))),
  idcol="iter")

# 8. ADD iters selection

res[, sel:=!del1 & !del2 & !del3]

# SAVE results + diagnostics
save(res, ssmases, mases, acfs, ccfs,
  file="model/corners/diagnostics.RData", compress="xz")

ggsave('~/masealb.png',
ggplot(res, aes(x=mase)) + geom_histogram(fill="grey", colour="black") +
  geom_vline(xintercept=1, colour="red") + ylab("") + xlab("MASE")
)

# SAVE corners OMs, results

corners$results <- res

save(corners, file="model/corners.RData", compress="xz")
