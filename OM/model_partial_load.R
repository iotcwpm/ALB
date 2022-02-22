# model_partial_load.R - DESC
# /model_partial_load.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ioalbmse)
library(r4ss)
library(ss3diags)

source("utilities.R")

library(doParallel)
registerDoParallel(3)


# --- LOAD

load("model/partial/grid.RData")

dirs <- list.dirs("model/partial", recursive=FALSE)

# OM list

partial<- loadOMS(subdirs=dirs, grid=grid, range=c(minfbar=1, maxfbar=12))

save(partial, file="model/partial/load.RData", compress="xz")

# retros

retros <- foreach(x=setNames(dirs, nm=seq(length(dirs)))) %dopar% {
  
  rdirs <- setNames(c(x, as.list(list.dirs(file.path(x, "retro"),
    recursive=FALSE))), nm=seq(0, 5))

  rretro <- lapply(rdirs, readOutputss3)

  return(SSsummarize(rretro, verbose=FALSE))
}

save(retros, grid, file="model/partial/retros.RData", compress="xz")


# --- DIAGNOSTICS & METRICS

load("model/partial/load.RData")
load("model/partial/retros.RData")

results <- partial$results
srr <- partial$sr

# 1. FIND unrealistic values (SSB_Virgin > 1e7 t, SSB_status > 3, F_2017 > 1)

results[, del1:=SSB_Virgin > 5e5 | SSB_status > 3 | F_endyr > 1]
results[(del1),]

# 2. CHECK convergence < 1e-4

results[, del2:=Convergence_Level > 1e-4]
results[(del2),]

# 3. COMPUTE retrospective Mohn's rho

mrhos <- lapply(retros, SSmohnsrho, startyr=2013, verbose=FALSE)

# ADD to res
results[, mrho:=unlist(lapply(mrhos, '[[', 'AFSC_Hurtado_SSB'))]

# 4. COMPUTE measure of process error

results[, Recr_sigma := c(sqrt(yearVars(residuals(srr)[, ac(1975:2015)])))]

# 5. COMPUTE hcxval prediction skill

ssmases <- rbindlist(foreach(x=retros) %dopar% {
  ma <- rbindlist(lapply(1:4, function(y) SSmase(x, Season=y, verbose=FALSE)))
  ma[!is.na(MASE), .(mase=mean((MAE.PR * n.eval) / (MAE.base * n.eval), na.rm=TRUE)),
    by=.(Index, Season)]
  }, idcol='iter')

ssmases <- rbindlist(foreach(x=retros) %dopar% {
  ma <- rbindlist(lapply(1:4, function(y) SSmase(x, Season=y, verbose=FALSE)))
  ma[!is.na(MASE), .(mase=MASE),
    by=.(Index, Season)]
  }, idcol='iter')

smases <- ssmases[ Season == 1 & Index %in% c('LLCPUE1', 'LLCPUE3'),]

# DEBUG DIFFERENCe between pmase and ssmase


mases <- rbindlist(foreach(i=seq(length(retros))) %dopar% {
  
  x <- data.table(retros[[i]]$indices)
  j <- results[iter == i, cpues]
  y <- rbindlist(list(
    x[Fleet == j & Seas == 1 & imodel == 2 & Yr == 2017],
    x[Fleet == j & Seas == 1 & imodel == 3 & Yr == 2016],
    x[Fleet == j & Seas == 1 & imodel == 4 & Yr == 2015],
    x[Fleet == j & Seas == 1 & imodel == 5 & Yr == 2014],
    x[Fleet == j & Seas == 1 & imodel == 6 & Yr == 2013]))

  return(cbind(y[1, .(Fleet, Fleet_name, Seas)], masep(y$Obs, y$Exp)))
  }, idcol="iter")

results <- merge(results, mases[, .(iter, mase, pvalue)], by="iter")

merge(smases, mases[, .(iter, mase, pvalue)], by="iter")

# ID runs with mase(LLCPUE3_S01) > 1

results[, del3 := mase > 1]

# 6. ACF for recruitment and SSB

# CREATE acfs table

ssbs <- lapply(partial$output, extractSSB)
ssbs[["0"]] <- extractSSB(readOutputss3("model/base"))

acfs <- lapply(ssbs, function(x)
  acf(c(x), lag.max=30, plot=FALSE))
acfs <- lapply(acfs, function(x)
  data.table(lag=c(x$lag), acf=c(x$acf)))
acfs <- rbindlist(acfs, idcol="iter")

# 7. COMPUTE ccf(rec, ssb)

recs <- lapply(partial$output, extractRec)
recs[["0"]] <- extractRec(readOutputss3("model/base"))

ccfs <- Map(function(x, y) ccf(c(x), c(y), plot=FALSE, lag.max=20),
  recs, ssbs)

ccfs <- rbindlist(lapply(ccfs, function(x) data.table(lag=c(x$lag), acf=c(x$acf))),
  idcol="iter")


# 8. NC 2018-2019

nc <- c(`2018`=41615, `2019`=39426)

stk <- stf(nounit(noseason(partial$stock)), end=2019)
range(stk, c("minfbar", "maxfbar")) <- c(1,12)

# PROJECT for NC plus 2x limit in yearly increase in F

fctrl <- fwdControl(list(year=2018:2019, quant="catch", value=nc),
    list(year=2018:2019, quant="fbar", relYear=2017:2018, min=0, max=2))

fut <- fwd(stk, sr=srr, control=fctrl)

# HC 2013-2017

hsr <- rec(stk)[, ac(2013:2017)]
hsr <- predictModel(model=rec~a, params=FLPar(c(hsr),
  dimnames=list(params="a", year=dimnames(hsr)$year, iter=dimnames(hsr)$iter)))

hc0 <- fwd(window(stk, end=2017), sr=hsr, catch=catch(stk)[, ac(2013:2017)])

hc1 <- fwd(window(stk, end=2017), sr=srr, catch=catch(stk)[, ac(2013:2017)])

osr <- rec(stk)[, ac(1970:1974)]
dimnames(osr) <- list(year=2013:2017)

hc2 <- fwd(window(stk, end=2017), sr=osr, catch=catch(stk)[, ac(2013:2017)])


plot(FLStocks(SS=window(stk, end=2017), HCrec=hc0, HCSRR=hc1)) + xlim(c(2000, NA))
plot(FLStocks(SS=window(stk, end=2017), HCSRR=hc1, HCold=hc2)) + xlim(c(2000, NA))


# IDENTIFY runs where C2018-19 < NC

range(catch(fut)[,'2018'] / nc[1])
range(catch(fut)[,'2019'] / nc[2])

idx <- c(catch(fut)[, '2018'] < nc['2018'] * 0.99) |
  c(catch(fut)[, '2019'] < nc['2019'] * 0.99)

# IDENTIFY runs where ssb 2019 is too low

ssb(fut)[,'2019']

range(ssb(fut)[,'2019',,,, !idx])
range(ssb(fut)[,'2019',,,, idx])
range(ssb_end(fut)[,'2019',,,, !idx])
range(ssb_end(fut)[,'2019',,,, idx])

results[, del4 := idx]


# 9. ADD iters selection

results[, sel:=!del1 & !del2 & !del3 & !del4]

sel <- results[(sel), iter]

te <- fwd(stf(iter(fut, sel), end=2029), sr=iter(partial$sr, sel),
  control=fwdControl(year=2020:2029, quant="fbar",
    value=partial$refpts$FMSY[, sel]))


# SAVE results + diagnostics
save(results, ssmases, mases, acfs, ccfs,
  file="model/partial/diagnostics.RData", compress="xz")


# --- SUBSET

idx <- results$sel

results <- results[idx,]
sr <- iter(partial$sr, idx)
refpts <- partial$refpts[, idx]
indices <- FLIndices(lapply(partial$indices, iter, idx))

stock <- partial$stock


# tests.R: TEST full model (2-sex, 4 seasons)


# --- SIMPLIFY season

# DEBUG stock <- simplify(iter(partial$stock, idx), "season")
# TODO fwd(2018-19)

# stock <- noseason(stock)

stock <- iter(stock, idx)
range(stock) <- c(minfbar=1, maxfbar=12)

# tests.R: TEST 2-sex model

save(stock, sr, refpts, results, indices, file="model/partial_sexes.RData",
  compress="xz")


# --- SIMPLIFY sex

# stock <- nounit(stock)
stock <- fut
range(stock) <- c(minfbar=1, maxfbar=12)

# DROP sratio, defaults to 1

params(sr) <- params(sr)[1:3,]

# indices: index. index.q (index.var), unit=1, season 1

indices[[3]] <- FLIndexBiomass(name=name(indices[[1]]), index=index(indices[[1]])[,,1,1])
index.q(indices[[3]])[] <- index.q(indices[[1]][,,1,1])

indices[[4]] <- FLIndexBiomass(name=name(indices[[2]]), index=index(indices[[2]])[,,1,1])
index.q(indices[[4]])[] <- index.q(indices[[2]][,,1,1])

indices <- FLIndices(LLCPUE1=indices[[3]], LLCPUE3=indices[[4]])

# tests.R: TEST 2-sex model

save(stock, sr, refpts, results, indices, file="model/partial_single.RData",
  compress="xz")

