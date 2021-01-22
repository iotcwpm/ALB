# model_maineffects.R - DESC
# OM/model_maineffects.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ioalbmse)

library(doParallel)
registerDoParallel(3)

# SETUP runs for grid corners

interact <- full[c("M", "lfreq")]

nsam <- prod(unlist(lapply(interact, length)))

ginteract <- setioalbgrid(interact, dir = "model/interact",
  base = "data/PSLFwt/CPUE_SouthWest", name = "abt", write=TRUE)

save(ginteract, file="model/interact/grid.RData")

# RUN models

# ls | parallel -j8 --bar --progress '(cd {}; ss_3.30.15)'
# parallel --jobs 200 --progress 'cd {} && ss3' ::: *


# LOAD results

res <- loadRES(dir = "model/corners", grid = gcorners)

outs <- loadOUT(dir = "model/corners")

flss <- loadFLS(dir = "model/corners")

sfls <- simplify(flss)
mes <- metrics(sfls, Rec=rec, SSB=ssb, F=fbar)

plot(mes) + geom_worm(mes)


# --- DIAGNOSTICS

# CONVERGENCE

final_gradient <- res$Convergence_Level > 1e-4

res[final_gradient,]

# TODO jitter final_gradient runs

# --- CHECK values and drop if unrealistic

valid <- !(res$Convergence_Level > 1e-4 | res$SSB_Virgin > 1e7)

stks <- FLStocks(lapply(setNames(outs[valid], nm=res$iter[valid]), buildFLSss330,
  range=c(minfbar=1, maxfbar=12)))

stk <- simplify(Reduce(combine, stks))

srrs <- FLSRs(lapply(setNames(outs[valid], nm=res$iter[valid]), buildFLSRss3))

# --- COMPUTE rec var

sigma_Rec <- rep(NA, 64)
sigma_Rec[valid] <- unlist(lapply(stks, function(x) sqrt(var(c(log(rec(x)))))))
sigma_Rec[valid] <- unlist(lapply(srrs, function(x) sqrt(var(c(residuals(x)), na.rm=TRUE))))
res[, sigma_Rec:=sigma_Rec]

save(res, valid, stk, srrs, file = "model/corners.RData", compress = "xz")

# --- CHARACTERIZE noise, refpts & trends


# --- TEST weighting schemes

# MASE

# LIKELIHOOD

fit <- rpart(LIKELIHOOD ~ M + sigmaR + steepness + cpues + lfreq + llq,
  data = res, maxdepth = 4)

rpart.plot(fit)

kable(res[, .(mean_lkhd=mean(LIKELIHOOD)), by=.(cpues, lfreq, steepness)])

res[, mean(LIKELIHOOD), by=lfreq]


# AIC

aic <- function(ss) {

  Total_LL <- ss$likelihoods_used$values[1]
  N_Params <- ss$N_estimated_parameters
  AIC  <-  2 * N_Params + 2 * Total_LL

  return(data.table(ll=Total_LL,n=N_Params,aic=AIC))
}

load("model/corners/outs.RData")
aics <- rbindlist(lapply(outs, aic))

# RUN hindcast on corners



# 

stk <- loadFLS(dir = "model/corners", combine=FALSE)
stks <- lapply(stk, simplify)

# CHECK convergence
lapply(outs, function(x) x$Convergence_Level)

conv <- unlist(lapply(file.path("model/corners", gcorners$id)[-1 * notrun],
  convergencelevel))

# CHECK SSB 2018
x <- unlist(lapply(stk, function(x) ssb(x)[,"1950","F",1]))
which(x > 1e7)


plot(stks[-which(x > e5)]) + theme(legend.position="none")



save(stk, stksimp, file="model/corners.RData", compress="xz")

plot(stksimp)

dat <- metrics(simplify(stk), list(Rec=rec, SSB=ssb, B=stock, C=catch))

mf <- DT(model.frame(dat))
mf[, iter:=as.numeric(iter)]
df <- DT(as.data.frame(dat))
df[, iter:=as.numeric(iter)]

mfg <- merge(mf, gcorners, by="iter")
dfg <- merge(df, gcorners, by="iter")


ggplot(dat$SSB, aes(x=year, y=data, colour=factor(iter), group=iter)) + geom_line()

ggplot(mfg, aes(x=SSB, y=C)) + geom_line() +
  facet_grid(M + steepness ~ sigmaR + cpues, scales="free")

ggplot(dfg[qname == "SSB",], aes(x=year, y=data)) + geom_line() +
  facet_grid(M + steepness + llq ~ sigmaR + cpues + lfreq, scales="free")

# IDENTIFY runs with large B0, R0

mfg[year == 1950,]

ggplot(mfg[ year == 1950,], aes(x=Rec, y=SSB)) + geom_point() +
  facet_grid(M + steepness + llq ~ sigmaR + cpues + lfreq,
    labeller = label_both)

ggplot(mfg[ year == 1950 & M > 0.2,], aes(x=1, y=SSB)) + geom_point() +
  facet_grid(M + steepness + llq ~ sigmaR + cpues + lfreq,
    labeller = label_both) +
  geom_hline(yintercept=118000)


plot(iter(dat$SSB, c(dat$SSB[, "1950"] < 1e7)))


# RUN diagnostics
