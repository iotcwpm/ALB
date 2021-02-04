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

corners <- lapply(full, function(x) c(x[1], x[length(x)]))

nsam <- prod(unlist(lapply(corners, length)))

grid_corners <- setioalbgrid(corners, dir = "model/corners",
  base = "data/PSLFwt/CPUE_SouthWest", name = "abt", write=FALSE)

save(grid_corners, file="model/corners/grid.RData")

# RUN models

# ls | parallel -j35 --bar --progress '(cd {}; ss_3.30.15)'
# parallel --jobs 35 --progress 'cd {} && ss3' ::: *

# LOAD results

load("model/corners/grid.RData")

oms_corners <- loadOMS(dir="model/corners", grid=grid_corners)

res_corners <- oms_corners$results

stk_corners <- oms_corners$stock

stks_corners <- simplify(stk_corners)

srr_corners <- oms_corners$sr

save(res_corners, stk_corners, stks_corners, srr_corners,
  file="model/corners.RData")


# --- DIAGNOSTICS

load("model/corners.RData")

# 1. FIND unrealistic values (SSB_Virgin > 1e7 t)

id1 <- res_corners$SSB_Virgin > 1e7

# 2. CHECK convergence < 1e-4

id2 <- res_corners$Convergence_Level > 1e-4

# 3. COMPUTE retrospective Mohn's rho

# ls | parallel -j20 --bar --progress '(cd {}; cd retro; for d in ./*/ ; do (cd "$d" && ss_3.30.15 && packss33run); done)'

dirs <- list.dirs('model/corners', rec=FALSE)

# lapply(dirs, prepareRetro)

# LOAD retros TODO packss3run retro folders

retros <- lapply(setNames(dirs, nm=seq(length(dirs))), function(x) {
  
  rdirs <- setNames(c(x, as.list(list.dirs(file.path(x, "retro"),
    recursive=FALSE))), nm=seq(0, 5))

  rretro <- foreach(i=rdirs) %dopar% readOutputss3(i)

  return(SSsummarize(rretro, verbose=FALSE))
  }
)

save(retros, file="model/corners/retros.RData", compress="xz")

# COMPUTE Mohn's rho

mrhos <- lapply(retros, SSmohnsrho, startyr=2013, verbose=FALSE)

# ADD to res
res_corners[, mrho:=unlist(lapply(mrhos, '[[', 'AFSC_Hurtado_SSB'))]


# 4. COMPUTE hcxval prediction skill

library(ss3diags)

mases <- rbindlist(foreach(x=retros) %dopar% {
  ma <- rbindlist(lapply(1:4, function(y) SSmase(x, Season=y, verbose=FALSE)))
  ma[!is.na(MASE), .(mase=mean((MAE.PR * n.eval) / (MAE.base * n.eval), na.rm=TRUE)),
  by=Index]
  }, idcol='iter')

mases <- merge(mases, grid_corners, by="iter")

ggplot(mases, aes(x=Index, y=mase)) + geom_boxplot(aes(fill=Index)) +
  theme(legend.position="none")

# --- SUBSET

idx <- !id1 & !id2

dres <- res_corners[!idx,]
res <- res_corners[idx,]

stk <- iter(stk_corners, idx)

stks <- iter(stks_corners, idx)

srr <- iter(srr_corners, idx)


save(res, valid, stk, srrs, file = "model/corners.RData", compress = "xz")

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



# --- INSPECT variability by factor & level


# SELECT worm iters
idx <- order(res$SSB_Virgin)
idx <- idx[round(seq(1, length(idx), length=5))]

# SSB_Virgin by factor / value

((ggplot(res, aes(x=factor(M), y=SSB_Virgin, fill=factor(M))) + geom_boxplot() +
  geom_jitter(width=0.1, alpha=0.2) + xlab("M") + ylab(expression(SSB[0])) +
  theme(legend.pos="none")) +

(ggplot(res, aes(x=factor(sigmaR), y=SSB_Virgin, fill=factor(sigmaR))) +
  geom_boxplot() + geom_jitter(width=0.1, alpha=0.2) + xlab("sigmaR") +
  ylab("") + theme(legend.pos="none")) +

(ggplot(res, aes(x=factor(steepness), y=SSB_Virgin, fill=factor(steepness))) +
  geom_boxplot() + geom_jitter(width=0.1, alpha=0.2) + xlab("steepness") +
  ylab("") + theme(legend.pos="none"))) /

((ggplot(res, aes(x=factor(cpues), y=SSB_Virgin, fill=factor(cpues))) +
  geom_boxplot() + geom_jitter(width=0.1, alpha=0.2) + xlab("cpues") +
  ylab(expression(SSB[0])) + theme(legend.pos="none")) +

(ggplot(res, aes(x=factor(lfreq), y=SSB_Virgin, fill=factor(lfreq))) +
  geom_boxplot() + geom_jitter(width=0.1, alpha=0.2) + xlab("lfreq") +
  ylab("") + theme(legend.pos="none")) +

(ggplot(res, aes(x=factor(llq), y=SSB_Virgin, fill=factor(llq))) +
  geom_boxplot() + geom_jitter(width=0.1, alpha=0.2) + xlab("llq") +
  ylab("") + theme(legend.pos="none")))


# SSB_endyr / SSB_MSY by factor / value

((ggplot(res, aes(x=factor(M), y=SSB_endyr / SSB_MSY, fill=factor(M))) +
  geom_boxplot() + geom_jitter(width=0.1, alpha=0.2) + xlab("M") +
  ylab(expression(SSB[2017]/SSB[MSY])) + theme(legend.pos="none")) +

(ggplot(res, aes(x=factor(sigmaR), y=SSB_endyr / SSB_MSY, fill=factor(sigmaR))) +
  geom_boxplot() + geom_jitter(width=0.1, alpha=0.2) + xlab("sigmaR") +
  ylab("") + theme(legend.pos="none")) +

(ggplot(res, aes(x=factor(steepness), y=SSB_endyr / SSB_MSY, fill=factor(steepness))) +
  geom_boxplot() + geom_jitter(width=0.1, alpha=0.2) + xlab("steepness") +
  ylab("") + theme(legend.pos="none"))) /

((ggplot(res, aes(x=factor(cpues), y=SSB_endyr / SSB_MSY, fill=factor(cpues))) +
  geom_boxplot() + geom_jitter(width=0.1, alpha=0.2) + xlab("cpues") +
  ylab(expression(SSB[2017]/SSB[MSY])) + theme(legend.pos="none")) +

(ggplot(res, aes(x=factor(lfreq), y=SSB_endyr / SSB_MSY, fill=factor(lfreq))) +
  geom_boxplot() + geom_jitter(width=0.1, alpha=0.2) + xlab("lfreq") +
  ylab("") + theme(legend.pos="none")) +

(ggplot(res, aes(x=factor(llq), y=SSB_endyr / SSB_MSY, fill=factor(llq))) +
  geom_boxplot() + geom_jitter(width=0.1, alpha=0.2) + xlab("llq") +
  ylab("") + theme(legend.pos="none")))

# PLOT stock w/worms 

plot(iter(stks_corners, -c(3,7))) +
  geom_worm(data=metrics(iter(stks_corners, idx)))




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


# LOAD kobe, COMPARE to MVLN base

kobes <- lapply(list.dirs("model/corners", recursive=FALSE), readKobess3)

kobe2019 <- rbindlist(lapply(kobes, function(x)
  data.table(B.BMSY=c(x$B.BMSY[,'2019']), F.FMSY=c(x$F.FMSY[,'2019']))))

kobe2019 <- cbind(kobe2019, gcorners)

ggplot(kobe2019, aes(B.BMSY, F.FMSY)) + geom_point(aes(colour=factor(M))) +
  geom_vline(xintercept=1) + geom_hline(yintercept=1)

ggplot(kobe2019, aes(B.BMSY, F.FMSY)) + geom_point(aes(colour=factor(sigmaR))) +
  geom_vline(xintercept=1) + geom_hline(yintercept=1)

ggplot(kobe2019, aes(B.BMSY, F.FMSY)) + geom_point(aes(colour=factor(llq))) +
  geom_vline(xintercept=1) + geom_hline(yintercept=1)

ggplot(kobe2019, aes(B.BMSY, F.FMSY)) + geom_point(aes(colour=factor(lfreq))) +
  geom_vline(xintercept=1) + geom_hline(yintercept=1)

ggplot(kobe2019, aes(B.BMSY, F.FMSY)) + geom_point(aes(colour=factor(cpues))) +
  geom_vline(xintercept=1) + geom_hline(yintercept=1)

ggplot(kobe2019, aes(B.BMSY, F.FMSY)) + geom_point(aes(colour=factor(steepness))) +
  geom_vline(xintercept=1) + geom_hline(yintercept=1)
