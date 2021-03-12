# zzz.R - DESC
# /zzz.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# CREATE data.table of production functions

production <- function(K, shape, r, p) {

  prodfun <- function (b, r, k, p) {
     t1 = b * (r / p)
     t3 = (b / k) ^ p
     t1 * (1 - t3)
  }

  bio <- seq(1e-4, 1, by=0.01)
  pro <- prodfun(bio * K, r, K, p)

  return(data.frame(SSB=bio * K, Yield=pro))
}

prods <- rbindlist(lapply(seq(dim(res)[1]), function(x)
  do.call(production, as.list(res[x, .(K, shape, r, p)]))), idcol="iter")



# --- REGRESSION trees

library(rpart)
library(rpart.plot)

# status

regssb <- rpart(SSB_status ~ M + sigmaR + steepness + cpues + lfreq + llq,
  data = res, maxdepth = 4)

rpart.plot(regssb)

# process error

regerr <- rpart(Recr_sigma ~ M + sigmaR + steepness + cpues + lfreq + llq,
  data = res, maxdepth = 4)

rpart.plot(regerr)

# MASE p-value
regmpv <- rpart(pvalue ~ M + sigmaR + steepness + cpues + lfreq + llq,
  data = res, maxdepth = 4)

rpart.plot(regmpv)










# --- PLOT oms ~ SA

load("model/base.RData")
sa <- simplify(base$stock)

plot(FLStocks(SA=sa, OM=oms))


# --- PLOTS SBMSY(oms) ~ factors

# M, sigmaR, steepness, cpues, lfreq, llq

sbmsy <- base$results$SSB_endyr / base$results$SSB_MSY

ggplot(res, aes(x=factor(M), y=SSB_status, fill=factor(M))) +
  geom_hline(yintercept=1, color="#2f2f2f", linetype=2) +
  geom_hline(yintercept=sbmsy, color="red", linetype=1) +
  geom_boxplot() +
  geom_point(data=res[SSB_status > 2,]) +
  xlab("M") + ylab(expression(SSB[2017] / SSB[MSY])) +
  theme(legend.position="none") +
  facet_wrap(~sel)

ggplot(res, aes(x=factor(sigmaR), y=SSB_status, fill=factor(sigmaR))) +
  geom_hline(yintercept=1, color="#2f2f2f", linetype=2) +
  geom_hline(yintercept=sbmsy, color="red", linetype=1) +
  geom_boxplot() +
  geom_point(data=res[SSB_status > 2,]) +
  xlab("sigmaR") + ylab(expression(SSB[2017] / SSB[MSY])) +
  theme(legend.position="none") +
  facet_wrap(~sel)


# PLOT box-plot Full ~ Subset (select) for major metrics

# CREATE data.table value ~ metric + select

dat <- melt(res[, .(SSB_Virgin, SSB_status, sigma_Rec, sel)],
  variable.name="metric",
  measure=c("SSB_Virgin", "SSB_status", "sigma_Rec"))

dat[, group:=ifelse(sel > 0, "Full", "Subset")]

ggplot(dat, aes(x=group, y=value, fill=group)) + geom_boxplot() +
  facet_wrap(~metric, scales="free")



mapply(function(x, y), alldevs, samps)




# --- CHECK outputs


outs <- corners$output

ssbs <- FLQuants(lapply(corners$output, extractSSB))

sbmsy <- lapply(corners$results$SSB_MSY, FLPar)

ssbs <- FLQuants(Map("/", ssbs, sbmsy))

ggplot(ssbs, aes(x=year, y=data)) + geom_line(aes(group=qname)) +
  geom_line(data=as.data.frame(ssb(base$stock)[,,1,1] / base$results$SSB_MSY),
    colour="red") +
  geom_hline(yintercept=1, colour="red", linetype=2)

dssbs <- data.table(as.data.frame(ssbs))
dssbs[, iter:=as.numeric(qname)]

dat <- merge(dssbs, res, by="iter")
datbase <- as.data.frame(ssb(base$stock)[,,1,1] / base$results$SSB_MSY)

ggplot(dat, aes(x=year, y=data)) +
  geom_line(aes(group=iter, colour=factor(M),linetype=factor(cpues))) +
  facet_grid(sigmaR + steepness ~ lfreq + llq,
    labeller = labeller(.cols=label_both, .rows = label_both)) +
  xlab("") + ylab("SSB (1000 t)") +
  geom_line(data=datbase, colour="black") +
  geom_hline(yintercept=1, linetype=2)

#
res[sigmaR == 0.4 & steepness == 0.9 & cpues == 14 & lfreq == 1 & llq == 1.01 & M == 0.35,]

plot(FLQuants(corner54=residuals(corners$sr)[,,,,,54], base=residuals(base$sr))) +
  geom_vline(xintercept=seq(1978,2017), alpha=0.2) + xlim(c(1977, 2017)) 


# --- CHECK SSB

ref <- ibind(FLQuants(lapply(outs, extractSSB)))

fsb <- ssb(corners$stock)[,,1,1]

fsbs <- ssb(simplify(corners$stock))

range(ref/fsb)
range(ref/fsbs)


# ---

original <- corners$stock

stock <- simplify(original)

srr <- corners$sr

save(results, stock, original, srr, file="model/corners/load.RData", compress="xz")


# -----------------

load("model/corners.RData")

dat <- merge(as.data.frame(ssb(stock)), results, by="iter")
datbase <- as.data.frame(ssb(base$stock[,, "F", 1]))

ggplot(dat, aes(x=year, y=data / 1000)) +
  geom_line(aes(group=iter, colour=factor(M),linetype=factor(cpues))) +
  facet_grid(sigmaR + steepness ~ lfreq + llq,
    labeller = labeller(.cols=label_both, .rows = label_both)) +
  xlab("") + ylab("SSB (1000 t)") +
  geom_line(data=datbase, colour="black")


dirs <- list.dirs('model/corners', rec=FALSE)

outs <- lapply(dirs, readOutputss3)



# --- INSPECT variability by factor & level

library(patchwork)

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

plot(iter(stks, -c(3,7))) +
  geom_worm(data=metrics(iter(stks, idx)))


x <- FLStocks(lapply(setNames(nm=dimnames(stks)$iter), function(x) iter(stks, x)))

plot(x) + theme(legend.position="none")
nolegend <- theme(legend.position="none")
plot(x) + nolegend


# --- COMPARE emergent properties

ggplot(results, aes())


# TODO PLOT mat, selex

# GET FLBRP run

library(FLBRP)

pars <- base$sr@params

abPars("bevholt", spr0=c(pars$v/pars$R0), s=c(pars$s), v=c(pars$v))

rrps <- brp(FLBRP(simplify(base$stock),
  sr=list(model=bevholt()$model, params=FLPar(a=16129, b=7905))))

plot(rrps, refpts=c("msy", "crash", "f0.1", "virgin", "spr.30"))


# TODO THINK about SSB bump

# TODO CHECK production

production <- function(dat) {

  prodfun <- function (b, r, k, p) {
     t1 = b * (r / p)
     t3 = (b / k) ^ p
     t1 * (1 - t3)
  }

  # k = SSB_unfished
  k <- dat$SSB_unfished
  # shape = SB[MSY] / k
  shape <- dat$SSB_MSY / k
  # p
  p <- optimise(function(x, y) (y-(1 / (1 + x)) ^ (1 / x)) ^ 2,
    c(-0.9999,  10),y=shape)$minimum
  # r = (1 + p) * (HR[MSY])
  r <- (1 + p) * (dat$Dead_Catch_MSY / dat$SSB_MSY)

  bio <- seq(1e-4, 1, by=0.01)
  pro <- prodfun(bio * dat$SSB_unfished, r, k, p)

  return(list(data.frame(SSB=bio * k, Yield=pro), c(r=r, K=k, shape=shape)))
}

pres <- rbindlist(lapply(setNames(nm=results$iter),
  function(x) production(results[iter == x,])[[1]]), idcol="iter")
pres[, iter:=as.numeric(iter)]

pres <- merge(results, pres, by='iter')

ggplot(pres, aes(SSB, Yield, group=iter)) + geom_line(alpha=0.4) +
  facet_wrap(~steepness)

ggplot(pres, aes(SSB, Yield, group=iter)) + geom_line(alpha=0.4) +
  facet_wrap(~M)

ggplot(pres, aes(SSB, Yield, group=iter)) + geom_line(alpha=0.4) +
  facet_grid(steepness + M ~ lfreq + sigmaR)

#

outs <- lapply(setNames(res$id, nm=res$iter), function(x) {
  readOutputss3(file.path("model", "corners", x))
    })

out <- readOutputss3("model/base")

ssbs <- lapply(outs, extractSSB)
ssb <- extractSSB(out)




#

dat <- data.table(as.data.frame(sp(stks)))
dat[, iter:=as.numeric(iter)]

dat <- merge(dat, res, by='iter')

ggplot(dat, aes(x=year, y=data, group=iter)) + geom_line(alpha=0.5) +
  facet_wrap(~M)

ggplot(dat, aes(x=year, y=data, group=iter)) + geom_line(alpha=0.5) +
  facet_wrap(~sigmaR)

ggplot(dat, aes(x=year, y=data, group=iter)) + geom_line(alpha=0.5) +
  facet_wrap(~steepness)

ggplot(dat, aes(x=year, y=data, group=iter)) + geom_line(alpha=0.5) +
  facet_wrap(~cpues)

ggplot(dat, aes(x=year, y=data, group=iter)) + geom_line(alpha=0.5) +
  facet_wrap(~lfreq)

ggplot(dat, aes(x=year, y=data, group=iter)) + geom_line(alpha=0.5) +
  facet_wrap(~llq)


# LOAD kobe, COMPARE to MVLN base

kobes <- foreach(i=dirs) %dopar% {
  cat("[", i, "]\n")
  readKobess3(i)
}

save(kobes, file="model/corners/kobes.RData", compress="xz")


kobe2019 <- rbindlist(lapply(kobes, function(x)
  data.table(B.BMSY=c(x$B.BMSY[,'2019']), F.FMSY=c(x$F.FMSY[,'2019']))))

kobe2019 <- cbind(kobe2019, grid)

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
