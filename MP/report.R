# report.R - DESC
# /report.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(ioalbmse)
library(ggplotFL)

source("utilities.R")

data(iotcstats)

load("data/om.RData")

# --- model.perfect.R {{{

load("model/perfect.RData")

ggsave("report/perfect_fixedF.png",
plot(window(om, start=2000),
  res$fixedF_05green, res$fixedF_06green, res$fixedF_07green)
)

ggsave("report/perfect_catchSSB.png",
plot(window(om, start=2000),
  res$catchSSB_05green, res$catchSSB_06green, res$catchSSB_07green)
)

ggsave("report/perfect_trend.png",
plot(window(om, start=2000),
  res$trendB_05green, res$trendB_06green, res$trendB_07green)
)

# performance

perf <- rbindlist(lapply(res, performance, metrics=mets,
  refpts=refpts(om), statistics=statistics, years=list(tperiod)), idcol="mp")

perf[, mean(data), by=.(mp, statistic, name)]

perf[statistic == "S6", mean(data), by=.(mp)]

perf[, label:=paste(name, statistic)]

ggplot(perf, aes(x=factor(mp), y=data, fill=sub("_.*", "", perf$mp))) +
  geom_boxplot() + facet_wrap(~label, scales="free")

library(mseviz)

plotBPs(perf)

plotTOs(perf)

# COMPARE fixedF ~ FMSY

# PLOT

plot(om, tun)

plot(tun, metrics=list(SB=function(x) ssb(x) / refpts(om)$SBMSY)) +
  geom_hline(yintercept=1, linetype=2) +
  geom_hline(yintercept=0.10, color="red", linetype=2)

# TUNED HCR param value

args(control(tun, 'hcr'))

args(control(tun, 'hcr'))$ftrg / refpts(om)$FMSY

# CHECK performance, 0.60

performance(tun, metrics=list(SB=ssb),
  statistics=stats["S8"], years=list(2030:2035))

performance(tun, metrics=list(SB=ssb),
  statistics=stats["S8"], years=2020:2040)

# COMPARE with FMSY

plot(om, tun, FLmse(om=window(pfmsy, start=2019)))

performance(pfmsy, metrics=list(SB=ssb),
  statistics=stats["S8"], years=list(2030:2035))

# }}}

# --- test_spict.R {{{

load("test/test_spict.RData")

# obs: list of observations from perfect.oem: stk (FLStock), idx (FLIndices)
# per:  spict runs with perfect index.
# dat, dat1, dat3:  spict runs with LLCPUE1-3, LLCPUE1 and LLCPUE3.

plot(window(FLQuants(SPfox=stock(per$stk) %/% stock(per$stk)[,1],
  SPschaefer=stock(perS$stk) %/% stock(perS$stk)[,1],
  OM=stock(om) %/% stock(om)[,1]), end=2019)) +
       ylim(c(0, 2))

# perfect index ~ actual indices

mpng("report/test_spict/indices.png",
plot(FLQuants(PER=window(index(obs$idx[[1]]), start=1980),
  LLCPUE1=index(observations(oem)$idx[[1]]),
  LLCPUE3=index(observations(oem)$idx[[2]]))) +
     ylim(c(0, 1.5))
)

# perfect and actual estimates ~ stock(om)

mpng("report/test_spict/runs.png",
plot(FLQuants(PER=stock(per$stk), LLCPUE1=stock(dat1$stk), 
  LLCPUE3=stock(dat3$stk), LLCPUEs=stock(dat$stk), OM=stock(stock(om)))) +
  ylim(c(0, 10e5))
)

# perfect and actual estimates / stock(om)

ref <- window(stock(stock(om)), end=2017)

plot(FLQuants(PER=stock(per$stk) / ref, LLCPUE1=stock(dat1$stk) / ref, 
  LLCPUE3=stock(dat3$stk) / ref, LLCPUEs=stock(dat$stk) / ref)) +
geom_hline(yintercept=1, linetype=2) +
ggtitle("Model fits using perfect, 1, 3 and both over OM") +
xlim(c(2010, 2017))

ggplot(iter(FLQuants(PER=stock(per$stk) / ref, LLCPUE1=stock(dat1$stk) / ref, 
  LLCPUE3=stock(dat3$stk) / ref, LLCPUEs=stock(dat$stk) / ref), 1:15),
  aes(x=year, y=data, group=iter)) + geom_line(aes(colour=factor(iter))) +
facet_grid(qname~.) +
geom_hline(yintercept=1, linetype=2) +
ggtitle("Model fits using perfect, 1, 3 and both over OM. Iters 1 to 15.") +
xlim(c(2010, 2017))


# perfect and actual index estimates / vb(om)

sels <- lapply(observations(oem)$idx, function(x)
  expand(sel.pattern(x)[,'2017'], year=1950:2017, fill=TRUE))
sel <- unitMeans(Reduce(ubind, sels))

plot(FLQuants(PER=stock(per$stk)/ window(vb(stock(om)), end=2017),
  LLCPUE=stock(dat$stk) / vb(window(stock(om), end=2017), sel=sel),
  LLCPUE1=stock(dat$stk) / vb(window(stock(om), end=2017), sel=sels[[1]]),
  LLCPUE3=stock(dat$stk) / vb(window(stock(om), end=2017), sel=sels[[2]]),
  )) +
  geom_hline(yintercept=1, linetype=2) +
ggtitle("Ratio of abundance estimate to vulnerable biomass.")

# perfect and actual index estimates / stock(om)

plot(FLQuants(PER=stock(per$stk)/ window(stock(stock(om)), end=2017),
  LLCPUE=stock(dat$stk) / window(stock(stock(om)), end=2017),
  LLCPUE1=stock(dat1$stk) / window(stock(stock(om)), end=2017),
  LLCPUE3=stock(dat3$stk) / window(stock(stock(om)), end=2017))) +
  geom_hline(yintercept=1, linetype=2) +
ggtitle("Ratio of abundance estimate to total biomass.")

# boxplots of status over B1

ggplot(FLQuants(PER=stock(per$stk)[,'2017'] / stock(per$stk)[,1],
  LLCPUE=stock(dat$stk)[,'2017'] / stock(dat$stk)[,1],
  LLCPUE1=stock(dat1$stk)[,'2017'] / stock(dat1$stk)[,1],
  LLCPUE3=stock(dat3$stk)[,'2017'] / stock(dat3$stk)[,1],
  OM=ssb(om)[,'2017'] / ssb(om)[,1]), aes(x=qname, y=1 - data)) +
  geom_boxplot(aes(fill=qname)) + xlab("") + ylab(expression(B[2017]/B[1950])) +
  ggtitle("Estimates of estimated depletion level")

# boxplots of status over BMSY

mpng("report/test_spict/statusbmsy.png",
ggplot(FLQuants(PER=stock(per$stk)[,'2017'] / attr(per$stk, 'refpts')$BMSY,
  LLCPUE=stock(dat$stk)[,'2017'] / attr(dat$stk, 'refpts')$BMSY,
  LLCPUE1=stock(dat1$stk)[,'2017'] / attr(dat1$stk, 'refpts')$BMSY,
  LLCPUE3=stock(dat3$stk)[,'2017'] / attr(dat3$stk, 'refpts')$BMSY,
  OM=ssb(om)[,'2017'] / refpts(om)$SBMSY), aes(x=qname, y=data)) +
  geom_boxplot(aes(fill=qname)) + xlab("") + ylab(expression(B[2017]/B[MSY])) +
  theme(legend.position="none")
)

# boxplots of status over K

mpng("report/test_spict/statusk.png",
ggplot(FLQuants(PER=stock(per$stk)[,'2017'] / attr(per$stk, 'refpts')$K,
  LLCPUEs=stock(dat$stk)[,'2017'] / attr(dat$stk, 'refpts')$K,
  LLCPUE1=stock(dat1$stk)[,'2017'] / attr(dat1$stk, 'refpts')$K,
  LLCPUE3=stock(dat3$stk)[,'2017'] / attr(dat3$stk, 'refpts')$K,
  OM=ssb(om)[,'2017'] / refpts(om)$SB0), aes(x=qname, y=data)) +
  geom_boxplot(aes(fill=qname)) + xlab("") + ylab(expression(B[2017]/K)) +
  theme(legend.position="none")
)

# boxplots of K

dt <- rbindlist(lapply(FLPars(
  PER=attr(per$stk, 'refpts')$BMSY,
  LLCPUE=attr(dat$stk, 'refpts')$BMSY,
  LLCPUE1=attr(dat1$stk, 'refpts')$BMSY,
  LLCPUE3=attr(dat3$stk, 'refpts')$BMSY,
  OM=refpts(om)$SBMSY), as.data.frame), idcol="run")

ggplot(dt, aes(x=run, y=data, fill=run)) +
   geom_boxplot(aes(fill=run)) + ylab("K (t)")
 

# ROCs on B/BMSY

res <- list(PER=per, LL=dat, LL1=dat1, LL3=dat3)

labels <- ssb(om)[,'2017'] / (refpts(om)$SBMSY * 2) > 1
scores <- lapply(res, function(x) stock(x$stk)[,'2017'] / attr(x$stk, 'refpts')$BMSY)

plotROC(roc(labels, scores)) 
plotROC(roc(labels, scores[[1]])) 

# ROCs on B/B0

labels <- ssb(om)[,'2017'] / refpts(om)$SB0 > 0.20
scores <- lapply(res, function(x) stock(x$stk)[,'2017'] / attr(x$stk, 'refpts')$K)

plotROC(roc(labels, scores))

# PLOT production functions

ggplot(model.frame(FLQuants(metrics(iter(om, 1:50), list(yield=catch, production=tsb)))),
  aes(x=production, y=yield)) + geom_boxplot() + xlim(c(0,NA)) + ylim(c(0,NA))

# }}}


# model_cpue

load("model/cpue.RData")

ggsave("report/cpue_tune.png",
plot(window(om, start=2000), res)
)

lapply(res, metrics, relmets)
