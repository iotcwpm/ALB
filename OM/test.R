# tests.R - DESC
# /tests.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(FLasher)
library(FLBRP)
library(patchwork)
source("utilities.R")

box <- annotate("rect", xmin = 2018, xmax = 2050, ymin=-Inf, ymax=Inf,
  fill="#E69F00", alpha = .1)

# TEST simplify Fs {{{

load("model/full/load.Rdata")

st <- iter(full$stock, 1)
nst <- noseason(st)
ou <- full$output[[1]]

# COMPARE Z_at_age and harvest

A <- seasonSums(harvest(st))
B <- zatage(ou) - seasonSums(m(st))
C <- harvest(stock.n(st)[,,,1], seasonSums(catch.n(st)), seasonSums(m(st)))
D <- harvest(stock.n(nst), catch.n(nst), m(nst))

range(abs(A - B))
range(abs(A - C))
range(abs(B - C))
range(abs(A - D))

plot(FLQuants(A=quantMeans(A[ac(1:12)]), B=quantMeans(B[ac(1:12)]),
  C=quantMeans(C[ac(1:12)]), D=quantMeans(D[ac(1:12)])))

F <- Reduce(combine, lapply(full$output, ss3om::extractFbar))

plot(FLQuants(A=unitMeans(quantMeans(A[ac(1:12)])), F=F[,,,,,1]))

# }}}

# --- TEST SRR prediction after simplifying stock {{{

load("model/base.RData")

re <- unitSums(window(rec(base$stock), end=1970)[,,,1])

sb <- window(ssb(nounit(base$stock))[,,,1], end=1970)
sb <- window(ssb(noseason(nounit(base$stock))), end=1970)

pre <- predict(base$sr, ssb=sb)

range(pre/re)

#

params(base$sr)

rec(base$stock)[,1,,1]

predict(base$sr, ssb=ssb(base$stock)[,1,,1])

predict(base$sr, ssb=ssb(nounit(base$stock[,1,,1])))

x <- base$stock
y <- nounit(x)
z <- simplify(x, 'unit')

unitSums(stock.n(x)[,1,,1])
stock.n(y)[,1,,1]
stock.n(z)[,1,,1]

unitMeans(stock.wt(x)[,1,,1])
stock.wt(y)[,1,,1]

# DEBUG mat
mat(x)[,1,,1]
unitMeans(mat(x)[,1,,1])
mat(y)[,1,,1]
mat(z)[,1,,1]

ssb(x)[,1,,1]
ssb(y)[,1,,1]
ssb(z)[,1,,1]

# }}}

# --- TEST base as 2-sex - model_base.R {{{

load("model/base.RData")

stock <- noseason(base$stock)
sr <- base$sr
params(sr)$sratio <- 0.5
refpts <- base$refpts

eql <- FLBRP(stock)

fut <- fwdWindow(stock, eql, end=2050)

# fwd(F=0)

tes0 <- fwd(fut, sr=sr, control=fwdControl(lapply(2018:2050,
  function(x) list(year=x, quant="fbar", value=0))))

plot(tes0)

# fwd(F=FMSY)

control <- fwdControl(lapply(2018:2050, function(x)
  list(year=x, quant="fbar", value=refpts$FMSY)))

tesmsy <- fwd(fut, sr=sr, control=control)

plot(tesmsy, control)

(plot(ssb(tesmsy)[,,'F'] %/% refpts$SBMSY, control) +
  geom_hline(yintercept=1, linetype=2) + ylab(expression(SB / SB[MSY])) +
  ggtitle("fwd(fbar=FMSY)")) /

(plot(fbar(tesmsy) %/% refpts$FMSY, control) +
  geom_hline(yintercept=1, linetype=2) + ylab(expression(F / F[MSY]))) /

(plot(ssb(tesmsy)[,,'F'] %/% ssb(tesmsy)[,1,'F'], control) +
  geom_hline(yintercept=0.20, linetype=2) + ylab(expression(SB / SB[0])) +
  ylim(c(0,NA)))

# }}}

# --- TEST base as 1-sex - model_base.R {{{

load("model/base.RData")

stock <- nounit(noseason(base$stock))
sr <- base$sr
params(sr) <- params(sr)[1:3,]
refpts <- base$refpts

eql <- FLBRP(stock)

fut <- fwdWindow(stock, eql, end=2050)

# fwd(F=0) - DEBUG SSB 2030-2040 is way too high

tes0 <- fwd(fut, sr=sr, control=fwdControl(lapply(2018:2050,
  function(x) list(year=x, quant="fbar", value=0))))

plot(tes0)

control <- fwdControl(lapply(2018:2050, function(x)
  list(year=x, quant="fbar", value=refpts$FMSY)))

tesmsy <- fwd(fut, sr=sr, control=control)

plot(tesmsy, control)

(plot(ssb(tesmsy) %/% refpts$SBMSY, control) +
  geom_hline(yintercept=1, linetype=2) + ylab(expression(SB / SB[MSY])) +
  ggtitle("fwd(fbar=FMSY)")) /

(plot(fbar(tesmsy) %/% refpts$FMSY, control) +
  geom_hline(yintercept=1, linetype=2) + ylab(expression(F / F[MSY]))) /

(plot(ssb(tesmsy) %/% ssb(tesmsy)[,1], control) +
  geom_hline(yintercept=0.20, linetype=2) + ylab(expression(SB / SB[0])) +
  ylim(c(0,NA)))

# }}}

# --- TEST full model (2-sex, 4 seasons) - model_partial_load.R {{{

library(FLBRP)
library(FLasher)

load("model/partial.RData")

eql <- FLBRP(stock)

fut <- fwdWindow(stock, eql, end=2050)

# fwd(F=0) - DEBUG SSB 2030-2040 is way too high

tes0 <- fwd(fut, sr=sr, control=fwdControl(lapply(2018:2050,
  function(x) list(year=x, season=1:4, quant="fbar", value=0))))

plot(window(rec(tes0), start=2010)) + facet_wrap(~season)

# DEBUG WRONG!!

stock.n(tes0)[1:3, '2017',,4]
stock.n(tes0)[1:3, '2018',,1]
stock.n(tes0)[1:3, '2018',,2]
stock.n(tes0)[1:3, '2018',,3]

# }}}

# --- TEST 2-sex model - model_partial_load.R {{{

load("model/partial.RData")

eql <- FLBRP(stock)

fut <- fwdWindow(stock, eql, end=2050)
discards.n(fut) <- 0
range(fut) <- c(minfbar=1, maxfbar=12)

# HINDCAST at fbar=0

hin <- fwd(fut, sr=sr, control=fwdControl(lapply(1977:2017,
  function(x) list(year=x, quant="fbar", value=0.05))))

plot(window(hin, end=2017))

# FWD(fbar=0)

tes0 <- fwd(fut, sr=sr, control=fwdControl(lapply(2017:2050,
  function(x) list(year=x, quant="fbar", value=0.01))),
  deviances=stock(fut) %=% 1)

plot(tes0) + ggtitle("fwd(fbar=0)")

verify(tes0)

# FWD(fbar=FMSY)

control <- fwdControl(lapply(2017:2050,
  function(x) list(year=x, quant="fbar", value=refpts$FMSY)))

tesmsy <- fwd(fut, sr=sr, control=control)

plot(tesmsy, control)

(plot(ssb(tesmsy)[,,'F'] %/% refpts$SBMSY, control) +
  geom_hline(yintercept=1, linetype=2) + ylab(expression(SB / SB[MSY])) +
  ggtitle("fwd(fbar=FMSY)")) /

(plot(fbar(tesmsy) %/% refpts$FMSY, control) +
  geom_hline(yintercept=1, linetype=2) + ylab(expression(F / F[MSY])))

(plot(ssb(tesmsy)[,,'F'] %/% ssb(tesmsy)[,1,'F'], control) +
  geom_hline(yintercept=0.20, linetype=2) + ylab(expression(SB / SB[0])) +
  ylim(c(0,NA)))

plot(catch(tesmsy) / vb(tesmsy)) + box + geom_hline(yintercept=1, colour="red")

# }}}

# --- TEST 1-sex model - model_partial_load.R {{{

load("model/partial_single.RData")

# INSPECT runs

# WHAT is behind low SB_2017 values?

ggplot(results, aes(SSB_status)) + geom_histogram()

ggplot(results, aes(SSB_Virgin, SSB_status)) + geom_point()

library(rpart)
library(rpart.plot)

regsb <- rpart(SSB_status ~ M + sigmaR + steepness + cpues + lfreq + llq,
  data = results, maxdepth = 4)
rpart.plot(regsb)

regr <- rpart(r ~ M + sigmaR + steepness + cpues + lfreq + llq,
  data = results, maxdepth = 4)
rpart.plot(regr)

# llq
ggplot(results, aes(x=factor(llq), y=SSB_status, fill=factor(llq))) +
  geom_boxplot() + guides(fill=FALSE)

# lfreq
ggplot(results, aes(x=factor(lfreq), y=SSB_status, fill=factor(lfreq))) +
  geom_boxplot() + guides(fill=FALSE)

# steepness
ggplot(results, aes(x=factor(steepness), y=SSB_status, fill=factor(steepness))) +
  geom_boxplot() + guides(fill=FALSE)



# HINDCAST at fbar=0

eql <- FLBRP(stock)

fut <- fwdWindow(stock, eql, end=2050)

hin <- fwd(fut, sr=sr, control=fwdControl(lapply(1977:2017,
  function(x) list(year=x, quant="fbar", value=0))))

plot(window(hin, end=2017))

# FWD(fbar=0)

tes0 <- fwd(fut, sr=sr, control=fwdControl(lapply(2017:2050,
  function(x) list(year=x, quant="fbar", value=0.001))))

plot(tes0) + ggtitle("fwd(fbar=0)")

verify(tes0)

# FWD(fbar=FMSY)

tesmsy <- fwd(fut, sr=sr, control=fwdControl(lapply(2017:2050,
  function(x) list(year=x, quant="fbar", value=refpts$FMSY))))

plot(tesmsy) + box

(plot(ssb(tesmsy) %/% refpts$SBMSY) + box +
  geom_hline(yintercept=1, linetype=2) + ylab(expression(SB / SB[MSY])) +
  ggtitle("fwd(fbar=FMSY)")) /

(plot(fbar(tesmsy) %/% refpts$FMSY) + box +
  geom_hline(yintercept=1, linetype=2) + ylab(expression(F / F[MSY]))) /

(plot(ssb(tesmsy) %/% ssb(tesmsy)[,1]) + box +
  geom_hline(yintercept=0.20, linetype=2) + ylab(expression(SB / SB[0])) +
  ylim(c(0,NA)))

plot(catch(tesmsy) / vb(tesmsy)) + box + geom_hline(yintercept=1, colour="red")

# }}}

# --- TEST 1-sex OM - output.R {{{

load("output/om_single.RData")

# FWD(fbar=0)

tes0 <- fwd(om, control=fwdControl(lapply(2017:2040,
  function(x) list(year=x, quant="fbar", value=0.001))),
  deviances=deviances$MOV)

plot(tes0) + ggtitle("fwd(fbar=0)") + box

# FWD(fbar=FMSY)

tesmsy <- fwd(om, control=fwdControl(lapply(2017:2040,
  function(x) list(year=x, quant="fbar", value=refpts(om)$FMSY))),
  deviances=deviances$MOV)

plot(tesmsy) + ggtitle("fwd(fbar=FMSY)") + box

(plot(ssb(tesmsy) %/% refpts(om)$SBMSY) + box +
  geom_hline(yintercept=1, linetype=2) + ylab(expression(SB / SB[MSY])) +
  ggtitle("fwd(fbar=FMSY)")) /

(plot(fbar(stock(tesmsy)) %/% refpts(om)$FMSY) + box +
  geom_hline(yintercept=1, linetype=2) + ylab(expression(F / F[MSY]))) /

(plot(ssb(tesmsy) %/% ssb(tesmsy)[,1]) + box +
  geom_hline(yintercept=0.20, linetype=2) + ylab(expression(SB / SB[0])) +
  ylim(c(0,NA)))

plot(catch(tesmsy) / vb(stock(tesmsy))) + box + geom_hline(yintercept=1, colour="red")

# }}}

# COMPARE deviances from FLStock & SS - output.R {{{

library(ss3om)
library(ggplotFL)

load("model/partial/load.RData")
load("model/partial.RData")

idx <- dimnames(stock)$iter

# CALCULATE deviances in FLR

pred <- predict(sr, ssb=ssb(stock)[, ac(1975:2015)])
pred[,,'M'] <- pred[,,'F'] / 2
pred[,,'F'] <- pred[,,'F'] / 2

fldevs <- log(rec(stock)[, ac(1975:2015)]) - log(pred)
alldevs <- residuals(sr)[, ac(1975:2015)]

# COMPARE FLR predicitions, rec & bias corrected

recss <- rbindlist(lapply(out, "[[", "recruit"), idcol="iter")

dif <- recss[era == "Main", .(iter, Yr, exp_recr, bias_adjusted, pred_recr, dev)]

# FLSR prediction
fp <- data.table(as.data.frame(unitSums(pred)))[, .(iter, year, data)]
dif[, flr_pred:=fp$data]

# FLStock rec
fr <- data.table(as.data.frame(unitSums(rec(stock))[, ac(1975:2015)]))[, .(iter, year, data)]
dif[, stk_rec:=fr$data]

# FLSR deviates
fd <- data.table(as.data.frame(alldevs[,,'F']))[, .(iter, year, data)]
dif[, flr_dev:=fd$data]

# FLR pred matches exp_recr

ggplot(dif, aes(flr_pred, exp_recr)) + geom_point() +
  geom_abline(slope=1, intercept=0)

ggplot(dif, aes(flr_pred, bias_adjusted)) + geom_point() +
  geom_abline(slope=1, intercept=0)

ggplot(dif, aes(flr_pred, pred_recr)) + geom_point() +
  geom_abline(slope=1, intercept=0)

ggplot(dif, aes(bias_adjusted, pred_recr)) + geom_point() +
  geom_abline(slope=1, intercept=0)

ggplot(dif, aes(flr_dev, dev)) + geom_point() +
  geom_abline(slope=1, intercept=0)

# TODO Do we need bias adjustment in prediction?

# SS dev is log(pred_recr) - log(bias_adjusted)
dif[, test:=log(pred_recr) - log(bias_adjusted)]

# FLR dev is log(pred_recr) - log(exp_recr)
dif[, test2:=log(pred_recr) - log(exp_recr)]

# }}}

# --- TEST for non-stationarity {{{

# FLife::rod

rodFn=FLife:::rodFn

res <- iter(window(residuals(base$sr), start=1975), 3)
res <- iter(window(srresid, start=1975), 3)

dat=rod(res)
ggplot(res)+
  geom_line(aes(year,data),col="red")+
  geom_polygon(aes(year,data,group=regime),fill="blue",alpha=.4,data=dat)+
  theme_bw()



# }}}

# --- TEST index.q {{{

load('model/base.RData')

out <- base$out

cpue <- data.table(out$cpue)

# COMPARE cpue$Exp ~ indices@index

(plot(lapply(base$indices[1:4], index)) + ylim(c(0,NA))) +

(ggplot(cpue[Fleet %in% 12:15], aes(x=ISOdate(Yr, Seas * 3 -1, 1), y=Exp)) +
  geom_line(aes(colour=factor(Fleet))) + facet_grid(Fleet~., scales="free") +
  ylim(c(0,NA)))

# CAN I obtain Exp from VB and index.q?

# Exp = Vuln_bio * Calc_Q
cpue[, Calc := Vuln_bio * Calc_Q]

# cpue$Calc_Q ~ indices@index.q

cpue[Fleet == 12, Calc_Q]
as.data.frame(index.q(base$indices[[1]]))$data

# DEBUG How to RECREATE VB 1979?

cpue[Fleet == 12 & Yr == 1979, Vuln_bio]

c(unitSums(quantSums(
stock.n(base$stock[, '1979']) * stock.wt(base$stock[, '1979']) * sel.pattern(base$indices[[1]])[, '1979']
)))

c(unitSums(vb(window(base$stock, start=1979, end=2017),
  sel=sel.pattern(base$indices[[1]]))[,'1979',,]))


# }}}

# SORT {{{

# INSPECT final set of runs

range(catch(om)[,'2017'] / vb(stock(om))[,'2017'])

range_ssb <- lapply(seq(1950, 2019), function(x)
  range(ssb(runfmsy)[,ac(x)]))

range_sbmsy <- Reduce(append, lapply(seq(1950, 2019), function(x) ssb(runfmsy)[,ac(x)] %/% refpts(runfmsy)$SBMSY))


# --- model_full {{{

load("output/om_full.RData")

# F=0

fut_f0 <- fwd(om, control=fwdControl(year=2020:2040, quant="fbar", value=0.001))

plot(fut_f0)

# F = FMSY

control <- fwdControl(
  lapply(2020:2040, function(x) list(year=x, quant="fbar", value=c(refpts(om)$FMSY))))

fut_fmsy <- fwd(om, control=control)

# 

all.equal(c(fbar(iter(window(fut_fmsy, start=2020, end=2020), 43))),
  control@iters[1,2,43])

all.equal(c(fbar(iter(window(fut_fmsy, start=2020, end=2020), 43))),
  control@iters[1,2,43], tolerance=1e-12)


# }}}

# TEST outputs

# F too high
fbar(stock)[,ac(2016:2019)]

fbs <- Reduce(combine, lapply(full$output, extractFbar))

# catch, vb and hr OK
catch(stock)[,ac(2016:2019)]
vb(stock)[,ac(2016:2019)]
catch(stock)[,ac(2016:2019)] / vb(stock)[,ac(2016:2019)]

# rec OK
rec(stock)[,ac(2016:2019)]

harvest(stock)[,ac(2016:2019)]

stock.n(stock)[,ac(2016:2019)]

rf <- harvest(stock.n(stock)[,ac(2016:2019)], catch.n(stock)[,ac(2016:2019)], m(stock)[,ac(2016:2019)])

rf / harvest(stock)[,ac(2016:2019)]

tf <- harvest(stock.n(stock), catch.n(stock), m(stock))
utf <- harvest(unitSums(stock.n(stock)), unitSums(catch.n(stock)), unitMeans(m(stock)))

tf1 <- harvest(stock.n(stock)[,,,1], catch.n(stock)[,,,1], m(stock)[,,,1])
tf2 <- harvest(stock.n(stock)[,,,2], catch.n(stock)[,,,2], m(stock)[,,,2])
#

stock <- fwd(stock, sr=sr, fbar=expand(unitMeans(fbar(stock))[,'2017'], year=2018:2019))

plot(stock)

# }}}
