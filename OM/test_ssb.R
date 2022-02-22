# test_ssb.R - DESC
# /test_ssb.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ioalbmse)

source("utilities.R")

load('model/full/load.RData')

# SSB

res <- full$results
stk <- full$stock


# SS

res$SSB_first

# FLStock

c(ssb(stk)[,1,1,1])

# nounit: 

c(quantSums(unitSums(stock.n(stk)[,1,,1] * stock.wt(stk)[,1,,1] * mat(stk)[,1,,1])))

ustk <- nounit(stk[,1])


ns <- stock.n(stk)[,1,1,1] + stock.n(stk)[,1,2,1]

ws <- (stock.wt(stk)[,1,1,1] * stock.n(stk)[,1,1,1] +
  stock.wt(stk)[,1,2,1] * stock.n(stk)[,1,2,1]) / ns

c(quantSums(ns * ws * mat(stk)[,1,1,1]) / 2)

# refpts

refpts$SB0 / ssb(stock)[,1]
refpts$B0 / stock(stock)[,1]
refpts$R0 / rec(stock)[,1]

library(FLBRP)

srp <- FLPar(s=params(sr)$s, v=params(sr)$v, spr0=params(sr)$s / params(sr)$R0)

srpab <- abPars('bevholt', s=params(sr)$s, v=params(sr)$v, spr0=params(sr)$s / params(sr)$R0)

predict(predictModel(model=bevholt()$model, params=FLPar(a=c(srpab$a), b=c(srpab$b))), ssb=ssb(stock))

# noseason

sstk <- noseason(stk[,1])

c(ssb(sstk)[,1,1,1])

# sex ratio

stock(stk)[,,1,1] / stock(stk)[,,2,1]


stock.n(stk)[,1,1,1] * mat(stk)[,1,1,1]
stock.n(stk)[,1,2,1] * mat(stk)[,1,1,1]
