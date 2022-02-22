# model.R - DESC
# /model.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

source("utilities.R")

# progressr

#library(progressr)
#handlers(global=TRUE)

#library(doFuture)
#registerDoFuture()
#plan(multicore, workers=25)

# DATA
load("data/om.Rdata")

# DEBUG
params(sr(om)) <- FLPar(c(params(sr(om))[,1,]), dimnames=dimnames(params(sr(om)))[-2])
refpts(om)$Ftarget <- refpts(om)$FMSY 
refpts(om)$SBlim <- refpts(om)$SBMSY * 0.20

# MSE arguments

mseargs <- list(iy=2019, data_lag=2, frq=3)

# METRICS

mets <- list(Rec=function(x) unitSums(rec(x)), SB=function(x) unitSums(ssb(x)),
  C=function(x) unitSums(catch(x)), F=function(x) unitMeans(fbar(x)))

# RELATIVE metrics

relmets <- list(
  BMSY=function(x) unitSums(stock(x)) %/% refpts(x)$BMSY,
  B0=function(x) unitSums(stock(x)) %/% refpts(x)$B0,
  FMSY=function(x) unitMeans(fbar(x)) %/% refpts(x)$FMSY,
  SB0=function(x) unitSums(ssb(x)) %/% unitSums(ssb(x)[,1])
) 
