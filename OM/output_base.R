# output_base.R - DESC
# /output_base.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

source("utilities.R")

load("model/base.RData")


# stock

stock <- noseason(base$stock)

# sr

sr <- base$sr

params(sr)$v <- c(ssb(stock)[,1])
params(sr)$R0 <- c(rec(stock)[,1])
params(sr)$sratio <- 1

# refpts

refpts <- base$refpts

refpts$SB0 <- c(ssb(stock)[,1])
refpts$SBMSY <- refpts$SBMSY * 2
refpts$R0 <- c(rec(stock)[,1])


om <- FLom(stock=stock, refpts=refpts, sr=sr, projection=mseCtrl(method=fwd.om))

save(om, file="output/om_base.RData", compress="xz")

