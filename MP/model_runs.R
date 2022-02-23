# model_runs.R - DESC
# /model_runs.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


source("model.R")

# load statistics
data(statistics)

# TPERIOD, last OM year + 11:15
tperiod <- list(2034:2039)

runs <- list()


# fwd(F=0.001)

runs$f0 <- fwd(om, control=fwdControl(year=2020:2040, quant="fbar",
  value=0.001), deviances=residuals(sr(om)))

performance(runs$f0, metrics=mets, statistics=statistics["green"],
  years=tperiod)
