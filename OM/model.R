# model.R - DESC
# /model.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ioalbmse)
library(ss3diags)
library(icesTAF)

library(FLasher)

source("utilities.R")

# model/full

make("model_full_grid.R", "data/PSLFwt/", "model/full/")

make("model_full_load.R", "model/full/", "model/full/load.Rdata")

make("model_full_rate.R", "model/full/load.Rdata", "model/full/rate.Rdata")

make("model_full_subset.R", c("model/full/load.Rdata", "model/full/rate.Rdata"),
  "model/full.Rdata")
