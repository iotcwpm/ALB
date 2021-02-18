# data.R - Load and inspect ALB WPTmT 2019 SS3 runs
# ALB/OM/data.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# IN
# - SS3 runs, bootstrap/data/*

# OUT
# - FLR objects, data/sa.Rdata


library(icesTAF)
library(ss3om)
library(ss3diags)

# COPY bootstrap/data to data

mkdir("data")
cp("bootstrap/data/*", "data")

# LOAD base case output

out <- lapply(list.dirs(file.path("data", c("PSLFwt", "LLselectFix")),
  rec=FALSE), readOutputss3)

# LOAD base case runs

nwe <- readFLSss3("data/PSLFwt/CPUE_NorthWest",
  range = c(minfbar = 1, maxfbar = 12))
swe <- readFLSss3("data/PSLFwt/CPUE_SouthWest",
  range = c(minfbar = 1, maxfbar = 12))
nwf <- readFLSss3("data/LLselectFix/CPUE_NorthWest",
  range = c(minfbar = 1, maxfbar = 12))
swf <- readFLSss3("data/LLselectFix/CPUE_SouthWest",
  range = c(minfbar = 1, maxfbar = 12))

runs <- FLStocks(NWE=nwe, SWE=swe, NWF=nwf, SWF=swf)

# LOAD indices

indices <- readFLIBss3("data/PSLFwt/CPUE_SouthWest")

# LOAD SRR, refpts

srr <- readFLSRss3("data/PSLFwt/CPUE_SouthWest")
rps <- readFLRPss3("data/PSLFwt/CPUE_SouthWest")

# LOAD SS_output for base case

out <- readOutputss3("data/PSLFwt/CPUE_SouthWest")

# SAVE

save(runs, indices, srr, rps, out, file="data/data.RData", compress="xz")
