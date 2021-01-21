# test.R - DESC
# /test.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# --- TEST SA runs make sense when loaded in FLR (swf)

swf <- readFLSss3("data/LLselectFix/CPUE_SouthWest",
  range = c(minfbar = 1, maxfbar = 12))
oswf <- readOutputss3("data/LLselectFix/CPUE_SouthWest")

sswf <- simplify(swf)

# DEBUG

plot(FLQuants(r4ss=extractFbar(oswf), flr=fbar(sswf))) +
  ylim(c(0, 0.015))

plot(FLQuants(r4ss=extractSSB(oswf), flr=ssb(sswf))) 

plot(FLQuants(r4ss=extractRec(oswf), flr=rec(sswf))) 
