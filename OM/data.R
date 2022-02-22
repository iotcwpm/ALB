# data.R - Run and load ALB WPTmT 2019 SS3 runs
# ALB/OM/data.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ss3om)
library(ss3diags)


# --- RUN


# --- LOAD base case runs

nwe <- readFLSss3("model/assessment/PSLFwt/CPUE_NorthWest",
  range = c(minfbar = 1, maxfbar = 12))
swe <- readFLSss3("model/assessment/PSLFwt/CPUE_SouthWest",
  range = c(minfbar = 1, maxfbar = 12))
nwf <- readFLSss3("model/assessment/LLselectFix/CPUE_NorthWest",
  range = c(minfbar = 1, maxfbar = 12))
swf <- readFLSss3("model/assessment/LLselectFix/CPUE_SouthWest",
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

save(runs, indices, srr, rps, out, file="data/runs.RData", compress="xz")

# --- TODO RESULTS 2016

load("data/2016/results.RData")

# COMPARE full ~ corners: ssb0, ssbmsy, fmsy, r, shape

library(patchwork)

th0 <- theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
th1 <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  axis.title.y = element_blank())

ggsave("report/data/corners_2016-full.png", width=12, height=3,

# SB0
((ggplot(results, aes(y=ssb0)) + geom_boxplot(fill="red") + xlab("Full grid") +
  ylab("") + ggtitle(expression(SSB[0] (t))) + ylim(c(0, 250000)) + th0) +
(ggplot(results[what == "corners"], aes(y=ssb0)) + geom_boxplot(fill="red") +
 th0 + th1 + xlab("Corners") + ylim(c(0, 250000)))) +

# SBMSY
((ggplot(results, aes(y=ssbmsy)) + geom_boxplot(fill="green") + xlab("Full grid") +
  ylab("") + ggtitle(expression(SSB[MSY] (t))) + ylim(c(0, 50000)) + th0) +
(ggplot(results[what == "corners"], aes(y=ssbmsy)) + geom_boxplot(fill="green") +
 th0 + th1 + xlab("Corners") + ylim(c(0, 50000)))) +

# FMSY
((ggplot(results, aes(y=fmsy)) + geom_boxplot(fill="orange") + xlab("Full grid") +
  ylab("") + ggtitle(expression(F[MSY])) + ylim(c(0, 0.5)) + th0) +
(ggplot(results[what == "corners"], aes(y=fmsy)) + geom_boxplot(fill="orange") +
 th0 + th1 + xlab("Corners") + ylim(c(0, 0.5)))) +

# R
((ggplot(results, aes(y=r)) + geom_boxplot(fill="cyan") + xlab("Full grid") +
  ylab("") + ggtitle(expression(r)) + ylim(c(0, 1)) + th0) +
(ggplot(results[what == "corners"], aes(y=r)) + geom_boxplot(fill="cyan") +
 th0 + th1 + xlab("Corners") + ylim(c(0, 1)))) +

# shape
((ggplot(results, aes(y=shape)) + geom_boxplot(fill="yellow") + xlab("Full grid") +
  ylab("") + ggtitle(expression(shape)) + ylim(c(0, 0.5)) + th0) +
(ggplot(results[what == "corners"], aes(y=shape)) + geom_boxplot(fill="yellow") +
 th0 + th1 + xlab("Corners") + ylim(c(0, 0.5)))) +

plot_layout(nrow = 1, width=0.5)
)
