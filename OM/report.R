# output.R - DESC
# ALB/OM/output.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(ggplotFL)
library(ss3diags)

library(knitr)

dir.create("report")

# --- data {{{

# data/runs: runs, indices, srr, rps, out
load("data/runs.RData")
# data/2014/sa: sa2014
load("data/2016/base.RData")

range(base, c('minfbar', 'maxfbar')) <- c(1,12)

runs <- lapply(runs[c("NWE", "SWE", "NWF")], simplify)
runs[["2014"]] <- base

dir.create("report/data")

# runs_wp stock plot

ggsave(file = "report/data/runs_wp.png",
plot(runs[1:3])
)

# runs_wp[c("NWE", "SWE", "NWF")] + 2014 stock plot
ggsave(file = "report/data/runs_wp_2014.png",
plot(runs, metrics=list(Rec=rec, SSB=ssb, F=fbar))
)

# indices plot

ggsave(file = "report/data/indices_all.png",
plot(indices) + theme(legend.pos=c(0.15, 0.24),
    legend.background = element_rect(fill=alpha('white', 0)))
)

# recent LL indices plot

ggsave(file = "report/data/indices_recent.png", 
plot(indices[c("LLCPUE1", "LLCPUE2", "LLCPUE3", "LLCPUE4")]) +
  theme(legend.pos=c(0.8, 0.8),
    legend.background = element_rect(fill=alpha('white', 0))) 
)

# recent LL indices plot per season

ggsave(file = "report/data/indices_season.png", 
ggplot(lapply(indices[c("LLCPUE1", "LLCPUE2", "LLCPUE3", "LLCPUE4")], index),
  aes(x=year, y=alb)) + geom_line(aes(colour=qname)) + 
  facet_grid(season ~ qname, labeller = labeller(.rows = label_both)) +
  theme(legend.position="none") +
  ylab("Relative abundance") + xlab("")
)

# recent LL indices selectivity

ggsave(file = "report/data/index_selex_2017.png",
ggplot(lapply(indices[1:4], function(x) seasonMeans(sel.pattern(x)[, "2017", 1])),
  aes(x=age, y=alb, group=season, colour=season)) +
  geom_line() + geom_point(pch=19, size=0.6) +
  facet_wrap(~qname) + theme(legend.position="none") +
  ylab("") + xlab("Age (y)")
)

# SS alb plot
png(file = "report/data/SSdata.png")
  SSplotData(out)
dev.off()


# 2016
load("data/2016/results.RData")

library(rpart)
library(rpart.plot)

# status

regssb0 <- rpart(ssb0 ~ SigmaR + Steepness + CPUE + ESS + Catchability +
  Selection + Mmat, data = smry, maxdepth = 4)

png(file = "report/data/regtree_ssb0.png")
rpart.plot(regssb0)
dev.off()

# }}}

# --- base {{{

dir.create("output/base")

load("base/base.RData")

stock <- base$stock
out <- base$out

# stock(s)

ggsave(file="output/base/stock.png",
plot(simplify(stock))
)

# retro

SSplotRetro(retrosumm, xmin=2005, print=TRUE, plotdir="output/base")

# runstest 

png(file = "output/base/runs_CPUE.png")
sspar(mfrow=c(2, 2), plot.cex = 0.7)
cpue_rtes <- SSplotRunstest(out, add=T, subplots="cpue", indexselect=1:4)
dev.off()

png(file = "output/base/runs_LF.png")
sspar(mfrow=c(2, 3), plot.cex = 0.7)
len_rtes <- SSplotRunstest(out, add=T, subplots="len")
dev.off()

# MASE XVAL

png(file = "output/base/mase1.png")
sspar(mfrow = c(2, 2), plot.cex = 0.7, labs = F)
SSplotHCxval(retrosumm, add=T, xmin=2000, legendcex = 0.7, indexselect=1,
  Season=1)
SSplotHCxval(retrosumm, add=T, xmin=2000, legendcex = 0.7, indexselect=1,
  Season=2)
SSplotHCxval(retrosumm, add=T, xmin=2000, legendcex = 0.7, indexselect=1,
  Season=3)
SSplotHCxval(retrosumm, add=T, xmin=2000, legendcex = 0.7, indexselect=1,
  Season=4)
dev.off()

png(file = "output/base/mase3.png")
sspar(mfrow = c(2, 2), plot.cex = 0.7, labs = F)
SSplotHCxval(retrosumm, add=T, xmin=2000, legendcex = 0.7, indexselect=3,
  Season=1)
SSplotHCxval(retrosumm, add=T, xmin=2000, legendcex = 0.7, indexselect=3,
  Season=2)
SSplotHCxval(retrosumm, add=T, xmin=2000, legendcex = 0.7, indexselect=3,
  Season=3)
SSplotHCxval(retrosumm, add=T, xmin=2000, legendcex = 0.7, indexselect=3,
  Season=4)
dev.off()

png(file = "output/base/mase4.png")
sspar(mfrow=c(2, 2), plot.cex = 0.7)
SSplotHCxval(retrosumm, add=T, xmin=2000, Season=4, legendcex = 0.7)
dev.off()

# MVLN

png(file = "output/base/kobe.png")
SSdeltaMVLN(out, run="MVLN")
dev.off()

tab <- SSdeltaMVLN(out, run="MVLN", plot=FALSE)
kb <- data.table(tab$kb)

ggsave(file="output/base/ssb_1952.png",
ggplot(kb[year == 1952,], aes(x=SSB, y=..density..)) +
  geom_histogram(fill="grey90", color="black", binwidth=12000) +
  geom_density() + ylab("") + xlab("SSB0") +
  theme(axis.ticks.y=element_blank(),
    axis.text.y=element_blank())
)

# }}}

# --- corners {{{

mkdir("report/corners")

load("model/corners/load.RData")
load("model/corners/diagnostics.RData")


# ACF TODO

ggplot(acfs[lag > 0,], aes(x=factor(lag), y=acf, fill=factor(lag))) +
  geom_boxplot() + theme(legend.position="none")

ggplot(acfs[iter > 0], aes(x=lag, y=acf)) +
  geom_line(aes(group=iter), alpha=0.2) + theme(legend.position="none") +
  geom_line(data=acfs[iter < 1,], colour="red")

ggplot(ccfs, aes(x=factor(lag), y=acf, fill=lag)) + geom_boxplot()


# PLOT MASE by LLCPUE, season

ggsave(file = "report/corners/mases.png",
ggplot(ssmases, aes(x=Index, y=mase)) + 
  geom_hline(yintercept=1, linetype=2, color="gray") +
  geom_boxplot(aes(fill=Index)) + facet_wrap(~Season) +
  theme(legend.position="none") + ylab("MASE") + xlab("")
)


# HISTOGRAM SSB0 selected corner runs

ggsave(file = "report/corners/ssb0_distr.png",
ggplot(corners$results[sel ==1,], aes(x=SSB_Virgin, y=..density..)) +
  geom_histogram(fill="grey90", color="black", binwidth=12000) +
  geom_density() + ylab("") + xlab("SSB0") +
  theme(axis.ticks.y=element_blank(),
    axis.text.y=element_blank())
)

# TIME SERIES of rec, ssb, fbar, valid runs

dat <- metrics(corners$stock[, ac(1950:2016)], list(Rec=rec, SSB=ssb, F=fbar))
idx <- dat$F[,'2016'] > 0.6

ggsave(file = "output/corners/metrics.png",
ggplot(dat, aes(x=year, y=data, group=iter)) + geom_line(alpha=0.3) +
  facet_grid(qname~., scales="free") +
  geom_line(data=as.data.frame(lapply(dat, iter, idx)), colour="red") +
  ylab("") + xlab("")
)

tab <- res[iter %in% which(idx), .(M, sigmaR, steepness, cpues, lfreq, llq)]


# --- WHAT is driving SSB0 values?

library(rpart)
library(rpart.plot)

ssb_tree <- rpart(SSB_Virgin ~ M + sigmaR + steepness + cpues + lfreq + llq,
  data = res[valid,], maxdepth = 4)

ssbmsy_tree <- rpart(SSB_endyr / SSB_MSY ~ M + sigmaR + steepness + cpues + lfreq + llq,
  data = res[valid,], maxdepth = 4)

sigmarec_tree <- rpart(sigma_Rec ~ M + sigmaR + steepness + cpues + lfreq + llq,
  data = res[valid,], maxdepth = 4)

png(file="output/corners/regtrees.png")
par(mfrow=c(3, 1))
rpart.plot(ssb_tree, main="Virgin SSB")
rpart.plot(sigmarec_tree, main="sigma Rec")
rpart.plot(ssbmsy_tree, main="SSB / SSBMSY")
dev.off()


t <- knitr::kable(res[, .(mean_lkhd=mean(LIKELIHOOD)), by=.(cpues, lfreq, steepness)],
  format="markdown")

# }}}

# --- model_maineffects {{{

mkdir("output/maineffects")

load("model/maineffects.RData")

# CHANGES in SSB_Virgin by main effect

ggsave(file="output/maineffects/ssb0.png",
ggplot(res[col != "base"], aes(x=factor(iter), y=SSB_Virgin)) +
  geom_col(aes(fill=col)) +
  geom_hline(yintercept=res[col == "base", SSB_Virgin]) +
  geom_text(data=res[col == "base"], aes(x=2, y=SSB_Virgin * 1.04,
    label="Base case")) +
  ylab("Virgin SBB (t)") + xlab("") +
  scale_x_discrete(labels=c(`1`="0.325", `2`="0.350", `3`="0.375", `4`="0.4",
    `5`="0.4", `6`="0.8", `7`="0.7", `8`="0.9", `9`="LL1 NW", `10`="0.001",
    `11`="0.01", `12`="0.1", `13`="1%/year")) +
  guides(fill=guide_legend("", nrow=1, byrow=TRUE))
)

# CHANGES in SSB/SSBMSY by main effect

ggsave(file="output/maineffects/ssbmsy.png",
ggplot(res[col != "base"], aes(x=factor(iter), y=SSB_endyr/SSB_MSY)) +
  geom_col(aes(fill=col)) +
  geom_hline(yintercept=res[col == "base", SSB_endyr/SSB_MSY]) +
  geom_text(data=res[col == "base"], aes(x=12, y=SSB_endyr/SSB_MSY * 1.04,
    label="Base case")) +
  ylab("SSB_endyr/SSB_MSY") + xlab("") +
  scale_x_discrete(labels=c(`1`="0.325", `2`="0.350", `3`="0.375", `4`="0.4",
    `5`="0.4", `6`="0.8", `7`="0.7", `8`="0.9", `9`="LL1 NW", `10`="0.001",
    `11`="0.01", `12`="0.1", `13`="1%/year")) +
  guides(fill=guide_legend("", nrow=1, byrow=TRUE))
)

# }}}

# --- OM
