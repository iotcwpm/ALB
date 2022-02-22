# output.R - DESC
# ALB/OM/output.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(mse)
library(ggplotFL)
library(patchwork)
library(ss3diags)

library(knitr)
library(icesTAF)

th0 <- theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
th1 <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  axis.title.y = element_blank())

# Box to shade forecast years

fwdbox <- function(xmin = 2020, xmax = 2040)
  annotate("rect", xmin = xmin, xmax = xmax, ymin=-Inf, ymax=Inf,
    fill="#E69F00", alpha = .1)

mkdir("report")

# --- model_assessment {{{

mkdir("report/assessment")

# 
load("model/assessment/load.RData")

# data/2014/sa: sa2014
load("data/2016/base.RData")
range(base, c('minfbar', 'maxfbar')) <- c(1,12)

#  runs

ggsave(file = "report/assessment/runs_wp.png",
plot(assessment$stock, metrics=list(Rec=rec, SSB=ssb, F=fbar))
)

# runs  + 2014 stock plot

ggsave(file = "report/assessment/runs_wp_2014.png",
plot(FLStocks(c(assessment$stock, `2014`=base)), metrics=list(Rec=rec, SSB=ssb, F=fbar))
)

# indices

ggsave(file = "report/assessment/indices_all.png",
plot(assessment$indices[[1]]) + theme(legend.pos=c(0.15, 0.24),
  legend.background = element_rect(fill=alpha('white', 0)))
)

# recent LL indices

ggsave(file = "report/assessment/indices_recent.png", 
plot(indices[c("LLCPUE1", "LLCPUE2", "LLCPUE3", "LLCPUE4")]) +
  theme(legend.pos=c(0.8, 0.2),
    legend.background = element_rect(fill=alpha('white', 0))) 
)

# recent LL indices per season

ggsave(file = "report/assessment/indices_season.png", 
ggplot(lapply(assessment$indices[[1]][c("LLCPUE1", "LLCPUE2", "LLCPUE3", "LLCPUE4")], index),
  aes(x=year, y=data)) + geom_line(aes(colour=qname)) + 
  facet_grid(season ~ qname, labeller = labeller(.rows = label_both)) +
  theme(legend.position="none") +
  ylab("Relative abundance") + xlab("")
)

# recent LL indices selectivity

ggsave(file = "report/assessment/index_selex_2017.png",
ggplot(lapply(assessment$indices[[1]][1:4], function(x) seasonMeans(sel.pattern(x)[, "2017", 1])),
  aes(x=age, y=data, group=season, colour=season)) +
  geom_line() + geom_point(pch=19, size=0.6) +
  facet_wrap(~qname) + theme(legend.position="none") +
  ylab("") + xlab("Age (y)")
)

# SS alb plot
png(file = "report/data/SSdata.png")
  SSplotData(assessment$out$NW)
dev.off()


# 2016
load("data/2016/results.RData")

library(rpart)
library(rpart.plot)

# status

regssb0 <- rpart(ssb0 ~ SigmaR + Steepness + CPUE + ESS + Catchability +
  Selection + Mmat, data = results, maxdepth = 4)

results[, regssb0:=regssb0$where]

ggplot(results, aes(x=ssb0)) +
  geom_histogram() + facet_grid(.~regssb0)

png(file = "report/data/regtree_ssb0.png")
rpart.plot(regssb0)
dev.off()

# productivity

regr <- rpart(r ~ SigmaR + Steepness + CPUE + ESS + Catchability +
  Selection + Mmat, data = results, maxdepth = 4)

png(file = "report/data/regtree_ssb0.png")
rpart.plot(regr)
dev.off()

# variability

regssb0 <- rpart(ssb0 ~ SigmaR + Steepness + CPUE + ESS + Catchability +
  Selection + Mmat, data = results, maxdepth = 4)

png(file = "report/data/regtree_ssb0.png")
rpart.plot(regssb0)
dev.off()

# }}}

# --- base {{{

mkdir("report/base")

load("model/base.RData")

# stock(s)

ggsave(file="report/base/stock.png",
plot(FLStocks(All=simplify(base$stock), Fema=nounit(noseason(base$stock))))
)

SSplotRetro(retrosumm, xmin=2005, print=TRUE, plotdir="report/base")

# runstest 

png(file = "report/base/runs_CPUE.png")
sspar(mfrow=c(2, 2), plot.cex = 0.7)
cpue_rtes <- SSplotRunstest(base$out, add=T, subplots="cpue", indexselect=1:4)
dev.off()

png(file = "report/base/runs_LF.png")
sspar(mfrow=c(2, 3), plot.cex = 0.7)
len_rtes <- SSplotRunstest(base$out, add=T, subplots="len")
dev.off()

# MASE XVAL

png(file = "report/base/mase1.png")
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

# --- maineffects {{{

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

# --- partial {{{

mkdir("report/partial")

# partial - list(stock, sr, refpts, results, indices, output)
load("model/partial/load.RData")

# results, ssmases, mases, acfs, ccfs
load("model/partial/diagnostics.RData")

# PLOT MASE by LLCPUE, season

ggsave(file = "report/partial/mases.png",
ggplot(ssmases, aes(x=Index, y=mase)) + 
  geom_hline(yintercept=1, linetype=2, color="gray") +
  geom_boxplot(aes(fill=Index)) + facet_wrap(~Season) +
  theme(legend.position="none") + ylab("MASE") + xlab("")
)

# HISTOGRAM SSB0 all runs

ggsave(file = "report/partial/ssb0_distr_all.png",
ggplot(results, aes(x=SSB_Virgin, y=..density..)) +
  geom_histogram(fill="grey90", color="black", binwidth=12000) +
  geom_density() + ylab("") + xlab("SSB0") +
  theme(axis.ticks.y=element_blank(),
    axis.text.y=element_blank())
)

# SSB and F all runs

stock <- simplify(partial$stock)
range(stock) <- c(minfbar=1, maxfbar=12)

ggsave(file = "report/partial/stock_all.png",
ggplot(metrics(stock, list(F=fbar, SSB=ssb)),
  aes(x=year, y=data, group=iter)) + geom_line() +
  facet_grid(qname~., scales="free") +
  xlab("")+ ylab("")
)

# RUN weights

ggsave(file = "report/partial/weights.png",
ggplot(results[sel==1,], aes(pvalue)) +
  geom_histogram(aes(y = ..density..), colour="black", fill = "grey") +
  geom_density() + xlab("p-value") + ylab("") +
  theme(axis.ticks.y=element_blank(),
    axis.text.y=element_blank())
)

# HISTOGRAM SSB0 selected runs

ggsave(file = "report/partial/ssb0_distr.png",
ggplot(results[sel == 1, ], aes(x=SSB_Virgin, y=..density..)) +
  geom_histogram(fill="grey90", color="black", binwidth=12000) +
  geom_density() + ylab("") + xlab("SSB0") +
  theme(axis.ticks.y=element_blank(),
    axis.text.y=element_blank())
)

ggsave(file = "report/partial/status_distr.png",
ggplot(results[(sel), ], aes(x=SSB_endyr / SSB_MSY, y=..density..)) +
  geom_histogram(fill="grey90", color="black") +
  geom_density() + ylab("") + xlab(expression(SSB[2017] / SSB[MSY])) +
  theme(axis.ticks.y=element_blank(),
    axis.text.y=element_blank()) +
  geom_vline(xintercept=1, linetype=4)
)


ggplot(metrics(iter(stk, results[, sel]),
  list(Rec=rec, SSB=ssb, F=fbar)), aes(x=year, y=data, group=iter)) +
  geom_line(alpha=0.5) + facet_grid(qname~., scales="free")


# OM vs. base SA

ggsave(file = "report/partial/stock_base.png", width=7, height=8,
plot(FLStocks(OM=stock, `Assessment WPTmT`=nounit(noseason(base$stock))),
  probs=c(0.10, 0.25, 0.50, 0.75, 0.90))
)

ggsave(file = "report/partial/om_refpts.png", width=7, height=6,
plot(FLQuants(`F/F[MSY]`=fbar(stk) %/% refpts$FMSY,
  `B/B[MSY]`=ssb(stk) %/% refpts$SBMSY)) +
  geom_hline(yintercept=1, linetype=2) +
  facet_grid(qname~., label = "label_parsed", scales="free")
)



# TIME SERIES of rec, ssb, fbar, valid runs


plot(metrics(iter(stk, results$sel)[, ac(1950:2016)], list(Rec=rec, SSB=ssb, F=fbar)))

idx <- dat$F[,'2016'] > 0.6

ggsave(file = "report/partial/metrics.png",
ggplot(dat, aes(x=year, y=data, group=iter)) + geom_line(alpha=0.3) +
  facet_grid(qname~., scales="free") +
  geom_line(data=as.data.frame(lapply(dat, iter, idx)), colour="red") +
  ylab("") + xlab("")
)

# --- WHAT is driving SSB0 values?

library(rpart)
library(rpart.plot)

ssb_tree <- rpart(SSB_Virgin ~ M + sigmaR + steepness + cpues + lfreq + llq,
  data = results[(sel),], maxdepth = 4)

ssbmsy_tree <- rpart(SSB_endyr / SSB_MSY ~ M + sigmaR + steepness + cpues + lfreq + llq,
  data = results[(sel),], maxdepth = 4)

sigmarec_tree <- rpart(sigma_Rec ~ M + sigmaR + steepness + cpues + lfreq + llq,
  data = results[(sel),], maxdepth = 4)

png(file="report/partial/regtrees.png")
par(mfrow=c(3, 1))
rpart.plot(ssb_tree, main="Virgin SSB")
rpart.plot(sigmarec_tree, main="sigma Rec")
rpart.plot(ssbmsy_tree, main="SSB / SSBMSY")
dev.off()

# }}}

# --- model_partial_single {{{

load("model/partial_single.RData")

# INSPECT results

dat <- melt(results[, .(iter, SSB_Virgin, Recr_Virgin, SSB_MSY, Fstd_MSY, SSB_status)],
  id.vars="iter", variable.name="metric", value.name="data")

ggplot(dat, aes(x=data, fill=metric)) + geom_histogram(colour="black") +
  facet_wrap(~metric, scales="free")

ggplot(results, aes(x=factor(M), y=SSB_Virgin, fill=factor(M))) +
  geom_boxplot() + guides(fill=FALSE)

ggplot(results, aes(x=factor(M), y=SSB_status, fill=factor(M))) +
  geom_boxplot() + guides(fill=FALSE)





#

plot(stock, metrics=list(Rec=rec, SSB=ssb, F=fbar))

plot(stock, metrics=list(`SB/SB[MSY]`=function(x) ssb(x) %/%refpts$SBMSY,
  `F/F[MSY]`=function(x) fbar(x) %/% refpts$FMSY)) +
  geom_hline(yintercept=1, linetype=3)


# }}}

# --- full {{{

load('output/om_full.RData')

load('model/base.RData')

# full om

ggsave("report/full/om.png",
plot(window(om, end=2019))
)


ggsave("report/full/om_refpts.png",
plot(window(om, end=2019), metrics=relmets) +
  geom_hline(yintercept=1, linetype=2, alpha=0.50)
)

mets <- list(Rec=rec, SSB=ssb, F=fbar)

# om sa

ggsave("report/full/omsa.png",
plot(FLStocks(OM=window(stock(om), end=2019), SA=simpler(base$stock))) +
  geom_vline(xintercept=2017, linetype=2, size=0.5, alpha=0.5)
)

ggsave("report/full/omsabmsy.png",
ggplot(results, aes(SSB_status)) + geom_histogram(aes(y = ..density..), colour="black",
    fill = "white") +
  geom_density(colour='black', fill='gray', alpha=0.15) +
  xlab(expression(SB/SB[MSY])) + ylab("") +
  theme(legend.position="none",
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) +
  geom_vline(xintercept=ssb(base$stock)[,'2017',1,1] / base$refpts$SBMSY,
    colour="red")
)

# by cpue
ggsave("report/full/om_factors-1.png", width=10, height=7,
(plot(split(window(stock(om), end=2019), results$cpues), metrics=mets) +
  ggtitle("CPUE")) +

# by M
(plot(split(window(stock(om), end=2019), results$M), metrics=mets) +
  ggtitle("M"))
)

# by sigmaR
ggsave("report/full/om_factors-2.png", width=10, height=7,
(plot(split(window(stock(om), end=2019), results$sigmaR), metrics=mets) +
 ggtitle(expression(sigma[R]))) +

# by steepness
(plot(split(window(stock(om), end=2019), results$steepness), metrics=mets) +
 ggtitle("h"))
)

# by lfreq
ggsave("report/full/om_factors-3.png", width=10, height=7,
(plot(split(window(stock(om), end=2019), results$lfreq), metrics=mets) +
  ggtitle("LF lambda")) +

# by llq
(plot(split(window(stock(om), end=2019), results$llq), metrics=mets) +
  ggtitle("LL catchability trend"))
)

# llq ~ cpues
ggsave("report/full/om_factors-depletion-1.png",
ggplot(results, aes(x=factor(cpues), y=SSB_depletion)) +
  geom_boxplot(aes(fill=factor(llq))) +
  labs(fill="LL catchability", x="CPUE", y="Depletion")
)

ggsave("report/full/om_factors-depletion-2.png",
ggplot(results, aes(x=factor(M), y=SSB_depletion)) +
  geom_boxplot(aes(fill=factor(steepness))) +
  labs(fill="SRR steepness", x="M", y="Depletion")
)

ggsave("report/full/om_factors-depletion-3.png",
ggplot(results, aes(x=factor(sigmaR), y=SSB_depletion)) +
  geom_boxplot(aes(fill=factor(lfreq))) +
  labs(fill="Lkhd weight LFreq", x="sigmaR", y="Depletion")
)

# refpts

ggsave("report/full/refpts.png",
ggplot(melt(results[, c("SSB_MSY", "Fstd_MSY", "Dead_Catch_MSY")]),
  aes(value)) + geom_histogram(aes(y = ..density..), colour="black",
    fill = "white") +
  geom_density(aes(colour=variable, fill=variable, alpha=0.15)) +
  xlab("") + ylab("") +
  theme(legend.position="none") +
  facet_wrap(~variable, scales="free", labeller = labeller(variable =
    c("SSB_MSY"="SBMSY", "Fstd_MSY"="FMSY", "Dead_Catch_MSY"="MSY")))
)

# MSY vs. factors

ggplot(results, aes(x=factor(M), SSB_MSY)) +
  geom_boxplot(aes(fill = factor(M))) + xlab("M") + ylab("SBMSY (t)") +
  theme(legend.position="none")

ggplot(results, aes(x=factor(steepness), SSB_MSY)) +
  geom_boxplot(aes(fill = factor(steepness))) + xlab("steepness") + ylab("SBMSY (t)") +
  theme(legend.position="none")

# K vs. factors

ggplot(results, aes(x=factor(cpues), SSB_MSY)) +
  geom_boxplot(aes(fill = factor(cpues))) + xlab("CPUE") + ylab("SBMSY (t)") +
  theme(legend.position="none")

# Properties vs. factors

load('model/full/load.RData')

library(rpart)
library(rpart.plot)

# K

regK <- rpart(K ~ M + sigmaR + steepness + cpues + llq + lfreq,
  data = results, maxdepth = 3)

png(file = "report/full/regtree_k.png")
rpart.plot(regK, main="K")
dev.off()

# status

regdep <- rpart(SSB_depletion ~ M + sigmaR + steepness + cpues + llq + lfreq,
  data = results, maxdepth = 3)

png(file = "report/full/regtree_dep.png")
rpart.plot(regdep, main=expression(SB/SB[0]))
dev.off()

# productivity

regr <- rpart(r ~ M + sigmaR + steepness + cpues + llq + lfreq,
  data = results, maxdepth = 4)

png(file = "report/full/regtree_r.png")
rpart.plot(regr, main="r")
dev.off()

# variability

regsigma <- rpart(sigma_Rec ~ M + sigmaR + steepness + cpues + llq + lfreq,
  data = results, maxdepth = 3)

png(file = "report/full/regtree_sigma.png")
rpart.plot(regsigma, main=expression(sigma[R]))
dev.off()

# SRR deviances

plot(window(deviances, end=2030)) +
  geom_vline(xintercept=2017, linetype=2) +
  geom_vline(xintercept=2017, linetype=2)



# }}}

# --- output_runs {{{



# }}}

# --- output {{{

load("output/om.RData")

# metrics to use, unifies sexes

mets <- list(Rec=function(x) unitSums(rec(x)), SSB=function(x) ssb(x)[,,'F'],
  Catch=function(x) unitSums(catch(x)), F=function(x) unitMeans(fbar(x)))

# PLOT stock

plot(window(stock(om), end=2017), metrics=mets)

# PLOT SR

dat <- melt(results[, .(iter, `SSB[0] (t)`=SSB_Virgin,
  `Rec[0] (1000)`=Recr_Virgin)],
  id.vars=c("iter"), measure.vars=c("SSB[0] (t)", "Rec[0] (1000)"),
  variable.name="param", value.name="data")

basedat <- melt(base$results[, .(`SSB[0] (t)`=SSB_Virgin,
  `Rec[0] (1000)`=Recr_Virgin)],
  measure.vars=c("SSB[0] (t)", "Rec[0] (1000)"),
  variable.name="param", value.name="data")

ggplot(dat, aes(x=data, y=..density..)) +
  geom_histogram(position="identity", fill="gray", colour="darkgrey", alpha=0.2) +
  geom_density() +
  facet_wrap(~param, scales="free", labeller=label_parsed) +
  xlab("") + ylab("") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  geom_vline(data=basedat, aes(xintercept=data), colour="red")




# }}}

# --- output_runs

# --- OM {{{

# PLOT distribution SSB_Virgin by p-value group
ggplot(res[sel == 1], aes(x=factor(round(pvalue, 1)), y=SSB_Virgin,
  fill=factor(round(pvalue, 1)))) + geom_boxplot() +
  xlab("p-value") + ylab("Virgin SSB (t)") +
  guides(fill=FALSE)

# PLOT stock

plot(ssb(stock) %/% refpts$"SBMSY") +
  geom_hline(yintercept=1, colour="red")

plot(fbar(stock) %/% refpts$"FMSY") +
  geom_hline(yintercept=1, colour="red")


# FACTORS on scale (SB/SBMSY)

pls <- lapply(c("M", "sigmaR", "steepness", "cpues", "lfreq", "llq"),
  function(x) {
    ggplot(me(ssb(stock) %/% refpts$"SBMSY", res),
      aes(x=year, y=data, group=iter)) +
      geom_line(aes_string(colour=x)) +
      geom_hline(yintercept=1, linetype=2) +
      guides(colour="none") + xlab("") + ylab("") +
      ggtitle(x)
  })

pls[[1]] <- pls[[1]] + ylab(expression(SB/SB[MSY]))
pls[[4]] <- pls[[4]] + ylab(expression(SB/SB[MSY]))

Reduce("+", pls)

# FACTORS on scale (SB/SBMSY)

pls <- lapply(c("M", "sigmaR", "steepness", "cpues", "lfreq", "llq"),
  function(x) {
    ggplot(me(rec(stock), res),
      aes(x=year, y=data, group=iter)) +
      geom_line(aes_string(colour=x)) +
      guides(colour="none") + xlab("") + ylab("") +
      ggtitle(x)
  })

pls[[1]] <- pls[[1]] + ylab(expression(Rec))
pls[[4]] <- pls[[4]] + ylab(expression(Rec))

Reduce("+", pls)




# -------

# PLOT stk 12 vs stk 14

st <- simplify(corners$stock)[-1,]
me <- list(Rec=rec, SSB=ssb, F=fbar)

range(st, c("minfbar", "maxfbar")) <- c(1,12)

ggsave("report/output/om_cpues.png",
plot(FLStocks(SW=iter(st, corners$results$cpues == 14),
  NW=iter(st, corners$results$cpues == 12)), metrics=me)
)

# Boxplots SBMSY by factor level vs. SA CIs

library(ss3diags)

# Quick checks
load("model/base.RData")
out <- readOutputss3("model/base/run")
mvln <- SSdeltaMVLN(out, run="SMAbase")
quas <- quantile(mvln$kb$SSB / mvln$kb$stock, c(0.05, 0.95))


library(patchwork)


ggsave("report/output/factors_base_sbmsy.png", width=12, height=3,

(ggplot(results[sel == 1,], aes(x=sel, y=SSB_MSY)) +
  geom_hline(yintercept=quas, linetype=2, alpha=0.6) +
  geom_boxplot(fill="white") + xlab("All") + ylab(expression(SB[MSY]))) +
(ggplot(results[sel == 1,], aes(x=factor(M), y=SSB_MSY)) +
  geom_hline(yintercept=quas, linetype=2, alpha=0.6) +
  geom_boxplot(fill="red") + xlab("M") + ylab("") +
 theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
(ggplot(results[sel == 1,], aes(x=factor(sigmaR), y=SSB_MSY)) +
  geom_hline(yintercept=quas, linetype=2, alpha=0.6) +
  geom_boxplot(fill="gray") + xlab("sigmaR") + ylab("") +
 theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
(ggplot(results[sel == 1,], aes(x=factor(steepness), y=SSB_MSY)) +
  geom_hline(yintercept=quas, linetype=2, alpha=0.6) +
  geom_boxplot(fill="green") + xlab("steepness") + ylab("") +
 theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
(ggplot(results[sel == 1,], aes(x=factor(cpues), y=SSB_MSY)) +
  geom_hline(yintercept=quas, linetype=2, alpha=0.6) +
  geom_boxplot(fill="cyan") + xlab("cpues") + ylab("") +
 theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
(ggplot(results[sel == 1,], aes(x=factor(lfreq), y=SSB_MSY)) +
  geom_hline(yintercept=quas, linetype=2, alpha=0.6) +
  geom_boxplot(fill="orange") + xlab("lfreq") + ylab("") +
 theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
(ggplot(results[sel == 1,], aes(x=factor(llq), y=SSB_MSY)) +
  geom_hline(yintercept=quas, linetype=2, alpha=0.6) +
  geom_boxplot(fill="yellow") + xlab("llq") + ylab("") +
 theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
plot_layout(nrow = 1, byrow = FALSE)

)

dat <- data.table(mvln$kb)

quas <- quantile(dat[year==2017, stock], c(0.05, 0.95))

ggsave("report/output/factors_base_status.png", width=12, height=3,

(ggplot(results[sel == 1,], aes(x=sel, y=SSB_status)) +
  geom_hline(yintercept=quas, linetype=2, alpha=0.6) +
  geom_boxplot(fill="white") + xlab("All") + ylab(expression(SB[2017]/SB[MSY]))) +
(ggplot(results[sel == 1,], aes(x=factor(M), y=SSB_status)) +
  geom_hline(yintercept=quas, linetype=2, alpha=0.6) +
  geom_boxplot(fill="red") + xlab("M") + ylab("") +
 theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
(ggplot(results[sel == 1,], aes(x=factor(sigmaR), y=SSB_status)) +
  geom_hline(yintercept=quas, linetype=2, alpha=0.6) +
  geom_boxplot(fill="gray") + xlab("sigmaR") + ylab("") +
 theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
(ggplot(results[sel == 1,], aes(x=factor(steepness), y=SSB_status)) +
  geom_hline(yintercept=quas, linetype=2, alpha=0.6) +
  geom_boxplot(fill="green") + xlab("steepness") + ylab("") +
 theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
(ggplot(results[sel == 1,], aes(x=factor(cpues), y=SSB_status)) +
  geom_hline(yintercept=quas, linetype=2, alpha=0.6) +
  geom_boxplot(fill="cyan") + xlab("cpues") + ylab("") +
 theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
(ggplot(results[sel == 1,], aes(x=factor(lfreq), y=SSB_status)) +
  geom_hline(yintercept=quas, linetype=2, alpha=0.6) +
  geom_boxplot(fill="orange") + xlab("lfreq") + ylab("") +
 theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
(ggplot(results[sel == 1,], aes(x=factor(llq), y=SSB_status)) +
  geom_hline(yintercept=quas, linetype=2, alpha=0.6) +
  geom_boxplot(fill="yellow") + xlab("llq") + ylab("") +
 theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
plot_layout(nrow = 1, byrow = FALSE)

)





fre <- fwd(fut, sr=srr,
  control=fwdControl(year=2018:2040, quant="fbar", value=1.2, relYear=2017:2039))

ggsave("report/om/fut_recs.png",
ggplot(metrics(fre, list(SSB=ssb, R=rec)), aes(x=year, y=data, group=iter)) +
  geom_line() + facet_wrap(~qname, scales="free")
)

ggsave("report/om/pred-recs.png",
ggplot(predict(srr, ssb=ssb(fre)[, ac(2010:2017)]) / rec(fre)[, ac(2010:2017)],
  aes(x=factor(year), y=data, group=factor(year))) +
  geom_hline(yintercept=1, color="red") +
  geom_boxplot(aes(fill=factor(year))) +
  xlab("") + ylab("pred / obs") + ggtitle("Recruitment deviates") +
  theme(legend.position="none")
)

# }}}
