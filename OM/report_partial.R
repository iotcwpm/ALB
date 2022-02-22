# report_partial.R - DESC
# /report_partial.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ggplotFL)
library(mse)
library(ss3diags)

library(knitr)
library(icesTAF)

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


ggplot(metrics(iter(stock, results[, sel]),
  list(Rec=rec, SSB=ssb, F=fbar)), aes(x=year, y=data, group=iter)) +
  geom_line(alpha=0.5) + facet_grid(qname~., scales="free")


# OM vs. base SA

load('model/base.RData')

refpts <- partial$refpts

ggsave(file = "report/partial/stock_base.png", width=7, height=8,
plot(FLStocks(OM=stock, `Assessment WPTmT`=nounit(noseason(base$stock))),
  probs=c(0.10, 0.25, 0.50, 0.75, 0.90))
)

ggsave(file = "report/partial/om_refpts.png", width=7, height=6,
plot(FLQuants(`F/F[MSY]`=fbar(stock) %/% refpts$FMSY,
  `B/B[MSY]`=ssb(stock) %/% refpts$SBMSY)) +
  geom_hline(yintercept=1, linetype=2) +
  facet_grid(qname~., label = "label_parsed", scales="free")
)

# TIME SERIES of rec, ssb, fbar, valid runs

plot(metrics(iter(stock, results$sel)[, ac(1950:2016)], list(Rec=rec, SSB=ssb, F=fbar)))

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



