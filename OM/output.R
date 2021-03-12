# om.R - DESC
# /om.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ioalbmse)

load("model/corners/load.RData")
load("model/corners.RData")


# -------

# PLOT stk 12 vs stk 14
# TODO FIND way to match FLQuants & results

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


# -------

# --- OM selection

its <- results[sel == 1, iter]

# SELECTED iters
stks <- iter(simplify(corners$stock), its)


# --- OM weighting

# EXTEND to 2037

fut <- stf(stks, end=2040)

srr <- iter(corners$sr, its)

rps <- corners$refpts[, its]

# DEBUG rec

m.spwn(stks) <- 0.85
harvest.spwn(stks) <- 0.85

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


# DEVIANCES, 500 per OM iter
alldevs <- lapply(results[its, Recr_sigma], function(x)
  rlnorm(1, FLQuant(0, dimnames=list(year=2018:2037, age=1)), x))

# GENERATE weights

weights <- results[its, pvalue]

results[sel == 1, weight:=weights]

ggsave("report/output/om_weights.png",
ggplot(results[sel == 1,], aes(x=iter, y=weight)) + geom_point(fill="black") +
  ylim(c(0,1)) + ylab("Resample weights") + xlab("Model run")
)

# TODO GENERATE resample

set.seed(47)

samps <- sample.int(length(its), size=500, prob=weights, replace=TRUE)

# RESAMPLE future deviances

alldevs <- lapply(results[its, Recr_sigma], function(x)
  rlnorm(100, FLQuant(0, dimnames=list(year=2018:2037, age=1)), x))

alldevs <- lapply(alldevs[samps], iter, iter=sample(20, 1))

deviances <- Reduce(combine, alldevs) 

# RESAMPLE valid runs based on weights

stk <- iter(stks, samps)
srr <- iter(corners$sr, samps)
residuals(srr) <- append(residuals(srr), deviances)
refpts <- iter(corners$refpts, samps)
results <- results[samps, ]
indices <- FLIndices(cpue1=iter(corners$indices$LLCPUE1[,,,1], samps),
  cpue3=iter(corners$indices$LLCPUE3[,,,1], samps))


ggsave("report/output/om_worm.png",
plot(metrics(stk)) + geom_worm(data=metrics(iter(stk, sample(seq(500), 3))))
)

ggsave("report/output/om_base.png",
plot(FLQuants(`SB/SB[MSY]`=ssb(stk) / refpts$SBMSY,
  `F/F[MSY]`=fbar(stk) / refpts$FMSY)) +
  geom_hline(yintercept=1, linetype=2, alpha=0.7) +
  geom_worm(data=FLQuants(`SB/SB[MSY]`=ssb(simplify(base$stock)) / base$refpts$SBMSY,
  `F/F[MSY]`=fbar(simplify(base$stock)) / base$refpts$FMSY), colour="green")
)

  
# TODO geom_flpar equivalebnt for iters ~ value/refpt


# OM

om <- FLom(stock=stf(stk, end=2040), sr=srr, refpts=refpts,
  projection=mseCtrl(method=fwd.om))

# OEM: observations (stk, idx), deviances(sr, idx, stk)

oem <- FLoem(observations=list(stk=stf(stk, end=2040),
  idx=lapply(indices, fwdWindow, end=2040)))


save(om, oem, results, file="model/om.RData", compress="xz")

load('model/om.RData')


tes <- fwd(stock(om),
  sr=predictModel(model=bevholtss3()$model, params=params(sr(om))), 
  control=fwdControl(year=2018:2040, quant="catch", value=1200))

tes <- fwd(iter(stock(oma), 1),
  sr=predictModel(model=bevholtss3()$model, params=params(sr(oma))), 
  control=fwdControl(year=2018, quant="catch", value=1200))

tes <- fwd(stock(oma),
  sr=predictModel(model=bevholtss3()$model, params=params(sr(oma))), 
  control=fwdControl(year=2018:2040, quant="fbar", value=0.1))






ggplot(ssb(oms), aes(x=year, y=data, group=iter)) + geom_line() +
  geom_label(aes(label=iter), x=1950)

ggplot(data.frame(iter=dimnames(oms)$iter, data=c(ssb(oms)[, '1990'])),
  aes(x=iter, y=data)) + geom_label(aes(label=iter))


## PLOT

plot(oms, om)


ggsave(file = "reports/corners/om_base.png",
plot(FLStocks(OM=om, SA=simplify(base$stock)), metrics=list(Rec=rec, SSB=ssb, F=fbar)) + ggtitle("IOTC ALB - OM vs. base case assessment")
)

library(patchwork)

ggsave(file = "output/corners/om_worms.png",
plot(om, metrics=list(Rec=rec, SSB=ssb, F=fbar)) +
  geom_worm(iter(metrics(om, metrics=list(Rec=rec, SSB=ssb, F=fbar)), c(1, 344, 87, 37))) +
  ggtitle("IOTC ALB - OM")
)



fut500 <- iter(fut, samps)

fut500 <- fwd(fut500,
  sr=predictModel(model=bevholtss3()$model, params=iter(params(srr), samps)), 
  control=fwdControl(lapply(2018:2037, function(x)
    list(year=x, quant="fbar", value=c(iter(rps$FMSY, samps))))), deviances=deviances)


ggsave("report/output/fut_fmsy.png",
plot(fut500) +
  annotate("rect", xmin = 2017, xmax = 2037, ymin = -Inf, ymax = Inf,
    fill = "#2a2a2a", alpha=0.1) +
  ggtitle(expression(paste(F[2018-37] == F[MSY])))
)

fut000 <- fwd(fut500,
  sr=predictModel(model=bevholtss3()$model, params=iter(params(srr), samps)), 
  control=fwdControl(lapply(2018:2037, function(x)
    list(year=x, quant="fbar", value=0))), deviances=deviances)

ggsave("report/output/fut_f0.png",
plot(fut000) +
  annotate("rect", xmin = 2017, xmax = 2037, ymin = -Inf, ymax = Inf,
    fill = "#2a2a2a", alpha=0.1)
)

fut001 <- fwd(fut500,
  sr=predictModel(model=bevholtss3()$model, params=iter(params(srr), samps)), 
  control=fwdControl(lapply(2018:2037, function(x)
    list(year=x, quant="fbar", value=rlnorm(500, log(0.09), 0.1)))), deviances=deviances)

fut002 <- fwd(fut500,
  sr=predictModel(model=bevholtss3()$model, params=iter(params(srr), samps)), 
  control=fwdControl(lapply(2018:2037, function(x)
    list(year=x, quant="fbar", value=rlnorm(500, log(0.03), 0.2)))), deviances=deviances)

ggsave(file = "report/output/fmps.png",
plot(window(FLStocks(MP01=fut001, MP02=fut002), start=2000),
  metrics=list(SB=ssb, C=catch)) +
  annotate("rect", xmin = 2017, xmax = 2037, ymin = -Inf, ymax = Inf,
    fill = "#2a2a2a", alpha=0.1) +
  ggtitle("IOTC ALB - F level MPs")
)



rps <- iter(corners$refpts, samps)

plot(FLQuants(`SSB/SSB[MSY]`=ssb(fut000) / rps$SBMSY,
  `F/F[MSY]`=fbar(fut000) / rps$FMSY)) +
  geom_hline(yintercept=1) +
  ggtitle(expression(paste(F[2018-37] == 0))) +
  facet_grid(qname~.,  labeller=label_parsed, scales="free")

plot(FLQuants(`SSB/SSB[MSY]`=ssb(fut500) / rps$SBMSY,
  `F/F[MSY]`=fbar(fut500) / rps$FMSY)) +
  geom_hline(yintercept=1) +
  ggtitle(expression(paste(F[2018-37] == F[MSY]))) +
  facet_grid(qname~.,  labeller=label_parsed, scales="free")


cprods <- merge(prods, corners$results[, .(iter, cpues)], by='iter')

ggplot(cprods[iter %in% its], aes(x=SSB, y=Yield, group=iter)) + geom_line(alpha=0.4) +
  facet_wrap(~cpues)


