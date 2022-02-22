# model_full_review.R - DESC
# /model_full_review.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# DEBUG HC 2013-2017

hsr <- rec(stk)[, ac(2013:2017)]
hsr <- predictModel(model=rec~a, params=FLPar(c(hsr),
  dimnames=list(params="a", year=dimnames(hsr)$year, iter=dimnames(hsr)$iter)))

hc0 <- fwd(window(stk, end=2017), sr=hsr, catch=catch(stk)[, ac(2013:2017)])

hc1 <- fwd(window(stk, end=2017), sr=srr, catch=catch(stk)[, ac(2013:2017)])

osr <- rec(stk)[, ac(1970:1974)]
dimnames(osr) <- list(year=2013:2017)

hc2 <- fwd(window(stk, end=2017), sr=osr, catch=catch(stk)[, ac(2013:2017)])

plot(FLStocks(SS=window(stk, end=2017), HCrec=hc0, HCSRR=hc1)) + xlim(c(2000, NA))
plot(FLStocks(SS=window(stk, end=2017), HCSRR=hc1, HCold=hc2)) + xlim(c(2000, NA))



# --- IDENTIFY runs where ssb 2019 is too low

# SELECTED runs

sel <- results$sel
stk <- iter(fut, sel)
res <- results[sel,]

# PLOT ssb

plot(FLQuants(ALL=ssb(fut), SEL=ssb(fut)[,,,,,sel], REJ=ssb(fut)[,,,,,!sel]))

# CHECK SSB 2019

range(ssb(stk)[,'2017'])
range(ssb(stk)[,'2019'])

ggplot(res, aes(x=factor(M), y=SSB_endyr, fill=factor(M))) + geom_boxplot()
ggplot(res, aes(x=factor(sigmaR), y=SSB_endyr, fill=factor(sigmaR))) + geom_boxplot()
ggplot(res, aes(x=factor(steepness), y=SSB_endyr, fill=factor(steepness))) + geom_boxplot()
ggplot(res, aes(x=factor(cpues), y=SSB_endyr, fill=factor(cpues))) + geom_boxplot()
ggplot(res, aes(x=factor(lfreq), y=SSB_endyr, fill=factor(lfreq))) + geom_boxplot()
ggplot(res, aes(x=factor(llq), y=SSB_endyr, fill=factor(llq))) + geom_boxplot()

which(ssb(stk)[,'2017'] == max(ssb(stk)[,'2017']))

ggplot(acfs[lag > 0,], aes(x=factor(lag), y=acf, fill=factor(lag))) +
  geom_boxplot() + theme(legend.position="none")

plot(iter(fut, results[(sel), sel]))

# PLOT mat & catch.sel 2019

ggplot(FLQuants(selex=catch.sel(fut)[,'2019',,,,1], mat=mat(fut)[,'2019',,,,1]),
  aes(x=age, y=data, group=qname)) + geom_line()

ggplot(FLQuants(selex=catch.sel(fut)[,,,,,1], mat=mat(fut)[,,,,,1]),
  aes(x=age, y=data, group=year)) + geom_line() + facet_wrap(~qname)

ggplot(catch.sel(fut)[,,,,,1],  aes(x=age, y=data, group=year)) + geom_line() + facet_wrap(~(floor(year/10)*10))


# SS selex

out <- full$output[[1]]

dat <- melt(DT(out$ageselex)[Yr %in% c(2016, 2017) & Factor == 'Asel2' &
  Fleet %in% c(1:4, 7), c(2,3,4,5,8:22)], id.vars=c("Fleet", "Seas", "Sex", "Yr"))

ggplot(dat, aes(x=variable, y=value, group=Sex, colour=factor(Yr))) + geom_line() +
  facet_grid(Fleet~Seas)

ggplot(catch.sel(stk)[,'2017'], aes(x=age, y=data, group=iter)) + geom_line()

which(catch.sel(stk)[12,'2017'] == max(catch.sel(stk)[12,'2017']))


ssb(iter(fut, sel))[,'2019']
range(ssb(iter(fut, sel))[,'2019'])
hist(ssb(iter(fut, sel))[,'2019'], 50)

te <- fwd(stf(iter(fut, sel), end=2029), sr=iter(full$sr, sel),
  control=fwdControl(year=2020:2029, quant="fbar",
    value=full$refpts$FMSY[, sel]))


# SAVE results + diagnostics
save(results, ssmases, acfs, ccfs,
  file="model/full/diagnostics.RData", compress="xz")



