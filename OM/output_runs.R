# output_runs.R - DESC
# /output_runs.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

# --- FWD runs

# load("output/om_sexes.RData")
load("output/om_full.RData")

# C_2020-2023 = C_2019

runc19 <- fwd(om, control=fwdControl(
    list(year=2020:2023, quant="catch", value=35000),
    list(year=2020:2023, relYear=2019:2022, quant="fbar", max=1.5)),
  deviances=deviances$N)

ggsave("report/runs/runc19.png",
plot(window(runc19, start=2000, end=2023)) + fwdbox(xmax=2023)
)

iterSums((catch(runc19)[, ac(2020:2023)] %/% 35000) < 0.98) / 500

# C_2020-2023 = MSY

runmsy <- fwd(om, control=fwdControl(
    list(year=2020:2023, quant="catch", value=refpts(om)$MSY)),
  deviances=deviances$N)

ggsave("report/runs/runmsy.png",
plot(window(runmsy, start=2000, end=2023)) + fwdbox(xmax=2023)
)

iterSums((catch(runmsy)[, ac(2020:2023)] %/% refpts(om)$MSY) < 0.999) / 500

hist(refpts(om)$MSY)

ggplot(results, aes(Dead_Catch_MSY))


# F=0

runf0 <- fwd(om, control=fwdControl(year=2020:2040, quant="fbar", value=0.0001),
  deviances=deviances$MOV)

ggsave("report/runs/runf0.png",
plot(runf0) + ggtitle(expression(fwd(F[2018-40]==0))) + box
)

performance(runf0, metrics=list(SB=function(x) unitSums(ssb(x))),
  indicator=indicators['S8'], years=list(2030:2040))

# F=FMSY

runfmsy <- fwd(om, control=fwdControl(lapply(2020:2040,
    function(x) list(year=x, quant="fbar", value=refpts(om)$FMSY))),
  deviances=deviances$MOV)

ggsave("report/runs/runfmsy.png",
plot(window(runfmsy, start=2000, end=2030)) + fwdbox(xmin=2020, xmax=2030)
)

runfmsylow <- fwd(om, control=fwdControl(lapply(2018:2040,
    function(x) list(year=x, quant="fbar", value=refpts(om)$FMSY))),
  deviances=deviances$RHO)

ggsave("report/runs/runfmsy-devs.png",
plot(window(FLStocks(Low=stock(runfmsylow), Mean=stock(runfmsy)), start=2000)) +
  ggtitle(expression(fwd(F[2018-40]==F[MSY]))) + box
)

iterSums(ssb(runfmsy) < 1500)

# F=F_last

runflast <- fwd(om, control=fwdControl(lapply(2018:2040,
  function(x) list(year=x, quant="fbar",
    value=unitMeans(fbar(stock(om))[,'2017'])))))

ggsave("report/runs/runf2017.png",
plot(runflast) + ggtitle(expression(fwd(F[2018-40]==F[2017]))) + box
)

ggsave("report/runs/runf2017-rel.png",
plot(runflast, metrics=list(SSB=function(x) ssb(x)%/%refpts(om)$SBMSY,
  F=function(x) fbar(x)%/%refpts(om)$FMSY)) +
  geom_hline(yintercept=1, linetype=4) + box
  # facet_grid(qname~., labeller=label_bquote(rows=.(qname) / .(qname)[MSY])
)

plot(deviances[,,1])


# C=C_last

runclast <- fwd(om, control=fwdControl(lapply(2017:2040,
    function(x) list(year=x, quant="catch",
    value=c(unitSums(catch(stock(om))[,'2017']))))))

plot(runclast) + ggtitle(expression(fwd(C[2018-40]==C[2017]))) + box

# SAVE runs

save(runf0, runfmsy, runflast, runclast, file="output/runs.RData",
  compress="xz")
