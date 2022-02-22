# test_jabba.R - DESC
# /test_jabba.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

source("utilities.R")

load("data/om.Rdata")

# RUN over all unique OM runs 

it <- !duplicated(results$orig)

stk <- window(iter(observations(oem)$stk, it), end=2017)
idx <- window(iter(observations(oem)$idx, it), end=2017)

out <- foreach(i=seq(dim(stk)[6]),
  .errorhandling = "pass") %dopar% {

  tryCatch(
    stock(jabba.sa(iter(stk, i), iter(idx, i),
      args=list(dy=2017, y0=1950, it=1, ay=2018),
      tracking=FLQuant(dimnames=list(metric='conv.est', year='2018')))$stk),
    error=function(e) NULL)
}

save(out, file="test/jabba_om.Rdata")

# RUN over

fut <- stf(stk, end=2047)

fut <- fwd(fut, sr=iter(sr(om), it),
  fbar=FLQuant(c(refpts(om)$FMSY) * 1.5, dimnames=list(year=2018:2047)))

fidx <- lapply(fwdWindow(idx, end=2047), survey, object=fut)

fout <- foreach(i=seq(dim(fut)[6]),
  .errorhandling = "pass") %dopar% {

  tryCatch(
    stock(jabba.sa(iter(fut, i), iter(fidx, i),
      args=list(dy=2047, y0=1950, it=1, ay=2048),
      tracking=FLQuant(dimnames=list(metric='conv.est', year='2048')))$stk),
    error=function(e) NULL)
}

save(out, fout, file="test/jabba_om.Rdata")

# PLOT

out <- Reduce(combine, out)
fout <- Reduce(combine, fout)

plot(out / vb(stk)) + geom_hline(yintercept=1)
plot(window(fout / vb(fut), start=2005)) + geom_hline(yintercept=1)

# PLOT

plot(stock(stk), stock(t01$stk)) + ylim(c(0, NA)) +
  theme(legend.position="bottom") +
  scale_color_manual(name="", values=c(v1="red", v2="purple"),
    labels=c(v1="OM", v2="JABBA"))

attr(t01$stk, "refpts")$BMSY
iter(refpts(om), it)

plot(stock(stk) %/% iter(refpts(om)$SBMSY, it),
  stock(t01$stk) %/% attr(t01$stk, "refpts")$BMSY) +
  ylim(c(0, NA)) +
  theme(legend.position="bottom") +
  scale_color_manual(name="", values=c(v1="red", v2="purple"),
    labels=c(v1="OM", v2="JABBA")) +
  geom_hline(yintercept=1)


# DIAGS
