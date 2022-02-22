# model_partial_results.R - DESC
# /model_partial_results.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(patchwork)
library(ggplotFL)


load("model/partial/diagnostics.RData")


# INSPECT


plot(fut)
plot(split(ssb(fut), idx))

plot(split(fut, idx))

plot(split(iter(fut, !idx), c(ssb(iter(fut, !idx))[, '2019'] < 50000)))
           results[,  < 50000]

# M steepness lfreq

ggplot(merge(rec(stk), results, by='iter'),
  aes(x=year, y=data, colour=factor(lfreq), group=iter)) +
  geom_line() + facet_grid(M ~ steepness) +
  ylab("Recruitment (1000s)") + xlab("")

# sigmaR cpues llq

ggplot(merge(rec(stk), results, by='iter'),
  aes(x=year, y=data, colour=factor(cpues), group=iter)) +
  geom_line() + facet_grid(sigmaR ~ llq) + ylab("Recruitment (1000s)") + xlab("")

# EFFECT per factor on emergent properties and status

\subsubsection*{Stock Status}
\subsubsection*{Production Function}
\subsubsection*{Process Error}
\subsubsection*{ACF}


# SCALE: SB0

Reduce("+", lapply(c("M", "sigmaR", "steepness", "cpues", "lfreq", "llq"), function(x)
  ggplot(results, aes_string(paste0("factor(", x, ")"), 'SSB_Virgin', group=x)) +
  geom_boxplot(aes_string(fill=paste0("factor(", x, ")"))) +
  guides(fill="none") + xlab(x) + ylab("SB0 (t)")
)) + plot_annotation(title = 'Distribution of SB0 by factor and level')

# STATUS: SB2017/SBMSY

Reduce("+", lapply(c("M", "sigmaR", "steepness", "cpues", "lfreq", "llq"), function(x)
  ggplot(results, aes_string(paste0("factor(", x, ")"), 'SSB_status', group=x)) +
  geom_boxplot(aes_string(fill=paste0("factor(", x, ")"))) +
  guides(fill="none") + xlab(x) + ylab("SB 2017 (t)")
)) + plot_annotation(title = 'SB 2017 by factor and level')

# STATUS: SB2017/SB0

Reduce("+", lapply(c("M", "sigmaR", "steepness", "cpues", "lfreq", "llq"), function(x)
  ggplot(results, aes_string(paste0("factor(", x, ")"), 'SSB_depletion', group=x)) +
  geom_boxplot(aes_string(fill=paste0("factor(", x, ")"))) +
  guides(fill="none") + xlab(x) + ylab("SB 2017 (t)")
)) + plot_annotation(title = 'Depletion 2017 by factor and level')


# M sigmaR steepness cpues lfreq  llq


# - stock status: B/BMSY, B/B0, F/FMSY

# - production function: K, r, shape

ggplot(results, aes(x=r, group=M, fill=factor(M))) + geom_density(alpha=0.3) +
  facet_grid(steepness ~ .)
ggplot(results, aes(x=K, group=M, fill=factor(M))) + geom_density(alpha=0.3)

ggplot(results, aes(x=shape, group=M, fill=factor(M))) + geom_density(alpha=0.3)
ggplot(results, aes(x=shape, group=steepness, fill=factor(steepness))) +
  geom_density(alpha=0.3) + facet_wrap(~M)

# - process error: Recr_sigma

hist(results[, sigma_Rec])


# - nature ts: acf

dat <- merge(results, acfs, by='iter')

ggplot(dat[lag < 10,], aes(x=factor(lag), y=acf)) +
  geom_boxplot(aes(fill=factor(lag))) +
  facet_grid(M ~ steepness, labeller = label_both) + guides(fill="none") +
  xlab("Lag (y)") + ylab("ACF") +
  ggtitle("Natural mortality ~ steepness")

ggplot(dat[lag < 10,], aes(x=factor(lag), y=acf)) +
  geom_boxplot(aes(fill=factor(lag))) +
  facet_grid(llq ~ cpues, labeller = label_both) + guides(fill="none") +
  xlab("Lag (y)") + ylab("ACF") +
  ggtitle("LL catchability increase ~ CPUE")

ggplot(dat[lag < 10,], aes(x=factor(lag), y=acf)) +
  geom_boxplot(aes(fill=factor(lag))) +
  facet_grid(sigmaR ~ lfreq, labeller = label_both) + guides(fill="none") +
  xlab("Lag (y)") + ylab("ACF") +
  ggtitle("sigma(rec) ~ LF weighting")

