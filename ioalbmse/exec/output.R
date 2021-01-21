# output.R - DESC
# /output.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


library(ioalbmse)
library(gridExtra)

# DATA

data(oms)

# RESULTS, 4 + 4 MPs

setwd('../../../runs/20170515/')

# --- TUNING CRITERIA 1, P(B == B_MSY) = 50%

load("tun1.RData")

qperf[, mp:=paste0("MP", as.numeric(factor(paste0(mp,run))))]
perf[, mp:=paste0("MP", as.numeric(factor(paste0(mp,run))))]

qperf[, `10%`:=`25%`]
qperf[, `90%`:=`75%`]

# 1. Boxplots

png(file="out/tun1-1.png")
ggplot(perf[indicator %in% c("S3", "S6", "F2", "Y1", "T1")],
  aes(x=mp, y=data, colour=mp)) +
  geom_boxplot(outlier.shape = NA, aes(fill=mp), colour="black") +
  facet_wrap(~name, scales='free_y') + xlab("") + ylab("")
dev.off()

# 2. plotTOs

png(file="out/tun1-2.png")
p1 <- plotTOs(qperf, "Y1", c("S3"), year=2034, colkey="mp")
p2 <- plotTOs(qperf, "Y1", c("S6"), year=2034, colkey="mp")
p3 <- plotTOs(qperf, "Y1", c("F2"), year=2034, colkey="mp")
p4 <- plotTOs(qperf, "Y1", c("T1"), year=2034, colkey="mp")
grid.arrange(p1, p2, p3, p4, ncol=2)
dev.off()

# 3. Table MPs ~ PMs

# 4. Kobe plot

png(file="out/tun1-3.png")
plotKobe(qperf, "S3", "S5", year=2034, colkey="mp", size=0.5) 
dev.off()

# 5. plotOMR SB/SB_MSY

png(file="out/tun1-4.png")
plotOMR(om,runs, qname="ssb")
dev.off()

# 6. plotOMR F/F_MSY

png(file="out/tun1-5.png")
plotOMR(om,runs, qname="fbar")
dev.off()

# 7. Table PMs by MP @ 1, 5, 10, 20 years


# --- TUNING CRITERIA 2, P(B == B_MSY) = 50%

load("tun2.RData")

qperf[, mp:=paste0("MP", as.numeric(factor(paste0(mp,run))))]
perf[, mp:=paste0("MP", as.numeric(factor(paste0(mp,run))))]

qperf[, `10%`:=`25%`]
qperf[, `90%`:=`75%`]

# 1. Boxplots

png(file="out/tun2-1.png")
ggplot(perf[indicator %in% c("S3", "S6", "F2", "Y1", "T1")],
  aes(x=mp, y=data, colour=mp)) +
  geom_boxplot(outlier.shape = NA, aes(fill=mp), colour="black") +
  facet_wrap(~name, scales='free_y') + xlab("") + ylab("")
dev.off()

# 2. plotTOs

png(file="out/tun2-2.png")
p1 <- plotTOs(qperf, "Y1", c("S3"), year=2034, colkey="mp")
p2 <- plotTOs(qperf, "Y1", c("S6"), year=2034, colkey="mp")
p3 <- plotTOs(qperf, "Y1", c("F2"), year=2034, colkey="mp")
p4 <- plotTOs(qperf, "Y1", c("T1"), year=2034, colkey="mp")
grid.arrange(p1, p2, p3, p4, ncol=2)
dev.off()

# 3. Table MPs ~ PMs

# 4. Kobe plot

png(file="out/tun2-3.png")
plotKobe(qperf, "S3", "S5", year=2034, colkey="mp", size=0.5) 
dev.off()

# 5. plotOMR SB/SB_MSY

png(file="out/tun2-4.png")
plotOMR(om,runs, qname="ssb")
dev.off()

# 6. plotOMR F/F_MSY

png(file="out/tun2-5.png")
plotOMR(om,runs, qname="fbar")
dev.off()

# 7. Table PMs by MP @ 1, 5, 10, 20 years

