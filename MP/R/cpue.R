# cpue.R - DESC
# /cpue.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# fyma.oem {{{

#' @examples
#' fyma.oem(stock(om), deviances(oem), observations(oem),
#'   c(mseargs, ay=2000), FLQuant())

fyma.oem <- function(stk, deviances, observations, args, tracking) {

  # ASSESSMENT and DATA years
  ay <- args$ay
  say <- ac(ay)
  say0 <- ac(ay-1)
  dlag <- args$data_lag
  dataYears <- ac(seq(args$y0 + 2, args$ay - dlag))
  
  # GET historical cpue
  cpue <- window(observations$idx[[1]], end=ay-dlag)

  # INDEX ages
  sag <- ac(do.call(seq, unname(dims(cpue)[c("min", "max")])))

  # SURVEY time
  ctime <- sum(range(cpue)[c("startf", "endf")]) / 2

  # GENERATE new observation of change in abundance
  obs <- stock.n(stk)[sag, say] *
    exp(-harvest(stk)[sag, say] * ctime - m(stk)[sag, say] * ctime) *
    stock.wt(stk)[sag, say] * sel.pattern(cpue)[, say0] *
    deviances$idx$index.q[sag, say]

  obs0 <- stock.n(stk)[sag, say0] *
    exp(-harvest(stk)[sag, say0] * ctime - m(stk)[sag, say0] * ctime) *
    stock.wt(stk)[sag, say0] * sel.pattern(cpue)[, say0] *
    deviances$idx$index.q[sag, say]

  rat <- obs / obs0

  # NEW cpue from RoC(stock.n)
  index(cpue)[, say] <- index(cpue)[, say0] * rat

  # NEW index up to ay
  idx <- FLIndices(setNames(list(cpue), names(observations$idx)))
  observations$idx[[1]][,say] <- idx[[1]][,say]

  # OBSERVED catch data does not include bcatch
  catch(stk)[,ac(ay-dlag)] <- pmax(1e-16, c(catch(stk)[,ac(ay-dlag)] -
    observations$birds$catch[, ac(ay-dlag)]))

  list(stk=stk[,dataYears], idx=idx, deviances=deviances,
    observations=observations, tracking=tracking)
} # }}}

# cpue.ind {{{

cpue.ind <- function(stk, idx, args, nyears=5, ayears=3, ...) {

  ay <- args$ay
  dlag <- args$data_lag
  tracking <- list(...)$tracking
  
  # INDEX slot
  ind <- index(idx[[1]])[1:2,]

  # SUBSET last nyears from ay - mlag
  ind <- quantSums(ind[, ac(seq(ay - dlag - (nyears - 1) , length=nyears))] *
    stock.wt(stk)[1:2, ac(seq(ay - dlag - (nyears - 1) , length=nyears))])

  # SLOPE by iter
  dat <- data.table(as.data.frame(ind))
  slope <- dat[, .(slope=coef(lm(log(data)~year))[2]), by=iter]

  # WEIGHTED average index of last ayears
  mind <- yearSums(tail(ind, ayears) * 
   c(0.50 * seq(1, ayears - 1) / sum(seq(1, ayears - 1)), 0.50))

  # OUTPUT
  tracking["cpue.slope", ac(ay), 1] <- c(slope[,slope])
  tracking["cpue.mean", ac(ay), 1] <- mind

  list(stk=stk, tracking=tracking)
} # }}}

# cpue.hcr {{{
#' cpue.hcr
#'
#' @examples
#' data(ple4)

cpue.hcr <- function(stk, args, k1, k2, k3, k4, target=1,
  dtaclow=0.85, dtacupp=1.15, start=2020, min.mcpue=missing,
  min.catch=missing, tracking){

  ay <- args$ay
  dlag <- args$data_lag
  mlag <- args$management_lag

  # RECOVER slope & mean(cpue)
  slope <- tracking["cpue.slope", ac(ay), 1]
  mcpue <- tracking["cpue.mean", ac(ay), 1]

  # CALCULATE new tac
  ka <- ifelse(slope > 0, k1, k2)
  kb <- ifelse(mcpue > target, k3, k4)
  
  # TAC_y-1 ~ TAC_y * 1 + ka * m + kb * (mcpue - target)
  tac <- unitSums(catch(stk)[, ac(ay-dlag)]) *
    (1 + ka * slope + kb * (mcpue - target))

  # MIN mcpue
  if(!missing(min.mcpue)) {
    low <- ((mcpue / target) < min.mcpue)
    tac[low] <- min.catch
  }

  # TAC limits
  if(ay >= start) {
    rat <- c(tac / catch(stk)[, ac(ay-dlag)])
    iter(tac, rat < dtaclow) <- iter(catch(stk)[, ac(ay-dlag)], rat < dtaclow) *
      dtaclow
    iter(tac, rat > dtacupp) <- iter(catch(stk)[, ac(ay-dlag)], rat > dtacupp) *
      dtacupp
  }

  # C > 0
  tac[tac < 1e-16] <- 1e-16

  # VERBOSE
  cat(paste0("C: ", format(median(catch(stk)[, ac(ay-dlag)]), digits=2),
    " - TAC: ", format(median(c(tac)), digits=2), "\n"))
  
  ctrl <- getCtrl(c(tac), "catch", ay + mlag, dim(tac)[6])
  
	return(list(ctrl=ctrl, tracking=tracking))
} # }}}
