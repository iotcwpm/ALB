# utilities.R - DESC
# ioalbmse/R/utilities.R

# Copyright European Union, 2015-2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# sel {{{
setGeneric("sel", function(x, ...) standardGeneric("sel"))

setMethod("sel", signature(x="FLStock"),
  function(x) {
    
    har <- harvest(x)
    minx <- apply(har, 2:6, min)
    maxx <- apply(har, 2:6, max)
    return((har %-% minx) %/% (maxx-minx))
  }
) # }}}

# plot(FLStock, FLStock) {{{
setMethod("plot", signature(x="FLStock", y="FLStock"),
  function(x, y, ...) {

    args <- list(...)

    # FIND FLStock objects in ...
    idx <- unlist(lapply(args, is, "FLStock"))

    # CREATE FLStocks list
    fls <- c(list(x, y))
    nms <- c(deparse(substitute(x)), deparse(substitute(y)))

    if(!is.null(idx)) {
      fls <- c(fls, args[idx])

      # DEPARSE args names
      anms <- as.list(substitute(list(...)))[-1][idx]
      names(anms)[names(anms)==""] <- as.character(anms)[names(anms)==""]
      nms <- c(nms, unlist(lapply(anms, as.character)))
    }
    
    names(fls) <- nms

    # TODO PASS on other args to 'plot', args[!idx]
    do.call("plot", list(x=FLStocks(fls)))
  }
) # }}}

# iterDrop {{{
iterDrop <- function(object) {

	foo <- function(x) {
  	if(sum(apply(x@.Data, 1:5, var, na.rm=TRUE), na.rm=TRUE) == 0)
    	x[,,,,,1]
    else
    x}

  return(qapply(object, foo))
} # }}}

# residuals(FLStock, model, params) {{{
setMethod("residuals", signature(object="FLStock"),
  function(object, model, params) {

    if(is(model, 'character') | is(model, 'function'))
      model <- do.call(model, list())$model

    dmo <- dims(object)

    env <- list(
      ssb = window(ssb(object), end=dmo$maxyear - dmo$min))


    env <- c(env, as(params, 'list'))

    rhat <- eval(model[[3]], env)
    rec <- window(rec(object), start=dmo$minyear + dmo$min)
    res <- rec / rhat

    dimnames(res) <- dimnames(rec)

    return(res)

  }
) # }}}

# slimFLStock {{{
# TODO MOVE to combine as arg
slimFLStock <- function(x) {
  discards(x) <- discards(x)[,,,,,1]
  discards.n(x) <- discards.n(x)[,,,,,1]
  discards.wt(x) <- discards.wt(x)[,,,,,1]
  stock.wt(x) <- stock.wt(x)[,,,,,1]
  mat(x) <- mat(x)[,,,,,1]
  harvest.spwn(x) <- harvest.spwn(x)[,,,,,1]
  m.spwn(x) <- m.spwn(x)[,,,,,1]

  return(x)
} # }}}

# slim {{{
slim <- function(object) {

  # FIND repeated iters
  res <- qapply(object, function(x) {
    if(sum(iterVars(x)) == 0)
      x[,,,,,1]
    else
      x
    })

  return(res)
} # }}}
