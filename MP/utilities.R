# utilities.R - DESC
# /utilities.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(JABBA)

# jabba.sa {{{

#' @examples
#' data(ple4)
#' data(ple4.index)
#' jabba.sa(ple4, FLIndices(A=ple4.index), FLQuant(
#' dimnames=list(metric='conv.est', year=1950:2020))

jabba.sa <- function(stk, idx, args, tracking, idx.se=rep(0.2, length(idx)),
  model.type = c("Fox", "Schaefer", "Pella", "Pella_m"), verbose=FALSE) {

  # PREPARE outputs

  B <- stock(stk) %=% an(NA)
  
  refpts <- FLPar(NA, dimnames=list(params=c("FMSY", "BMSY", "MSY", "K", "B0"),
    iter=dimnames(stk)$iter), units=c("t", "f", "t", "t", "t"))

  conv <- rep(0, args$it)

  # LOOP
  for(i in seq(args$it)) {

    # EXTRACT catch and index
    
    ca <- as.data.frame(iter(catch(stk), i), drop=TRUE)
    id <- model.frame(window(iter(lapply(idx, index), i), start=args$y0), drop=TRUE)
    se <- id

    # ASSIGN idx.se
    se[, -1] <- as.list(idx.se)

    # CONSTRUCT input object
    inp <- build_jabba(catch=ca, cpue=id, se=se,
      assessment="STK", scenario="jabba.sa", model.type=match.arg(model.type),
      sigma.est=FALSE, fixed.obsE=0.05, verbose=verbose)
    
    # FIT
    # capture.output({fit <- fit_jabba(inp, quickmcmc=TRUE)}, type="message")
    fit <- tryCatch(
      fit_jabba(inp, quickmcmc=TRUE, verbose=FALSE, progress.bar="none"),
      # error, RETURN 0 output
      error = function(e) return(list(
        timeseries=array(9, dim=c(dim(ca)[1],1,1)),
        refpts=data.frame(k=9, bmsy=1, fmsy=0.1, msy=1, b0=9)))
    )

    # B
    iter(B, i)[] <- fit$timeseries[,1,1]

    # refpts
    iter(refpts, i) <- unlist(fit$refpts[1, c('fmsy', 'bmsy', 'msy', 'k', 'k')])

    # tracking
    if(length(fit) > 2) {
      conv[i] <- 1
    }
    # message(i)
  }

  # STORE outputs: biomass in @stock
  stock(stk) <- B

  # refpts as attribute
  attr(stk, "refpts") <- refpts
  
  # EMPTY stock.n to avoid calls to ssb()
  stock.n(stk) <- as.numeric(NA)

  # TRACK convergence
  track(tracking, "conv.est", ac(args$ay)) <- conv
  
  list(stk = stk, tracking = tracking)
}
# }}}

# refpts(FLStock) {{{

setMethod("refpts", signature(object="FLStock"),
  function(object) {

    if(is.null(attr(object, "refpts")))
      return(FLPar())
    else
      return(attr(object, "refpts"))
  }
)
# }}}
