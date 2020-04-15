# plotmo.R: plot the model response when varying one or two predictors
#
# Stephen Milborrow Sep 2006 Cape Town

plotmo <- function(object = stop("no 'object' argument"),
    type         = NULL,
    nresponse    = NA,
    pmethod      = "plotmo",

    pt.col       = 0,
    jitter       = .5,
    smooth.col   = 0,
    level        = 0,
    func         = NULL,
    inverse.func = NULL,
    nrug         = 0,
    grid.col     = 0,
    type2        = "persp",

    degree1      = TRUE,
    all1         = FALSE,
    degree2      = TRUE,
    all2         = FALSE,

    do.par       = TRUE,
    clip         = TRUE,
    ylim         = NULL,
    caption      = NULL,
    trace        = 0,

    grid.func    = NULL,
    grid.levels  = NULL,
    extend       = 0,
    ngrid1       = 50,
    ngrid2       = 20,

    ndiscrete    = 5,
    npoints      = 3000,
    center       = FALSE,
    xflip        = FALSE,
    yflip        = FALSE,
    swapxy       = FALSE,
    int.only.ok  = TRUE,

    ...)
{
    init.global.data()
    on.exit({init.global.data(); gc()}) # release memory on exit
    object.name <- quote.deparse(substitute(object))
    object # make sure object exists
    trace <- as.numeric(check.integer.scalar(trace, logical.ok=TRUE))
    # Associate the model environment with the object.
    # (This is instead of passing it as an argument to plotmo's data access
    # functions.  It saves a few hundred references to model.env in the code.)
    attr(object, ".Environment") <- get.model.env(object, object.name, trace)
    temp <- plotmo_prolog(object, object.name, trace, ...)
        object  <- temp$object
        my.call <- temp$my.call
    # We will later make two passes through the plots if we need to
    # automatically determine ylim (see get.ylim.by.dummy.plots).
    # The trace2 variable is used for disabling tracing on the second pass.
    trace2 <- trace
    # trace=100 to 103 are special values used for development
    # (they are for tracing just plotmo_x with no plotting)
    special.trace <- FALSE
    if(trace >= 100 && trace <= 103) {
        special.trace <- TRUE
        trace <- trace - 100
    }
    pmethod <- match.choices(pmethod, c("plotmo", "partdep", "apartdep"), "pmethod")
    clip    <- check.boolean(clip)
    all1    <- check.boolean(all1)
    all2    <- check.integer.scalar(all2, min=0, max=2)
    center  <- check.boolean(center)
    swapxy  <- check.boolean(swapxy)
    xflip   <- check.boolean(xflip)
    yflip   <- check.boolean(yflip)
    type2   <- match.choices(type2, c("persp", "contour", "image"), "type2")
    level   <- get.level(level, ...)
    pt.col  <- get.pt.col(pt.col, ...)
    jitter  <- get.jitter(jitter, ...)
    smooth.col <- get.smooth.col(smooth.col, ...)
    check.integer.scalar(ndiscrete, min=0)
    extend <- check.numeric.scalar(extend)
    stopifnot(extend > -.3, extend <= 10) # .3 prevents shrinking to nothing, 10 is arb
    if(!is.specified(degree1))   degree1   <- 0
    if(!is.specified(degree2))   degree2   <- 0
    if(!is.specified(nresponse)) nresponse <- NA
    if(!is.specified(clip))      clip      <- FALSE
    if(center && clip) {
        clip <- FALSE # otherwise incorrect clipping (TODO revisit)
        warning0("forcing clip=FALSE because center=TRUE ",
                 "(a limitation of the current implementation)")
    }
    # get x so we can get the predictor names and ux.list
    x <- plotmo_x(object, trace)
    if(NCOL(x) == 0 || NROW(x) == 0)
        stop("x is empty") # seen with an intercept only model for some model classes (not earth)
    if(special.trace) # special value of trace was used?
        return(invisible(x))
    meta <- plotmo_meta(object, type, nresponse, trace,
                msg.if.predictions.not.numeric=
                    if(level > 0) "the level argument is not allowed" else NULL,
                ...)
      y         <- meta$y.as.numeric.mat # y as a numeric mat, only the nresponse column
      nresponse <- meta$nresponse        # column index
      resp.name <- meta$resp.name        # used only in automatic caption, may be NULL
      resp.levs <- meta$resp.levs        # to convert predicted strings to factors, may be NULL
      type      <- meta$type             # always a string (converted from NULL if necessary)

    ngrid1 <- get.ngrid1(ngrid1, y, ...)
    ngrid2 <- get.ngrid2(ngrid2, y, ...)
    n.apartdep <- ngrid1

    # following prevents aliasing on nrow(data) to ensure we catch the following:
    # "warning: predict(): newdata' had 31 rows but variable(s) found have 30 rows"
    if(ngrid1 == length(y)) {
        trace2(trace, "changed ngrid1 from %g to %g\n", ngrid1, ngrid1+1)
        ngrid1 <- ngrid1 + 1
    }
    temp <- get.unique.xyvals(x, y, npoints, trace)
        ux.list <- temp$ux.list # list, each elem is unique vals in a column of x
        uy      <- temp$uy      # unique y vals
        npoints <- temp$npoints

    y <- apply.inverse.func(inverse.func, y, object, trace)
    if(center)
        y <- my.center(y, trace)

    # get iresponse
    ncases <- nrow(x)
    iresponse <- NULL
    if(is.specified(pt.col)) {
        iresponse <- get.iresponse(npoints, ncases)
        if(is.null(iresponse))
            pt.col <- 0
    }
    # singles is a vector of indices of predictors for degree1 plots
    singles <- plotmo_singles(object, x, nresponse, trace, degree1, all1)
    nsingles <- length(singles)

    # each row of pairs is the indices of two predictors for a degree2 plot
    pairs <- plotmo_pairs(object, x, nresponse, trace, all2, degree2)
    npairs <- NROW(pairs)

    temp <- get.pred.names(colnames.x=colnames(x), nfigs=nsingles + npairs, ...)
        pred.names      <- temp$pred.names
        abbr.pred.names <- temp$abbr.pred.names
        def.cex.main    <- temp$def.cex.main

    is.int.only <- FALSE # is intercept only model?
    if(nsingles == 0 && npairs == 0) {
        # is this an intercept only model? (which causes nsingles == 0 && npairs == 0)
        # if so, we plot it anyway (unless degree1=0)
        trace2(trace, "\n----plotmo_singles for %s object, all1=FALSE %s \n",
               class(object)[1], "(determine if is.int.only)")
        sing <- plotmo.singles(object=object, x=x, nresponse=nresponse,
                               trace=trace, all1=FALSE) # note that all1=FALSE
        is.int.only <- length(sing) == 0
        trace2(trace, if(is.int.only) "intercept-only model\n\n"
                      else            "model has an intercept\n\n")
    }
    if(is.int.only && int.only.ok && !all(degree1 == 0)) {
        singles  <- 1 # plot the first predictor
        nsingles <- 1
    }
    if(nsingles > 64 && trace >= 0) {
        cat0("More than 64 degree1 plots.\n",
             "Consider using plotmo's degree1 argument to limit the number of plots.\n",
             "For example,  degree1=1:10  or  degree1=c(\"",
             pred.names[singles[1]],
             "\", \"",
             pred.names[singles[2]],
             "\")\n",
             "Call plotmo with trace=-1 to make this message go away.\n\n")
    }
    else if(nsingles > 200) { # 220 is arb, 15 * 15
        warning0("Will plot only the first 200 degree1 plots (of ",
                 nsingles, " degree1 plots)")
        singles <- singles[1:200]
        nsingles <- length(singles)
    }
    if(npairs > 64 && trace >= 0) {
        cat0("More than 64 degree2 plots.\n",
             "Consider using plotmo's degree2 argument to limit the number of plots.\n",
             "For example,  degree2=1:10  or  degree2=\"",
             pred.names[singles[1]], "\"\n",
             "Call plotmo with trace=-1 to make this message go away.\n\n")
    }
    else if(npairs > 200) {
        warning0("Will plot only the first 200 degree2 plots (of ",
                  npairs, " degree2 plots)")
        pairs <- pairs[1:200,]
        npairs <- NROW(pairs)
    }
    if(extend != 0 && npairs) {
        warning0("extend=", extend, ": will not plot degree2 plots ",
                 "(extend is not yet implemented for degree2 plots)")
        pairs <- NULL
        npairs <- 0
    }
    nfigs <- nsingles + npairs
    if(nfigs == 0) {
        if(trace >= 0)
            warning0("plotmo: nothing to plot")
        return(invisible())
    }
    do.par <- check.do.par(do.par, nfigs) # do.par is 0, 1, or 2
    # Prepare caption --- we need it now for do.par() but
    # can only display it later after at least one plot.
    # nfigs=2 (any number greater than 1) because by default we do.par in plotmo.
    caption <- get.caption(nfigs=2, do.par, caption, resp.name, type,
                           getCall(object), object.name, my.call)
    if(do.par) {
        # TODO document what happens here and in plotres if only one plot
        oldpar <- par(no.readonly=TRUE)
        # need xlab etc. so so we can figure out margin sizes in do.par
        xlab <- dota("xlab", DEF="", ...)
        ylab <- dota("ylab", DEF="", ...)
        main <- dota("main",         ...)
        do.par(nfigs=nfigs, caption=caption, main1=main,
               xlab1=xlab, ylab1=ylab, trace=trace, def.cex.main=def.cex.main, ...)
        if(do.par == 1)
            on.exit(par(oldpar), add=TRUE)
    } else { # do.par=FALSE
        oldpar <- do.par.dots(..., trace=trace)
        if(length(oldpar))
            on.exit(do.call(par, oldpar), add=TRUE)
    }
    trace2(trace, "\n----Figuring out ylim\n")
    is.na.ylim <- !is.null(ylim) && anyNA(ylim)
    jittered.y <- apply.jitter(as.numeric(y), jitter)
    # get.ylim will do dummy plots if necessary
    temp <- get.ylim(object=object,
        type=type, nresponse=nresponse, pmethod=pmethod,
        pt.col=pt.col, jitter=jitter, smooth.col=smooth.col, level=level,
        func=func, inverse.func=inverse.func, nrug=nrug, grid.col=grid.col,
        type2=type2, degree1=degree1, all1=all1, degree2=degree2, all2=all2,
        do.par=do.par, clip=clip, ylim=ylim, caption=caption, trace=trace,
        grid.func=grid.func, grid.levels=grid.levels, extend=extend,
        ngrid1=ngrid1, ngrid2=ngrid2, npoints=npoints, ndiscrete=ndiscrete,
        int.only.ok=int.only.ok, center=center, xflip=xflip, yflip=yflip,
        swapxy=swapxy, def.cex.main=def.cex.main,
        x=x, y=y, singles=singles, resp.levs=resp.levs,
        ux.list=ux.list,
        pred.names=pred.names, abbr.pred.names=abbr.pred.names,
        nsingles=nsingles, npairs=npairs, nfigs=nfigs, uy=uy,
        is.na.ylim=is.na.ylim, is.int.only=is.int.only, trace2=trace2,
        pairs=pairs, iresponse=iresponse, jittered.y=jittered.y,
        n.apartdep=n.apartdep, ...)
    ylim   <- temp$ylim
    trace2 <- temp$trace2
    if(nsingles)
        plot.degree1(object=object, degree1=degree1, all1=all1, center=center,
            ylim=if(is.na.ylim) NULL else ylim, # each graph has its own ylim?
            type=type, nresponse=nresponse, pmethod=pmethod,
            trace=trace, trace2=trace2,
            pt.col=pt.col, jitter=jitter, iresponse=iresponse,
            smooth.col=smooth.col, grid.col=grid.col, inverse.func=inverse.func,
            grid.func=grid.func, grid.levels=grid.levels, extend=extend,
            ngrid1=ngrid1, is.int.only=is.int.only, level=level,
            func=func, nrug=nrug,
            draw.plot=TRUE, x=x, y=y, singles=singles, resp.levs=resp.levs,
            ux.list=ux.list, ndiscrete=ndiscrete,
            pred.names=pred.names, abbr.pred.names=abbr.pred.names,
            nfigs=nfigs, uy=uy, xflip=xflip, jittered.y=jittered.y,
            n.apartdep=n.apartdep, ...)
    if(npairs)
        plot.degree2(object=object, degree2=degree2, all2=all2, center,
            ylim=if(is.na.ylim) NULL else ylim, # each graph has its own ylim?
            type=type, nresponse=nresponse, pmethod=pmethod,
            clip=clip, trace=trace, trace2=trace2,
            pt.col=pt.col, jitter=jitter, iresponse=iresponse,
            inverse.func=inverse.func,
            grid.func=grid.func, grid.levels=grid.levels, extend=extend,
            type2=type2, ngrid2=ngrid2, draw.plot=TRUE, do.par=do.par, x=x, y=y,
            pairs=pairs, resp.levs=resp.levs, ux.list=ux.list,
            ndiscrete=ndiscrete,
            pred.names=pred.names, abbr.pred.names=abbr.pred.names,
            nfigs=nfigs, nsingles=nsingles, npairs=npairs, xflip=xflip, yflip=yflip,
            swapxy=swapxy, def.cex.main=def.cex.main, n.apartdep=n.apartdep,
            ...)
    draw.caption(caption, ...)
    invisible(x)
}
# plotmo.retval <- function(x, singles, pairs, pred.names) # plotmo's return value
# {
#     degree1 <- vector("list", length(singles))
#     names <- vector("character", length(singles))
#     for(isingle in seq_along(singles)) {
#         ipred <- singles[isingle] # ipred is the predictor index i.e. col in model mat
#         temp <- degree1.data(isingle)
#         stopifnot(!is.null(temp))
#         stopifnot(nrow(temp$xframe) == length(temp$yhat))
#         data <- data.frame(temp$xframe[[ipred]], temp$yhat)
#         names[isingle] <- pred.names[ipred]
#         colnames(data) <- c(pred.names[ipred], "PLOTMO")
#         degree1[[isingle]] <- data
#     }
#     names(degree1) <- names
#
#     npairs <- NROW(pairs)
#     degree2 <- vector("list", npairs)
#     names <- vector("character", npairs)
#     for(ipair in seq_len(npairs)) {
#         ipred1 <- pairs[ipair,1]  # index of first predictor
#         ipred2 <- pairs[ipair,2]  # index of second predictor
#         temp <- degree2.data(ipair)
#         stopifnot(!is.null(temp))
#         # TODO this fails if blockify.degree2.frame kicks in
#         stopifnot(nrow(temp$xframe) == length(temp$yhat))
#         data <- data.frame(temp$xframe[ipred1], temp$xframe[ipred2], as.vector(temp$yhat))
#         names[ipair] <- paste0(pred.names[ipred1], ":", pred.names[ipred1])
#         colnames(data) <- c(pred.names[ipred1], pred.names[ipred2], "PLOTMO")
#         degree2[[ipair]] <- data
#     }
#     names(degree2) <- names
#
#     list(x=x, degree1=degree1, degree2=degree2)
# }
plotmo_prolog <- function(object, object.name, trace, ...)
{
    object <- plotmo.prolog(object, object.name, trace, ...)
    my.call <- call.as.char(n=2)
    SHOWCALL <- dota("SHOWCALL", ...)
    if(!is.specified(SHOWCALL))
        my.call <- NULL
    list(object=object, my.call=my.call)
}
get.pred.names <- function(colnames.x, nfigs, ...)
{
    # numbers below are somewhat arb
    nrows <- ceiling(sqrt(nfigs)) # nrows in plot grid
    minlength <- 20; def.cex.main <- 1.2
    if     (nrows >= 9) { minlength <- 6;  def.cex.main <- .7  }
    else if(nrows >= 8) { minlength <- 7;  def.cex.main <- .8  }
    else if(nrows >= 7) { minlength <- 7;  def.cex.main <- .8  }
    else if(nrows >= 6) { minlength <- 7;  def.cex.main <- .8  }
    else if(nrows >= 5) { minlength <- 8;  def.cex.main <- 1   }
    else if(nrows >= 4) { minlength <- 9;  def.cex.main <- 1.1 }
    stopifnot(!is.null(colnames.x)) # plotmo_x always returns colnames (unless no columns)
    minlength <- dota("prednames.minlength", DEF=minlength, ...)
    prednames.abbreviate <- dota("prednames.abbreviate", DEF=TRUE, ...)
    prednames.abbreviate <- check.boolean(prednames.abbreviate)
    abbr.pred.names <-
        if((prednames.abbreviate))
            abbreviate(strip.space(colnames.x), minlength=minlength, method="both.sides")
        else
            colnames.x
    list(pred.names      = colnames.x,
         abbr.pred.names = abbr.pred.names,
         def.cex.main    = def.cex.main)
}
# always returns a vector of 2 elems, could be c(-Inf, Inf)
get.ylim <- function(object,
    type, nresponse,  pmethod,
    pt.col, jitter, smooth.col, level, func,
    inverse.func, nrug, grid.col, type2, degree1, all1, degree2, all2,
    do.par, clip, ylim, caption, trace,
    grid.func, grid.levels, extend=extend, ngrid1, ngrid2,
    npoints, ndiscrete, int.only.ok, center, xflip, yflip, swapxy, def.cex.main,
    x, y, singles, resp.levs, ux.list, pred.names, abbr.pred.names,
    nsingles, npairs, nfigs, uy,
    is.na.ylim, is.int.only, trace2, pairs,
    iresponse, jittered.y, n.apartdep, ...)
{
    get.ylim.by.dummy.plots <- function(..., trace)
    {
        # call the plotting functions with draw.plot=FALSE to get the ylim
        trace2(trace, "--get.ylim.by.dummy.plots\n")
        all.yhat <- NULL
        if(nsingles) { # get all.yhat by calling with draw.plot=FALSE
            # have to use explicit arg names to prevent alias probs
            # with dots, because the user can pass in any name with dots
            all.yhat <- c(all.yhat,
              plot.degree1(object=object, degree1=degree1, all1=all1,
                center=center,
                ylim=ylim, type=type,  nresponse=nresponse, pmethod=pmethod,
                trace=trace, trace2=trace2, pt.col=pt.col,
                jitter=jitter, iresponse=iresponse,
                smooth.col=smooth.col, grid.col=grid.col,
                inverse.func=inverse.func, grid.func=grid.func,
                grid.levels=grid.levels, extend=extend, ngrid1=ngrid1,
                is.int.only=is.int.only,
                level=level, func=func, nrug=nrug, draw.plot=FALSE, x=x, y=y,
                singles=singles, resp.levs=resp.levs,
                ux.list=ux.list, ndiscrete=ndiscrete,
                pred.names=pred.names, abbr.pred.names=abbr.pred.names,
                nfigs=nfigs, uy=uy, xflip=xflip, jittered.y=jittered.y,
                n.apartdep=n.apartdep, ...))
        }
        if(npairs) {
            all.yhat <- c(all.yhat,
                plot.degree2(object=object, degree2=degree2, all2=all2,
                    center=center, ylim=ylim,
                    type=type, nresponse=nresponse, pmethod=pmethod,
                    clip=clip, trace=trace, trace2=trace2, pt.col=pt.col,
                    jitter=jitter, iresponse=iresponse,
                    inverse.func=inverse.func, grid.func=grid.func,
                    grid.levels=grid.levels, extend=extend,
                    type2=type2, ngrid2=ngrid2,
                    draw.plot=FALSE, do.par=do.par, x=x, y=y, pairs=pairs,
                    resp.levs=resp.levs, ux.list=ux.list,
                    ndiscrete=ndiscrete,
                    pred.names=pred.names, abbr.pred.names=abbr.pred.names,
                    nfigs=nfigs,
                    nsingles=nsingles, npairs=npairs, xflip=xflip, yflip=yflip,
                    swapxy=swapxy, def.cex.main=def.cex.main, n.apartdep=n.apartdep,
                    ...))
        }                            #  1    2   3    4  5
        q <- quantile(all.yhat, probs=c(0, .25, .5, .75, 1), names=FALSE)
        ylim <- c(q[1], q[5]) # all the data
        check.vec(ylim, "automatic ylim", expected.len=2)
        # iqr test to prevent clipping in some pathological cases
        iqr  <- q[4] - q[2]   # middle 50% of the data (inter-quartile range)
        if(clip && !is.na(iqr) && iqr > .05 * (max(y) - min(y))) {
            median <- q[3]
            ylim[1] <- max(ylim[1], median - 10 * iqr)
            ylim[2] <- min(ylim[2], median + 10 * iqr)
        }
        if(is.specified(pt.col) || is.specified(smooth.col) || is.specified(level))
            ylim <- range1(ylim, jittered.y) # ensure ylim big enough for resp points
        else if(is.specified(smooth.col))
            ylim <- range1(ylim, y)
        # binary or ternary reponse?
        # the range(uy) test is needed for binomial models specified using counts
        else if(length(uy) <= 3 || all(range(y) == c(0,1)))
            ylim <- range1(ylim, y)
        if(is.specified(nrug)) # space for rug
            ylim[1] <- ylim[1] - .1 * (ylim[2] - ylim[1])
        trace2(trace, "--done get.ylim.by.dummy.plots\n\n")
        # have called the plot functions, minimize tracing in further calls to them
        trace2 <<- 0 # note <<- not <-
        ylim
    }
    #--- get.ylim starts here
    if(!(is.null(ylim) || is.na(ylim[1]) || length(ylim) == 2))
        stop0("ylim must be one of:\n",
              "  NULL        all graphs have same vertical axes\n",
              "  NA          each graph has its own vertical axis\n",
              "  c(min,max)  ylim for all graphs")
    if(length(ylim) == 2 && ylim[2] <= ylim[1])
        stop0("ylim[2] ", ylim[2], " is not greater than ylim[1] ", ylim[1])
    if(is.na.ylim)
        ylim <- c(NA, NA)  # won't be used
    else if(is.null(ylim)) # auto ylim
        ylim <-
            if(is.yaxis.a.probability(object, type, trace)) {
                if(is.specified(pt.col))
                    c(-0.1, 1.1) # leave space for possibly jittered points
                else
                    c(0, 1)
            } else if(is.int.only)
                range(y, na.rm=TRUE)
            else
                get.ylim.by.dummy.plots(trace=trace, ...)
    if(!anyNA(ylim))
        ylim <- fix.lim(ylim)
    if(trace >= 2)
        printf("ylim c(%.4g, %.4g)    clip %s\n\n",
                ylim[1], ylim[2], if(clip) "TRUE" else "FALSE")
    list(ylim=ylim, trace2=trace2)
}
do.persp.auto.par <- function(simple.ticktype) # want small margins for bigger persp plots
{
    # persp ignores both the global mgp and any mgp passed as arguments
    # directly to persp so we must adjust margins using par()
    old.mar <- par("mar")
    axis.space <- max(par("mgp"))
    mar <- old.mar
    if(simple.ticktype) {
        # Reduce bottom and left margins so we get a bigger persp plot.
        # This puts the bottom corner of the perp plot at same height at the
        # bottom of the axis labels on the degree1 plots.
        mar[1] <- max(mar[1] - axis.space - .5,  .5) # bottom margin
        mar[2] <- max(mar[2] - axis.space - .5, .5)  # left margin
    } else { # detailed
        mar[1] <- min(mar[1], 1)                     # enough space for axes
        mar[2] <- min(mar[2], 1)
    }
    par(mar=mar)
}
do.degree2.auto.par <- function(type2, nfigs, simple.ticktype)
{
    if(type2 == "persp")                # perspective plot
        do.persp.auto.par(simple.ticktype)
    else {                              # contour or image plot
        nrows <- ceiling(sqrt(nfigs))
        if(nrows >= 5)
            mar <- c(2, 2, 1.2, .5)     # space for bottom and left axis labels
        else
            mar <- c(3, 3, 2, .5)
        par(mar=mar)
        cex <- par("cex")               # TODO would be better to use nfigs here?
        mgp <-                          # compact title and axis annotations
            if     (cex < .7) c(1.2, 0.2, 0)
            else if(cex < .8) c(1.3, 0.3, 0)
            else              c(1.5, 0.4, 0)
        par(mgp=mgp)
    }
}
plotmo_singles <- function(object, x, nresponse, trace, degree1, all1)
{
    trace2(trace, "\n----plotmo_singles for %s object\n", class(object)[1])
    singles <- plotmo.singles(object=object, x=x, nresponse=nresponse,
                              trace=trace, all1=all1)
    if(is.character(degree1)) # get all singles, not just those used in the model?
        singles <- seq_len(NCOL(x))
    if(!is.null(singles) && any(is.na(singles))) { # !is.null required only for old R
        # Following occurs when plotting
        #   train(Petal.Length ~ ., data=iris, method="rpart", tuneLength=4)
        # because caret converts factor predictors to indicator columns and
        # thus creates new variable names e.g. Speciesversicolor
        warning0("NA in singles, will plot all variables (as if all1=TRUE)")
        singles <- seq_len(NCOL(x))
    }
    if(length(singles))
        singles <- sort.unique(singles) # this will drop NAs if any
    nsingles <- length(singles)
    if(length(singles)) {
        degree1 <- check.index(degree1, "degree1", singles, colnames=colnames(x),
                               allow.empty=TRUE, is.degree.spec=TRUE)
        singles <- singles[degree1]
    } else if(is.degree.specified(degree1) && degree1[1] != 0 && trace >= 0)
        warning0("'degree1' specified but no degree1 plots")
    if(trace >= 2) {
        if(length(singles))
            cat("singles:", paste0(singles, " ", colnames(x)[singles], collapse=", "), "\n")
        else
            cat("no singles\n")
    }
    singles # a vector of indices of predictors for degree1 plots
}
plotmo_pairs <- function(object, x, nresponse, trace, all2, degree2)
{
    trace2(trace, "\n----plotmo_pairs for %s object\n", class(object)[1])
    pairs <- NULL
    if(is.character(degree2) && length(degree2) == 2) {
        # degree2 is a two element character vector
        # treat as a special case (intentional inconsistency)
        singles <- seq_len(NCOL(x)) # get all singles, not just those used in the model
        i1  <- check.index(degree2[1], "degree2", singles, colnames=colnames(x))
        if(length(i1) > 0) {
            i2 <- check.index(degree2[2], "degree2", singles, colnames=colnames(x))
            if(length(i2) > 0) {
                if(i1[1] == i2[1])
                    warning0("both elements of degree2 are the same")
                pairs <- matrix(c(i1[1], i2[1]), nrow=1, ncol=2)
            }
        }
    } else {
        pairs <-
            if(all2)
                get.all.pairs.from.singles(object, x, trace, all2)
            else
                plotmo.pairs(object, x, nresponse, trace, all2)
        if(NROW(pairs)) {
            # put lowest numbered predictor first and remove duplicate pairs
            pairs <- unique(t(apply(pairs, 1, sort)))
            # order the pairs on the predictor order
            order <- order(pairs[,1], pairs[,2])
            pairs <- pairs[order, , drop=FALSE]
            i <- check.index(degree2, "degree2", pairs, colnames=colnames(x),
                             allow.empty=TRUE, is.degree.spec=TRUE)
            pairs <- pairs[i, , drop=FALSE] # length(i) will be 0 if check.index not ok
        } else if (is.degree.specified(degree2) && degree2[1] != 0 && trace >= 0)
            warning0("'degree2' specified but no degree2 plots")
    }
    if(trace >= 2) {
        if(NROW(pairs)) {
            cat("pairs:\n")
            print(matrix(paste(pairs, colnames(x)[pairs]), ncol=2))
        } else
            cat("no pairs\n")
    }
    pairs
}
# pt.col is a formal arg, but for back compat we also support col.response
get.pt.col <- function(pt.col, ...)
{
    pt.col <- pt.col
    if(!is.specified(pt.col) && !is.dot("col", ...))
        pt.col <- dota("col.response", EX=0, ...) # partial match, "col" excluded above
    # if any other response argument is specified, set the response color
    if(!is.specified(pt.col) &&
            is.dot("pch cex.response pch.response pt.cex pt.pch",
                   EX=c(1,1,1,0,0), ...))
        pt.col <- "slategray4"
    if(!is.specified(pt.col))
        pt.col <- 0
    pt.col
}
get.jitter <- function(jitter, ...)
{
    if(anyNA(jitter)) # allow jitter=NA
        jitter <- 0
    check.numeric.scalar(jitter, logical.ok=TRUE)
    jitter <- as.numeric(jitter)
    if(jitter < 0 || jitter > 100)
        stop0("jitter=", jitter, " is illegal")
    jitter
}
get.smooth.col <- function(smooth.col, ...)
{
    smooth.col <- dota("col.smooth", DEF=smooth.col, ...) # back compat
    # if any other smooth argument is specified, set the smooth color
    if(!is.specified(smooth.col) &&
            is.dot("lty.smooth lwd.smooth lwd.loess smooth.lty smooth.lwd",
                   EX=c(1,1,1,0,0), ...))
        smooth.col <- 2
    if(!is.specified(smooth.col))
        smooth.col <- 0
    smooth.col
}
get.ngrid1 <- function(ngrid1, y, ...)
{
    check.integer.scalar(ngrid1)
    if(ngrid1 < 2)
        stop0("illegal ngrid1 ", ngrid1)
    if(ngrid1 > 1000) {
        warning0("clipped ngrid1=", ngrid1, " to 1000")
        ngrid1 <- 1000
    }
    ngrid1
}
get.ngrid2 <- function(ngrid2, y, ...)
{
    check.integer.scalar(ngrid2)
    if(ngrid2 < 2)
        stop0("illegal ngrid2 ", ngrid2)
    if(ngrid2 > 500) {
        warning0("clipped ngrid2=", ngrid2, " to 500")
        ngrid2 <- 500
    }
    ngrid2
}
get.level <- function(level, ...)
{
    if(anyNA(level) || is.null(level)) # treat NA and NULL as 0
        level <- 0
    check.numeric.scalar(level)
    # some code for backward compatibility (se is now deprecated)
    se <- 0
    if(is.dot("se", ...))
        se <- dota("se", ...)
    check.numeric.scalar(se, logical.ok=TRUE)
    if(se && level) # both specified?
        stop0("plotmo's 'se' argument is deprecated, please use 'level' instead")
    if(identical(se, TRUE)) {
        level <- .95
        warning0(
            "plotmo's 'se' argument is deprecated, please use 'level=.95' instead")
    } else if (se < 0 || se > 5) # 5 is arb
        stop0("plotmo's 'se' argument is deprecated, please use 'level=.95' instead")
    else if (se > 0 && se < 1)   # e.g. se=.95
        stop0("plotmo's 'se' argument is deprecated, please use 'level=.95' instead")
    else if (se > 0) {
        level <- 1 - 2 * (1 - pnorm(se)) # se=2 becomes level=.954
        warning0(sprint(
            "plotmo's 'se' argument is deprecated, please use 'level=%.2f' instead",
            level))
    } else if(level != 0 && (level < .5 || level >= 1))
        stop0("level=", level, " is out of range, try level=.95")
    level
}
get.unique.xyvals <- function(x, y, npoints, trace)
{
    # convert special values of npoints
    ncases <- nrow(x)
    check.integer.scalar(npoints, min=-1, null.ok=TRUE, logical.ok=TRUE)
    npoints.was.neg <- FALSE
    if(is.null(npoints))
        npoints <- 0
    else if(is.logical(npoints))
        npoints <- if(npoints) ncases else 0
    else if(npoints == -1) {
        npoints.was.neg <- TRUE
        npoints <- ncases
    } else if(npoints > ncases)
        npoints <- ncases

    # Use a maximum of NMAX cases for calculating ux.list and uy
    # (unless npoints is bigger or TRUE or negative).
    # Allows plotmo to be fast even on models with millions of cases.
    NMAX <- 1e4
    nmax <- max(NMAX, npoints)
    if(!npoints.was.neg && ncases > nmax) {
        trace2(trace, "using %g of %g cases to calculate unique x and y values\n",
               npoints, ncases)
        isubset <- get.isubset(y, npoints)
        y <- y[isubset]
        x <- x[isubset, , drop=FALSE]
    }
    list(ux.list = get.ux.list(x, trace),
         uy      = unique(y),
         npoints = npoints)
}
# return a list, each element is the unique levels for corresponding column of x
# TODO this is where we spend a lot of time in plotmo for big data

get.ux.list <- function(x, trace)
{
    ux.list <- list(colnames(x))

    for(i in seq_len(ncol(x)))
        ux.list[[i]] <- if(is.factor(x[,i])) levels(x[,i])
                        else                 sort.unique(x[,i])

    trace2(trace, "number of x values: %s\n",
           paste.trunc(colnames(x), sapply(ux.list, length)))

    ux.list
}
points.or.text <- function(..., x, y, pt.col, iresponse)
{
    stopifnot(!is.na(pt.col))

    cex <- dota("pt.cex cex.response", DEF=1, EX=c(0,1), NEW=1, ...)
    cex <- cex * pt.cex(NROW(x))

    pch <- dota("pt.pch pch.response pch", DEF=20, EX=c(0,1,1), NEW=1, ...)

    # recycle then select only iresponse points
    n <- length(y)
    col <- repl(pt.col, n)[iresponse]
    pch <- repl(pch, n)[iresponse]
    cex <- repl(cex, n)[iresponse]

    x <- x[iresponse]
    y <- y[iresponse]

    if(is.character(pch) && pch[1] != ".")
        call.plot(graphics::text.default, PREFIX="pt.",
            force.x      = x,
            force.y      = y,
            force.labels = pch,
            force.col    = col,
            force.cex    = pmax(.1, .9 * cex),
            def.xpd      = NA, # allow writing beyond plot area
            ...)
    else
        call.plot(graphics::points.default, PREFIX="pt.",
            force.x    = x,
            force.y    = y,
            force.pch  = pch,
            force.col  = col,
            force.cex  = cex,
            # commented out because looks messy in image plots
            # def.xpd    = NA, # allow writing beyond plot area
            ...)
}
# TODO Following handling of global variables is unpleasant.
# I would prefer to have two namespace level variables,
# degree1.data.global and degree2.data.global (similar to
# degree1.xgrid.global etc.)
# But CRAN check won't allow
# unlockBinding(degree1.data.global, asNamespace("plotmo"))
# so we can update those variables.
# Also, we can't directly use assignInMyNamespace for these
# variables because we need to update individual list elements.

make.static.list <- function() {
    data <- list()
    func <- function(i, newdata=NULL) {
        if(is.null(i))              # init the data?
            data <<- list()
        else if(!missing(newdata))  # assign to the data?
             data[[i]] <<- newdata
        else if(i <= length(data))  # return the data element
            data[[i]]
        else                        # return the element, but it's NULL
            NULL
    }
    func
}
# The following global variables are for efficiency when we make two
# passes through the plot.  We store the data from the first pass so we
# don't have to regenerate it.  (We make two passes if we need to
# precalculate ylim before doing the actual plotting.)
# NULL is used here to indicate uninitialized.

degree1.xgrid.global  <- NULL
degree2.xgrid.global  <- NULL
partdep.x.global      <- NULL # dataframe of background vars we integrate over

degree1.data <- make.static.list()
degree2.data <- make.static.list()

# the following global variables are for communicating across functions

trace.call.global <- 0 # nonzero to trace call to predict, residuals, etc

init.global.data <- function()
{
    assignInMyNamespace("trace.call.global",    0)
    assignInMyNamespace("degree1.xgrid.global", NULL)
    assignInMyNamespace("degree2.xgrid.global", NULL)
    assignInMyNamespace("partdep.x.global",     NULL)
    degree1.data(NULL) # clear the degree1 data by passing NULL
    degree2.data(NULL)
}
plot.degree1 <- function( # plot all degree1 graphs
    # copy of args from plotmo, some have been tweaked slightly
    object, degree1, all1, center,
    ylim, type, nresponse, pmethod,
    trace, trace2,
    pt.col, jitter, iresponse,
    smooth.col, grid.col,
    inverse.func, grid.func, grid.levels, extend,
    ngrid1,
    is.int.only, level,
    func, nrug,
    # the following args are generated in plotmo
    draw.plot, # draw.plot=FALSE means get predictions but don't actually plot
    x, y, singles, resp.levs, ux.list, ndiscrete,
    pred.names, abbr.pred.names, nfigs, uy,
    xflip, jittered.y, n.apartdep,
    ...)
{
    get.degree1.data <- function(isingle)
    {
        # check if plot.degree1 was already called by get.ylim.by.dummy.plots
        data <- degree1.data(isingle)
        if(!is.null(data)) # data is already initialized?
            return(data)   # yes, use it

        intervals <- NULL  # prediction intervals, NULL if level argument not used

        # create data.frame of predictor values to be plotted,
        # by updating xgrid for this predictor (one column gets updated)
        xframe <- get.degree1.xframe(xgrid, x, ipred, ngrid1,
                                     ndiscrete, ux.list, extend)
        trace2(trace, "degree1 plot%d (pmethod \"%s\") variable %s\n",
               isingle, pmethod, pred.names[ipred])
        if(pmethod == "partdep" || pmethod == "apartdep") {
            stopifnot(!is.na(partdep.x) && !is.null(partdep.x))
            yhat <- degree1.partdep.yhat(object,
                        type, nresponse, pmethod, inverse.func, trace2,
                        partdep.x, xframe, ipred, pred.names, resp.levs, ...)
            if(level > 0) { # get prediction intervals?
                warning0(
"ignoring the 'level' argument because plotmo pmethod=\"", pmethod, "\"")
                level <- 0
            }
        } else { # classic plotmo plot
            yhat <- plotmo_predict(object, xframe, nresponse,
                        type, resp.levs, trace2, inverse.func, ...)$yhat
            if(level > 0) # get prediction intervals?
                intervals <- plotmo_pint(object, xframe, type, level, trace2,
                                         ipred, inverse.func)
        }
        temp <- blockify.degree1.frame(xframe, yhat, intervals,
                                       ipred, ux.list, ndiscrete)
        xframe    <- temp$xframe
        yhat      <- temp$yhat
        intervals <- temp$intervals
        if(center) {
            yhat               <- my.center(yhat, trace2)
            intervals$fit      <- my.center(intervals$fit, trace2)
            intervals$lwr      <- my.center(intervals$lwr, trace2)
            intervals$upr      <- my.center(intervals$upr, trace2)
            intervals$cint.lwr <- my.center(intervals$cint.lwr, trace2)
            intervals$cint.upr <- my.center(intervals$cint.upr, trace2)
        }
        all.yhat <- c(all.yhat, yhat,
                      intervals$lwr, intervals$upr,
                      intervals$cint.lwr, intervals$cint.upr)
        data <- list(xframe=xframe, yhat=yhat, intervals=intervals, all.yhat=all.yhat)
        if(!draw.plot) # save the data, if there is going to be a next time
            degree1.data(isingle, data)
        data
    }
    draw.degree1 <- function(...)
    {
        draw.degree1.fac <- function(...)
        {
            draw.grid(grid.col, nx=NA, ...) # nx=NA for horiz-only grid
            draw.fac.intervals(xframe[,ipred], intervals, ...)
            if(is.specified(pt.col))
               points.or.text(x=jittered.x, y=yscale * (yshift + jittered.y),
                              pt.col=pt.col, iresponse=iresponse, ...)
            draw.smooth1(smooth.col, x, ipred, yscale * (yshift + y),
                         ux.list, ndiscrete, center, ...)
            # formal args for plot.factor, needed because "CRAN check"
            # doesn't allow ":::" and plot.factor isn't public
            plot.factor.formals <- c("x", "y", "legend.text")

            call.plot(graphics::plot, # calls plot.factor
                PREFIX    = "degree1.",
                FORMALS   = plot.factor.formals,
                TRACE     = if(isingle == 1 && trace >= 2) trace-1 else 0,
                force.x   = xframe[,ipred], force.y=yhat,
                force.add = TRUE,
                def.xaxt  = if(xaxis.is.levs) "n" else "s",
                def.yaxt  = if(yaxis.is.levs) "n" else "s",
                force.lty = 1, # else lty=2 say is printed weirdly
                force.lwd = 1,
                ...)
            if(xaxis.is.levs)               # plot x level names along the x axis
                mtext(xlevnames, side=1, at=1:length(xlevnames),
                      cex=par("cex") * cex.lab, line=.5, las=get.las(xlevnames))
            if(yaxis.is.levs)               # plot y level names along the y axis
                mtext(ylevnames, side=2, at=1:length(ylevnames),
                      cex=par("cex") * cex.lab, line=.5, las=get.las(ylevnames))
        }
        draw.degree1.numeric <- function(...)
        {
            draw.grid(grid.col, ...)
            draw.numeric.intervals(xframe[,ipred], intervals, ...)
            draw.func(func, object, xframe, ipred, center, trace, ...)
            if(is.specified(pt.col))
               points.or.text(x=jittered.x, y=yscale * (yshift + jittered.y),
                              pt.col=pt.col, iresponse=iresponse, ...)
            draw.smooth1(smooth.col, x, ipred, yscale * (yshift + y),
                         ux.list, ndiscrete, center, ...)
            call.plot(graphics::lines.default, PREFIX="degree1.",
                force.x = xframe[,ipred], force.y = yhat,
                force.col = dota("degree1.col col.degree1 col",
                                EX=c(0,1,1), DEF=1, NEW=1, ...),
                force.lty = dota("degree1.lty lty.degree1 lty",
                                EX=c(0,1,1), DEF=1, NEW=1, ...),
                force.lwd = dota("degree1.lwd lwd.degree1 lwd",
                                EX=c(0,1,1), DEF=1, NEW=1, ...),
                ...)
            draw.degree1.numeric.rug(nrug, numeric.x, jittered.x, ...)
        }
        #--- draw.degree1 starts here
        x1 <- x[,ipred]
        numeric.x <- jittered.x <- as.numeric(x1)
        jittered.x <- apply.jitter(numeric.x, jitter)
        xlim <- get.degree1.xlim(ipred, xframe, ux.list, ndiscrete,
                                 pt.col, jittered.x, xflip, ...)
        # title of the current plot
        main <- dota("main", ...)
        main <- if(is.specified(main))
                    repl(main, isingle)[isingle]
                else {
                    main <- ""
                    if(nfigs > 1 && !is.degree.specified(degree1))
                        main <- paste0(isingle, " ") # show plot number in headers
                    paste(main, abbr.pred.names[ipred])
                }
        xlevnames <- abbreviate(levels(xframe[,ipred]), minlength=6, strict=TRUE)
        xaxis.is.levs <- is.factor(x1) && length(xlevnames) <= 12
        yaxis.is.levs <- length(resp.levs) >= 1 && length(resp.levs) <= 12
        if(yaxis.is.levs)
            ylevnames <- abbreviate(resp.levs, minlength=6, strict=TRUE)
        yaxis.is.levs <- FALSE # TODO should only do this if response is a string or a factor
        xlab <- dota("xlab", ...)
        xlab <- if(is.null(xlab))           abbr.pred.names[ipred]
                else if(is.specified(xlab)) repl(xlab, isingle)[isingle]
                else                        ""
        ylab <- dota("ylab", DEF=NULL, ...)
        ylab <- if(is.specified(ylab)) repl(ylab, isingle)[isingle]
                else                   ""

        call.plot(graphics::plot.default, PREFIX="degree1.",
             TRACE = if(isingle == 1 && trace >= 2) trace-1 else 0,
             force.x    = xframe[,ipred],
             force.y    = yhat,
             force.type = "n", # nothing in interior of plot yet
             force.main = main,
             force.xlab = xlab,
             force.ylab = ylab,
             force.xlim = xlim,
             force.ylim = ylim,
             def.xaxt   = if(xaxis.is.levs) "n" else "s",
             def.yaxt   = if(yaxis.is.levs) "n" else "s",
             ...)
        if(yaxis.is.levs) # plot y level names along the y axis
            mtext(ylevnames, side=2, at=1:length(ylevnames),
                  cex=par("cex") * cex.lab, line=.5, las=get.las(ylevnames))
        if(center &&
                !is.specified(grid.col) &&
                !is.specified(dota("col.grid", ...)))
            abline(h=0, col="gray", lwd=.6) # gray line at y=0
        temp <-  get.y.shift.scale(pt.col, ylim, uy, ndiscrete, trace)
            yshift <- temp$yshift
            yscale <- temp$yscale
        if(is.factor(x1))
            draw.degree1.fac(...)
        else
            draw.degree1.numeric(...)
        if(is.int.only) # make it obvious that this is an intercept-only model
            legend("topleft", "intercept-only model", bg="white")
    }
    #--- plot.degree1 starts here
    trace2(trace, "--plot.degree1(draw.plot=%s)\n", if(draw.plot) "TRUE" else "FALSE")
    # get the x matrix we will plot, will be updated later for each predictor one by one
    if(!is.null(degree1.xgrid.global))  # already have the data?
        xgrid <- degree1.xgrid.global   # yes, use it
    else {
        xgrid <- get.degree1.xgrid(x, grid.func, grid.levels, pred.names, ngrid1)
        if(!draw.plot) # save the data, if there is going to be a next time
            assignInMyNamespace("degree1.xgrid.global", xgrid)
    }
    if(!is.null(partdep.x.global))      # already have partdep.x?
        partdep.x <- partdep.x.global   # yes use it
    else {
        partdep.x <- get.partdep.x(pmethod, x, y, n.apartdep, grid.levels, pred.names)
        if(!draw.plot) # save the data, if there is going to be a next time
            assignInMyNamespace("partdep.x.global", partdep.x)
    }
    if(pmethod == "plotmo" && draw.plot && trace >= 0 && ncol(xgrid) > 1)
        print.grid.values(xgrid, trace)
    cex.lab <- dota("cex.lab", DEF=.8 * par("cex.main"), ...)
    all.yhat <- NULL
    for(isingle in seq_along(singles)) {
        if(isingle == 2 && trace2 == 2) {
            trace2 <- 1
            printf("Reducing trace level for subsequent degree1 plots\n")
        }
        ipred <- singles[isingle] # ipred is the predictor index i.e. col in model mat
        # following happens with lm if you do e.g. ozone1$doy <- NULL after using ozone1
        # this won't catch all such errors
        if(ipred > NCOL(x))
            stop0("illegal index=", ipred, " (missing column in x?) NCOL(x)=", NCOL(x))
        temp <- get.degree1.data(isingle)
            xframe    <- temp$xframe
            yhat      <- temp$yhat
            intervals <- temp$intervals
            all.yhat  <- temp$all.yhat
        if(draw.plot)
            draw.degree1(...)
    }
    all.yhat # numeric vector of all predicted values
}
# When we are predicting a probability (0 to 1), we want the displayed
# points to be on the plot, even if factor levels are say 1 and 2.
# In that situation, we scale the displayed points into range 0...1.

get.y.shift.scale <- function(pt.col, ylim, uy, ndiscrete, trace)
{
    yshift <- 0
    yscale <- 1
    if(is.specified(pt.col)) { # for efficiency, only calculate if necessary
        ymin <- min(uy)
        ymax <- max(uy)
        if(is.specified(ylim[1]) && round(ylim[1]) >= 0 &&
           is.specified(ylim[2]) && round(ylim[2]) <= 1 &&
            # check that y is a factor (or factor-like)
           round(ymax) == ymax && length(uy) <= ndiscrete && min(uy) >= 0) {
         yshift <- -ymin
         yscale <- 1 / (yshift + ymax)
         trace2(trace,
"Will shift and scale displayed points specified by pt.col: yshift %g yscale %g\n",
                yshift, yscale)
        }
    }
    list(yshift=yshift, yscale=yscale)
}
get.degree1.xlim <- function(ipred, xframe, ux.list, ndiscrete,
                             pt.col, jittered.x, xflip, ...)
{
    xlim <- dota("xlim", ...)
    if(is.specified(xlim))
        stopifnot(is.numeric(xlim), length(xlim) == 2)
    else {
        x1 <- xframe[,ipred]
        xlim <- range1(x1)
        if(is.factor(x1)) {
            xlim[1] <- xlim[1] - .4
            xlim[2] <- xlim[2] + .4
        } else if(length(ux.list[[ipred]]) <= ndiscrete)
            xlim <- c(xlim[1] - .1, xlim[2] + .1)
        if(is.specified(pt.col))
            xlim <- range1(xlim, jittered.x)
    }
    xlim <- fix.lim(xlim)
    if(xflip) {
        temp <- xlim[1]
        xlim[1] <- xlim[2]
        xlim[2] <- temp
    }
    xlim
}
apply.jitter <- function(x, jitter, adjust=1)
{
    if(jitter == 0)
        return(x)
    jitter(x, factor=adjust * jitter)
}
get.iresponse <- function(npoints, ncases) # get indices of xrows
{
    check.integer.scalar(npoints)
    if(npoints == 0)
        return(NULL)
    if(npoints == 1)
        npoints <- -1
    if(npoints <= 1 || npoints > ncases) # -1 or TRUE means all cases
        npoints <- ncases
    if(npoints == ncases)
        seq_len(ncases)
    else
        sample(seq_len(ncases), size=npoints, replace=FALSE)
}
draw.smooth1 <- function(smooth.col, x, ipred, y, ux.list, ndiscrete, center, ...)
{
    if(!is.specified(smooth.col))
        return(NULL)
    x1 <- x[,ipred]
    is.discrete.x <- FALSE
    if(is.factor(x1)) {
        is.discrete.x <- TRUE
        levels <- sort.unique(as.numeric(x1))
    } else if(length(ux.list[[ipred]]) <= ndiscrete) {
        is.discrete.x <- TRUE
        levels <- ux.list[[ipred]]
    }
    if(is.discrete.x) {
        # x1 has discrete levels, display the mean y at each value of x1
        smooth <- sapply(split(y, x1), mean)
        if(center)
            smooth <- my.center(smooth) else smooth
        call.plot(graphics::lines.default, PREFIX="smooth.", drop.f=1,
            force.x    = levels,
            force.y    = smooth,
            force.col  = smooth.col,
            force.lty  = dota("smooth.lty lty.smooth",
                             EX=c(0,1), DEF=1, NEW=1, ...),
            force.lwd  = dota("smooth.lwd lwd.smooth lwd.loess",
                             EX=c(0,1,1), DEF=1, NEW=1, ...),
            force.pch  = dota("smooth.pch", DEF=20, EX=0, ...),
            def.type   = "b",
            ...)
    } else {
        # For less smoothing (so we can better judge earth inflection points),
        # we use a default value for f lower than the default 2/3.
        smooth.f <- dota("smooth.f loess.f", DEF=.5, NEW=1, ...)
        check.numeric.scalar(smooth.f)
        stopifnot(smooth.f > .01, smooth.f < 1)
        smooth <- lowess(x1, y, f=smooth.f)
        y <- if(center) my.center(smooth$y) else smooth$y
        call.plot(graphics::lines.default, PREFIX="smooth.", drop.f=1,
            force.x      = smooth$x,
            force.y      = y,
            force.col    = smooth.col,
            force.lty    = dota("smooth.lty lty.smooth", EX=c(0,1), DEF=1, NEW=1, ...),
            force.lwd    = dota("smooth.lwd lwd.smooth lwd.loess",
                                EX=c(0,1,1), DEF=1, NEW=1, ...),
            force.pch    = dota("smooth.pch", DEF=20, EX=0, ...),
            ...)
    }
}
draw.degree1.numeric.rug <- function(nrug, numeric.x, jittered.x, ...)
{
    if(is.character(nrug))
        draw.density.along.the.bottom(numeric.x, ...)
    else { # must be numeric nrug
        check.integer.scalar(nrug, logical.ok=TRUE)
        rug.x <- # nrug < 0 is for backwards compat
            if(nrug == 1 || nrug < 0 || nrug > length(numeric.x))
                jittered.x
            else if(nrug > 0)
                quantile(numeric.x, probs=seq(from=0, to=1, length.out=nrug+1),
                         na.rm=TRUE, names=FALSE)
            else
                NA
        if(length(rug.x) > 1) {
            stopifnot(length(jittered.x) == length(numeric.x))
            call.plot(graphics::rug, force.x=rug.x, def.quiet=TRUE, ...)
        }
    }
}
draw.grid <- function(grid.col, nx=NULL, ...)
{
    if(is.specified(grid.col) || is.specified(dota("col.grid", ...))) {
        if(is.specified(grid.col) && is.logical(grid.col) && grid.col)
            grid.col <- "lightgray"
        grid.col <- if(is.specified(grid.col)) grid.col
                    else dota("col.grid", DEF="lightgray", ...)
        # grid() doesn't have a dots arg so we invoke call.plot without dots
        call.plot(graphics::grid,
                  force.nx  = dota("grid.nx",  DEF=nx,   ...),
                  force.ny  = dota("grid.ny",  DEF=NULL, ...),
                  force.col = grid.col,
                  force.lty = dota("grid.lty", DEF=1,    ...),
                  force.lwd = dota("grid.lwd", DEF=1,    ...))
    }
}
get.level.shades <- function(intervals, ...)
{
    level.shade <- dota("level.shade shade.pints", DEF="mistyrose2", ...)
    if(is.null(intervals$lwr) || is.null(intervals$cint.lwr))
        c(level.shade, level.shade)
    else { # use level.shade2 only if two kinds of intervals
        # use exact match here because level.shade2 is also matched by level.shade
        level.shade2 <- dota("level.shade2 shade2.pints", DEF="mistyrose4", ...)
        c(level.shade, level.shade2)
    }
}
# draw std err bars for a numeric predictor
draw.numeric.intervals <- function(x, intervals, ...)
{
    if(!is.null(intervals)) {
        level.shades <- get.level.shades(intervals, ...)
        if(!is.null(intervals$lwr))
            polygon1(x=x, lwr=intervals$lwr, upr=intervals$upr,
                    shade=level.shades[1], ...)
        if(!is.null(intervals$cint.lwr))
            polygon1(x=x, lwr=intervals$cint.lwr, upr=intervals$cint.upr,
                    shade=level.shades[2])
        if(!is.null(intervals$lwr) || !is.null(intervals$cint.lwr))
            box() # replot the box because intervals sometimes drawn over it
    }
}
# TODO you can't get just the confidence lines with no shading, following looks not ok:
#      plotmo(a, level=.8, level.lty=1, level.border=1, level.shade=2, level.density=0)

polygon1 <- function(x, lwr, upr, shade, ...)
{
    call.plot(graphics::polygon, PREFIX="level.", drop.shade=1, drop.shade2=1,
            force.x    = c(x[1],   x,   rev(x)),
            force.y    = c(lwr[1], lwr, rev(upr)),
            force.col  = shade,
            def.border = shade,
            def.lty    = 0,
            ...)
}
# draw std err bands for a factor predictor
draw.fac.intervals <- function(x, intervals, ...)
{
    draw.intervals <- function(lwr, upr, shade)
    {
        for(ilev in seq_along(levels(x))) {
            min <- min(lwr[[ilev]])
            max <- max(upr[[ilev]])
            polygon(c(ilev - .4, ilev - .4, ilev + .4, ilev + .4),
                    c(min, max, max, min), col=shade, border=shade, lty=0)
        }
    }
    if(!is.null(intervals)) {
        level.shades <- get.level.shades(intervals, ...)
        if(!is.null(intervals$lwr))
            draw.intervals(split(intervals$lwr, x),
                           split(intervals$upr, x), level.shades[1])
        if(!is.null(intervals$cint.lwr))
            draw.intervals(split(intervals$cint.lwr, x),
                           split(intervals$cint.upr, x), level.shades[2])
        if(!is.null(intervals$lwr) || !is.null(intervals$cint.lwr))
            box() # replot the box because intervals sometimes drawn over it
    }
}
# draw the func arg, if specified
draw.func <- function(func, object, xframe, ipred, center, trace, ...)
{
    if(!is.null(func)) {
        print_summary(xframe, "Data for func", trace)
        if(!is.function(func))
            stop0("'func' is not a function");
        y <- process.y(func(xframe), object, type="response", nresponse=1,
                       nrow(xframe), expected.levs=NULL, trace, "func returned")$y
        if(center)
            y <- my.center(y, trace)
        call.plot(graphics::lines.default, PREFIX="func.",
                  force.x   = xframe[,ipred],
                  force.y   = y,
                  def.type  = "l",
                  force.col = dota("func.col col.func",
                                   EX=c(0,1), DEF="lightblue3", NEW=1, ...),
                  force.lty = dota("func.lty lty.func",
                                   EX=c(0,1), DEF=1, NEW=1, ...),
                  force.lwd = dota("func.lwd lwd.func",
                                   EX=c(0,1), DEF=2, NEW=1, ...),
                  ...)
    }
}
get.def.nticks <- function(x, ipred1, ipred2) # for persp plot
{
    # nticks is just a suggestion for persp, so we don't fret over it too much
    nticks <- 5 # default nticks if both axes numeric (no factors)
    if(is.factor(x[[ipred1]]))                # use number of factor levels to
        nticks <- length(levels(x[[ipred1]])) # avoid e.g. "1.5" on factor axes
    if(is.factor(x[[ipred2]]))
        nticks <- max(nticks, length(levels(x[[ipred2]])))
    nticks <- max(nticks, 2) # must be at least 2
    min(nticks, 6)           # but not more than 6 (not enough space)
}
plot.degree2 <- function(  # plot all degree2 graphs
    # copy of args from plotmo, some have been tweaked slightly
    object, degree2, all2, center, ylim,
    type, nresponse, pmethod,
    clip, trace, trace2, pt.col,
    jitter, iresponse,
    inverse.func, grid.func, grid.levels, extend,
    type2, ngrid2,
    # the following args are generated in plotmo
    draw.plot, # draw.plot=FALSE means get and return all.yhat but don't actually plot
    do.par,
    x, y, pairs, resp.levs, ux.list, ndiscrete,
    pred.names, abbr.pred.names, nfigs, nsingles, npairs,
    xflip, yflip, swapxy, def.cex.main, n.apartdep,
    ...)
{
    get.degree2.data <- function(ipair)
    {
        data <- degree2.data(ipair)
        if(!is.null(data))          # data is already initialized?
            return(data)            # yes, use it

        # create data.frame of x values to be plotted,
        # by updating xgrid for this predictor (two columns get updated)
        # (but for partdep plots, xframe isn't used, we use just x1grid and x2grid)

        temp <- get.degree2.xframe(xgrid, x, ipred1, ipred2,
                                   ngrid2, xranges, ux.list, ndiscrete)
            xframe <- temp$xframe  # data frame of medians
            x1grid <- temp$x1grid # vec of values for the first predictor
            x2grid <- temp$x2grid # vec of values for the second predictor

        trace2(trace, "degree2 plot%d (pmethod \"%s\") variables %s:%s\n",
               ipair, pmethod, pred.names[ipred1], pred.names[ipred2])
        if(pmethod == "partdep" || pmethod == "apartdep") {
            stopifnot(!is.na(partdep.x) && !is.null(partdep.x))
            yhat <- degree2.partdep.yhat(object,
                        type, nresponse, pmethod, inverse.func, trace,
                        partdep.x, x1grid, ipred1, x2grid, ipred2,
                        pred.names, resp.levs, ...)
        } else { # classic plotmo plot
            yhat <- plotmo_predict(object, xframe, nresponse,
                        type, resp.levs, trace2, inverse.func, ...)$yhat
        }
        x1grid <- as.numeric(x1grid)
        x2grid <- as.numeric(x2grid)
        # image plots for factors look better if not blockified
        if(type2 != "image") {
            temp <- blockify.degree2.frame(x, yhat, x1grid, x2grid,
                                           ipred1, ipred2, ux.list, ndiscrete)
            yhat  <- temp$yhat
            x1grid <- temp$x1grid
            x2grid <- temp$x2grid
        }
        if(center)
            yhat <- my.center(yhat, trace2)
        data <- list(xframe=xframe, x1grid=x1grid, x2grid=x2grid,
                     yhat=matrix(yhat, nrow=length(x1grid), ncol=length(x2grid)),
                     def.nticks=get.def.nticks(x, ipred1, ipred2))
        if(!draw.plot) # save the data, if there is going to be a next time
            degree2.data(ipair, data)
        data
    }
    draw.degree2 <- function(type2 = c("persp", "contour", "image"), def.nticks, ...)
    {
        name1 <- abbr.pred.names[ipred1]
        name2 <- abbr.pred.names[ipred2]
        # title of the current plot
        main <- dota("main", ...)
        main <- if(is.specified(main))
                    repl(main, nsingles+ipair)[nsingles+ipair]
                 else {
                    main <- ""
                    if(nfigs > 1 && !is.degree.specified(degree2))
                        main <- paste0(ipair, " ") # show plot number in headers
                     if(swapxy)
                         paste0(main, name2, ": ", name1)
                     else
                         paste0(main, name1, ": ", name2)
                 }
        if(clip) {
            yhat[yhat < ylim[1]] <- NA
            # we don't clip upper values for persp plot because its own clipping is ok
            # (whereas its own clipping for lower values tends to allow overwrite of axes).
            if(type2 != "persp")
                yhat[yhat > ylim[2]] <- NA
        }
        switch(type2,
            persp=plot.persp(
                x=x, x1grid=x1grid, x2grid=x2grid, yhat=yhat, name1=name1, name2=name2,
                ipred1=ipred1, ipred2=ipred2, ipair=ipair, nsingles=nsingles,
                trace=trace, ylim=ylim, xflip=xflip, yflip=yflip, swapxy=swapxy,
                ngrid2=ngrid2, main2=main, ticktype2=ticktype,
                def.cex.main=def.cex.main, def.nticks=def.nticks, ...),
            contour=plot.contour(
                x=x, x1grid=x1grid, x2grid=x2grid, yhat=yhat, name1=name1, name2=name2,
                ipred1=ipred1, ipred2=ipred2, xflip=xflip, yflip=yflip, swapxy=swapxy,
                main2=main, pt.col=pt.col,
                jitter=jitter,
                ux.list=ux.list, ndiscrete=ndiscrete, iresponse=iresponse,
                ...),
            image=plot.image(
                x=x, x1grid=x1grid, x2grid=x2grid, yhat=yhat, name1=name1, name2=name2,
                ipred1=ipred1, ipred2=ipred2, xflip=xflip, yflip=yflip, swapxy=swapxy,
                main2=main, pt.col=pt.col,
                jitter=jitter,
                ux.list=ux.list, ndiscrete=ndiscrete, iresponse=iresponse,
                ...))
    }
    #--- plot.degree2 starts here
    trace2(trace, "--plot.degree2(draw.plot=%s)\n", if(draw.plot) "TRUE" else "FALSE")
    stopifnot(npairs > 0)
    # need ticktype to determine degree2 margins
    ticktype <- dota("persp.ticktype", DEF="simple", EX=0, ...)
    ticktype <- match.choices(ticktype, c("simple", "detailed"), "ticktype")
    simple.ticktype <- substr(ticktype, 1, 1) == "s"
    if(draw.plot) {
        if(do.par) {
            opar=par("mar", "mgp")
            on.exit(par(mar=opar$mar, mgp=opar$mgp))
            do.degree2.auto.par(type2, nfigs, simple.ticktype)
        } else if (nsingles && type2 == "persp") {
            # persp needs smaller margins than degree1 plots
            # the nsingles check above prevents us from modifying margins
            # if the user is simply plotting one or more degree2 plots
            opar=par("mar", "mgp")
            on.exit(par(mar=opar$mar, mgp=opar$mgp))
            do.persp.auto.par(simple.ticktype)
        }
    }
    # get the x matrix we will plot, will be updated later for each pair of predictors
    xranges <- get.degree2.xranges(x, extend, ux.list, ndiscrete)
    if(!is.null(degree2.xgrid.global))  # already have the data?
        xgrid <- degree2.xgrid.global   # yes, use it
    else {
        xgrid <- get.degree2.xgrid(x, grid.func, grid.levels, pred.names, ngrid2)
        if(!draw.plot) # save the data, if there is going to be a next time
            assignInMyNamespace("degree2.xgrid.global", xgrid)
    }
    if(!is.null(partdep.x.global))      # already have partdep.x?
        partdep.x <- partdep.x.global   # yes use it
    else {
        partdep.x <- get.partdep.x(pmethod, x, y, n.apartdep, grid.levels, pred.names)
        if(!draw.plot) # save the data, if there is going to be a next time
            assignInMyNamespace("partdep.x.global", partdep.x)
    }
    all.yhat <- NULL
    for(ipair in seq_len(npairs)) {
        ipred1 <- pairs[ipair,1]  # index of first predictor
        ipred2 <- pairs[ipair,2]  # index of second predictor
        if(ipair == 2 && trace2 == 2) {
            trace2 <- 1
            printf("Reducing trace level for subsequent degree2 plots\n")
        }
        temp <- get.degree2.data(ipair)
            xframe   <- temp$xframe
            x1grid   <- temp$x1grid
            x2grid   <- temp$x2grid
            yhat     <- temp$yhat
            all.yhat <- c(all.yhat, yhat)

        if(draw.plot)
            draw.degree2(type2, temp$def.nticks, ...)
    }
    all.yhat
}
get.degree2.xranges <- function(x, extend, ux.list, ndiscrete)
{
    # we use a data.frame for xranges so columns can have different types (e.g. Dates)
    xranges <- as.data.frame(matrix(NA, ncol=ncol(x), nrow=2))
    colnames(xranges) <- colnames(x)
    for(icol in seq_len(ncol(x))) {
        x1 <- x[,icol]
        xrange <- range1(x1, na.rm=TRUE)
        nxvals <- length(ux.list[[icol]])
        # TODO this extends xrange correctly but that doesn't suffice
        #      because get.degree2.xframe doesn't necessarily use xranges
        if(extend != 0 && nxvals > ndiscrete && !is.factor(x1)) {
            stopifnot(xrange[2] >= xrange[1])
            ext <- extend * (xrange[2] - xrange[1])
            xrange[1] <- xrange[1] - ext
            xrange[2] <- xrange[2] + ext
        }
        xranges[,icol] <- xrange
    }
    xranges
}
draw.response.sites <- function(x, ipred1, ipred2, pt.col, jitter,
                                ux.list, ndiscrete, iresponse, swapxy, ...)
{
    if(swapxy) {
        x1 <- x[,ipred2]
        x2 <- x[,ipred1]
    } else {
        x1 <- x[,ipred1]
        x2 <- x[,ipred2]
    }
    points.or.text(
        x=apply.jitter(as.numeric(x1), jitter, adjust=1.5),
        y=apply.jitter(as.numeric(x2), jitter, adjust=1.5),
        pt.col=pt.col, iresponse=iresponse, ...)
}
get.diag.val <- function(yhat, diag1, diag2) # return first non NA along diag
{
    vals <- yhat[diag1, diag2]
    (vals[!is.na(vals)])[1] # return first non NA in vals, length zero if all NA
}
plot.persp <- function(x, x1grid, x2grid, yhat, name1, name2, ipred1, ipred2,
                       ipair, nsingles, trace, ylim, xflip, yflip, swapxy, ngrid2,
                       main2, ticktype2, def.cex.main, def.nticks, ...)
{
    get.theta <- function(...) # theta arg for persp()
    {
        theta <- dota("persp.theta theta", EX=c(0,1), ...)
        if(anyNA(theta)) { # theta not specified by the user?
            # rotate graph so highest point is farthest (this can swap axes)
            # imax corner numbering with theta=-35
            #    1
            # 2 /\ 4
            #   \/
            #   3
            theta <- -35
            nr <- nrow(yhat)
            nc <- ncol(yhat)
            imax <- which.max(c(
                    get.diag.val(yhat, nr:1, nc:1),
                    get.diag.val(yhat, 1:nr, nc:1),
                    get.diag.val(yhat, 1:nr, 1:nc),
                    get.diag.val(yhat, nr:1, 1:nc)))
            if(length(imax))   # length>0 unless entire diag is NA
                theta <- theta + switch(imax, 0, 90, 180, 270)
        }
        theta
    }
    #--- plot.persp starts here
    # following needed because persp() rejects a reversed xlim or ylim
    if(xflip)
        warning0("ignoring xflip=TRUE for persp plot")
    if(yflip)
        warning0("ignoring yflip=TRUE for persp plot")
    theta <- get.theta(...)
    cex1 <- par("cex") # persp needs an explicit cex arg, doesn't use par("cex")
    trace2(trace, "persp(%s:%s) theta %.3g\n", name1, name2, theta)
    if(swapxy) {
        temp <- x1grid; x1grid <- x2grid; x2grid <- temp # swap x1grid and x2grid
        temp <- ipred1; ipred1 <- ipred2; ipred2 <- temp # swap ipred1 and ipred2
        temp <- name1;  name1  <- name2;  name2  <- temp # swap name1 and name2
        yhat <- t(yhat)
    }
    zlab <- dota("ylab", DEF="", ...) # use ylab as zlab if specified
    zlab <- repl(zlab, nsingles+ipair)[nsingles+ipair]
    # zlab <- paste0("\n", zlab) # else zlab is too close to axis labels
    cex.lab <- dota("persp.cex.lab",
                    # make the labels small if multiple figures
                    DEF=if(def.cex.main < 1) .8 * def.cex.main else 1, ...)

    # persp ignores mgp so prefix a newline to space the axis label
    # we also prepend spaces else bottom of label tends to get cut off
    if(theta < 0) theta <- theta + 360
    theta <- theta %% 360
    if((0 < theta && theta <= 90) || (180 < theta && theta <= 270)) {
        xlab <- paste0("\n", name1, "    ")
        ylab <- paste0("\n    ", name2)
    } else {
        xlab <- paste0("\n    ", name1)
        ylab <- paste0("\n", name2, "    ")
    }
    # We use deprefix directly (and not call.plot) because
    # we have to do a bit of manipulation of the args for nticks.
    # Also we cannot use graphics:::persp.default because CRAN check complains
    # about ":::". Instead we explicitly pass the formal argnames with formals.

    persp.def.formals <- c( # formal args for persp.default (R version 3.2.0)
        "x", "y", "z", "xlim", "zlim", "xlab", "ylab", "zlab", "main", "sub",
        "theta", "phi", "r", "d", "scale", "expand", "col", "border", "ltheta",
        "lphi", "shade", "box", "axes", "nticks", "ticktype")

    args <- deprefix(graphics::persp, # calls persp.default
        FNAME   = "persp",
        KEEP    = "PREFIX,PLOT.ARGS",
        FORMALS = persp.def.formals,
        TRACE   = if(ipair == 1 && trace >= 2) trace-1 else 0,
        force.x       = x1grid,
        force.y       = x2grid,
        force.z       = yhat,
        force.xlim    = range(x1grid), # prevent use of user specified xlim and ylim
        force.ylim    = range(x2grid),
                        # persp won't accept zlim=NULL
        force.zlim    = if(is.null(ylim)) ylim <- range(yhat) else ylim,
        force.xlab    = xlab,
        force.ylab    = ylab,
        force.theta   = theta,
        force.phi     = dota("persp.phi phi",    EX=c(0,1), DEF=30, ...),
        force.d       = dota("persp.d   dvalue", EX=c(0,1), DEF=1,  ...),
        force.main    = main2,
        def.cex.lab   = cex.lab,
        def.cex.axis  = cex.lab,
        def.zlab      = zlab,
        def.ticktype  = "simple",
        def.nticks    = def.nticks,
        def.cex       = cex1,
        force.col     = dota("persp.col col.persp",
                             EX=c(0,1), DEF="lightblue", NEW=1, ...),
        def.border    = NULL,
        def.shade     = .5,
        ...)

    # if ticktype="simple" we must call persp without the nticks arg
    # else persp emits confusing error messages
    if(substr(ticktype2, 1, 1) == "s")
        args["nticks"] <- NULL

    # We use suppressWarnings below to suppress the warning
    # "surface extends beyond the box" that was introduced in R 2.13-1.
    # This warning may be issued multiple times and may be annoying to the plotmo user.
    # (Unfortunately this also suppress any other warnings in persp.)
    # TODO Want to use lab=c(2,2,7) or similar in persp but persp ignores it

    suppressWarnings(
        do.call.trace(graphics::persp, args, fname="graphics::persp", trace=0))
}
plot.contour <- function(x, x1grid, x2grid, yhat, name1, name2, ipred1, ipred2,
                         xflip, yflip, swapxy, main2, pt.col,
                         jitter, ux.list, ndiscrete, iresponse, ...)
{
    get.lim <- function(xflip, x1grid, ipred)
    {
        # contour() automatically extends ylim, so we don't need to do it here
        xrange <- range(x1grid)
        if(xflip)
            c(xrange[2], xrange[1])
        else
            c(xrange[1], xrange[2])
    }
    #--- plot.contour starts here
    x1 <- x[,ipred1]
    x2 <- x[,ipred2]
    levnames1 <- levels(x1)
    levnames2 <- levels(x2)
    is.fac1 <- is.factor(x1) && length(levnames1) <= 12
    is.fac2 <- is.factor(x2) && length(levnames2) <= 12
    xlab <- if(is.fac1) "" else name1 # no lab if fac else on top of lev name
    ylab <- if(is.fac2) "" else name2
    if(swapxy) {
        temp <- levnames2; levnames2 <- levnames1; levnames1 <- temp
        temp <- is.fac2;   is.fac2 <- is.fac1;     is.fac1 <- temp
        temp <- ylab;      ylab <- xlab;           xlab <- temp
    }
    xlim <- get.lim(xflip, x1grid, ipred1)
    ylim <- get.lim(yflip, x2grid, ipred2)
    if(swapxy) {
        temp <- xlim; xlim <- ylim; ylim <- temp
    }
    levels <- get.contour.levs(yhat)
    labels <- signif(levels, 2) # else contour prints labels like 0.0157895
    cex.lab <- par("cex") * dota("cex.lab", DEF=1, ...)

    # We use suppressWarnings below to suppress the warning "all z values are
    # equal" This warning may be issued multiple times and may be annoying to
    # the plotmo user.  (Unfortunately this also suppress any other warnings
    # in contour.default.)

    suppressWarnings(
        call.plot(graphics::contour.default,
            force.x    = if(swapxy) x2grid else x1grid,
            force.y    = if(swapxy) x1grid else x2grid,
            force.z    = if(swapxy) t(yhat) else yhat,
            force.xlim = xlim,
            force.ylim = ylim,
            force.xlab = xlab,
            force.ylab = ylab,
            def.xaxt   = if(is.fac1) "n" else "s",
            def.yaxt   = if(is.fac2) "n" else "s",
            def.main   = main2,
            def.levels = levels,
            def.labels = labels,
            def.labcex = par("cex") * cex.lab,
            ...))

    if(is.fac1) {
        levnames1 <- abbreviate(levnames1, minlength=6, strict=TRUE)
        mtext(levnames1, side=1, at=1:length(levnames1),
              cex=cex.lab, line=.5, las=get.las(levnames1))
    }
    if(is.fac2)
        mtext(abbreviate(levnames2, minlength=6, strict=TRUE),
              side=2, at=1:length(levnames2),
              cex=cex.lab, line=.5, las=2)

    if(is.specified(pt.col))
        draw.response.sites(x=x, ipred1=ipred1, ipred2=ipred2,
            pt.col=pt.col, jitter=jitter, ux.list=ux.list,
            ndiscrete=ndiscrete, iresponse=iresponse, swapxy=swapxy, ...)
}
get.contour.levs <- function(yhat)
{
    # the default, as calculated internally by plot.contour
    levs <- pretty(range(yhat, finite=TRUE), 10)
    # reduce the default if the number of unique yhat values is less
    # this is mainly for factors
    unique.yhat <- sort.unique(yhat)
    if(length(unique.yhat) > 1 && length(unique.yhat) < length(levs))
        levs <- unique.yhat
    levs
}
plot.image <- function(x, x1grid, x2grid, yhat, name1, name2, ipred1, ipred2,
                       xflip, yflip, swapxy, main2, pt.col,
                       jitter, ux.list, ndiscrete, iresponse, ...)
{
    # like image but fill the plot area with lightblue first so NAs are obvious
    image.with.lightblue.na <- function(x1grid, x2grid, yhat, ...)
    {
        if(anyNA(yhat)) {
            image(x1grid, x2grid, matrix(0, nrow(yhat), ncol(yhat)),
                  col="lightblue",
                  xlab="", ylab="", xaxt="n", yaxt="n", bty="n", main="")
            par(new=TRUE) # so next plot is on top of this plot
        }
        call.plot(graphics::image.default,
                  force.x=x1grid, force.y=x2grid, force.z=yhat, ...)
        box() # image() tends to overwrite the borders of the box
    }
    get.lim <- function(xflip, x1grid, is.discrete)
    {
        xrange <- range(x1grid)
        if(is.discrete) {
            xrange[1] <- xrange[1] - .5
            xrange[2] <- xrange[2] + .5
        } else {
            range <- xrange[2] - xrange[1]
            # .025 seems the max we can use without getting unsightly
            # gaps at the edges of the plot
            xrange[1] <- xrange[1] - .025 * range
            xrange[2] <- xrange[2] + .025 * range
        }
        if(xflip)
            c(xrange[2], xrange[1])
        else
            c(xrange[1], xrange[2])
    }
    #--- plot.image starts here
    x1 <- x[,ipred1]
    x2 <- x[,ipred2]
    levnames1 <- levels(x1)
    levnames2 <- levels(x2)
    use.fac.names1 <- is.factor(x1) && length(levnames1) <= 12
    use.fac.names2 <- is.factor(x2) && length(levnames2) <= 12
    xlab <- if(use.fac.names1) "" else name1 # no lab if fac else on top of lev name
    ylab <- if(use.fac.names2) "" else name2
    if(swapxy) {
        temp <- levnames2;      levnames2 <- levnames1;           levnames1 <- temp
        temp <- use.fac.names2; use.fac.names2 <- use.fac.names1; use.fac.names1 <- temp
        temp <- ylab;           ylab <- xlab;                     xlab <- temp
    }
    xlim <- get.lim(xflip, x1grid,
                    use.fac.names1 || length(ux.list[[ipred1]]) <= ndiscrete)
    ylim <- get.lim(yflip, x2grid,
                    use.fac.names2 || length(ux.list[[ipred2]]) <= ndiscrete)

    # default col: white high values (snowy mountain tops), dark low values (dark depths)
    if(swapxy)
        image.with.lightblue.na(x1grid=x2grid, x2grid=x1grid, yhat=t(yhat),
            force.col     = dota("image.col col.image", EX=c(0,1),
                                 DEF=grDevices::gray((0:10)/10), NEW=1, ...),
            force.main    = main2,
            force.xlim    = ylim,
            force.ylim    = xlim,
            force.xaxt    = if(use.fac.names1) "n" else "s",
            force.yaxt    = if(use.fac.names2) "n" else "s",
            force.xlab    = xlab,
            force.ylab    = ylab,
            ...)
    else
        image.with.lightblue.na(x1grid=x1grid, x2grid=x2grid, yhat=yhat,
            force.col     = dota("image.col col.image", EX=c(0,1),
                                 DEF=grDevices::gray((0:10)/10), NEW=1, ...),
            force.main    = main2,
            force.xlim    = xlim,
            force.ylim    = ylim,
            force.xaxt    = if(use.fac.names1) "n" else "s",
            force.yaxt    = if(use.fac.names2) "n" else "s",
            force.xlab    = xlab,
            force.ylab    = ylab,
            ...)

    cex.lab <- par("cex") * dota("cex.lab", DEF=1, ...)

    if(use.fac.names1) {
        levnames1 <- abbreviate(levnames1, minlength=6, strict=TRUE)
        mtext(levnames1, side=1, at=1:length(levnames1),
              cex=cex.lab, line=.5, las=get.las(levnames1))
    }
    if(use.fac.names2)
        mtext(abbreviate(levnames2, minlength=6, strict=TRUE),
              side=2, at=1:length(levnames2),
              cex=cex.lab, line=.5, las=2)

    if(is.specified(pt.col))
        draw.response.sites(x=x, ipred1=ipred1, ipred2=ipred2,
            pt.col=pt.col, jitter=jitter, ux.list=ux.list,
            ndiscrete=ndiscrete, iresponse=iresponse, swapxy=swapxy, ...)
}
apply.inverse.func <- function(inverse.func, y, object, trace)
{
    if(!is.null(inverse.func)) {
        if(!is.numeric(y[1]))
            stopf("inverse.func cannot be used on \"%s\" values", class(y[1])[1])
        y <- process.y(inverse.func(y), object, type="response", nresponse=1,
                       length(y), NULL, trace, "inverse.func")$y
    }
    y
}
# should the factor labels on the x axis be printed horizontally or vertically?
get.las <- function(labels)
{
    if(length(labels) * max(nchar(labels)) <= 20)   # 20 is arbitrary
        0   # horizontal
    else
        2   # vertical
}
# true if a plot was selected by the user (excluding the default setting)
is.degree.specified <- function(degree)
{
    !is.logical(degree) || length(degree) > 1
}
my.center <- function(x, trace=FALSE)
{
    if(!is.null(x) && !is.factor(x)) {
        x <- x - mean(x[is.finite(x)], na.rm=TRUE)
        if(trace >= 2) {
            name <- paste0("centered ", trunc.deparse(substitute(x)))
            cat(name, "length ", length(x))
            print_first_few_elements_of_vector(x, trace, name)
        }
    }
    x
}
