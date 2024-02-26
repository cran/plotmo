# plot_gbm.R: plot gbm models
#
# This code is derived from code in gbm 2.1.1 (Aug 2016).
#
# TODO when selecting best n.trees, why is OOB smoothed but not test or CV?
# TODO maybe add arg to rescale errs e.g. RSquared rather than Squared Error
# TODO add right hand axis for OOB, or scale OOB to same units when possible?
# TODO if gbm calculated CV stddev across folds then we could plot CV conf bands

plot_gbm <- function(object=stop("no 'object' argument"),
    smooth = c(0, 0, 0, 1),
    col = c(1, 2, 3, 4),
    ylim = "auto",
    legend.x = NULL,
    legend.y = NULL,
    legend.cex = .8,
    grid.col = NA,
    n.trees = NA,
    col.n.trees ="darkgray",
    ...)
{
    # GBMFit was added in Oct 2016 for Paul Metcalfe's changes to gbm (version 2.2)
    check.classname(object, "object", c("gbm", "GBMFit"))
    obj <- object
    if((!is.numeric(smooth) && !is.logical(smooth)) ||
       any(smooth != 0 & smooth != 1))
        stop0("smooth should be a four-element vector specifying if train, ",
              "test, CV, and OOB curves are smoothed, e.g. smooth=c(0,0,0,1)")
    smooth <- rep_len(smooth, 4) # recycle smooth if necessary
    col <- rep_len(col, 4)       # recycle col if necessary
    col[is.na(col)] <- 0         # make using col below a bit easier
    check.integer.scalar(n.trees, min=1, max=n.trees,
                         na.ok=TRUE, logical.ok=FALSE)
    n.alltrees = gbm.n.trees(obj)
    # final.max is max of values on the right of the curves (excluding OOB)
    train.error <- gbm.train.error(obj)
    valid.error <- gbm.valid.error(obj)
    cv.error    <- gbm.cv.error(obj)
    final.max <- max(train.error[length(train.error)],
                     valid.error[length(valid.error)],
                     cv.error   [length(cv.error)],
                     na.rm=TRUE)

    if(any1(col)) { # must anything be plotted?
        par <- par("mar", "mgp") # will be modified in init.gbm.plot
        on.exit(par(mar=par$mar, mgp=par$mgp))
        init.gbm.plot(obj, ylim, final.max, par$mar, ...)
        if(is.specified(grid.col[1]))
            grid(col=grid.col[1], lty=3)
        # draw n.trees vertical gray line first, so other plots go on top of it
        if(is.specified(n.trees))
            vertical.line(n.trees, col.n.trees, 1, 0)
    }
    leg.text <- leg.col <- leg.lty <- leg.vert <- leg.imin <- NULL # for legend
    voffset <- 0 # slight offset to prevent overplotting of dotted vertical lines

    # train curve
    y <- maybe.smooth(train.error, "train", smooth[1], n.alltrees)
    imin <- which.min1(y)      # index of minimum train error
    imins <- c(imin, 0, 0, 0)  # index of train, test, CV, OOB minima
    names(imins) <- c("train", "test", "CV", "OOB")
    train.fraction <- gbm.train.fraction(obj)
    if(is.specified(col[1])) {
        lines(y, col=col[1])
        leg.text <- c(leg.text,
            if(train.fraction == 1) "train"
            else sprint("train (frac %g)", train.fraction))
        leg.col  <- c(leg.col, col[1])
        leg.lty  <- c(leg.lty, 1)
        leg.vert <- c(leg.vert, FALSE)
        leg.imin <- imin
    }
    # test curve (aka valid.error curve)
    if(train.fraction != 1) {
        y <- maybe.smooth(valid.error, "test", smooth[2], n.alltrees)
        imin <- imins[2] <- which.min1(y)
        if(is.specified(col[2])) {
            if(imin)
                vertical.line(imin, col[2], 3, voffset)
            voffset <- voffset + 1
            lines(y, col=col[2])
            leg.text <- c(leg.text,
                if(!imin) "test not plotted"
                else      sprint("test (frac %g)", 1-train.fraction))
            leg.col  <- c(leg.col, col[2])
            leg.lty  <- c(leg.lty, 1)
            leg.vert <- c(leg.vert, FALSE)
            leg.imin <- c(leg.imin, imin)
        }
    }
    # CV curve
    if(!is.null(cv.error)) {
        y <- maybe.smooth(cv.error, "CV", smooth[3], n.alltrees)
        imin <- imins[3] <- which.min1(y)
        if(is.specified(col[3])) {
            if(imin)
                vertical.line(imin, col[3], 3, voffset)
            voffset <- voffset + 1
            lines(y, col=col[3])
            leg.text <- c(leg.text,
                if(!imin) "CV not plotted"
                else      sprint("CV (%g fold)", gbm.cv.folds(obj)))
            leg.col  <- c(leg.col, col[3])
            leg.lty  <- c(leg.lty, 1)
            leg.vert <- c(leg.vert, FALSE)
            leg.imin <- c(leg.imin, imin)
        }
    }
    # OOB curve
    bag.fraction <- gbm.bag.fraction(obj)
    if(bag.fraction != 1) {
        oobag.improve <- gbm.oobag.improve(obj)
        y <- maybe.smooth(-cumsum(oobag.improve), "OOB", smooth[4], n.alltrees)
        imin <- imins[4] <- which.min1(y)
        if(is.specified(col[4])) {
            if(imin)
                draw.oob.curve(y, imin, voffset, col[4], smooth, train.error)
            voffset <- voffset + 1
            leg.text <- c(leg.text,
                if(!imin) "OOB not plotted"
                else      "OOB (rescaled)")
            leg.col  <- c(leg.col, col[4])
            leg.lty  <- c(leg.lty, 2)
            leg.vert <- c(leg.vert, FALSE)
            leg.imin <- c(leg.imin, imin)
        }
    }
    # legend entry for vertical line at n.trees
    if(is.specified(n.trees)) {
        leg.text <- c(leg.text, "predict n.trees")
        leg.col  <- c(leg.col, col.n.trees)
        leg.lty  <- c(leg.lty, 1)
        leg.vert <- c(leg.vert, TRUE)
        leg.imin <- c(leg.imin, n.trees)
    }
    if(any1(col)) { # was anything plotted?
        box() # replot box because vertical.line overplots it slightly
        gbm.legend(legend.x, legend.y, legend.cex,
                   leg.text, leg.col, leg.lty, leg.vert, leg.imin)
        gbm.top.labels(leg.imin, leg.text, leg.col)
    }
    invisible(imins)
}
init.gbm.plot <- function(obj, ylim, final.max, mar, ...)
{
    xlim <- dota("xlim", ...)   # get xlim from dots, NA if not in dots
    n.alltrees <- gbm.n.trees(obj)
    if(!is.specified(xlim))
        xlim <- c(0, n.alltrees)
    xlim <- fix.lim(xlim)
    ylim <- get.gbm.ylim(obj, xlim, ylim, final.max)
    ylab <- get.gbm.ylab(obj)
    # set mar[3] space for top labels and possibly (user-specified) main
    main <- dota("main", ...)   # get main from dots, NA if not in dots
    nlines.needed.for.main <- if(is.specified(main)) nlines(main) + .5 else 0
    par(mar=c(mar[1], mar[2], max(mar[3], nlines.needed.for.main + 1), mar[4]))
    par(mgp=c(1.5, .4, 0))      # squash axis annotations

    # Call graphics::plot but drop args in dots that aren't graphics args
    # or formal args of graphics::plot.
    # If argname below is prefixed with force. then ignore any such arg in dots.
    # Any argname below prefixed with def. can be overridden by a user arg in dots.
    # force.main="" because we add (user-specified) main manually because top labels.

    train.error <- gbm.train.error(obj)
    call.plot(graphics::plot, force.x=1:n.alltrees, force.y=train.error,
        force.type="n", force.main="",  force.xlim=xlim, def.ylim=ylim,
        def.xlab="Number of Trees", def.ylab=ylab, ...)

    if(is.specified(main))
        mtext(main, side=3, line=1.3, cex=par("cex")) # above top labels
}
get.gbm.ylim <- function(obj, xlim, ylim, final.max)
{
    train.error <- gbm.train.error(obj)
    valid.error <- gbm.valid.error(obj)
    cv.error    <- gbm.cv.error(obj)
    if(is.character(ylim) && substr(ylim[1], 1, 1) == "a") { # auto ylim?
        imin <- max(1, min(1, xlim[1]))
        imax <- min(length(train.error), max(length(train.error), xlim[2]))
        cv.error <- gbm.cv.error(obj)
        ylim <- range(train.error[imin:imax],
                      valid.error[imin:imax],
                      cv.error   [imin:imax], na.rm=TRUE)
        # decrease ylim[2] to put more resolution in the "interesting"
        # part of the curve by putting final.max half way up plot
        ylim[2] <- ylim[1] + 2 * (final.max - ylim[1])
        # ensure 75% of training curve is visible
        # (typically needed when no test or CV curve)
        i <- floor(xlim[1] + .25 * (xlim[2] - xlim[1]))
        if(i >= 1 && i <= length(train.error[imin:imax]))
            ylim[2] <- max(ylim[2], train.error[i])
    } else if(!is.specified(ylim)) # ylim=NULL or ylim=NA
         ylim <- range(train.error, valid.error, cv.error, na.rm=TRUE)
    fix.lim(ylim)
}
get.gbm.ylab <- function(obj)
{
    dist <- gbm.short.distribution.name(obj)
    if(dist =="pa") # pairwise
        switch(obj$distribution$metric,
               conc="Fraction of Concordant Pairs",
               ndcg="Normalized Discounted Cumulative Gain",
               map ="Mean Average Precision",
               mrr ="Mean Reciprocal Rank",
               stop0("unrecognized pairwise metric: ",
                     obj$distribution$metric))
    else # not pairwise
        switch(dist,
               ga="Squared Error Loss",      # gaussian
               la="Absolute Loss",           # laplace
               td="t-distribution deviance",
               be="Bernoulli Deviance",      # logistic
               hu="Huberized Hinge Loss",
               mu="Multinomial Deviance",
               ad="Adaboost Exponential Bound",
               ex="Exponential Loss",
               po="Poisson Deviance",
               co="Cox Partial Deviance",
               qu="Quantile Loss",
               stop0("unrecognized distribution name: ",
                     obj$distribution.name))
}
vertical.line <- function(x, col=1, lty=1, voffset=0) # draw a vertical line at x
{
    if(is.specified(col)) {
        usr <- par("usr") # xmin, xmax, ymin, ymax
        range <- usr[4] - usr[3]
        lwd <- 1
        if(lty == 3) { # dotted line?
            # increase lwd to make dotted lines more visible
            lwd <- min(1.5, 2 * par("cex"))
            # small vertical offset so multiple dotted lines at same xpos visible
            voffset <- 0.008 * voffset * range
        } else
            voffset <- 0
        lines(x=c(x, x), y=c(usr[3], usr[4]) - voffset, col=col, lty=lty, lwd=lwd)
        lines(x=c(x, x), y=c(usr[3], usr[3] + .02 * range), col=col, lty=1) # tick
    }
}
# this returns a single NA if y has non finite values
maybe.smooth <- function(y, yname, must.smooth, n.alltrees)
{
    if(any(!is.finite(y))) {
        # infinities in OOB curve occur with distribution="huberized"
        warning0("plot_gbm: cannot plot ", yname,
                 " curve (it has some non-finite values)")
        return(NA)
    }
    if(must.smooth) {
        x <- 1:n.alltrees
        if(n.alltrees < 10) # loess tends to fail for small n.alltrees, use lowess instead
            y <- lowess(x, y)$y
        else             # use loess for compatibility with gbm
            y <- loess(y~x,
                       na.action=na.omit,  # paranoia, prevent warnings from loess
                       # enp.target is the same as gbm.perf for compatibility
                       # (this does only minimal smoothing)
                       enp.target=min(max(4, n.alltrees/10), 50))$fitted
    }
    y
}
which.min1 <- function(x) # like which.min but return 0 if x is all NA
{
    if(all(is.na(x)))
        return(0)
    which.min(x)
}
draw.oob.curve <- function(y, imin, voffset, col, smooth, train.error)
{
    stopifnot(!is.na(imin))
    vertical.line(imin, col, 3, voffset)
    # rescale y to fit into plot
    usr <- par("usr") # xmin, xmax, ymin, ymax
    y <- y - min(y)
    y <- y / max(y) # y is now 0..1
    e <- train.error
    n <- length(e)
    # start and end of OOB curve same as 10% into train curve and end train curve
    y <- e[n] + (e[max(1, 0.1 * n)] - e[n]) * y
    lines(1:n, y, col=col, lty=2)
}
gbm.legend <- function(legend.x, legend.y, legend.cex,
                       leg.text, leg.col, leg.lty, leg.vert, leg.imin)
{
    xjust <- 0
    usr <- par("usr") # xmin, xmax, ymin, ymax
    if(is.null(legend.y))
        legend.y <- usr[3] + .65 * (usr[4] - usr[3])
    if(is.null(legend.x)) {
        # Automatically position the legend just to the left of the
        # leftmost vertical line that is to the right of .7 * usr[2].
        # Hopefully that puts it not on top of anything interesting.
        xjust <- 1
        imin <- c(usr[2],
                  leg.imin[which(leg.imin > usr[1] + .7 * (usr[2]-usr[1]))])
        legend.x <- min(imin) - .05 * (usr[2] - usr[1])
        legend.y <- usr[4] - .05 * (usr[4] - usr[3])
    }
    if(is.specified(legend.x))
        elegend(x=legend.x, y=legend.y,
                legend=leg.text, col=leg.col, lty=leg.lty,
                vert=leg.vert, # vert is supported by elegend but not by legend
                bg="white", cex=legend.cex, xjust=xjust, yjust=xjust)
}
# print the best number-of-trees for each curve along the top of the plot
gbm.top.labels <- function(leg.imin, leg.text, leg.col)
{
    # don't print number-of-trees for the training curve
    stopifnot(substring(leg.text[1], 1, 5) == "train")
    leg.col[1] <- 0
    # darker than darkgray seems needed for top text
    # to be perceived as darkgray, not sure why
    leg.col[leg.col == "darkgray"] <- lighten("darkgray", -0.1)
    usr <- par("usr") # xmin, xmax, ymin, ymax
    # TODO spread.labs is buggy for horizontal labels (too much space sometimes)?
    x <- spread.labs(leg.imin,mindiff=par("cex") * max(strwidth(paste0(leg.imin, " "))),
            min=usr[1], max=usr[2])
    # use of "ok" prevents display off the right or left of the plot
    # (necessary if user specifies xlim)
    # check against leg.imin is for when which.lim1(NA) returns 0
    margin <- .05 * (usr[2] - usr[1])
    ok <- (x > usr[1] - margin) & (x < usr[2] + margin) & (leg.imin != 0)
    if(any(ok))
        text(x=x[ok], # this call to text works with call to text in init.gbm.plot
             y=usr[4] + .4 * strheight("X"), # just above plot
             labels=leg.imin[ok], col=leg.col[ok],
             adj=c(.5, 0), # x is middle of text, y is bottom of text
             xpd=NA)       # allow plotting out the plot area
}
