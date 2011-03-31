# plotmo.R: plot the model response when varying one or two predictors
#
# Comments containing "TODO" mark known issues.
# Stephen Milborrow Sep 2006 Cape Town
#
# TODO allow NAs, revisit NA code
# TODO would like to add a data argument, but NA handling seems insoluble
# TODO allow degree2 to explicitly specify two varnames, likewise for degree1
# TODO allow partial residual plots and variations
# TODO allow user to specify x range on a per predictor basis
# TODO get.plotmo.x should allow allow unnamed cols in x if get
#           from object$x or object$call$x
# TODO would like to add loess of response option
# TODO use ngrid1 to limit the number points plotted for "func" argument
#
# TODO following causes error in model.frame: invalid type (list) for variable 'trees[, -3]'
#      a <- earth(trees[,3] ~ as.matrix(trees[,-3])); plotmo(a)
#    work around:
#      a <- earth(trees[,-3], trees[,3]); plotmo(a)
#    or:
#      Girth <- trees[,1]; Height <- trees[,2]; Volume <- trees[,3]
#      a <- earth(Volume ~ Girth + Height); plotmo(a, trace=TRUE)
#
# TODO revisit warning: 'newdata' had 10 rows but variable(s) found have 31 rows
#    e.g. newdata <- as.data.frame(trees[1:10,-3]); predict(a, newdata=newdata)
#
# TODO Fix this i.e. if predictor in munged formula is a numeric then delete it:
# dat <- data.frame(x1=1:20, x2 = sample(1:20), y = rnorm(20)+1:20)
# fm <- lm(y ~ x1+x2+sin(x1*x2/10), dat)
# plotmo(fm, se=2)
# Error in terms.formula(formula, data = data) : invalid model formula in ExtractVars

#------------------------------------------------------------------------------
# If ylim==NULL we do an initial pass across all plots (both degree1 and degree2)
# without actually plotting to calculate the min and max y limits (actually z limits
# on degree2 plots).  If col.response is set then we also incorporate
# the response min,max.

plotmo <- function(object = stop("no 'object' arg"),
    type        = NULL,
    nresponse   = NA,
    clip        = TRUE,
    ylim        = NULL,
    degree1     = TRUE,
    all1        = FALSE,
    degree2     = TRUE,
    all2        = FALSE,
    grid.func   = median,
    grid.levels = NULL,
    col.response    = 0,
    cex.response    = 1,
    pch.response    = 1,
    jitter.response = 0,
    inverse.func = NULL,
    xflip       = FALSE,
    yflip       = FALSE,
    swapxy      = FALSE,
    trace       = FALSE,
    nrug        = 0,
    se          = 0,
    col.shade   = "lightblue",
    col.se      = 0,
    lty.se      = 2,
    func        = NULL,
    col.func    = "pink",
    lwd.func    = 1,
    lty.func    = 1,
    ngrid1      = 500,
    lty.degree1 = 1,
    col.degree1 = 1,
    type2       = "persp",
    ngrid2      = 20,
    col.image   = grey(0:9/10),
    col.persp   = "lightblue",
    theta       = NA,
    phi         = 30,
    shade       = 0.5,
    do.par      = TRUE,
    caption     = NULL,
    main        = NULL,
    xlab        = "",
    ylab        = "",
    cex         = NULL,
    cex.lab     = 1,
    ...)
{
    # call the plotting functions with draw.plot=FALSE to get the ylims
    get.ylim.by.dummy.plots <- function(ylims, trace, ...)
    {
        if(nsingles) # get ylim=c(miny, maxy) by calling with draw.plot=FALSE
            ylims <- plot.degree1(object, degree1, all1, ylim, nresponse, type,
                        clip, trace, col.response, cex.response, pch.response,
                        jitter.response, inverse.func, grid.func, grid.levels,
                        ngrid1, lty.degree1, col.degree1, se, lty.se, col.se,
                        col.shade, func, col.func, lwd.func, lty.func, nrug,
                        draw.plot=FALSE, x, y, singles, ylims, func.arg, pred.names,
                        inverse.func.arg, clip.limits, nfigs, main, xlab, ylab, cex,
                        cex.lab, xflip, dots)$ylims

        if(npairs)
            ylims <- plot.degree2(object, degree2, all2, ylim, nresponse, type,
                        clip, trace, col.response, cex.response, pch.response,
                        jitter.response, inverse.func, grid.func, grid.levels,
                        type2, ngrid2, col.persp, col.image, draw.plot=FALSE, x, y,
                        pairs, ylims, func.arg, pred.names, inverse.func.arg,
                        clip.limits, nfigs, nsingles, npairs, do.par, main, xflip,
                        yflip, swapxy, xlab, ylab, cex, cex.lab, theta, phi, shade,
                        ...)$ylims

        if(!is.na.or.zero(col.response))
            ylims <- range(ylims, y, finite=TRUE) # extend ylims to include response range
        if(any(is.na(ylims)) || any(!is.finite(ylims)) ||
                abs(ylims[1] - ylims[2]) < 1e-8) {
            cat("\nylim", ylims, "\n\n")
            if(clip)
                stop0("plotmo cannot calculate ylim, try clip=FALSE")
            else
                stop0("plotmo cannot calculate ylim, try manually specifying ylim")
        }
        ylims
    }
    get.ylims <- function(...) # check ylim arg and calculate min,max limits of y axis
    {
        if(!(is.null(ylim) || is.na(ylim[1]) || length(ylim) == 2))
            stop0("'ylim' must be one of:\n",
                "  NULL         all graphs have same vertical axes\n",
                "  NA           each graph has its own vertical axis\n",
                "  c(ymin,ymax) specify y axis min and max")
        if(length(ylim) == 2 && ylim[2] <= ylim[1])
            stop0("ylim[2] ", ylim[2], " is not greater than ylim[1] ", ylim[1])
        ylims <- c(NA, NA)
        if(is.null(ylim)) { # automatic ylim, the default
            temp <- get.plotmo.auto.ylim(object, type, trace)
            if(!is.na(temp[1]))
                ylims <- temp
            else
                ylims <- get.ylim.by.dummy.plots(ylims, trace)
        }
        if(trace)
            cat("\nylim", ylims, "\n")
        ylims
    }
    #--- plotmo starts here ---
    plotmo.prolog(object, deparse(substitute(object)))
    func.arg <- deparse(substitute(func))
    inverse.func.arg <- deparse(substitute(inverse.func))
    stopifnot(is.character(type2) && length(type2) == 1)
    dots <- match.call(expand.dots=FALSE)$...
    check.dots.args(type2, dots)
    stopifnot.boolean(clip)
    stopifnot.boolean(all1)
    stopifnot.boolean(all2)
    stopifnot.boolean(swapxy)
    stopifnot.boolean(xflip)
    stopifnot.boolean(yflip)
    stopifnot.boolean(do.par)
    stopifnot(is.numeric(ngrid1) && length(ngrid1) == 1)
    trace <- check.trace.arg(trace)
    if(ngrid1 == -1)
        ngrid1 <- nrow(x)
    else if(ngrid1 < 1 || ngrid1 > 1e5)
        stop0("illegal ngrid1 ", ngrid1, ", allowed range is 1 to 1e5 or -1")
    stopifnot(is.numeric(ngrid2) && length(ngrid2) == 1 && ngrid2 > 1)
    if(ngrid2 > 500) {
        warning0("clipped ngrid2=", ngrid2, " to 500")
        ngrid2 <- 500
    }
    temp <- check.se.arg(se, missing(lty.se), lty.se, col.se, col.shade)
        se <- temp$se
        col.se <- temp$col.se
    env <- parent.frame() # the environment from which plotmo was called
    type <- get.plotmo.type(object, type)
    x <- get.plotmo.x.wrapper(object, env, trace)
    pred.names <- colnames(x)
    y <- get.plotmo.y.wrapper(object, env, nresponse, nrow(x), trace)
    y <- apply.inverse.func(y, trace, inverse.func, inverse.func.arg)
    clip.limits <- c(-Inf, Inf)
    if(clip)
        clip.limits <- get.plotmo.clip.limits.wrapper(object, type, y, trace)
    # singles is a vector of indices of predictors for degree1 plots
    singles <- get.plotmo.singles.wrapper(object, env, x, trace, degree1, all1)
    nsingles <- length(singles)
    # each row of pairs is the indices of two predictors for a degree2 plot
    pairs <- get.plotmo.pairs.wrapper(object, env, x, trace, all2, degree2)
    npairs <- NROW(pairs)
    nfigs <- nsingles + npairs
    if(nfigs == 0) {
        warning0("plotmo: nothing to plot")
        return(invisible())
    }
    ylims <- get.ylims(...) # check ylim arg and calculate min,max limits of y axis
    user.specified.caption <- !is.null(caption)
    if(!user.specified.caption) {
        if(is.null(object$call)) {
            caption <- deparse(substitute(object))
            if(length(caption) != 1 || nchar(caption) > 30) # sanity check
                caption <- "plotmo"
        } else
            caption <- strip.white.space(paste0(deparse(object$call), collapse=""))
    }
    if(do.par) {
        old.par <- par(no.readonly=TRUE)
        on.exit(par(old.par))
        do.par(nfigs, cex, xlab, ylab, caption)
    }
    else if(!is.null(cex)) {
        old.cex <- par("cex")
        on.exit(par(cex=old.cex))
        par(cex=cex)
    }
    response.name <- NULL # column name when predict returns multiple columns
    if(nsingles) {
        response.name <- plot.degree1(object, degree1, all1, ylim, nresponse, type,
                    clip, trace, col.response, cex.response, pch.response,
                    jitter.response, inverse.func, grid.func, grid.levels,
                    ngrid1, lty.degree1, col.degree1, se, lty.se, col.se,
                    col.shade, func, col.func, lwd.func, lty.func, nrug,
                    draw.plot=TRUE, x, y, singles, ylims, func.arg, pred.names,
                    inverse.func.arg, clip.limits, nfigs, main, xlab, ylab, cex,
                    cex.lab, xflip, dots)$response.name
    }
    if(npairs) {
        if(do.par)
            do.par.degree2(type2, caption, dots)
        temp <- plot.degree2(object, degree2, all2, ylim, nresponse, type,
                    clip, trace, col.response, cex.response, pch.response,
                    jitter.response, inverse.func, grid.func, grid.levels,
                    type2, ngrid2, col.persp, col.image, draw.plot=TRUE, x, y,
                    pairs, ylims, func.arg, pred.names, inverse.func.arg,
                    clip.limits, nfigs, nsingles, npairs, do.par, main, xflip,
                    yflip, swapxy, xlab, ylab, cex, cex.lab, theta, phi, shade,
                    ...)

        if(is.null(response.name))
            response.name <- temp$response.name
    }
    if(!is.null(response.name) && !user.specified.caption)
        caption <- paste0(response.name, ": ", caption)
    if(user.specified.caption || do.par)
        show.caption(caption, trim=!user.specified.caption,
                     cex=if(is.null(cex)) 1 else 1.2 * cex)
    invisible()
}
# Check dots arguments, if any.  We want to make sure that amongst other
# things if the user uses an illegal argument she gets told about it.
# The only way we can do that (that I know of) and also support ... is
# to do explicit checks here.

check.dots.args <- function(type2, dots)
{
    check.illegal <- function(illegal.argname, msg)
    {
        # if illegal.argname is in dots then issue msg
        ibad <- pmatch(names(dots), illegal.argname, 0)
        if(any(ibad)) {
            culprit <- names(dots)[as.logical(ibad)][1]
            if(culprit == illegal.argname)
                stop0(msg)
            else # partial match, tell user what was matched
                stop0(msg, " (\"", culprit, "\" taken to mean \"",
                     illegal.argname, "\")")
        }
    }
    #--- check.dots starts here
    if(is.null(dots))
        return()
    check.illegal("ndegree1",
                  "\"ndegree1\" is no longer legal, use \"ngrid1\" instead")
    check.illegal("ycolumn",
                  "\"ycolumn\" is no longer legal, use \"nresponse\" instead")
    check.illegal("title",
                  "\"title\" is illegal, use \"caption\" instead")
    # Not all of the args in legal.all may make sense or be used in plotmo.
    # Following excluded because we use them when calling plot: cex.axis xaxt yaxt.
    # TODO "lty" not allowed, partial matching problem
    legal.all <- c("adj", "ann", "ask", "bg", "bty", "cex.main",
                    "cex.sub", "cin", "col.axis", "col.lab", "col.main",
                    "col.sub", "cra", "crt", "csi", "cxy", "family", "fg",
                    "fig", "fin", "font", "font.axis", "font.lab", "font.main",
                    "font.sub", "lab", "lend", "lheight", "ljoin", "lmitre",
                    "lty", "lwd", "mai", "mar", "mex", "mfg", "mgp", "mkh",
                    "sub", "tck", "tcl", "usr", "xaxs", "xlog", "xpd", "yaxp",
                    "yaxs", "yaxt", "ylog")

    if(pmatch(type2, "persp", 0)) {
        check.illegal("zlab", # a common mistake
                      "\"zlab\" is illegal, use \"ylab\" instead")
        legal.dots.args <- # "d" is not included, causes partial matching issues
            c("phi", "r", "scale", "expand", "ltheta", "lphi",
              "border", "shade", "box", "axes", "ticktype", "nticks", legal.all)
    } else if(pmatch(type2, "contour", 0))
        legal.dots.args <-
            c("nlevels", "levels", "labels", "labcex", "drawlabels",
              "method", "vfont,", "axes", "frame.plot", legal.all)
    else if(pmatch(type2, "image", 0))
        legal.dots.args <-
            c("breaks", "oldstyle", legal.all)
    else
        stop0("type2=\"", type2, "\" is illegal")
    names <- names(dots)
    pmatch <- pmatch(names, legal.dots.args, nomatch=FALSE, duplicates.ok=TRUE)
    if(any(!pmatch)) {
        # report the first illegal arg
        ibad <- (1:length(dots))[!pmatch]
        stop0("plotmo: illegal argument \"", names[ibad][1], "\"")
    }
    duplicated <- duplicated(pmatch)
    if(any(duplicated))
        stop0("duplicated arguments ",
              paste.quoted.names(names[pmatch == pmatch[which(duplicated)[1]]]))
}
check.se.arg <- function(se, is.missing.lty.se, lty.se, col.se, col.shade)
{
    # check se argument
    stopifnot(length(se) == 1)
    if(is.na(se))
        se <- 0
    if(is.logical(se)) {
        if(identical(se, TRUE)) {
            # allow user to use se=TRUE for compat with termplot
            warning0("plotmo: converted se=TRUE to se=2")
            se <- 2
        } else
            se <- 0 # silently treat FALSE or NA as 0
    }
    stopifnot(is.numeric(se))
    if(se < 0 || se > 9) # 9 is arbitrary
        stop0("se=", se, " is out of range")
    if(!is.missing.lty.se && lty.se != 0 && col.se[1] == 0)
        col.se <- 1  # needed if user sets just lty.se but doesn't set col.se
    if(se && (col.se == 0 || lty.se == 0) && col.shade == 0)
        warning0("plotmo: 'se' ignored because (col.se == 0 || lty.se == 0) && ",
                 "col.shade == 0)")
    list(se=se, col.se=col.se)
}
do.par <- function(nfigs, cex, xlab, ylab, caption)
{
    nrows <- ceiling(sqrt(nfigs))
    par(mfrow=c(nrows, nrows))
    par(mar=c(3.5, 2.5, 1.7, 0.5)) # small margins to pack figs in
    par(mgp=c(1.5, .5, 0))         # flatten axis elements (TODO ignored by persp)
    if(!is.null(cex))
        par(cex=cex)
    if(is.null(ylab) || nchar(ylab) > 0)
        make.space.for.left.axis()
    if(is.null(xlab) || nchar(xlab) > 0)
        make.space.for.bottom.axis()
    make.space.for.caption(caption)
}
do.par.degree2 <- function(type2, caption, dots) # adjust par settings for degree2 plots
{
    if(!pmatch(type2, "persp", 0)) {    # contour or image plot?
        make.space.for.bottom.axis()
        make.space.for.left.axis()
    } else {                            # persp plot, must look at ticktype arg
        i <- as.logical(pmatch(names(dots), "ticktype", 0)) # get ticktype arg
        if(any(i) && pmatch(dots[i], "detailed", 0)) # ticktype="detailed"?
           par(mar=c(1, .3, 1.7, 0.1))  # extra space at bottom for axis labs
        else { # ticktype="simple"
            par(mar=c(.2, .3, 1.7, 0.1))
            # avoid a very obscure error message from persp
            if(any(pmatch(names(dots), "nticks", 0)))
                stop0("nticks is illegal with ticktype=\"simple\"")
        }
        par(mgp=c(2, 0.6, 0))           # TODO this doesn't work, persp ignores mgp
    }
    make.space.for.caption(caption)
}
get.plotmo.clip.limits.wrapper <- function(object, type, y, trace)
{
    clip.limits <- get.plotmo.clip.limits(object, type, y, trace)
    if(is.na(clip.limits[1]))
        clip.limits <- range(y, finite=TRUE)
    if(trace)
        cat("\nclip.limits", clip.limits, "\n")
    clip.limits # a two elem vec
}
get.plotmo.singles.wrapper <- function(object, env, x, trace, degree1, all1)
{
    if(identical(degree1, NA)) # allow NA to mean 0
        degree1 <- 0
    if(!is.numeric(degree1) && !is.logical(degree1)) {
        if(pmatch(degree1, "all", 0)) # backwards compatibility message
            stop0("degree1=\"", degree1, "\" is no longer legal, use all1=TRUE instead")
        stop0("degree1 must be an index vector (numeric or logical)")
    }
    if(trace > 1)
        cat("\n--get.plotmo.singles\n\n")
    singles <- get.plotmo.singles(object, env, x, trace > 1, all1)
    if(length(singles))
        singles <- sort(unique(singles))
    nsingles <- length(singles)
    if(nsingles) {
        singles <- sort(singles)
        check.index.vec("degree1", degree1, singles)
        singles <- singles[degree1]
    }
    if(trace > 1) {
        if(nsingles)
            cat("singles:", paste(singles, colnames(x)[singles], collapse=", "), "\n")
        else
            cat("no singles\n")
    }
    if(nsingles == 0 && is.specified(degree1) && degree1[1] != 0)
        warning0("\"degree1\" specified but no degree1 plots")
    singles # a vector of indices of predictors for degree1 plots
}
get.plotmo.pairs.wrapper <- function(object, env, x, trace, all2, degree2)
{
    if(identical(degree2, NA)) # allow NA to mean 0
        degree2 <- 0
    if(!is.numeric(degree2) && !is.logical(degree2)) {
        if(pmatch(degree2, "all", 0)) # backwards compatibility message
            stop0("degree2=\"", degree2, "\" is no longer legal, use all2=TRUE instead")
        stop0("degree2 must be an index vector (numeric or logical)")
    }
    if(trace > 1)
        cat("\n--get.plotmo.pairs\n\n")
    pairs <- get.plotmo.pairs(object, env, x, trace > 1, all2)
    if(!NROW(pairs) || !NCOL(pairs))
        pairs <- NULL
    npairs <- NROW(pairs)
    if(npairs) {
        # put lowest numbered predictor first and remove duplicate pairs
        pairs <- unique(t(apply(pairs, 1, sort)))
        # order the pairs on the predictor order
        order <- order(pairs[,1], pairs[,2])
        pairs <- pairs[order, , drop=FALSE]
        check.index.vec("degree2", degree2, pairs)
        pairs <- pairs[degree2, , drop=FALSE]
    }
    if(trace > 1) {
        if(npairs) {
            cat("pairs:\n")
            print(matrix(paste(pairs, colnames(x)[pairs]), ncol=2))
        } else
            cat("no pairs\n")
    }
    if(npairs == 0 && is.specified(degree2) && degree2[1] != 0)
        warning0("\"degree2\" specified but no degree2 plots")
    pairs
}
get.plotmo.x.wrapper <- function(object, env, trace)
{
    x <- get.plotmo.x(object, env, trace)
    x <- as.data.frame(x)
    subset. <- get.subset(object, env, trace)
    if(!is.null(subset.)) {
        check.index.vec("subset.", subset., x, check.empty=TRUE, allow.duplicates=TRUE)
        x <- x[subset., , drop=FALSE]
    }
    if(trace) {
        cat0("\nx[", nrow(x), ",", ncol(x), "]:\n")
        print(head(x, 3))
        if(any(sapply(x, is.factor)))
            cat("\nis.factor", sapply(x, is.factor), "\n")
    }
    x
}
get.plotmo.y.wrapper <- function(object, env, y.column, expected.len, trace)
{
    y <- get.plotmo.y(object, env, expected.len, trace)
    if(inherits(object, "earth") || inherits(object, "bagEarth")) {
        # do the same processing on y as earth does, e.g. if y is a two
        # level factor, convert it to an indicator column of 0s and 1s
        y <- expand.arg(y, env, is.y.arg=TRUE, colnames(y))
        if(identical(colnames(y), "y"))
            colnames(y) <- NULL # remove artificial colname added by expand.arg
        if(!is.null(object$glm.list[[1]])) # if an earth.glm model, use y.column 1
            y.column <- 1
    } else
        y.column <- 1
    subset. <- get.subset(object, env, trace)
    y <- check.and.print.y(y, "get.plotmo.y", nresponse=y.column,
                           expected.len, trace, subset.)
    if(!is.null(subset.)) {
        check.index.vec("subset", subset., y, check.empty=TRUE, allow.duplicates=TRUE)
        y <- y[subset.]
    }
    y
}
plotmo.predict.wrapper <- function(object, newdata, type, se.fit,
                                   trace1, pred.names, ipred1, ipred2=0)
{
    stopifnot(is.character(type) && length(type) == 1)
    if(trace1) {
        if(ipred2 == 0)
            cat0("\nplotmo.predict(type=\"", type, "\") for predictor \"",
                pred.names[ipred1], "\" ")
        else
            cat0("\nplotmo.predict(type=\"", type, "\") for predictors \"",
                 pred.names[ipred1], "\" and \"", pred.names[ipred2], "\" ")
        if(se.fit)
            cat("se.fit=TRUE ")
        cat0("with newdata[", NROW(newdata), ",", NCOL(newdata), "]:\n")
        print(head(newdata, 3))
    }
    y.predict <- plotmo.predict(object, newdata, type, se.fit, trace1 > 1)
    if(trace1 == 1)
        cat("\n")
    y.predict
}
# plot degree one plots i.e. main effects

plot.degree1 <- function(
    # copy of args from plotmo, some have been tweaked slightly
    object, degree1, all1, ylim, nresponse, type, clip, trace,
    col.response, cex.response, pch.response, jitter.response,
    inverse.func, grid.func, grid.levels,
    ngrid1, lty.degree1, col.degree1, se, lty.se, col.se, col.shade,
    func, col.func, lwd.func, lty.func, nrug,

    # args generated in plotmo, draw.plot=FALSE means get ylims but don't actually plot
    draw.plot, x, y, singles, ylims, func.arg, pred.names,
    inverse.func.arg, clip.limits, nfigs,

    # copy of par args from plotmo
    main, xlab, ylab, cex, cex.lab, xflip, dots)
{
    draw.plot.degree1 <- function()
    {
        # draw std err bands and lines for a factor predictor
        draw.factor.se <- function(y) # draw std err bands and lines for a factor predictor
        {
            y.se.lower1 <- split(y.se.lower, xframe[,ipred])
            y.se.upper1 <- split(y.se.upper, xframe[,ipred])
            for(ilev in seq_along(levels(xframe[,ipred]))) {
                min <- min(y.se.lower1[[ilev]])
                max <- max(y.se.upper1[[ilev]])
                if(!is.na.or.zero(col.shade))
                    polygon(c(ilev - .4, ilev - .4, ilev + .4, ilev + .4),
                        c(min, max, max, min), col=col.shade, lty=0, border=NA)
                if(lty.se != 0 && !is.na.or.zero(col.se)) {
                    segments(ilev -.4, min, ilev + .4, min, lty=lty.se, col=col.se)
                    segments(ilev -.4, max, ilev + .4, max, lty=lty.se, col=col.se)
                }
            }
        }
        draw.numeric.se <- function() # draw std err bars and lines for a numeric predictor
        {
            if(!is.na.or.zero(col.shade))
                polygon(c(xframe[,ipred], rev(xframe[,ipred])),
                        c(y.se.lower, rev(y.se.upper)),
                        col=col.shade, lty=0, border=NA)
            if(lty.se != 0 && !is.na.or.zero(col.se)) {
                lines(xframe[,ipred], y.se.lower, lty=lty.se, col=col.se)
                lines(xframe[,ipred], y.se.upper, lty=lty.se, col=col.se)
            }
        }
        # get title if any for the current plot
        get.main1 <- function(main, isingle, nfigs, degree1, all1, pred.names, ipred)
        {
            main <- ""
            # show degree1 plot numbers in headers if plotting all predictors
            if(nfigs > 1 && (!is.specified(degree1) || all1))
                main <- paste0(isingle, " ")
            paste(main, pred.names[ipred])
        }
        #--- draw.plot.degree1 starts here ---
        if(is.null(ylim.org))           # same ylim for each graph?
            ylim <- ylims
        else if(is.na(ylim.org[1])) {   # each graph has its own ylim?
            ylim <- range1(y.predict, finite=TRUE)
            if(!is.null(y.se.lower))
                ylim <- range1(y.predict, y.se.lower, y.se.upper, finite=TRUE)
            if(any(!is.finite(ylim)))
                stop0("ylim argument to plotmo is NA but cannot generate ",
                      "ylim internally from predicted values (predictor \"",
                      pred.names[ipred], "\")")
        }
        if(nrug && (is.null(ylim.org) || is.na(ylim.org[1])))
            ylim[1] <- ylim[1] - .05 * (ylim[2] - ylim[1])
        main <- get.main(main, isingle, get.main1,
                         nfigs, degree1, all1, pred.names, ipred)
        if(is.null(xlab))
            xlab <- pred.names[ipred]
        levnames <- levels(xframe[,ipred])
        plot.levnames <- is.fac && length(levnames) <= 12
        xlim <-NULL
        xcol <- xframe[,ipred]
        if(xflip)
            if(is.factor(xcol))
                xlim <- c(nlevels(xcol), 1)
            else
                xlim <- c(max(xcol), min(xcol))

        plot(xframe[,ipred], y.predict, type="n",    # calls boxplot if is.fac
                main=main, xlab=xlab, ylab=ylab,
                xlim=xlim, ylim=ylim,
                xaxt=if(plot.levnames) "n" else "s",
                cex.lab=1.1 * cex.lab, cex.axis=cex.lab,
                col.axis=eval(dots$col.axis), col.lab=eval(dots$col.lab),
                font=eval(dots$font), font.axis=eval(dots$font.axis),
                lab=eval(dots$lab), mgp=eval(dots$mgp))

        if(is.fac) { # x[,ipred] is a factor?
            if(!is.null(y.se.lower))
                draw.factor.se()
            if(!is.na.or.zero(col.response))
                points(jitter(as.numeric(x[,ipred]), factor=.5),
                       jitter(y, factor=jitter.response),
                       col=col.response, cex=cex.response, pch=pch.response)
            plot(xframe[,ipred], y.predict, add=TRUE,
                 xaxt=if(plot.levnames) "n" else "s",
                 lwd=eval(dots$lwd))
            if(plot.levnames) { # plot level names along the x axis
                levnames <- abbreviate(levnames, minlength=6, strict=TRUE)
                mtext(levnames, side=1, at=1:length(levnames),
                      cex=par("cex") * cex.lab, line=.5, las=get.las(levnames),
                      col.axis=eval(dots$col.axis), col.lab=eval(dots$col.lab),
                      font.axis=eval(dots$font.axis))
            }
        } else {    # not is.fac
            if(!is.null(y.se.lower))
                draw.numeric.se()
            draw.degree1.func(func, func.arg, # draw the func arg, if specified
                              xframe, ipred, trace1, col.func, lwd.func, lty.func)
            if(!is.na.or.zero(col.response))
                points(jitter(x[,ipred], factor=jitter.response),
                       jitter(y, factor=jitter.response),
                       col=col.response, cex=cex.response, pch=pch.response)
            lines(x=xframe[,ipred], y=y.predict,
                 lty=lty.degree1, col=col.degree1, lwd=eval(dots$lwd))
        }
        if(nrug)
            rug(jitter(as.numeric(x[irug, ipred])))
    }
    #--- plot.degree1 starts here ---
    trace1 <- if(!draw.plot) trace else 0
    if(trace1 || trace > 1)
        cat0("\n--plot.degree1(draw.plot=", draw.plot, ")\n")
    ylim.org <- ylim
    # get the x matrix we will plot, will be updated later for each predictor one by one
    xgrid <- get.degree1.xgrid(x, grid.func, grid.levels, pred.names, ngrid1)
    if(draw.plot)
        print.grid.values(xgrid)
    irug <- get.degree1.irug(nrug, x, draw.plot) # get the positions of the rug points, if any
    out.of.range.preds <- NULL # used solely for warning messages
    response.name <- NULL
    for(isingle in seq_along(singles)) {
        ipred <- singles[isingle] # ipred is the predictor index i.e. col in model mat
        # following happens with lm if you do e.g. ozone1$doy <- NULL after using ozone1
        # TODO I am not sure if this is enough to always catch such errors
        if(ipred > NCOL(x))
            stop0("bad index (missing column in x?)")
        is.fac <- is.factor(x[,ipred])
        if(isingle > 1 && trace == 1)  # trace only the first graph if trace=1
            trace1 <- 0
        # create data.frame of x values to be plotted, by updating xgrid for this predictor
        xframe <- get.degree1.xframe(xgrid, x, ipred, ngrid1)

        y.predict <- plotmo.predict.wrapper(object, xframe, type, se.fit=FALSE,
                                            trace1, pred.names, ipred)
        temp <- check.and.print.y(y.predict, paste0("predict()"),
                                  nresponse, nrow(xframe), trace1, return.yname=TRUE)
            y.predict     <- temp$y
            response.name <- temp$yname

        temp <- get.degree1.se(y.predict, se, object, type, xframe, pred.names,
                               ipred, trace1, inverse.func, inverse.func.arg)
            y.se.lower <- temp$y.se.lower
            y.se.upper <- temp$y.se.upper

        y.predict <- apply.inverse.func(y.predict,
                                        trace1, inverse.func, inverse.func.arg)
        if(clip) {
            y.lt <- y.predict < clip.limits[1]
            y.gt <- y.predict > clip.limits[2]
            if(all(y.lt) || all(y.gt))
                out.of.range.preds <- c(out.of.range.preds, ipred)
            else
                y.predict[y.lt | y.gt] <- NA
        }
        ylims <- range1(ylims, y.predict, y.se.lower, y.se.upper, finite=TRUE)
        if(draw.plot)
            draw.plot.degree1()
    }
    if(clip)
        possibly.issue.degree1.out.of.range.warning(out.of.range.preds, pred.names,
                                                    clip.limits, draw.plot)
    list(ylims=ylims, response.name=response.name)
}
get.degree1.irug <- function(nrug, x, draw.plot)
{
    if(!draw.plot || nrug == 0)
        return(NULL)
    if(nrug < 0)
        nrug <- nrow(x)
    if(nrug > nrow(x))
        nrug <- nrow(x)
    as.integer(seq(from=1, to=nrow(x), length.out=nrug))
}
get.degree1.se <- function(y.predict, se, object, type, xframe, pred.names,
                           ipred, trace1, inverse.func, inverse.func.arg)
{
    y.se.lower <- NULL
    y.se.upper <- NULL
    if(!is.na.or.zero(se)) {
        if(trace1)
            cat("begin se handling, ")
        temp <- plotmo.predict.wrapper(object, xframe, type, se.fit=TRUE,
                                       trace1, pred.names, ipred)

        if(typeof(temp) == "list" && !is.null(temp$se.fit)) {
            temp$se.fit <- check.and.print.y(temp$se.fit, "predict with se=TRUE",
                                             nresponse=1, nrow(xframe), trace1)
            y.se.lower <- y.predict - se * temp$se.fit
            y.se.lower <- apply.inverse.func(y.se.lower, trace1,
                                             inverse.func, inverse.func.arg)
            y.se.upper <- y.predict + se * temp$se.fit
            y.se.upper <- apply.inverse.func(y.se.upper, trace1,
                                             inverse.func, inverse.func.arg)
        } else if(trace1)
            cat("no standard errs because is.null(temp$se.fit)\n")

        if(trace1)
            cat("end se handling\n")
    }
    list(y.se.lower=y.se.lower, y.se.upper=y.se.upper)
}
draw.degree1.func <- function(func, func.arg,
                              xframe, ipred, trace, col.func, lwd.func, lty.func)
{
    if(exists.and.not.null(func.arg, "function", "func")) {
        if(trace) {
            cat("\nApplying \"func\" arg to\n")
            print(head(xframe, 3))
        }
        y.func <- func(xframe)
        y.func <- check.and.print.y(y.func, paste0("func=", func.arg),
                                    nresponse=1, nrow(xframe), trace)
        lines(xframe[,ipred], y.func, col=col.func, lwd=lwd.func, lty=lty.func)
    }
}
possibly.issue.degree1.out.of.range.warning <- function(out.of.range.preds, pred.names,
                                                        clip.limits, draw.plot)
{
    if(length(out.of.range.preds) && !draw.plot)
        warning0("predicted values in the \"",
            pred.names[out.of.range.preds[1]],
            "\" graph ",
            if(length(out.of.range.preds) > 1) "(and others) " else "",
            "are out of ylim=(",
            sprintf("%.2g", clip.limits[1]), ", ", sprintf("%.2g", clip.limits[2]),
            ").\n         Use clip=FALSE to make this warning go away.")
}
# plot degree two plots

plot.degree2 <- function(
    # copy of args from plotmo, some have been tweaked slightly
    object, degree2, all2, ylim, nresponse, type, clip, trace,
    col.response, cex.response, pch.response, jitter.response,
    inverse.func, grid.func, grid.levels, type2, ngrid2, col.persp, col.image,

    # args generated in plotmo, draw.plot=FALSE means get ylims but don't actually plot
    draw.plot, x, y, pairs, ylims, func.arg, pred.names,
    inverse.func.arg, clip.limits, nfigs, nsingles, npairs,

    # copy of args from plotmo
    do.par, main, xflip, yflip, swapxy, xlab, ylab, cex, cex.lab,
    theta, phi, shade, ...)
{
    draw.plot.degree2 <- function(type2 = c("persp", "contour", "image"), ...)
    {
        # get title if any for the current plot
        get.main2 <- function(main, imain, nfigs, degree2, all2, ipair,
                              pred.names, ipred1, ipred2)
        {
            main <- ""
            # show degree2 plot numbers in headers if plotting all predictors
            if(nfigs > 1 && (!is.specified(degree2) || all2))
                main <- paste0(ipair, " ")
            if(swapxy)
                paste0(main, pred.names[ipred2], ": ", pred.names[ipred1])
            else
                paste0(main, pred.names[ipred1], ": ", pred.names[ipred2])
        }
        #--- draw.plot.degree2 starts here ---
        main <- get.main(main, nsingles+ipair, get.main2,
                         nfigs, degree2, all2, ipair, pred.names, ipred1, ipred2)
        if(is.null(ylim))           # same ylim for each graph? (actually zlim)
            ylim <- ylims
        else if(is.na(ylim[1])) {   # each graph has its own ylim?
            ylim <- range1(y.predict, finite=TRUE)
            if(any(!is.finite(ylims))) {
                cat("ylim", ylims, "\n\n")
                stop0("Cannot generate ylim automatically for \"",
                    pred.names[ipred1], "\" and \"", pred.names[ipred2], "\".\n",
                    "       Specify ylim to make this message go away.")
            }
        }
        switch(match.arg1(type2),
            plot.persp(x, grid1, grid2, y.predict, pred.names, ipred1, ipred2,
                       trace, ylab, ylim, xflip, yflip, swapxy, ngrid2,
                       col.persp, theta, phi, shade,
                       main, cex, col.image, cex.lab,
                       col.response, cex.response, pch.response, jitter.response, ...),
            plot.contour(x, grid1, grid2, y.predict, pred.names, ipred1, ipred2,
                       xflip, yflip, swapxy,
                       main, cex, col.image, cex.lab,
                       col.response, cex.response, pch.response, jitter.response, ...),
            plot.image(x, grid1, grid2, y.predict, pred.names, ipred1, ipred2,
                       xflip, yflip, swapxy,
                       main, cex, col.image, cex.lab,
                       col.response, cex.response, pch.response, jitter.response, ...),
            NULL)
    }
    #--- plot.degree2 starts here ---
    trace1 <- if(!draw.plot) trace else 0
    if(trace1 || trace > 1)
        cat0("\n--plot.degree2(draw.plot=", draw.plot, ")\n")
    if(!trace1 && trace > 1)
        cat("\n")
    stopifnot(npairs > 0)
    xranges <- sapply(x, range1, na.rm=TRUE)
    # get the x matrix we will plot, will be updated later for each pair of predictors
    xgrid <- get.degree2.xgrid(x, grid.func, grid.levels, pred.names, ngrid2)
    response.name <- NULL
    for(ipair in 1:npairs) {
        ipred1 <- pairs[ipair,1]        # index of first predictor
        ipred2 <- pairs[ipair,2]        # index of second predictor
        if(ipair > 1 && trace1 == 1)    # trace only the first graph if trace=1
            trace1 <- 0
        # create data.frame of x values to be plotted, by updating xgrid for this pair
        temp <- get.degree2.xframe(xgrid, x, ipred1, ipred2, ngrid2, xranges)
            xframe <- temp$xframe
            grid1  <- temp$grid1
            grid2  <- temp$grid2

        y.predict <- plotmo.predict.wrapper(object, xframe, type, se.fit=FALSE,
                                            trace1, pred.names, ipred1, ipred2)

        temp <- check.and.print.y(y.predict, paste0("predict()"),
                                  nresponse, nrow(xframe), trace1, return.yname=TRUE)
            y.predict     <- temp$y
            response.name <- temp$yname

        y.predict <- apply.inverse.func(y.predict, trace1,
                                        inverse.func, inverse.func.arg)
        if(pmatch(type2, "persp", 0) &&
                (is.factor(x[[ipred1]]) || is.factor(x[[ipred2]]))) {
            temp <- fix.frame.for.persp(x, y.predict, grid1, grid2, ipred1, ipred2)
                y.predict <- temp$y.predict
                grid1     <- temp$grid1
                grid2     <- temp$grid2
        }
        y.predict <- matrix(y.predict, nrow=length(grid1), ncol=length(grid2))
        if(clip) {
            y.predict[y.predict < clip.limits[1] | y.predict > clip.limits[2]] <- NA
            if(all(is.na(y.predict)))
                stop0("all predicted values are out of the range ",
                      "of the original response, try clip=FALSE\n")
        }
        ylims <- range1(ylims, y.predict, finite=TRUE)
        if(draw.plot)
            draw.plot.degree2(type2, ...)
    }
    if(trace > 1 && pmatch(type2, "persp", 0))
        cat("\n")
    list(ylims=ylims, response.name=response.name)
}
plot.response.sites <- function(x, ipred1, ipred2, col, cex, pch, jitter, swapxy)
{
    if(swapxy) {
        x1 <- x[,ipred2]
        x2 <- x[,ipred1]
    } else {
        x1 <- x[,ipred1]
        x2 <- x[,ipred2]
    }
    points(jitter(as.numeric(x1), factor=jitter), # as.numeric needed if x1 is a factor
           jitter(as.numeric(x2), factor=jitter),
           col=col, cex=cex, pch=pch)
}
plot.persp <- function(x, grid1, grid2, y.predict, pred.names, ipred1, ipred2,
       trace, ylab, ylim, xflip, yflip, swapxy, ngrid2,
       col.persp, theta, phi, shade,
       main, cex, col.image, cex.lab,
       col.response, cex.response, pch.response, jitter.response, ...)
{
    get.theta <- function()
    {
        get.diag.val <- function(diag1, diag2) # return first non NA along diag
        {
            vals <- y.predict[diag1, diag2]
            (vals[!is.na(vals)])[1] # return first non NA in vals
        }
        theta1 <- theta
        if(is.na(theta)) {      # no user specified theta?
            # rotate graph so highest point is farthest
            # TODO this could be improved
            theta1 <- -35
            nr <- nrow(y.predict)
            nc <- ncol(y.predict)
            imax <- which.max(c(
                    get.diag.val(nr:1, nc:1),
                    get.diag.val(1:nr, nc:1),
                    get.diag.val(1:nr, 1:nc),
                    get.diag.val(nr:1, 1:nc)))
            if(length(imax))   # length>0 unless entire diag is NA
                theta1 <- theta1 + switch(imax, 0, 90, 180, 270)
        }
        theta1                  # theta arg for persp()
    }
    #--- plot.persp starts here ---
    # following needed because persp() rejects a reversed xlim or ylim
    if(xflip)
        warning0("ignoring xflip=TRUE for persp plot")
    if(yflip)
        warning0("ignoring yflip=TRUE for persp plot")
    theta1 <- get.theta()
    cex1 <- par("cex") # TODO persp needs an explicit cex arg, doesn't use par("cex")
    if(trace)
        printf("persp(%s:%s) theta %.3g ylim %.3g %.3g cex %.3g phi %.3g\n",
                pred.names[ipred1], pred.names[ipred2], theta1,
                ylim[1], ylim[2], cex1, phi)
    if(swapxy) {
        temp <- grid1;  grid1  <- grid2;  grid2 <- temp  # swap grid1 and grid2
        temp <- ipred1; ipred1 <- ipred2; ipred2 <- temp # swap ipred1 and ipred2
        y.predict <- t(y.predict)
    }
    # TODO want to use lab=c(2,2,7) or similar here but persp ignores it
    persp(grid1, grid2, y.predict,
          main=main, xlab=pred.names[ipred1], ylab=pred.names[ipred2], zlab=ylab,
          cex.lab=1.1 * cex.lab, cex.axis=cex.lab,
          zlim=ylim, col=col.persp, cex=cex1,
          theta=theta1, phi=phi, shade=shade, ...)
}
plot.contour <- function(x, grid1, grid2, y.predict, pred.names, ipred1, ipred2,
               xflip, yflip, swapxy,
               main, cex, col.image, cex.lab,
               col.response, cex.response, pch.response, jitter.response, ...)
{
    xcol1 <- x[,ipred1]
    xcol2 <- x[,ipred2]
    levnames1 <- levels(xcol1)
    levnames2 <- levels(xcol2)
    is.fac1 <- is.factor(xcol1) && length(levnames1) <= 12
    is.fac2 <- is.factor(xcol2) && length(levnames2) <= 12
    xlab <- if(is.fac1) "" else pred.names[ipred1] # no lab if fac else on top of lev name
    ylab <- if(is.fac2) "" else pred.names[ipred2]
    if(swapxy) {
        temp <- levnames2; levnames2 <- levnames1; levnames1 <- temp
        temp <- is.fac2;   is.fac2 <- is.fac1;     is.fac1 <- temp
        temp <- ylab;      ylab <- xlab;           xlab <- temp
    }
    xrange <- range(grid1)
    x.lim <- if(xflip) c(xrange[2], xrange[1]) else c(xrange[1], xrange[2])
    yrange <- range(grid2)
    y.lim <- if(yflip) c(yrange[2], yrange[1]) else c(yrange[1], yrange[2])

    if(swapxy)
        contour(grid2, grid1, t(y.predict),
            main=main, xlab=xlab, ylab=ylab,
            xlim=y.lim, ylim=x.lim,
            xaxt=if(is.fac1) "n" else "s",
            yaxt=if(is.fac2) "n" else "s",
            cex.lab=1.1 * cex.lab, cex.axis=cex.lab,
            labcex=.8 * cex.lab * par("cex"), ...)
    else
        contour(grid1, grid2, y.predict,
            main=main, xlab=xlab, ylab=ylab,
            xlim=x.lim, ylim=y.lim,
            xaxt=if(is.fac1) "n" else "s",
            yaxt=if(is.fac2) "n" else "s",
            cex.lab=1.1 * cex.lab, cex.axis=cex.lab,
            labcex=.8 * cex.lab * par("cex"), ...)

    if(is.fac1) {
        levnames1 <- abbreviate(levnames1, minlength=6, strict=TRUE)
        mtext(levnames1, side=1, at=1:length(levnames1),
              cex=par("cex") * cex.lab, line=.5, las=get.las(levnames1))
    }
    if(is.fac2)
        mtext(abbreviate(levnames2, minlength=6, strict=TRUE),
              side=2, at=1:length(levnames2),
              cex=par("cex") * cex.lab, line=.5, las=2)

    if(!is.na.or.zero(col.response))
        plot.response.sites(x, ipred1, ipred2, col.response, cex.response,
                            pch.response, jitter.response, swapxy)
}
plot.image <- function(x, grid1, grid2, y.predict, pred.names, ipred1, ipred2,
                       xflip, yflip, swapxy,
                       main, cex, col.image, cex.lab,
                       col.response, cex.response, pch.response, jitter.response, ...)
{
    xcol1 <- x[,ipred1]
    xcol2 <- x[,ipred2]
    levnames1 <- levels(xcol1)
    levnames2 <- levels(xcol2)
    is.fac1 <- is.factor(xcol1) && length(levnames1) <= 12
    is.fac2 <- is.factor(xcol2) && length(levnames2) <= 12
    xlab <- if(is.fac1) "" else pred.names[ipred1] # no lab if fac else on top of lev name
    ylab <- if(is.fac2) "" else pred.names[ipred2]
    if(swapxy) {
        temp <- levnames2; levnames2 <- levnames1; levnames1 <- temp
        temp <- is.fac2;   is.fac2 <- is.fac1;     is.fac1 <- temp
        temp <- ylab;      ylab <- xlab;           xlab <- temp
    }
    plot.response <- !is.na.or.zero(col.response)
    if(!xflip && !yflip && !is.fac1 && !is.fac2) {
        # following for backwards compat (else axis lims slightly different)
        if(swapxy)
            image(grid2, grid1, t(y.predict),
                main=main, xlab=xlab, ylab=ylab,
                xaxt=if(is.fac1) "n" else "s",
                yaxt=if(is.fac2) "n" else "s",
                cex.lab=1.1 * cex.lab, cex.axis=cex.lab,
                col=col.image, ...)
        else {
            image(grid1, grid2, y.predict,
                main=main, xlab=xlab, ylab=ylab,
                xaxt=if(is.fac1) "n" else "s",
                yaxt=if(is.fac2) "n" else "s",
                cex.lab=1.1 * cex.lab, cex.axis=cex.lab,
                col=col.image, ...)
        }
    } else {
        xrange <- range(grid1)
        if(is.fac1) {
            xrange[1] <- xrange[1] - .25
            xrange[2] <- xrange[2] + .25
        } else {
            range <- xrange[2] - xrange[1]
            xrange[1] <- xrange[1] - .02 * range
            xrange[2] <- xrange[2] + .02 * range
        }
        x.lim <- if(xflip) c(xrange[2], xrange[1]) else c(xrange[1], xrange[2])
        yrange <- range(grid2)
        if(is.fac2) {
            yrange[1] <- yrange[1] - .25
            yrange[2] <- yrange[2] + .25
        } else {
            range <- yrange[2] - yrange[1]
            yrange[1] <- yrange[1] - .02 * range
            yrange[2] <- yrange[2] + .02 * range
        }
        y.lim <- if(yflip) c(yrange[2], yrange[1]) else c(yrange[1], yrange[2])
        if(swapxy)
            image(grid2, grid1, t(y.predict),
                main=main, xlab=xlab, ylab=ylab,
                xlim=y.lim, ylim=x.lim,
                xaxt=if(is.fac1) "n" else "s",
                yaxt=if(is.fac2) "n" else "s",
                cex.lab=1.1 * cex.lab, cex.axis=cex.lab,
                col=col.image, ...)
        else
            image(grid1, grid2, y.predict,
                main=main, xlab=xlab, ylab=ylab,
                xlim=x.lim, ylim=y.lim,
                xaxt=if(is.fac1) "n" else "s",
                yaxt=if(is.fac2) "n" else "s",
                cex.lab=1.1 * cex.lab, cex.axis=cex.lab,
                col=col.image,...)
    }
    if(is.fac1) {
        levnames1 <- abbreviate(levnames1, minlength=6, strict=TRUE)
        mtext(levnames1, side=1, at=1:length(levnames1),
              cex=par("cex") * cex.lab, line=.5, las=get.las(levnames1))
    }
    if(is.fac2)
        mtext(abbreviate(levnames2, minlength=6, strict=TRUE),
              side=2, at=1:length(levnames2),
              cex=par("cex") * cex.lab, line=.5, las=2)

    if(plot.response)
        plot.response.sites(x, ipred1, ipred2, col.response, cex.response,
                            pch.response, jitter.response, swapxy)
}
get.main <- function(main, imain, main.func, ...)
{
    if(is.null(main))
        main <- main.func(main, imain, ...)
    else if(length(main) > 1) { # user supplied a vector main?
        if(imain > length(main)) {
            if(imain == length(main)+1) # issue warning only once
                warning0("not enough elements in \"main\" ",
                         "(there are more plots than strings in \"main\")")
            main <- main.func(main, imain, ...) # revert to defaults
        } else
            main <- main[imain]
    }
    main
}
range1 <- function(x, ...)
{
    if(is.factor(x))
        c(1, nlevels(x))
    else
        range(x, ...)
}
get.plotmo.type <- function(obj, type, func.name="plotmo")
{
    if(is.null(type))
        type <- get.default.plotmo.type(obj)
    else {
        stopifnot(is.character(type))
        stopifnot(length(type) == 1)
        if(pmatch(type, "terms", nomatch=0))
            stop0("type=\"terms\" is not allowed by ", func.name)
    }
    type
}
# Check that y is good.  Also if y has multiple columns
# this returns just the column specified by nresponse.

check.and.print.y <- function(y, msg, nresponse, expected.len, trace,
                              subset.=NULL, return.yname=FALSE)
{
    print.matrix.info <- function(xname, x)
    {
        rownames(x) <- NULL
        if(NROW(x) <= 6) { # head prints 6 rows
            cat0("Contents of ", xname, " are\n")
            print(x)
        } else {
            rowstring <-
                if(class(x)[1] == "numeric" || class(x)[1] == "factor")
                     "elements"
                else
                     "rows"
            cat0("First few ", rowstring, " of ", xname, " are\n")
            print(head(x, n=6))
        }
    }
    get.nresponse <- function()
    {
        if(is.null(nresponse) || is.na(nresponse)[1]) {
            if(NCOL(y) > 1) {
                cat("\n")
                print.matrix.info(msg, y)
                cat("\n")
                if(!is.null(colnames.))
                    msg1 <- paste0("       Specify a column index like nresponse=2 or ",
                                   "a column name like nresponse=\"", colnames.[2], "\"")
                else
                    msg1 <- paste0("       Specify a column index like nresponse=2")
                stop0("predicted response has multiple columns (see above) ",
                      "but nresponse is not specified\n", msg1)
            }
            nresponse <- 1
        } else if (is.character(nresponse)) {
            # convert column name to column index
            stopifnot(length(nresponse) == 1)
            if(is.vector(y))
                stop0("nresponse=\"", nresponse,
                      "\" cannot be used because y is a vector (it has no columns)")
            if(is.factor(y))
                stop0("nresponse=\"", nresponse,
                      "\" cannot be used because y is a factor (it has no columns)")
            if(is.null(colnames.))
                stop0("nresponse=\"", nresponse,
                      "\" cannot be used because y has no column names")
            nresponse <- match.choices(nresponse, colnames., "nresponse")
        }
        check.index.vec("nresponse", nresponse, y, check.empty = TRUE,
                        use.as.col.index=TRUE, allow.negative.indices=FALSE,
                        treat.NA.as.one=TRUE)
        nresponse
    }
    #--- check.and.print.y starts here
    if(is.null(y))
        stop0(msg,  " returned NULL")
    if(length(y) == 0)
        stop0(msg, " returned a zero length value (length(y) == 0)")
    colnames. <- NULL
    if(length(dim(y)) > 1) # work around crash in colnames() for 1D arrays
        colnames. <- colnames(y)
    nresponse <- get.nresponse()
    yname <- colnames.[nresponse]
    if(NCOL(y) > 1)
        y <- y[, nresponse, drop=FALSE]
    if(NCOL(y) > 1)
        stop0("\"nresponse\" specifies more than one column")
    # convert data.frame to matrix, needed for test.earth.glm.R test a21 TODO revisit
    if(is.data.frame(y))
        y <- as.matrix(y)
    if(any(!is.double(y))) # convert logical or factor or whatever to double
        y <- as.vector(y, mode="numeric")
    if(NROW(y) == 1 && NCOL(y) > 1)
        y <- as.vector(y[, 1])
    warn.if.not.all.finite(y, "y")
    if(trace) {
        yname1 <- if(is.null(yname)) "" else paste0("\"", yname, "\" ")
        cat0(msg, " returned ", yname1, "length ", length(y))
        if(!is.null(subset.))
            cat(" (before taking subset)")
        try(cat(" min", min(y), "max", max(y)), silent=TRUE)
        cat(" values ")
        for(i in 1:min(10, length(y)))
            cat(y[i], "")
        cat("...\n")
    }
    if(is.null(subset.) && length(y) != expected.len)
        warning0(msg, " returned a response of the wrong length ", length(y),
                 ", expected ", expected.len)
    if(return.yname)
        return(list(y=y, yname=yname))
    y
}
get.subset <- function(object, env, trace)
{
    is.valid <- function(subset.)
    {
        !is.null(subset.) && (is.numeric(subset.) || is.logical(subset.))
    }
    #--- get.subset starts here
    subset. <- object$subset
    if(!is.valid(subset.)) {
        subset. <- try(eval(object$call$subset, env), silent=TRUE)
        if(is.try.error(subset.))
            subset. <- NULL
    }
    if(!is.valid(subset.))
        subset. <- NULL
    else if(trace) {
        cat0("subset length " , length(subset.))
        try(cat(" min", min(subset.), "max", max(subset.)), silent=TRUE)
        cat(" values ")
        for(i in 1:min(10, length(subset.)))
            cat(subset.[i], "")
        cat("...\n")
    }
    subset.
}
apply.inverse.func <- function(y, trace, inverse.func, inverse.func.arg)
{
    if(exists.and.not.null(inverse.func.arg, "function", "inverse.func")) {
        y <- inverse.func(y)
        y <- check.and.print.y(y, paste0("inverse.func=", inverse.func.arg),
                               1, length(y), trace)
    }
    y
}
# Should the factor labels on the x axis be printed horizontally or vertically?

get.las <- function(labels)
{
    if(length(labels) * max(nchar(labels)) <= 20)
        0   # horizontal
    else
        2   # vertical
}
# TRUE if a plot was selected by the user (excluding the default setting)

is.specified <- function(degree)
{
    !is.logical(degree) || length(degree) > 1
}
