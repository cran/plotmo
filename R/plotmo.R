# plotmo.R: plot the model response when varying one or two predictors
#
# Comments containing "TODO" mark known issues.
# Stephen Milborrow Sep 2006 Cape Town
#
# TODO allow NAs, revisit NA code
#        It would be nice to use whatever NA handling the original model function
#        used, but there seems to be no general way of knowing what that is.
# TODO would like to add a newdata argument, but NA handling seems insoluble
# TODO add "add" option so can overlya graphs e.g. with different grid.levels.
# TODO allow degree2 to explicitly specify two varnames, likewise for degree1
# TODO allow partial residual plots and variations
# TODO allow user to specify x range on a per predictor basis
# TODO get.plotmo.x should allow allow unnamed cols in x if get
#        from object$x or object$call$x
# TODO get.plotmo.x and get.plotmo.y both eval model frame, expensive, could share?
# TODO use ngrid1 to limit the number of points plotted for "func" argument
# TODO allow display quantiles for rug, say nrug=-10 means 10% quantiles.
# TODO add ycolumn argument for multiple response models (for earth we currently
#        always use the first column).
# TODO Fix this i.e. if predictor in munged formula is a numeric then delete it:
# dat <- data.frame(x1=1:20, x2 = sample(1:20), y = rnorm(20)+1:20)
# fm <- lm(y ~ x1+x2+sin(x1*x2/10), dat)
# plotmo(fm, se=2)
# Error in terms.formula(formula, data = data) : invalid model formula in ExtractVars

degree1.global <- NULL # cached degree1 data to avoid calling the same predict twice
degree2.global <- NULL
printed.na.warning.global <- FALSE # global variable to print certain warnings only once

#------------------------------------------------------------------------------
# If ylim==NULL we do an initial pass across all plots (both degree1 and degree2)
# without actually plotting to calculate the min and max y limits (actually z limits
# on degree2 plots).  If col.response is set then we also incorporate
# the response min and max.

plotmo <- function(object = stop("no 'object' arg"),
    type        = NULL,
    nresponse   = NA,
    clip        = TRUE,
    ylim        = NULL,
    center      = FALSE,
    ndiscrete   = 5,
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
    npoints         = -1,
    inverse.func    = NULL,
    trace       = FALSE,
    nrug        = 0,
    col.degree1 = 1,
    lty.degree1 = 1,
    lwd.degree1 = 1,
    col.smooth  = 0,
    lty.smooth  = 1,
    lwd.smooth  = 1,
    se          = 0,
    col.shade   = "lightgray",
    col.se      = 0,
    lty.se      = 2,
    func        = NULL,
    col.func    = "lightblue",
    lty.func    = 1,
    lwd.func    = 1,
    ngrid1      = 50,
    grid        = FALSE,
    type2       = "persp",
    ngrid2      = 20,
    col.image   = gray(0:10/10),
    col.persp   = "lightblue",
    theta       = NA,
    phi         = 30,
    dvalue      = 1,
    shade       = 0.5,
    do.par      = TRUE,
    caption     = NULL,
    main        = NULL,
    xlab        = "",
    ylab        = "",
    cex         = NULL,
    cex.lab     = 1,
    xflip       = FALSE,
    yflip       = FALSE,
    swapxy      = FALSE,
    ...)
{
    # call the plotting functions with draw.plot=FALSE to get the ylims
    get.ylim.by.dummy.plots <- function(ylims, trace, ...)
    {
        if(nsingles) # get ylim=c(miny, maxy) by calling with draw.plot=FALSE
            ylims <- plot.degree1(object, degree1, all1, center, ylim, type, nresponse,
                        clip, trace, trace1, col.response, cex.response,
                        pch.response, jitter.response, iresponse,
                        col.smooth, lty.smooth, lwd.smooth,
                        inverse.func, grid.func, grid.levels,
                        ngrid1, grid,
                        col.degree1, lty.degree1, lwd.degree1, se, lty.se, col.se,
                        col.shade, func, col.func, lty.func, lwd.func, nrug,
                        draw.plot=FALSE,
                        x, y, singles, ylims, xlevs, ndiscrete, func.name, pred.names,
                        inverse.func.name, clip.limits, nfigs,
                        main, xlab, ylab, cex, cex.lab, xflip, dots)$ylims

        if(npairs)
            ylims <- plot.degree2(object, degree2, all2, center, ylim, type, nresponse,
                        clip, trace, trace1, col.response, cex.response,
                        pch.response, jitter.response, iresponse,
                        inverse.func, grid.func, grid.levels,
                        type2, ngrid2, col.persp, col.image,
                        draw.plot=FALSE,
                        x, y, pairs, ylims, xlevs, ndiscrete, func.name, pred.names,
                        inverse.func.name, clip.limits, nfigs, nsingles, npairs,
                        main, xflip, yflip, swapxy, xlab, ylab, cex, cex.lab,
                        theta, phi, shade, dvalue, ...)$ylims

        if(!is.zero(col.response))
            ylims <- range(ylims, y, finite=TRUE) # extend ylims to include response range
        if(any(is.na(ylims)) || any(!is.finite(ylims)) ||
                abs(ylims[1] - ylims[2]) < 1e-8) {
            cat("\nylim", ylims, "\n\n")
            if(clip)
                stop0("plotmo cannot calculate ylim, try clip=FALSE")
            else
                stop0("plotmo cannot calculate ylim, try manually specifying ylim")
        }
        # we have called the plot functions, so minimize tracing in further calls to them
        trace1 <<- 0 # note <<- not <-
        ylims
    }
    get.ylims <- function(...) # check ylim arg and calculate min,max limits of y axis
    {
        if(!(is.null(ylim) || is.na(ylim[1]) || length(ylim) == 2))
            stop0("'ylim' must be one of:\n",
                  "  NULL          all graphs have same vertical axes\n",
                  "  NA            each graph has its own vertical axis\n",
                  "  c(ymin,ymax)  ylim for all graphs")
        if(length(ylim) == 2 && ylim[2] <= ylim[1])
            stop0("ylim[2] ", ylim[2], " is not greater than ylim[1] ", ylim[1])
        ylims <- c(NA, NA)
        if(is.null(ylim)) { # automatic ylim, the default
            temp <- get.plotmo.ylim(object, env, type, trace)
            if(!is.na(temp[1]))
                ylims <- temp
            else
                ylims <- get.ylim.by.dummy.plots(ylims, trace)
        }
        if(trace)
            cat("\nylim", ylims, "\n")
        ylims
    }
    get.default.caption <- function(object.name)
    {
        type.string <- response.string <- nresponse.string <- suffix <- ""
        if(type != "response")
            type.string <- paste0(type, "   ")
        if(!is.null(response.name))
            response.string <- paste0(response.name, "   ")
        else if(!is.na(nresponse))
            nresponse.string <- paste0("[", nresponse, "]   ")
        if(!is.null(object$call))
            suffix <- paste(strip.white.space(paste0(deparse(object$call), collapse="")))
        else if(length(object.name) == 1 && nchar(object.name) <= 30) # sanity check
            suffix <- paste(object.name)
        else
            suffix <- "plotmo"
        paste0(type.string, response.string, nresponse.string, suffix)
    }
    #--- plotmo starts here
    type2s <- c("persp", "contour", "image")
    type2 <- type2s[match.choices(type2, type2s, "type2")]
    dots <- match.call(expand.dots=FALSE)$...
    check.dots.args(type2, dots)
    func.name <- deparse(substitute(func))
    inverse.func.name <- deparse(substitute(inverse.func))
    stopifnot.boolean(clip)
    stopifnot.boolean(all1)
    stopifnot.boolean(all2)
    stopifnot.boolean(center)
    stopifnot.boolean(swapxy)
    stopifnot.boolean(xflip)
    stopifnot.boolean(yflip)
    stopifnot(length(do.par) == 1 && (do.par == 0 || do.par == 1 || do.par == 2))
    stopifnot.scalar(jitter.response)
    stopifnot(jitter.response >= 0 && jitter.response <= 5) # 5 is arbitrary
    if(all(is.na(col.response))) # allow NA to mean 0
        col.response <- 0
    if(all(is.na(col.smooth)))
        col.smooth <- 0
    if(all(is.na(col.shade)))
        col.shade <- 0
    if(all(is.na(col.se)))
        col.se <- 0
    if(all(is.na(col.func)))
        col.func <- 0
    if(all(is.na(se)))
        se <- 0
    if(all(is.na(degree1)))
        degree1 <- 0
    if(all(is.na(degree2)))
        degree2 <- 0
    if(is.null(nresponse) || identical(nresponse, 0))
        nreponse <- NA
    stopifnot.integer(trace)
    trace <- as.numeric(trace)
    trace1 <- trace
    stopifnot.integer(ngrid1)
    if(ngrid1 < 2)
        stop0("illegal ngrid1 ", ngrid1)
    if(ngrid2 > 1000) {
        warning0("clipped ngrid1=", ngrid1, " to 1000")
        ngrid1 <- 1000
    }
    stopifnot.integer(ngrid2)
    if(ngrid2 < 2)
        stop0("illegal ngrid2 ", ngrid2)
    if(ngrid2 > 500) {
        warning0("clipped ngrid2=", ngrid2, " to 500")
        ngrid2 <- 500
    }
    stopifnot.integer(ndiscrete)
    temp <- check.se.arg(se, missing(lty.se), lty.se, col.se, col.shade)
        se <- temp$se
        col.se <- temp$col.se
    # clear global flag (used to print certain warnings only once)
    unlockBinding("printed.na.warning.global", asNamespace("plotmo"))
    printed.na.warning.global <<- FALSE        # note <<- not <-
    plotmo.prolog(object, env, deparse(substitute(object)))
    # initialize the variables used to store global data
    unlockBinding("degree1.global", asNamespace("plotmo"))
    unlockBinding("degree2.global", asNamespace("plotmo"))
    degree1.global <<- degree2.global <<- NULL # note <<- not <-
    # Get the environment in which the model function was originally called.
    # If that is not available, use the environment in which plotmo was called.
    .Environment <- attr(object$terms, ".Environment")
    env <- if(is.null(.Environment)) parent.frame() else .Environment
    if(center && clip) {
        clip <- FALSE # otherwise incorrect clipping (TODO revisit)
        warning0("forcing clip=FALSE because center=TRUE ",
                 "(a limitation of the current implementation)")
    }
    x <- get.plotmo.x.wrapper(object, env, trace)
    pred.names <- colnames(x)
    stopif(is.null(pred.names))
    # list, each element is the unique levels for correspondig column of x
    xlevs <- get.xlevs(x, trace)
    if(!is.zero(col.response)) {
        iresponse <- get.iresponse(npoints, x)
        if(is.null(iresponse))
            col.response <- 0
    }
    y <- get.plotmo.y.wrapper(object, env, nresponse, nrow(x), trace)
    y <- apply.inverse.func(y, object, trace, inverse.func, inverse.func.name)
    if(center)
        y <- my.center(y, trace)
    type <- get.plotmo.type.wrapper(object, env, type)
    clip.limits <- c(-Inf, Inf)
    if(clip)
        clip.limits <- get.plotmo.clip.limits.wrapper(object, env, type, y, trace)
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
    if(do.par) {
        old.par <- par(no.readonly=TRUE)
        if(do.par == 1)
            on.exit(par(old.par))
        do.par(nfigs, cex, xlab, ylab)
    }
    else if(!is.null(cex)) {
        old.cex <- par("cex")
        on.exit(par(cex=old.cex))
        par(cex=cex)
    }
    response.name <- NULL # column name when predict returns multiple columns
    if(nsingles) {
        response.name <- plot.degree1(object, degree1, all1, center, ylim, type, nresponse,
                    clip, trace, trace1, col.response, cex.response,
                    pch.response, jitter.response, iresponse,
                    col.smooth, lty.smooth, lwd.smooth,
                    inverse.func, grid.func, grid.levels,
                    ngrid1, grid,
                    col.degree1, lty.degree1, lwd.degree1, se, lty.se, col.se,
                    col.shade, func, col.func, lty.func, lwd.func, nrug,
                    draw.plot=TRUE,
                    x, y, singles, ylims, xlevs, ndiscrete, func.name, pred.names,
                    inverse.func.name, clip.limits, nfigs,
                    main, xlab, ylab, cex, cex.lab, xflip, dots)$response.name
    }
    if(npairs) {
        if(do.par)
            do.par.degree2(type2, dots)
        temp <- plot.degree2(object, degree2, all2, center, ylim, type, nresponse,
                    clip, trace, trace1, col.response, cex.response,
                    pch.response, jitter.response, iresponse,
                    inverse.func, grid.func, grid.levels,
                    type2, ngrid2, col.persp, col.image,
                    draw.plot=TRUE,
                    x, y, pairs, ylims, xlevs, ndiscrete, func.name, pred.names,
                    inverse.func.name, clip.limits, nfigs, nsingles, npairs,
                    main, xflip, yflip, swapxy, xlab, ylab, cex, cex.lab,
                    theta, phi, shade, dvalue, ...)

        if(is.null(response.name))
            response.name <- temp$response.name
    }
    trim.caption <- FALSE
    if(is.null(caption) && do.par) {
        trim.caption <- TRUE
        if(is.null(object$call))
            object.name <- deparse(substitute(object))
        caption <- get.default.caption(object.name)
    }
    if(!is.null(caption))
        show.caption(caption, trim=trim.caption, cex=if(is.null(cex)) 1 else 1.2 * cex)
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
    legal.all <- c("adj", "ann", "ask", "bg", "bty", "cex.main",
                    "cex.sub", "cin", "col.axis", "col.lab", "col.main",
                    "col.sub", "cra", "crt", "csi", "cxy", "family", "fg",
                    "fig", "fin", "font", "font.axis", "font.lab", "font.main",
                    "font.sub", "lab", "lend", "lheight", "ljoin", "lmitre",
                    "mai", "mar", "mex", "mfg", "mgp", "mkh",
                    "sub", "tck", "tcl", "usr", "xaxs", "xlog", "xpd", "yaxp",
                    "yaxs", "yaxt", "ylog")
    persp.args <- c("phi", "r", "scale", "expand", "ltheta", "lphi",
                    "border", "shade", "box", "axes", "ticktype", "nticks")
    contour.args <- c("nlevels", "levels", "labels", "labcex", "drawlabels",
                      "method", "vfont,", "axes", "frame.plot")
    image.args <- c("breaks", "oldstyle")
    if(type2 == "persp") {
        check.illegal("zlab", # a common mistake
                      "\"zlab\" is illegal, use \"ylab\" instead")
        legal.dots.args <- c(persp.args, legal.all)
    } else if(type2 == "contour")
        legal.dots.args <- c(contour.args, legal.all)
    else if(type2 == "image")
        legal.dots.args <- c(image.args, legal.all)
    else
        stop0("type2=\"", type2, "\" is illegal")
    names <- names(dots)
    pmatch <- pmatch(names, legal.dots.args, nomatch=FALSE, duplicates.ok=TRUE)
    if(any(!pmatch)) {
        # report the first illegal arg
        ibad <- (1:length(dots))[!pmatch]
        would.be.legal.args <-
            if(type2 == "persp")
                c(contour.args, image.args)
            else if(type2 == "contour")
                c(persp.args, image.args)
            else if(type2 == "image")
                c(persp.args, image.args)
        if(any(pmatch(names, would.be.legal.args, nomatch=FALSE)))
            stop0("the ", names[ibad][1], " argument is illegal for type2=\"", type2, "\"")
        else
            stop0("illegal argument \"", names[ibad][1], "\"")
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
    if(is.logical(se)) {
        if(se) {
            # allow user to use se=TRUE for compat with termplot
            warning0("plotmo: converted se=TRUE to se=2")
            se <- 2
        } else
            se <- 0 # silently treat FALSE or NA as 0
    }
    stopifnot.scalar(se)
    if(se < 0 || se > 9) # 9 is arbitrary
        stop0("se=", se, " is out of range")
    if(!is.missing.lty.se && lty.se != 0 && col.se[1] == 0)
        col.se <- 1  # needed if user sets just lty.se but doesn't set col.se
    if(se && (col.se == 0 || lty.se == 0) && col.shade == 0)
        warning0("plotmo: 'se' ignored because (col.se == 0 || lty.se == 0) && ",
                 "col.shade == 0)")
    list(se=se, col.se=col.se)
}
do.par <- function(nfigs, cex, xlab, ylab)
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
    # make space for caption
    oma <- par("oma")
    if(oma[3] < 3) {
        oma[3] <- 3
        par(oma=oma)
    }
}
do.par.degree2 <- function(type2, dots) # adjust par settings for degree2 plots
{
    if(type2 != "persp") {    # contour or image plot?
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
}
get.plotmo.clip.limits.wrapper <- function(object, env, type, y, trace)
{
    clip.limits <- get.plotmo.clip.limits(object, env, type, y, trace)
    if(is.na(clip.limits[1]))
        clip.limits <- range(y, finite=TRUE)
    if(trace)
        cat("\nclip.limits", clip.limits, "\n")
    clip.limits # a two elem vec
}
get.plotmo.singles.wrapper <- function(object, env, x, trace, degree1, all1)
{
    if(!is.numeric(degree1) && !is.logical(degree1)) {
        if(pmatch(degree1, "all", 0)) # backwards compatibility message
            stop0("degree1=\"", degree1, "\" is no longer legal, use all1=TRUE instead")
        stop0("degree1 must be an index vector (numeric or logical)")
    }
    if(trace >= 2)
        cat("\n--get.plotmo.singles for", class(object)[1], "object\n\n")
    singles <- get.plotmo.singles(object, env, x, trace >=2 , all1)
    if(length(singles))
        singles <- sort(unique(singles))
    nsingles <- length(singles)
    if(nsingles) {
        check.index.vec("degree1", degree1, singles)
        singles <- singles[degree1]
    } else if(is.specified(degree1) && degree1[1] != 0)
        warning0("\"degree1\" specified but no degree1 plots")
    if(trace >= 2) {
        if(nsingles)
            cat("singles:", paste(singles, colnames(x)[singles], collapse=", "), "\n")
        else
            cat("no singles\n")
    }
    singles # a vector of indices of predictors for degree1 plots
}
get.plotmo.pairs.wrapper <- function(object, env, x, trace, all2, degree2)
{
    if(!is.numeric(degree2) && !is.logical(degree2)) {
        if(pmatch(degree2, "all", 0)) # backwards compatibility message
            stop0("degree2=\"", degree2, "\" is no longer legal, use all2=TRUE instead")
        stop0("degree2 must be an index vector (numeric or logical)")
    }
    if(trace >= 2)
        cat("\n--get.plotmo.pairs for", class(object)[1], "object\n\n")
    pairs <- get.plotmo.pairs(object, env, x, trace >= 2, all2)
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
    if(trace >= 2) {
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
    if(colnames(x)[1] == "(Intercept)") # seen with lm(y~x1+x2, x=TRUE, y=TRUE)
        x <- x[, -1, drop=FALSE]
    subset <- get.and.check.subset(x, object, env, trace)
    if(!is.null(subset))
        x <- x[subset, , drop=FALSE]
    if(trace) {
        cat("\n")
        print.first.few.rows(x, trace, "x",
            if(!is.null(subset)) " (after taking subset)" else "")
    }
    x
}
# return a list, each element is the unique levels for correspondig column of x
get.xlevs <- function(x, trace)
{
    NA.numeric <- as.numeric(NA)

    xlevs <- list(colnames(x))
    for(i in 1:ncol(x))
        xlevs[[i]] <- if(is.factor(x[,i])) levels(x[,i]) else sort(unique(x[,i]))
    if(trace)
        cat("nlevels:",
            strip.white.space(paste(colnames(x), "=", sapply(xlevs, length))), "\n")
    xlevs
}
get.plotmo.y.wrapper <- function(object, env, y.column, expected.len, trace)
{
    y <- get.plotmo.y(object, env, y.column, expected.len, trace)
    if(is.list(y) && !is.null(y$y.column)) {
        # TODO revisit this special handling for earth, move into get.plotmo.y.earth
        y.column <- y$y.column
        if(is.na(y.column)[1])
            y.column <- 1
        if(!is.null(y$y))
            y <- y$y
    } else
        y.column <- 1
    subset <- get.and.check.subset(y, object, env, trace)
    y <- check.and.print.y(y, "get.plotmo.y", nresponse=y.column,
                           object, expected.len, trace,
                           will.take.subset=!is.null(subset))
    if(!is.null(subset)) {
        y <- y[subset]
        if(length(y) != expected.len)
            error.bad.ylen(y, "get.plotmo.y", object, expected.len)
    }
    y
}
plotmo.predict.wrapper <- function(object, newdata, type, se.fit,
                                   trace, pred.names, ipred1, ipred2=0)
{
    stopifnot(is.character(type) && length(type) == 1)
    if(trace) {
        if(ipred2 == 0)
            cat0("\nplotmo.predict(type=\"", type, "\") for degree1 plot \"",
                pred.names[ipred1], "\" ")
        else
            cat0("\nplotmo.predict(type=\"", type, "\") for degree2 plot \"",
                 pred.names[ipred1], ":", pred.names[ipred2], "\" ")
        if(se.fit)
            cat0("se.fit=", se.fit, " ")
        print.first.few.rows(newdata, trace, "with newdata")
    }
    plotmo.predict(object, newdata, type, se.fit, trace >= 2)
}
# plot degree one plots i.e. main effects

plot.degree1 <- function(
    # copy of args from plotmo, some have been tweaked slightly
    object, degree1, all1, center, ylim, type, nresponse, clip, trace, trace1,
    col.response, cex.response, pch.response, jitter.response, iresponse,
    col.smooth, lty.smooth, lwd.smooth,
    inverse.func, grid.func, grid.levels,
    ngrid1, grid,
    col.degree1, lty.degree1, lwd.degree1, se, lty.se, col.se, col.shade,
    func, col.func, lty.func, lwd.func, nrug,
    # args generated in plotmo, draw.plot=FALSE means get ylims but don't actually plot
    draw.plot,
    x, y, singles, ylims, xlevs, ndiscrete, func.name, pred.names,
    inverse.func.name, clip.limits, nfigs,

    # copy of par args from plotmo
    main, xlab, ylab, cex, cex.lab, xflip, dots)
{
    get.degree1.data <- function(isingle)
    {
        if(!is.null(degree1.global[[isingle]]))
            return(degree1.global[[isingle]]) # use cached values

        # create data.frame of x values to be plotted, by updating xgrid for this predictor
        xframe <- get.degree1.xframe(xgrid, x, ipred, ngrid1, xlevs)

        y.predict <- plotmo.predict.wrapper(object, xframe, type, se.fit=FALSE,
                                            trace1, pred.names, ipred)
        temp <- check.and.print.y(y.predict,
                                  paste0("predict.", class(object)[1],
                                         "(xgrid, type=\"", type, "\")"),
                                  nresponse, object, nrow(xframe), trace1,
                                  return.yname=TRUE)
            y.predict     <- temp$y
            response.name <- temp$yname

        temp <- get.degree1.se(y.predict, se, object, type, xframe, pred.names,
                               ipred, trace1, inverse.func, inverse.func.name)
            y.se.lower <- temp$y.se.lower
            y.se.upper <- temp$y.se.upper

        y.predict <- apply.inverse.func(y.predict, object,
                                        trace1, inverse.func, inverse.func.name)

        temp <- blockify.degree1.frame(xframe, y.predict, y.se.lower, y.se.upper,
                                       ipred, xlevs, ndiscrete)
            xframe     <- temp$xframe
            y.predict  <- temp$y.predict
            y.se.lower <- temp$y.se.lower
            y.se.upper <- temp$y.se.upper

        if(center) {
            y.predict  <- my.center(y.predict, trace1)
            y.se.lower <- my.center(temp$y.se.lower, trace1)
            y.se.upper <- my.center(temp$y.se.upper, trace1)
        }
        clipped.y <- y.predict
        if(clip) {
            y.lt <- clipped.y < clip.limits[1]
            y.gt <- clipped.y > clip.limits[2]
            if(all(y.lt) || all(y.gt))
                out.of.range.preds <- c(out.of.range.preds, ipred)
            else
                clipped.y[y.lt | y.gt] <- NA
        }
        ylims <- range1(ylims, clipped.y, y.se.lower, y.se.upper, finite=TRUE)
        degree1.data <- list(xframe=xframe, y.predict=y.predict,
                             y.se.lower=y.se.lower, y.se.upper=y.se.upper,
                             response.name=response.name, ylims=ylims,
                             out.of.range.preds=out.of.range.preds)
        # cache the data for next time
        if(!draw.plot) { # only if there is going to be a next time
            if(is.null(degree1.global)) {
                # init degree1.global list,  note <<- not <- below
                stopifnot(isingle == 1)
                degree1.global <<- vector("list", length(singles))
            }
            degree1.global[[isingle]] <<- degree1.data
        }
        degree1.data
    }
    draw.plot.degree1 <- function()
    {
        # get title if any for the current plot
        get.main1 <- function(main, isingle, nfigs, degree1, all1, pred.names, ipred)
        {
            main <- ""
            if(nfigs > 1 && !is.specified(degree1))
                main <- paste0(isingle, " ") # show plot number in headers
            paste(main, pred.names[ipred])
        }
        get.degree1.xlim <- function()
        {
            xcol <- xframe[,ipred]
            nlevels <- length(xlevs[[ipred]])
            if(is.factor(xcol))
                xlim <- c(.6, nlevels+.4)
            else {
                xlim <- c(min(xcol), max(xcol))
                xrange <- xlim[2] - xlim[1]
                xlim[1] <- xlim[1] - .07 * xjitter * xrange
                xlim[2] <- xlim[2] + .07 * xjitter * xrange
            }
            if(xflip) {
                temp <- xlim[1]; xlim[1] <- xlim[2]; xlim[2] <- temp
            }
            xlim
        }
        get.degree1.ylim <- function()
        {
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
            # possibly add extra space for the rug and jittered response points
            preadjusted.ylim <<- ylim # to determine if we need to add gray horiz lines
            if((nrug || yjitter) &&
                    (is.null(ylim.org) || is.na(ylim.org[1])) && # not user specified?
                    (center || length(unique.y) > 3)) { # see draw.degree1.response
                yrange <- ylim[2] - ylim[1]
                # expansion factor here is just approximately correct
                ylim[1] <- ylim[1] - .2 * yjitter * yrange
                ylim[2] <- ylim[2] + .2 * yjitter * yrange
                if (nrug)
                    ylim[1] <- ylim[1] - .05 * yrange
            }
            ylim
        }
        draw.degree1.response <- function(x, iresponse, ipred,
                                          col.response, cex.response, pch.response)
        {
            length.y <- length(y)
            jitted.y <- jitter(y[iresponse], factor=yjitter)
            if(!center && length(unique.y) <= 3) { # binary or ternary response?
                # shift so jitted points in vertical range 0 to 1
                min <- min(jitted.y)
                max <- max(jitted.y)
                jitted.y <-
                    ifelse(y[iresponse] < .333, jitted.y[iresponse] - min,      # bottom points
                    ifelse(y[iresponse] < .667, jitted.y[iresponse],            # middle points
                                                jitted.y[iresponse] + 1 - max)) # top points
            }
            points(jitter(as.numeric(x[iresponse, ipred]), factor=xjitter),
                   jitted.y,
                   col=rep(col.response, length.out=length.y)[iresponse], # recycle
                   cex=rep(cex.response, length.out=length.y)[iresponse],
                   pch=rep(pch.response, length.out=length.y)[iresponse])
        }
        draw.smooth <- function(x, ipred, y, unique.y, col.smooth, lty.smooth, lwd.smooth)
        {
            xcol <- x[,ipred]
            is.discrete <- FALSE
            if(is.factor(xcol)) {
                is.discrete <- TRUE
                levels <- sort(unique(as.numeric(xcol)))
                nlevels <- nlevels(xcol)
            } else if(length(xlevs[[ipred]]) <= ndiscrete) {
                is.discrete <- TRUE
                levels <- xlevs[[ipred]]
                nlevels <- length(levels)
            }
            if(is.discrete) {
                # xcol has discrete levels, display the mean y at each value of xcol
                smooth <- sapply(split(y, xcol), mean)
                lines(levels, if(center) my.center(smooth) else smooth,
                      type="b", col=col.smooth, lty=lty.smooth, lwd=lwd.smooth, pch=20)
            } else {
                # For less smoothing (so we can better judge inflection points),
                # we use a value for f lower than the default 2/3. Also iter=0 is
                # best for lowess with binary responses, so says Harrell 2.4.6.
                smooth <- lowess(xcol, y, f=.5, iter=if(length(unique.y) > 2) 3 else 0)
                lines(smooth$x, if(center) my.center(smooth$y) else smooth$y,
                      type="l", col=col.smooth, lty=lty.smooth, lwd=lwd.smooth)
            }
        }
        draw.degree1.fac <- function()
        {
            draw.se.fac <- function(y) # draw std err bands for a factor predictor
            {
                y.se.lower1 <- split(y.se.lower, xframe[,ipred])
                y.se.upper1 <- split(y.se.upper, xframe[,ipred])
                for(ilev in seq_along(levels(xframe[,ipred]))) {
                    min <- min(y.se.lower1[[ilev]])
                    max <- max(y.se.upper1[[ilev]])
                    if(!is.zero(col.shade))
                        polygon(c(ilev - .4, ilev - .4, ilev + .4, ilev + .4),
                            c(min, max, max, min), col=col.shade, lty=0, border=NA)
                    if(lty.se != 0 && !is.zero(col.se)) {
                        segments(ilev -.4, min, ilev + .4, min, lty=lty.se, col=col.se)
                        segments(ilev -.4, max, ilev + .4, max, lty=lty.se, col=col.se)
                    }
                }
            }
            #--- draw.degree1.fac starts here
            if(!identical(grid, FALSE))
                grid(col=grid, lty=1, nx=NA, ny=NULL) # horizontal grid
            if(!is.null(y.se.lower))
                draw.se.fac()
            if(!is.zero(col.response))
                draw.degree1.response(x, iresponse, ipred,
                                      col.response, cex.response, pch.response)
            if(!is.zero(col.smooth))
                draw.smooth(x, ipred, y, unique.y,
                            col.smooth, lty.smooth, lwd.smooth)
            plot(xframe[,ipred], y.predict, add=TRUE,
                 xaxt=if(plot.levnames) "n" else "s",
                 # yaxt=if(center) "n" else "s",
                 cex.lab=1.1 * cex.lab, cex.axis=cex.lab,
                 col.axis=eval(dots$col.axis), col.lab=eval(dots$col.lab),
                 font=eval(dots$font), font.axis=eval(dots$font.axis),
                 lab=eval(dots$lab), mgp=eval(dots$mgp)) # TODO ignored?

            if(plot.levnames) { # plot level names along the x axis
                levnames <- abbreviate(levnames, minlength=6, strict=TRUE)
                mtext(levnames, side=1, at=1:length(levnames),
                      cex=par("cex") * cex.lab, line=.5, las=get.las(levnames),
                      col.axis=eval(dots$col.axis), col.lab=eval(dots$col.lab),
                      font.axis=eval(dots$font.axis))
            }
            if(nrug)
                rug(jitter(as.numeric(x[irug, ipred]), factor=max(1, xjitter)), quiet=TRUE)
        }
        draw.degree1.numeric <- function()
        {
            draw.se.numeric <- function() # draw std err bars for a numeric predictor
            {
                if(!is.zero(col.shade))
                    polygon(c(xframe[,ipred], rev(xframe[,ipred])),
                            c(y.se.lower, rev(y.se.upper)),
                            col=col.shade, lty=0, border=NA)
                if(lty.se != 0 && !is.zero(col.se)) {
                    lines(xframe[,ipred], y.se.lower, lty=lty.se, col=col.se)
                    lines(xframe[,ipred], y.se.upper, lty=lty.se, col=col.se)
                }
            }
            #--- draw.degree1.numeric starts here
            if(!identical(grid, FALSE))
                grid(col=grid, lty=1)
            if(!is.null(y.se.lower))
                draw.se.numeric()
            draw.degree1.func(func, func.name, # draw the func arg, if specified
                              object, xframe, ipred, center, trace,
                              col.func, lty.func, lwd.func)
            if(!is.zero(col.response))
                draw.degree1.response(x, iresponse, ipred,
                                      col.response, cex.response, pch.response)
            if(!is.zero(col.smooth))
                draw.smooth(x, ipred, y, unique.y,
                            col.smooth, lty.smooth, lwd.smooth)
            lines(x=xframe[,ipred], y=y.predict,
                  col=col.degree1, lty=lty.degree1, lwd=lwd.degree1)
            if(nrug)
                rug(jitter(x[irug, ipred], factor=max(1, xjitter)), quiet=TRUE)
        }
        #--- draw.plot.degree1 starts here
        is.fac.x <- is.factor(x[,ipred])
        xjitter <- yjitter <- jitter.response
        if(!is.zero(col.response)) {
            if(is.fac.x && length(unique.y) <= 3)
                xjitter <- max(.8, 2 * xjitter) # big default jitter, discrete x and y
            else if(is.fac.x || (length(xlevs[[ipred]]) <= ndiscrete))
                xjitter <- max(.4, 2 * xjitter) # not so big default jitter, discrete x
            if(length(unique.y) <= ndiscrete)
                yjitter <- max(.3, yjitter)
        }
        preadjusted.ylim <- NULL
        ylim <- get.degree1.ylim()
        xlim <- get.degree1.xlim()
        if(is.null(xlab))
            xlab <- pred.names[ipred]
        main <- get.main(main, isingle, get.main1,
                         nfigs, degree1, all1, pred.names, ipred)
        levnames <- levels(xframe[,ipred])
        plot.levnames <- is.fac.x && length(levnames) <= 12
        plot(xframe[,ipred], y.predict, type="n",
             main=main, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim,
             xaxt=if(plot.levnames) "n" else "s",
             # yaxt=if(center) "n" else "s",
             cex.lab=1.1 * cex.lab, cex.axis=cex.lab,
             col.axis=eval(dots$col.axis), col.lab=eval(dots$col.lab),
             font=eval(dots$font), font.axis=eval(dots$font.axis),
             lab=eval(dots$lab), mgp=eval(dots$mgp))

        if(identical(grid, FALSE) || is.na(grid)) { # no grid?
            if(is.na(grid)) # be lenient, allow NA
                grid <- FALSE
            if(center)
                abline(h=0, col="gray", lwd=.6)
            # add horizontal gray lines if ylim was 0,1 but is no longer 0,1
            # because it was expanded for space for jittered points
            # TODO do this any time y is discrete with just a few levels?
            if(yjitter && all(preadjusted.ylim == c(0,1))) {
                abline(h=preadjusted.ylim[1], col="gray", lwd=.6)
                abline(h=preadjusted.ylim[2], col="gray", lwd=.6)
            }
        } else {        # want grid
            stopifnot(is.numeric(grid) || is.logical(grid) || is.character(grid))
            if(is.logical(grid))    # grid=TRUE?
                grid <- "lightgray" # default color
        }
        if(is.fac.x)    # x[,ipred] is a factor?
            draw.degree1.fac()
        else
            draw.degree1.numeric()
    }
    #--- plot.degree1 starts here
    if(trace1 || trace >= 2)
        cat0("\n--plot.degree1(draw.plot=", draw.plot, ")\n")
    ylim.org <- ylim
    # get the x matrix we will plot, will be updated later for each predictor one by one
    xgrid <- get.degree1.xgrid(x, grid.func, grid.levels, pred.names, ngrid1)
    if(draw.plot)
        print.grid.values(xgrid)
    irug <- get.degree1.irug(nrug, x, draw.plot) # get the indices of the rug points, if any
    unique.y <- unique(y)
    must.return.ylims <- is.null(degree1.global)
    out.of.range.preds <- NULL # used solely for warning messages
    response.name <- NULL
    for(isingle in seq_along(singles)) {
        if(isingle > 1 && trace == 1)  # trace only the first graph if trace=1
            trace1 <- 0
        ipred <- singles[isingle] # ipred is the predictor index i.e. col in model mat
        # following happens with lm if you do e.g. ozone1$doy <- NULL after using ozone1
        # TODO I am not sure if this is enough to always catch such errors
        if(ipred > NCOL(x))
            stop0("bad index (missing column in x?)")

        temp <- get.degree1.data(isingle)
            xframe             <- temp$xframe
            y.predict          <- temp$y.predict
            y.se.lower         <- temp$y.se.lower
            y.se.upper         <- temp$y.se.upper
            response.name      <- temp$response.name
            out.of.range.preds <- temp$out.of.range.preds
            if(must.return.ylims)
                ylims <- temp$ylims

        if(draw.plot)
            draw.plot.degree1()
    }
    if(clip)
        possibly.issue.degree1.out.of.range.warning(out.of.range.preds, pred.names,
                                                    clip.limits, draw.plot)
    list(ylims=ylims, response.name=response.name)
}
get.degree1.irug <- function(nrug, x, draw.plot) # get indices of xrows to go into rug
{
    stopifnot.integer(nrug, logical.acceptable=TRUE)
    if(!draw.plot || nrug == 0)
        return(NULL)
    if(nrug == 1) # kicks in if user has nrug=TRUE
        nrug <- -1
    if(nrug < 0 || nrug > nrow(x))
        nrug <- nrow(x)
    if(nrug == nrow(x))
        1:nrow(x)
    else
        sample(1:nrow(x), size=nrug, replace=FALSE)
}
get.iresponse <- function(npoints, x) # get indices of xrows
{
    stopifnot.integer(npoints)
    if(npoints == 0)
        return(NULL)
    if(npoints == 1)
        npoints <- -1
    if(npoints < 0 || npoints > nrow(x))
        npoints <- nrow(x)
    if(npoints == nrow(x))
        1:nrow(x)
    else
        sample(1:nrow(x), size=npoints, replace=FALSE)
}
get.degree1.se <- function(y.predict, se, object, type, xframe, pred.names,
                           ipred, trace1, inverse.func, inverse.func.name)
{
    y.se.lower <- NULL
    y.se.upper <- NULL
    if(!is.zero(se)) {
        if(trace1)
            cat("begin se handling, ")
        temp <- plotmo.predict.wrapper(object, xframe, type, se.fit=TRUE,
                                       trace1, pred.names, ipred)

        if(typeof(temp) == "list" && !is.null(temp$se.fit)) {
            temp$se.fit <- check.and.print.y(temp$se.fit,
                                paste0("predict.", class(object)[1],
                                       "(xgrid, type=\"", type, "\", se=...)"),
                                nresponse=1, object, nrow(xframe), trace1)
            y.se.lower <- y.predict - se * temp$se.fit
            y.se.lower <- apply.inverse.func(y.se.lower, object, trace1,
                                             inverse.func, inverse.func.name)
            y.se.upper <- y.predict + se * temp$se.fit
            y.se.upper <- apply.inverse.func(y.se.upper, object, trace1,
                                             inverse.func, inverse.func.name)
        } else if(trace1)
            cat("no standard errs because is.null(temp$se.fit)\n")

        if(trace1)
            cat("end se handling\n")
    }
    list(y.se.lower=y.se.lower, y.se.upper=y.se.upper)
}
draw.degree1.func <- function(func, func.name, object,
                              xframe, ipred, center, trace, col.func, lty.func, lwd.func)
{
    if(exists.and.not.null(func.name, "function", "func")) {
        if(trace) {
            cat("\nApplying \"func\" arg to\n")
            print(head(xframe, if(trace >= 3) 1e5 else 3))
        }
        y.func <- func(xframe)
        y.func <- check.and.print.y(y.func, paste0("func=", func.name),
                                    nresponse=1, object, nrow(xframe), trace)
        if(center)
            y.func <- my.center(y.func, trace)

        lines(xframe[,ipred], type="l", y.func, col=col.func, lty=lty.func, lwd=lwd.func)
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
    object, degree2, all2, center, ylim, type, nresponse,
    clip, trace, trace1, col.response, cex.response,
    pch.response, jitter.response, iresponse,
    inverse.func, grid.func, grid.levels,
    type2, ngrid2, col.persp, col.image,

    # args generated in plotmo, draw.plot=FALSE means get ylims but don't actually plot
    draw.plot,
    x, y, pairs, ylims, xlevs, ndiscrete, func.name, pred.names,
    inverse.func.name, clip.limits, nfigs, nsingles, npairs,

    # copy of args from plotmo
    main, xflip, yflip, swapxy, xlab, ylab, cex, cex.lab,
    theta, phi, shade, dvalue, ...)
{
    get.degree2.data <- function(ipair)
    {
        if(!is.null(degree2.global[[ipair]]))
            return(degree2.global[[ipair]]) # use cached values

        # create data.frame of x values to be plotted, by updating xgrid for this pair
        temp <- get.degree2.xframe(xgrid, x, ipred1, ipred2, ngrid2, xranges, xlevs)
            xframe <- temp$xframe
            grid1  <- temp$grid1
            grid2  <- temp$grid2

        y.predict <- plotmo.predict.wrapper(object, xframe, type, se.fit=FALSE,
                                            trace1, pred.names, ipred1, ipred2)

        temp <- check.and.print.y(y.predict,
                                  paste0("predict.", class(object)[1],
                                         "(xgrid, type=\"", type, "\")"),
                                  nresponse, object, nrow(xframe), trace1,
                                  return.yname=TRUE)
            y.predict     <- temp$y
            response.name <- temp$yname

        y.predict <- apply.inverse.func(y.predict, object, trace1,
                                        inverse.func, inverse.func.name)

        if(type2 != "image") {
            temp <- blockify.degree2.frame(x, y.predict, grid1, grid2,
                                           ipred1, ipred2, xlevs, ndiscrete)
                y.predict <- temp$y.predict
                grid1     <- temp$grid1
                grid2     <- temp$grid2
        }
        if(center)
            y.predict <- my.center(y.predict, trace)
        if(clip) {
            y.predict[y.predict < clip.limits[1] | y.predict > clip.limits[2]] <- NA
            if(all(is.na(y.predict)))
                stop0("all predicted values are out of the range ",
                      "of the original response, try clip=FALSE\n")
        }
        y.predict <- matrix(y.predict, nrow=length(grid1), ncol=length(grid2))
        ylims <- range1(ylims, y.predict, finite=TRUE)
        degree2.data <- list(xframe=xframe, grid1=grid1, grid2=grid2,
                             y.predict=y.predict, ylims=ylims)
        # cache the data for next time
        if(!draw.plot) { # only if there is going to be a next time
            if(is.null(degree2.global)) {
                # init degree2.global list,  note <<- not <- below
                stopifnot(ipair == 1)
                degree2.global <<- vector("list", npairs)
            }
            degree2.global[[ipair]] <<- degree2.data
        }
        degree2.data
    }
    draw.plot.degree2 <- function(type2 = c("persp", "contour", "image"), ...)
    {
        # get title if any for the current plot
        get.main2 <- function(main, imain, nfigs, degree2, all2, ipair,
                              pred.names, ipred1, ipred2)
        {
            main <- ""
            if(nfigs > 1 && !is.specified(degree2))
                main <- paste0(ipair, " ") # show plot number in headers
            if(swapxy)
                paste0(main, pred.names[ipred2], ": ", pred.names[ipred1])
            else
                paste0(main, pred.names[ipred1], ": ", pred.names[ipred2])
        }
        #--- draw.plot.degree2 starts here
        main <- get.main(main, nsingles+ipair, get.main2,
                         nfigs, degree2, all2, ipair, pred.names,
                         ipred1, ipred2)
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
        switch(type2,
            persp=plot.persp(x, grid1, grid2, y.predict, pred.names, ipred1, ipred2,
                             trace, ylab, ylim, xflip, yflip, swapxy, ngrid2,
                             col.persp, theta, phi, shade, dvalue, main, cex.lab, ...),
            contour=plot.contour(x, grid1, grid2, y.predict, pred.names, ipred1, ipred2,
                                 xflip, yflip, swapxy, main, cex.lab,
                                 col.response, cex.response, pch.response,
                                 jitter.response, xlevs, ndiscrete, iresponse, ...),
            image=plot.image(x, grid1, grid2, y.predict, pred.names, ipred1, ipred2,
                             xflip, yflip, swapxy,
                             main, col.image, cex.lab,
                             col.response, cex.response, pch.response,
                             jitter.response, xlevs, ndiscrete, iresponse, ...))
    }
    #--- plot.degree2 starts here
    if(trace1 || trace >= 2)
        cat0("\n--plot.degree2(draw.plot=", draw.plot, ")\n")
    if(!trace1 && trace >= 2)
        cat("\n")
    stopifnot(npairs > 0)
    must.return.ylims <- is.null(degree2.global)
    xranges <- sapply(x, range1, na.rm=TRUE)
    # get the x matrix we will plot, will be updated later for each pair of predictors
    xgrid <- get.degree2.xgrid(x, grid.func, grid.levels, pred.names, ngrid2)
    response.name <- NULL
    for(ipair in 1:npairs) {
        ipred1 <- pairs[ipair,1]  # index of first predictor
        ipred2 <- pairs[ipair,2]  # index of second predictor
        if(ipair > 1 && trace1 == 1)    # trace only the first graph if trace=1
            trace1 <- 0

        temp <- get.degree2.data(ipair)
            xframe    <- temp$xframe
            grid1     <- temp$grid1
            grid2     <- temp$grid2
            y.predict <- temp$y.predict
            if(must.return.ylims)
                ylims <- temp$ylims

        if(draw.plot)
            draw.plot.degree2(type2, ...)
    }
    if(trace >= 2 && type2=="persp")
        cat("\n")
    list(ylims=ylims, response.name=response.name)
}
plot.response.sites <- function(x, ipred1, ipred2, col, cex, pch,
                                jitter, xlevs, ndiscrete, iresponse, swapxy)
{
    if(swapxy) {
        x1 <- x[,ipred2]
        x2 <- x[,ipred1]
    } else {
        x1 <- x[,ipred1]
        x2 <- x[,ipred2]
    }
    if(is.factor(x1) || length(xlevs[[ipred1]]) <= ndiscrete)
        xjitter <- max(.8, 2 * jitter) # lots of space, so use it
    else
        xjitter <- jitter
    if(is.factor(x2) || length(xlevs[[ipred2]]) <= ndiscrete)
        yjitter <- max(.8, 2 * jitter)
    else
        yjitter <- jitter

    points(jitter(as.numeric(x1[iresponse]), factor=xjitter),
           jitter(as.numeric(x2[iresponse]), factor=yjitter),
           col=rep(col, length.out=nrow(x))[iresponse],
           cex=rep(cex, length.out=nrow(x))[iresponse],
           pch=rep(pch, length.out=nrow(x))[iresponse])
}
plot.persp <- function(x, grid1, grid2, y.predict, pred.names, ipred1, ipred2,
                       trace, ylab, ylim, xflip, yflip, swapxy, ngrid2,
                       col.persp, theta, phi, shade, dvalue, main, cex.lab, ...)
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
    #--- plot.persp starts here
    # following needed because persp() rejects a reversed xlim or ylim
    if(xflip)
        warning0("ignoring xflip=TRUE for persp plot")
    if(yflip)
        warning0("ignoring yflip=TRUE for persp plot")
    theta1 <- get.theta()
    cex1 <- par("cex") # persp needs an explicit cex arg, doesn't use par("cex")
    if(trace)
        printf("persp(%s:%s) theta %.3g ylim %.3g %.3g cex %.3g\n",
                pred.names[ipred1], pred.names[ipred2], theta1,
                ylim[1], ylim[2], cex1)
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
          theta=theta1, phi=phi, shade=shade, d=dvalue, ...)
}
plot.contour <- function(x, grid1, grid2, y.predict, pred.names, ipred1, ipred2,
                         xflip, yflip, swapxy, main, cex.lab,
                         col.response, cex.response, pch.response,
                         jitter.response, xlevs, ndiscrete, iresponse, ...)
{
    get.lim <- function(xflip, grid1, is.fac1, is.discrete)
    {
        xrange <- range(grid1)
        if(!is.zero(col.response) && jitter.response > 0) {
            # extra space for jittered points
            if(is.fac1) {
                xrange[1] <- xrange[1] - .5 * jitter.response
                xrange[2] <- xrange[2] + .5 * jitter.response
            } else {
                xrange[1] <- xrange[1] - .02 * jitter.response * (xrange[2]-xrange[1])
                xrange[2] <- xrange[2] + .02 * jitter.response * (xrange[2]-xrange[1])
            }
        }
        if(xflip)
            c(xrange[2], xrange[1])
        else
            c(xrange[1], xrange[2])
    }
    #--- plot.contour starts here
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
    xlim <- get.lim(xflip, grid1, is.fac1)
    ylim <- get.lim(yflip, grid2, is.fac2)
    if(swapxy)
        contour(grid2, grid1, t(y.predict),
            main=main, xlab=xlab, ylab=ylab,
            xlim=ylim, ylim=xlim,
            xaxt=if(is.fac1) "n" else "s",
            yaxt=if(is.fac2) "n" else "s",
            cex.lab=1.1 * cex.lab, cex.axis=cex.lab,
            labcex=.8 * cex.lab * par("cex"), ...)
    else
        contour(grid1, grid2, y.predict,
            main=main, xlab=xlab, ylab=ylab,
            xlim=xlim, ylim=ylim,
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

    if(!is.zero(col.response))
        plot.response.sites(x, ipred1, ipred2, col.response, cex.response,
                            pch.response, jitter.response, xlevs, ndiscrete,
                            iresponse, swapxy)
}
plot.image <- function(x, grid1, grid2, y.predict, pred.names, ipred1, ipred2,
                       xflip, yflip, swapxy,
                       main, col.image, cex.lab,
                       col.response, cex.response, pch.response,
                       jitter.response, xlevs, ndiscrete, iresponse, ...)
{
    # like image but fill the plot area with lightblue first so NA's obvious
    my.image <- function(grid1, grid2, y.predict, col, ...)
    {
        if(any(is.na(y.predict))) {
            image(grid1, grid2, matrix(0, nrow(y.predict), ncol(y.predict)),
                  col="lightblue", ...)
            par(new=TRUE)
        }
        image(grid1, grid2, y.predict, col=col, ...)
    }
    get.lim <- function(xflip, grid1, is.discrete)
    {
        xrange <- range(grid1)
        if(is.discrete) {
            xrange[1] <- xrange[1] - .5
            xrange[2] <- xrange[2] + .5
        } else {
            range <- xrange[2] - xrange[1]
            adj <- 0
            if(plot.response && jitter.response > 0)
                adj <- .12 * jitter.response # extra space for jittered points
            if(adj < .02)
                adj <- .02
            xrange[1] <- xrange[1] - adj * range
            xrange[2] <- xrange[2] + adj * range
        }
        if(xflip)
            c(xrange[2], xrange[1])
        else
            c(xrange[1], xrange[2])
    }
    #--- plot.image starts here
    xcol1 <- x[,ipred1]
    xcol2 <- x[,ipred2]
    levnames1 <- levels(xcol1)
    levnames2 <- levels(xcol2)
    use.fac.names1 <- is.factor(xcol1) && length(levnames1) <= 12
    use.fac.names2 <- is.factor(xcol2) && length(levnames2) <= 12
    xlab <- if(use.fac.names1) "" else pred.names[ipred1] # no lab if fac else on top of lev name
    ylab <- if(use.fac.names2) "" else pred.names[ipred2]
    if(swapxy) {
        temp <- levnames2;      levnames2 <- levnames1;           levnames1 <- temp
        temp <- use.fac.names2; use.fac.names2 <- use.fac.names1; use.fac.names1 <- temp
        temp <- ylab;           ylab <- xlab;                     xlab <- temp
    }
    plot.response <- !is.zero(col.response)
    # following for backwards compat (else axis lims slightly different)
    if(!xflip && !yflip && !use.fac.names1 && !use.fac.names2 &&
            !(plot.response && jitter.response > 0)) {
        # following for backwards compat (else axis lims slightly different)
        if(swapxy)
            my.image(grid2, grid1, t(y.predict), col=col.image,
                main=main, xlab=xlab, ylab=ylab,
                xaxt=if(use.fac.names1) "n" else "s",
                yaxt=if(use.fac.names2) "n" else "s",
                cex.lab=1.1 * cex.lab, cex.axis=cex.lab, ...)
        else
            my.image(grid1, grid2, y.predict, col=col.image,
                main=main, xlab=xlab, ylab=ylab,
                xaxt=if(use.fac.names1) "n" else "s",
                yaxt=if(use.fac.names2) "n" else "s",
                cex.lab=1.1 * cex.lab, cex.axis=cex.lab, ...)
    } else {
        xlim <- get.lim(xflip, grid1, use.fac.names1 || length(xlevs[[ipred1]]) <= ndiscrete)
        ylim <- get.lim(yflip, grid2, use.fac.names2 || length(xlevs[[ipred2]]) <= ndiscrete)
        if(swapxy)
            my.image(grid2, grid1, t(y.predict), col=col.image,
                main=main, xlab=xlab, ylab=ylab,
                xlim=ylim, ylim=xlim,
                xaxt=if(use.fac.names1) "n" else "s",
                yaxt=if(use.fac.names2) "n" else "s",
                cex.lab=1.1 * cex.lab, cex.axis=cex.lab, ...)
        else
            my.image(grid1, grid2, y.predict,  col=col.image,
                main=main, xlab=xlab, ylab=ylab,
                xlim=xlim, ylim=ylim,
                xaxt=if(use.fac.names1) "n" else "s",
                yaxt=if(use.fac.names2) "n" else "s",
                cex.lab=1.1 * cex.lab, cex.axis=cex.lab, ...)
    }
    if(use.fac.names1) {
        levnames1 <- abbreviate(levnames1, minlength=6, strict=TRUE)
        mtext(levnames1, side=1, at=1:length(levnames1),
              cex=par("cex") * cex.lab, line=.5, las=get.las(levnames1))
    }
    if(use.fac.names2)
        mtext(abbreviate(levnames2, minlength=6, strict=TRUE),
              side=2, at=1:length(levnames2),
              cex=par("cex") * cex.lab, line=.5, las=2)

    if(plot.response)
        plot.response.sites(x, ipred1, ipred2, col.response, cex.response,
                            pch.response, jitter.response, xlevs, ndiscrete,
                            iresponse, swapxy)
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
get.plotmo.type.wrapper <- function(obj, env, type, func.name="plotmo")
{
    if(is.null(type))
        type <- get.plotmo.default.type(obj, env) # get default type for this object class
    else {
        stopifnot(is.character(type))
        stopifnot(length(type) == 1)
        if(pmatch(type, "terms", nomatch=0))
            stop0("type=\"terms\" is not allowed by ", func.name)
    }
    type
}
# provided for backwards compatibility, earth:::plotd calls this
# TODO remove this once earth has been updated to call get.plotmo.type.wrapper
get.plotmo.type <- function(obj, type, func.name)
{
    get.plotmo.type.wrapper(obj, parent.frame(n=2), type, func.name)
}
# Check that y is good.  Also if y has multiple columns
# this returns just the column specified by nresponse.

check.and.print.y <- function(y, msg, nresponse, object, expected.len,
                              trace, will.take.subset=FALSE, return.yname=FALSE)
{
    get.nresponse <- function()
    {
        stopifnot(length(nresponse) == 1)
        if(is.na(nresponse)) {
            if(NCOL(y) > 1) {
                cat("\n")
                print.first.few.rows(y, trace, paste0(msg, " returned "))
                if(!is.null(colnames))
                    msg1 <- paste0("       Specify a column index like nresponse=1 or ",
                                   "a column name like nresponse=\"", colnames[1], "\"")
                else
                    msg1 <- paste0("       Specify a column index like nresponse=1")
                stop0("predicted response has multiple columns (see above) ",
                      "but nresponse is not specified.\n", msg1)
            }
            nresponse <- 1
        } else if (is.character(nresponse)) {
            # convert column name to column index
            if(is.vector(y))
                stop0("nresponse=\"", nresponse,
                      "\" cannot be used because the predicted response is a vector (it has no columns)")
            if(is.factor(y))
                stop0("nresponse=\"", nresponse,
                      "\" cannot be used because the predicted response is a factor (it has no columns)")
            if(is.null(colnames))
                stop0("nresponse=\"", nresponse,
                      "\" cannot be used because the predicted response has no column names")
            nresponse <- match.choices(nresponse, colnames, "nresponse")
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
        stop0(msg, " returned a zero length value")
    colnames <- NULL
    if(length(dim(y)) > 1) # work around crash in colnames() for mats with one column
        colnames <- colnames(y)
    nresponse <- get.nresponse()
    yname <- colnames[nresponse]
    if(NCOL(y) > 1)
        y <- y[, nresponse, drop=FALSE]
    if(NCOL(y) > 1)
        stop0("\"nresponse\" specifies more than one column")
    if(is.data.frame(y))            # needed for test.earth.glm.R test a21
        y <- as.matrix(y)
    if(class(y)[1] == "character")  # predict.nnet(type="class") returns characters
        y <- factor(y)              # TODO this doesn't preserve factor ordering
    if(any(!is.double(y)))          # convert logical or factor or whatever to double
        y <- as.vector(y, mode="numeric")
    if(NROW(y) == 1 && NCOL(y) > 1)
        y <- as.vector(y[, 1])
    if(trace) {
        cat0(msg, " ",
             if(is.null(yname)) "" else paste0("column \"", yname, "\" "),
             "returned length ", length(y))
        if(will.take.subset)
            cat(" (before taking subset)")
        print.first.few.elements.of.vector(y, trace, msg)
    }
    any.nas <- if(printed.na.warning.global) FALSE else any(is.na(y))
    any.non.finites <- FALSE
    any.non.finites <- !printed.na.warning.global && !any.nas && any(!is.finite(y))
    if(any.nas || any.non.finites) {
        # set global flag (we want to print NA or non.finite warning only once)
        unlockBinding("printed.na.warning.global", asNamespace("plotmo"))
        printed.na.warning.global <<- TRUE     # note <<- not <-
    }
    if(any.nas) {
        cat("\n")
        warning0("NAs returned by ", msg)
    }
    if(any.non.finites) {
        cat("\n")
        warning0("non-finite values returned by ", msg)
    }
    if(!will.take.subset && length(y) != expected.len)
        error.bad.ylen(y, msg, object, expected.len)
    if(return.yname)
        return(list(y=y, yname=yname))
    y
}
get.and.check.subset <- function(x, object, env, trace)
{
    is.valid <- function(subset)
    {
        !is.null(subset) && (is.numeric(subset) || is.logical(subset))
    }
    #--- get.and.check.subset starts here
    subset <- object$subset
    if(is.valid(subset))
        msg <- "object$subset"
    else {
        subset <- try(eval(object$call$subset, env), silent=TRUE)
        if(is.try.error(subset))
            subset <- NULL
        else
            msg <- "object$call$subset"
    }
    if(!is.valid(subset))
        subset <- NULL
    else {
        check.index.vec("subset", subset, x, check.empty=TRUE, allow.duplicates=TRUE)
        if(trace) {
            cat0("got subset from ", msg, " length " , length(subset))
            print.first.few.elements.of.vector(subset, trace)
           }
    }
    subset
}
apply.inverse.func <- function(y, object, trace, inverse.func, inverse.func.name)
{
    if(exists.and.not.null(inverse.func.name, "function", "inverse.func")) {
        y <- inverse.func(y)
        y <- check.and.print.y(y, paste0("inverse.func=", inverse.func.name),
                               1, object, length(y), trace)
    }
    y
}
# Error mesage for the aftermath of:
#   "Warning: 'newdata' had 100 rows but variable(s) found have 30 rows"
#
# I would classify this as a bug in lm or model.frame.default: lm can create
# models for which it is impossible to use predict.lm with new data.
# To see the underlying problem occur (without plotmo):
#
#   x <- matrix(c(1,3,2,4,5,2,3,4,5,6), ncol=2) # actual values not important
#   y <- 3:7
#   model <- lm(y~x)
#   # There is no way to use predict on new data with this model!
#   # Following gives the above warning and returns bad data:
#   predict(model, newdata=as.data.frame(x[1:3,])
#   print(attr(model$terms, "variables")) # gives list(y, x), confusing.
#
#   predict on following model also causes the same warning:
#   a <- lm(y~x[,1]+x[,2])

error.bad.ylen <- function(y, msg, object, expected.len)
{
    cat("\n")
    stop0(msg, " returned the wrong length (got ", length(y),
          " expected ", expected.len, ")")
}
# Should the factor labels on the x axis be printed horizontally or vertically?

get.las <- function(labels)
{
    if(length(labels) * max(nchar(labels)) <= 20)   # 20 is arbitrary
        0   # horizontal
    else
        2   # vertical
}
# TRUE if a plot was selected by the user (excluding the default setting)

is.specified <- function(degree)
{
    !is.logical(degree) || length(degree) > 1
}
