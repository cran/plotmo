# plot_glmnet.R:
#
# This code is based on code in glmnet version 2.0-5 (march 2016).

plot_glmnet <- function(x=stop("no 'x' argument"),
                        xvar=c("rlambda", "lambda", "norm", "dev"),
                        label=10, nresponse=NA, grid.col=NA, s=NA, ...)
{
    check.classname(x, "x", c("glmnet", "multnet"))
    obj <- x
    beta <- get.beta(obj$beta, nresponse)
    ibeta <- nonzeroCoef(beta) # ibeta is a vector of coefficient indices
    if(length(ibeta) == 0) {
        plot(0:1, 0:1, col=0) # dummy plot
        legend("topleft", legend="all glmnet coefficients are zero", bty="n")
        return(invisible(NULL))
    }
    # following was in original plot.glmnet code but seems unnecessary
    # if(length(ibeta) == 1) {
    #     warning("1 or less nonzero coefficients; glmnet plot is not meaningful")
    #     plot(0:1, 0:1, col=0)
    #     legend("topleft", legend="only one coefficient is nonzero", bty="n")
    #     return()
    # }
    beta <- as.matrix(beta[ibeta, , drop=FALSE])
    xlim <- dota("xlim", ...) # get xlim from dots, NA if not in dots
    xvar <- match.arg1(xvar)
    switch(xvar,
    "norm"= {
        if(inherits(obj, "multnet") || inherits(obj, "mrelnet")) {
            # we don't (yet) precalc norm or support type.coef, so have to stop here
            stop0("xvar=\"norm\" is not supported by plotres for multiple responses")
        }
        x <- apply(abs(beta), 2, sum)
        if(!is.specified(xlim))
            xlim <- c(min(x), max(x))
        xlab <- "L1 Norm"
        approx.f <- 1
    },
    "lambda"= {
        x <- log(obj$lambda)
        if(!is.specified(xlim))
            xlim <- c(min(x), max(x))
        xlab <- "Log Lambda"
        approx.f <- 0
    },
    "rlambda"= {
        x <- log(obj$lambda)
        if(!is.specified(xlim))
            xlim <- c(max(x), min(x)) # backwards
        xlab <- "Log Lambda"
        approx.f <- 0
    },
    "dev"= {
        x <- obj$dev.ratio
        if(!is.specified(xlim))
            xlim <- c(min(x), max(x))
        xlab <- "Fraction Deviance Explained"
        approx.f <- 1
    })
    xlim <- fix.lim(xlim)
    if(xvar != "rlambda")
        stopifnot(xlim[1] < xlim[2])
    else if(xlim[2] >= xlim[1]) # backwards
         stop0("xlim[1] must be bigger than xlim[2] for xvar=\"rlambda\"")

    iname <- get.iname(beta, ibeta, label)  # index of varnames on rhs of plot

    old.par <- par("mar", "mgp", "cex.axis", "cex.lab")
    on.exit(par(mar=old.par$mar, mgp=old.par$mgp, cex.axis=old.par$cex.axis,
                cex.lab=old.par$cex.lab))
    mar4 <- old.par$mar[4]      # right hand margin
    if(length(iname)) {
        cex.names <- min(1, max(.5, 2.5 / sqrt(length(iname)))) # seems ok
        # ensure right margin is big enough for the varnames
        # can't use strwidth because no plot yet, so just estimate
        mar4 <- max(old.par$mar[4] + 1,
                    .75 * cex.names * par("cex") * max(nchar(names(iname))))
    }
    # set mar[3] with space for top axis and maybe main, and mar[4] for rhs labels
    main <- dota("main", ...)   # get main from dots, NA if not in dots
    nlines.needed.for.main <- if(is.specified(main)) nlines(main) + .5 else 0
    par(mar=c(old.par$mar[1],
              old.par$mar[2],
              max(old.par$mar[3], nlines.needed.for.main + 2.6),
              mar4))
    par(mgp=c(1.5, .4, 0))      # squash axis annotations
    par(cex.axis=.8)
    ylab <- "Coefficients"
    if(is.list(obj$beta))       # multiple response model?
        ylab <- paste0(ylab, ": Response ", rownames(obj$dfmat)[nresponse])
    coef.col <- get.coef.col(..., beta=beta)  # color of coef lines

    # discard lines with color NA or 0
    keep <- which((coef.col != "NA") & (coef.col != "0"))
    iname <- iname[iname %in% keep]
    beta[-keep,] <- NA

    # Call graphics::matplot but drop args in dots that aren't graphics args
    # or formal args of graphics::matplot.
    # If argname below is prefixed with force. then ignore any such arg in dots.
    # Any argname below prefixed with def. can be overridden by a user arg in dots.
    # force.main="" because we later manually add a top axis and possibly main.

    call.plot(graphics::matplot, force.x=x, force.y=t(beta),
        force.main="", force.col=coef.col,
        def.xlim=xlim, def.xlab=xlab, def.ylab=ylab,
        def.lty=1, def.lwd=1, def.type="l", ...)

    abline(h=0, col="gray", lty=3) # zero axis line
    maybe.grid(x=x, beta=beta, grid.col=grid.col, coef.col=coef.col, ...)
    if(xvar == "rlambda") {
        # args are named below to prevent potential clash with argnames in dots
        annotate.rlambda(lambda=obj$lambda, x=x, beta=beta, s=s,
                         grid.col=grid.col, coef.col=coef.col, ...)
        toplab <- "Lambda"
    } else {
        top.axis(obj, x, nresponse, approx.f)
        toplab <- "Degrees of Freedom"
    }
    mtext(toplab, side=3, line=1.5, cex=par("cex") * par("cex.lab"))
    if(is.specified(main))
        mtext(main, side=3, line=3, , cex=par("cex")) # above top axis
    if(length(iname))
        right.labs(beta, iname, cex.names, coef.col)
    invisible(NULL)
}
get.beta <- function(beta, nresponse)
{
    if(is.list(beta)) { # multiple response model?
        check.integer.scalar(nresponse, min=1, max=length(beta),
                             na.ok=TRUE, logical.ok=FALSE)
        if(is.na(nresponse))
            stop0(
"Use the nresponse argument to specify a response for this multiple response model.\n",
"       Example: nresponse=", length(beta))
        check.index(nresponse, "nresponse", beta)
        beta <- beta[[nresponse]]
    }
    beta
}
get.coef.col <- function(..., beta)
{
    # default colors are distinguishable yet harmonious (at least to my eye)
    # adjacent colors are as different as easily possible
    def.col <- c("black", "red", "gray50", "orangered3", "darkorange", "magenta2")

    col <- dota("col", DEF=def.col, ...) # get col from dots, def.col if not in dots

    # the colors must stay in the above order as we move down rhs of plot
    order <- order(beta[, ncol(beta)], decreasing=TRUE)
    coef.col <- vector(mode="character", nrow(beta))
    coef.col[order] <- rep_len(col, nrow(beta))
    coef.col
}
# named index of varnames to be printed on right of plot, NULL if none
get.iname <- function(beta, ibeta, label)
{
    iname <- NULL
    check.integer.scalar(label, min=0, logical.ok=TRUE, na.ok=TRUE)
    if(!is.na(label) && label) { # allow label=NA, treat as FALSE
        names <- if(is.null(rownames(beta))) paste(ibeta)
                 else                        rownames(beta)
        names[!nzchar(names)] <- paste(ibeta)[!nzchar(names)]
        iname <- order(abs(beta[, ncol(beta)]), decreasing=TRUE)
        if(is.logical(label)) # label=TRUE is special meaning all
            iname <- iname[1:length(iname)]
        else if(length(iname) > label)
            iname <- iname[1:label]
        names(iname) <- abbreviate(names[iname], minlength=8)
    }
    iname # named index of varnames to be printed, NULL if none
}
maybe.grid <- function(x, beta, grid.col, coef.col, ...)
{
    if(is.specified(grid.col[1])) {
        grid(col=grid.col[1], lty=1)
        # replot over the grid (using add=TRUE)
        call.plot(graphics::matplot, force.x=x, force.y=t(beta),
            force.add=TRUE, force.main="", force.col=coef.col,
            def.lty=1, def.lwd=1, def.type="l", ...)
    }
}
right.labs <- function(beta, iname, cex.names, coef.col) # varnames on right of plot
{
    usr <- par("usr")
    text(x=usr[2] + .01 * (usr[2] - usr[1]),
         y=TeachingDemos::spread.labs(beta[iname, ncol(beta)],
                                      mindiff=1.2 * cex.names * strheight("X")),
         labels=names(iname), cex=cex.names, col=coef.col[iname], adj=0, xpd=NA)
}
top.axis <- function(obj, x, nresponse, approx.f)
{
    at <- pretty(x)
    # use is.list(obj$beta) to determine if multiple response model
    df <- if(is.list(obj$beta)) obj$dfmat[nresponse,] else obj$df
    # compute df by interpolating to df at next smaller lambda
    # thanks to Yunyang Qian
    prettydf <- approx(x=x, y=df, xout=at,
                       rule=2, method="constant", f=approx.f)$y
    axis(3, at=at, labels=prettydf)
}
# Draw the top axis of an rlambda plot.
# Also draw a labeled vertical line at lambda=s, if s isn't NA.
# Dot arguments prefixed with "s". can be used to set the annotation
# attributes e.g. s.col=NA or s.col=0 for no vertical line.
# This is achieved with call.plot(text.on.white, PREFIX="s.", ...) below.
annotate.rlambda <- function(lambda, x, beta, s, grid.col, coef.col, ...)
{
    check.numeric.scalar(s, na.ok=TRUE, null.ok=TRUE, logical.ok=FALSE)
    s.col <- dota("s.col", DEF=1, ...) # get s.col from dots, 1 if not in dots
    add.s.line <- !is.null(s) && !is.na(s) && is.specified(s.col)

    # top axis
    at <- pretty(x)
    labs <- signif(exp(at), digits=2)
    # hack: delete confusing rightmost lab (if any) with a value greater
    # than s but drawn to the right of the vertical line at s
    if(add.s.line && s <= labs[1])
        labs[1] <- ""
    axis(3, at=at, labels=labs)

    if(add.s.line) # add vertical line showing s?
        add.s.line(lambda=lambda, x=x, beta=beta, s=s,
                   grid.col=grid.col, coef.col=coef.col, s.col=s.col, ...)
}
add.s.line <- function(lambda, x, beta, s, grid.col, coef.col, s.col, ...)
{
    line.col <- "gray"
    line.lty <- 1
    if(is.specified(grid.col)) {
            line.col <- 1
            line.lty <- 3
    }
    log.s <- log(max(lambda[length(lambda)], s))

    abline(v=log.s, col=line.col, lty=line.lty) # vertical line at s

    # replot over the vertical line (using add=TRUE)
    call.plot(graphics::matplot, force.x=x, force.y=t(beta),
              force.add=TRUE, force.main="", force.col=coef.col,
              def.lty=1, def.lwd=1, def.type="l", ...)

    # add s label on vertical line
    # to minimize overplotting, y coord of label is biggest gap between matplot lines
    usr <- par("usr") # xmin, xmax, ymin, ymax
    col.index <- which.min(abs(lambda-s)) # lambda column corresponding to s
    y <- sort(c(usr[3], beta[, col.index], usr[4])) # include plot edges, and sort
    which <- which.max(diff(y))
    # call graphics::matplot() but drop args in dots that aren't graphics args
    # or argnames prefixed with "s." or formal args of text.on.white
    call.plot(text.on.white, PREFIX="s.",
            force.x=log.s, force.y=(y[which]+y[which+1]) / 2,
            force.label= # gsub below drops leading and trailing zeros for compactness
                if(s == 0) "s=0"
                else        paste0("s=", gsub("^0|0$|\\.0*$", "", signif(s,2))),
            force.col=s.col, force.cex=.8, def.srt=90, def.xpd=NA, ...)
}
# Return NULL or an integer vector
# Reproduced here (from glmnet version 2.0-5, march 2016)
# so don't have to import glmnet into plotmo.

nonzeroCoef = function (beta, bystep = FALSE)
{
### bystep = FALSE means which variables were ever nonzero
### bystep = TRUE means which variables are nonzero for each step
  nr=nrow(beta)
  if (nr == 1) {#degenerate case
    if (bystep)
      apply(beta, 2, function(x) if (abs(x) > 0)
            1
      else NULL)
    else {
      if (any(abs(beta) > 0))
        1
      else NULL
    }
  }
  else {
    beta=abs(beta)>0 # this is sparse
    which=seq(nr)
    ones=rep(1,ncol(beta))
    nz=as.vector((beta%*%ones)>0)
    which=which[nz]
    if (bystep) {
      if(length(which)>0){
        beta=as.matrix(beta[which,,drop=FALSE])
        nzel = function(x, which) if (any(x))
          which[x]
        else NULL
        which=apply(beta, 2, nzel, which)
        if(!is.list(which))which=data.frame(which)# apply can return a matrix!!
        which
      }
      else{
        dn=dimnames(beta)[[2]]
        which=vector("list",length(dn))
        names(which)=dn
        which
      }

    }
    else which
  }
}
