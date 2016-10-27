# w1.R: plotres functions for the which=1 plot

plot_w1 <- function(object,
    which, # currently used only to get the total nbr of plots (for xlim and ylim)
    info, standardize, delever, level, versus,
    id.n, labels.id, smooth.col, grid.col,
    do.par, caption, trace,
    npoints, center,
    type, nresponse,
    object.name,
    SHOWCALL=NA, # this is here to absorb SHOWCALL from dots
    ...)
{
    call.w1 <- function(FUNC, ...)
    {
        # Decided not to do the following, too complicated.
        #
        # # if which) == 1 then pass all w1. and plot args directly
        # # to the w1 plot (else only those args with a  w1. prefix)
        # # if both say w1.xlim and xlim are specified then xlim takes precedence
        #                                # DROP="*" so drop everything...
        # keep <- if(all(which == 1))
        #             "PREFIX,PLOT.ARGS" # except args matching PREFIX and PLOT.ARGS
        #         else
        #             "PREFIX"           # except args matching PREFIX

        keep <- "PREFIX"         # drop everything except args matching PREFIX

        fname <- trunc.deparse(substitute(FUNC))

        call.dots(FUNC=FUNC, PREFIX="w1.",
                  DROP="*",      # drop everything
                  KEEP=keep,     # except args matching keep
                  TRACE=trace >= 1,
                  FNAME=fname, force.anon=object, ...)
    }
    retval <- NULL
    plotted = TRUE
    if(inherits(object, "train")) { # caret
        check.is.caret.train.object(object)
        object <- object[["finalModel"]]
        # fall through to process the finalModel object
    }
    if(inherits(object, "earth"))
        retval <- call.earth.modsel(object=object,
                                    trace=trace, grid.col=grid.col, ...)
    else if(inherits(object, "mars")) { # mda::mars, convert first to an earth model
        if(trace)
            printf("calling mars.to.earth (needed for the model selection plot)\n")
        earth.mod <- earth::mars.to.earth(object, trace=trace >= 2)
        earth.mod <- update(earth.mod, trace=trace >= 2)
        retval <- call.earth.modsel(object=earth.mod,
                                    trace=trace, grid.col=grid.col, ...)
    } else if(inherits(object, "lm")) {
        # check that the model supports hatvalues(), needed for versus=V4LEVER.
        if(is.try.err(try(hatvalues(object), silent=TRUE)))
            plotted <- FALSE
        else {
            # do a recursive call to plotres to plot the residuals versus leverage plot
            if(trace >= 1)
                printf(
"plotres(object, which=3, versus=4, ...)  (recursive call for leverage plot)\n")
            plotres(object=object, which=W3RESID, info=info,
                standardize=standardize, delever=delever, level=level,
                versus=V4LEVER,
                id.n=id.n, labels.id=labels.id, smooth.col=smooth.col,
                grid.col=grid.col,
                do.par=FALSE, caption=caption,
                trace=if(trace==1) 0 else trace,
                npoints=npoints, center=center,
                type=type, nresponse=nresponse,
                object.name=object.name,
                ...)
        }
    } else if(inherits(object, "rpart")) {
        if(requireNamespace("rpart.plot", quietly=TRUE))
            # # plotmo 3.1.5 (aug 2016): use prp not rpart.plot for a more
            # minimal plot because there isn't much space using (mfrow=c(2,2))
            retval <- call.w1(rpart.plot::prp, def.box.palette="auto", ...)
        else {
            printf("Please install the \"rpart.plot\" package for better rpart plots.\n")
            plot(object, compress=TRUE, uniform=TRUE)
            retval <- text(object, xpd=NA)
        }
    } else if(inherits(object, "tree")) {
        call.w1(graphics::plot, def.type="uniform", ...)
        n <- nrow(object$frame)
        def.cex <- if(n < 8) 1 else if(n < 20) .9 else .8
        retval <- call.w1(graphics::text,
                          def.pretty=3, def.digits=3, def.cex=def.cex, ...)
    } else if(inherits(object, "randomForest"))
        retval <- call.w1(graphics::plot, ...,
                          def.main=dota("main",
                                        DEF="Error vs Number of Trees", ...))
    else if(inherits(object, c("gbm", "GBMFit"))) {
        # don't allow w1.n.trees argument, except w1.n.trees=NA
        predict.n.trees <- dota("predict.n.trees", DEF=gbm.n.trees(object),  ...)
        w1.n.trees      <- dota("w1.n.trees",      DEF=predict.n.trees, ...)
        if(!is.na(w1.n.trees) && w1.n.trees != predict.n.trees) {
            if(is.na(dota("predict.n.trees", EX=0, ...)))
                stop0("w1.n.trees is not allowed (please use predict.n.trees)")
            else
                stop0("w1.n.trees is not allowed")
        }
        check.integer.scalar(w1.n.trees, min=1, max=gbm.n.trees(object),
                             na.ok=TRUE, logical.ok=FALSE,
                             object.name="n.trees")
        retval <- call.w1(plot_gbm, w1.n.trees=w1.n.trees, ...)
    } else if(inherits(object, "cosso"))
        retval <- call.w1(graphics::plot, def.M=2, ...)
    else if(inherits(object, c("glmnet")))
        retval <- call.w1(plot_glmnet,
                    def.xvar="rlambda", def.grid.col=grid.col,
                    force.s=attr(object, "plotmo.s"),
                    force.nresponse=nresponse, ...)
    else if(inherits(object, c("lars", "sparsenet", "cv.glmnet"))) {
        if(length(which) > 1) {
            # slightly smaller axis annotations to fit all top labels
            old.cex.axis <- par("cex.axis")
            on.exit(par(cex.axis=old.cex.axis))
            par(cex.axis=min(old.cex.axis, .9))
        }
        retval <- call.w1(graphics::plot, ...)
    }
    # # TODO commented out because plot.nn uses grid graphics
    # #      which doesn't coexist with base graphics
    # } else if(inherits(object, "nn")) { # neuralnet package
    #     rep <- dota("w1.rep", DEF="best", ...)
    #     if(is.null(rep))
    #         stop0("rep=NULL is not allowed here for plot.nn ",
    #               "(because it invokes dev.new)")
    #     retval <- call.w1(plot.nn, def.rep=rep, ...)

    else
        plotted <- FALSE

   draw.caption(caption, ...)
   list(plotted=plotted, retval=retval)
}
# Note that by specifying col and lty in the arg list we drop
# them from dots passed to earth_plotmodsel, else get
# 'col' matches both the 'col.rsq' and 'col.grsq' arguments.
# TODO call.dot should be able to do this dropping for us but currently can't
call.earth.modsel <- function(object, trace, grid.col, col=NA, lty=NA, ...)
{
    call.dots(earth::earth_plotmodsel, PREFIX="w1.",
             DROP="*", KEEP="PREFIX,PLOT.ARGS,PLOTMO.ARGS",
             trace=trace >= 1,
             force.x=object, grid.col=grid.col, ...)
}
nlines.in.w1.main <- function(object)
{
    1
}
