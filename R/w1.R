# w1.R: plotres functions for the which=1 plot

plot_w1 <- function(object,
    which, # currently used only to get the total nbr of plots (for xlim and ylim)
    # most of these args are merely for the recursive call to plotres for lm models
    info, standardize, delever, level, versus,
    id.n, labels.id, smooth.col, grid.col,
    do.par, caption, trace,
    npoints, center,
    type, nresponse,
    object.name,
    SHOWCALL=NA, # this is here to absorb SHOWCALL from dots
    ...)
{
    if(inherits(object, "train")) { # caret
        check.is.caret.train.object(object)
        object <- object[["finalModel"]]
        # fall through to process the finalModel object
    }
    else if(inherits(object, "WrappedModel")) { # mlr package
        learner.field <- get.learner.field(object)
        object <- eval(parse(text=sprint("object%s", learner.field)))
        # fall through to process the learner.model object
    }
    if(inherits(object, "lm")) {
        # check that the model supports hatvalues(), needed for versus=V4LEVER.
        if(is.try.err(try(hatvalues(object), silent=TRUE)))
            retval <- list(plotted=FALSE, retval=NULL)
        else {
            # do a recursive call to plotres to plot the residuals versus leverage plot
            if(trace >= 1)
                printf(
"plotres(object, which=3, versus=4, ...)  (recursive call for leverage plot)\n")
            retval <- plotres(object=object, which=W3RESID, info=info, versus=V4LEVER,
                standardize=standardize, delever=delever, level=level,
                id.n=id.n, labels.id=labels.id, smooth.col=smooth.col,
                grid.col=grid.col,
                do.par=FALSE, caption=caption,
                trace=if(trace==1) 0 else trace,
                npoints=npoints, center=center,
                type=type, nresponse=nresponse,
                object.name=object.name,
                ...)
        }
    } else # call method function for object
        retval <- w1(object=object, trace=trace,
                     type=type, nresponse=nresponse,
                     which=which, grid.col=grid.col, ...)

    draw.caption(caption, ...) # necessary if w1 is only plot called by plotres

    retval
}
w1 <- function(object, trace, type, nresponse, which, grid.col, ...)
{
    UseMethod("w1")
}
w1.default <- function(object, trace, type, nresponse, which, grid.col, ...)
{
   list(plotted=FALSE, retval=NULL)
}
w1.earth <- function(object, trace, type, nresponse, which, grid.col, ...)
{
    call.earth.modsel(object=object, trace=trace, grid.col=grid.col, ...)
}
w1.mars <- function(object, trace, type, nresponse, which, grid.col, ...)
{
    # mda::mars, convert first to an earth model
    if(trace)
        printf("calling mars.to.earth (needed for the model selection plot)\n")
    earth.mod <- earth::mars.to.earth(object, trace=trace >= 2)
    earth.mod <- update(earth.mod, trace=trace >= 2)
    call.earth.modsel(object=earth.mod, trace=trace, grid.col=grid.col, ...)
}
# Note that by specifying col and lty in the arg list we drop
# them from dots passed to earth_plotmodsel, else get
# 'col' matches both the 'col.rsq' and 'col.grsq' arguments.
# TODO call.dot should be able to do this dropping for us but currently can't
call.earth.modsel <- function(object, trace, grid.col, col=NA, lty=NA, ...)
{
   list(plotted = TRUE,
        retval  = call.dots(earth::earth_plotmodsel, PREFIX="w1.",
                     DROP="*", KEEP="PREFIX,PLOT.ARGS,PLOTMO.ARGS",
                     trace=trace >= 1,
                     force.x=object, grid.col=grid.col, ...))
}
w1.rpart <- function(object, trace, type, nresponse, which, grid.col, ...)
{
    if(requireNamespace("rpart.plot", quietly=TRUE))
        # plotmo 3.1.5 (aug 2016): use prp not rpart.plot for a more
        # minimal plot because there isn't much space using (mfrow=c(2,2))
        call.w1(rpart.plot::prp, def.box.palette="auto", ...,
                object=object, trace=trace)
    else {
        printf("Please install the \"rpart.plot\" package for better rpart plots.\n")
        plot(object, compress=TRUE, uniform=TRUE)
        list(plotted=TRUE, retval=text(object, xpd=NA))
    }
}
w1.tree <- function(object, trace, type, nresponse, which, grid.col, ...)
{
    call.w1(graphics::plot, def.type="uniform", ...,
            object=object, trace=trace)
    n <- nrow(object$frame)
    def.cex <- if(n < 8) 1 else if(n < 20) .9 else .8
    call.w1(graphics::text,
            def.pretty=3, def.digits=3, def.cex=def.cex, ...,
            object=object, trace=trace)
}
w1.randomForest <- function(object, trace, type, nresponse, which, grid.col, ...)
{
    call.w1(graphics::plot, ...,
            def.main=dota("main", DEF="Error vs Number of Trees", ...),
            object=object, trace=trace)
}
w1.gbm <- function(object, trace, type, nresponse, which, grid.col, ...)
{
    # # don't allow n.trees argument to prevent a common mistake
    # if(!is.na(dota("n.trees", EX=0, ...)))
    #     stop0("n.trees is not allowed (please use predict.n.trees)")
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
    call.w1(plot_gbm, w1.n.trees=w1.n.trees, ...,
            object=object, trace=trace)
}
w1.GBMFit <- function(object, trace, type, nresponse, which, grid.col, ...)
{
    w1.gbm(object, trace, type, nresponse, which, grid.col, ...)
}
w1.cosso <- function(object, trace, type, nresponse, which, grid.col, ...)
{
    call.w1(graphics::plot, def.M=2, ...,
            object=object, trace=trace)
}
w1.glmnet <- function(object, trace, type, nresponse, which, grid.col, ...)
{
    call.w1(plot_glmnet,
            def.xvar="rlambda", def.grid.col=grid.col,
            force.s=attr(object, "plotmo.s"),
            force.nresponse=nresponse, ...,
            object=object, trace=trace)
}
plot.with.axis.par <- function(object, which, trace, type, ...)
{
    if(length(which) > 1) {
        # slightly smaller axis annotations to fit all top labels
        old.cex.axis <- par("cex.axis")
        on.exit(par(cex.axis=old.cex.axis))
        par(cex.axis=min(old.cex.axis, .9))
    }
    call.w1(graphics::plot, ..., object=object, trace=trace)
}
w1.lars <- function(object, trace, type, nresponse, which, grid.col, ...)
{
    plot.with.axis.par(object, which, trace, type, ...)
}
w1.sparsenet <- function(object, trace, type, nresponse, which, grid.col, ...)
{
    plot.with.axis.par(object, which, trace, type, ...)
}
w1.cv.glmnet <- function(object, trace, type, nresponse, which, grid.col, ...)
{
    plot.with.axis.par(object, which, trace, type, ...)
}
w1.pre <- function(object, trace, type, nresponse, which, grid.col, ...) # pre package
{
    importance <- try(pre::importance(object, plot=FALSE), silent=TRUE)
    if(is.try.err(importance)) {
        warning0("pre::importance(pre.object) failed")
       list(plotted=FALSE, retval=NULL)
    } else if(NROW(importance$varimps) == 0) # based on code in importance function in pre.R
        list(plotted=FALSE, retval=NULL)
    else
        call.w1(pre::importance, force.plot=TRUE, ...,
                object=object, trace=trace)
}
call.w1 <- function(FUNC, ..., object, trace)
{
    keep <- "PREFIX" # drop everything except args matching PREFIX
    fname <- trunc.deparse(substitute(FUNC))
    list(plotted = TRUE,
         retval  = call.dots(FUNC=FUNC, PREFIX="w1.",
                             DROP="*",      # drop everything
                             KEEP=keep,     # except args matching keep
                             TRACE=trace >= 1,
                             FNAME=fname, force.anon=object, ...))
}

# # TODO commented out because plot.C5.0 ignores par settings
# w1.C5.0 <- function(object, trace, type, nresponse, which, grid.col, ...)
# {
#     call.w1(graphics::plot, ...)
# }

# TODO commented out because plot.nn uses grid graphics
#      which doesn't coexist with base graphics
# w1.nn <- function(object, trace, type, nresponse, which, grid.col, ...)
# {
#     rep <- dota("w1.rep", DEF="best", ...)
#     if(is.null(rep))
#         stop0("rep=NULL is not allowed here for plot.nn ",
#               "(because it invokes dev.new)")
#     call.w1(plot.nn, def.rep=rep, ..., object=object, trace=trace)
# }
