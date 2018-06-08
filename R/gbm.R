# gbm.R: plotmo functions for gbm objects
#
# TODO Add support for plotmo's level argument (quantile regression).

plotmo.prolog.gbm <- function(object, object.name, trace, ...) # invoked when plotmo starts
{
    if(is.null(object$data)) # TODO could do more if object had a call component
        stop0("use keep.data=TRUE in the call to gbm ",
              "(cannot determine the variable importances)")

    # "importance" is a vector of variable indices (column numbers in x), most
    # important vars first, no variables with relative.influence < 1%.  We attach
    # it to the object to avoid calling summary.gbm twice (it's expensive).

    importance <- order.gbm.vars.on.importance(object)
    attr(object, "plotmo.importance") <- importance
    if(trace > 0)
        cat0("importance: ",
             paste.trunc(object$var.names[importance], maxlen=120), "\n")

    object
}
order.gbm.vars.on.importance <- function(object)
{
    # order=FALSE so importances correspond to orig variable indices
    importance <- summary(object, plotit=FALSE,     # calls summary.gbm
                          order=FALSE, normalize=TRUE)$rel.inf

    # NA assignment below so order() drops vars with importance < .01
    importance[importance < .01] <- NA
    stopifnot(length(importance) > 0)
    importance <- order(importance, decreasing=TRUE, na.last=NA)
    # return a vector of variable indices, most important vars first
    importance[!is.na(importance)]
}
plotmo.singles.gbm <- function(object, x, nresponse, trace, all1, ...)
{
    if(all1)
        return(1:length(object$var.names))
    importance <- attr(object, "plotmo.importance")
    stopifnot(!is.null(importance)) # uninitialized?
    # indices of vars with importance >= 1%, max of 10 variables
    # (10 becauses plotmo.pairs returns 6, total is 16, therefore 4x4 grid)
    importance[1: min(10, length(importance))]
}
plotmo.pairs.gbm <- function(object, ...)
{
    # pairs of four most important variables (i.e. 6 plots)
    importance <- attr(object, "plotmo.importance")
    stopifnot(!is.null(importance)) # uninitialized?
    form.pairs(importance[1: min(4, length(importance))])
}
# following is used by plotmo.x.gbm and plotmo.x.GBMFit
plotmo.x.gbm.aux <- function(x, x.order, var.levels)
{
    stopifnot(!is.null(x))
    stopifnot(!is.null(x.order) && !is.null(dim(x.order)))
    stopifnot(!is.null(var.levels) && is.list(var.levels))

    # Return the first ntrain rows of the x matrix.  The x matrix is stored
    # with the gbm object as a vector, so we must convert it back to
    # a data.frame here, one column for each variable.

    ntrain <- nrow(x.order)
    if(is.null(dim(x))) # for efficiency (new versions of gbm don't require this)
        x <- matrix(x, ncol=ncol(x.order))
    stopifnot(ncol(x) == ncol(x.order))
    x <- data.frame(x[seq_len(ntrain), ])
    colnames(x) <- colnames(x.order)

    # convert numeric columns that are actually factors
    # TODO this only works correctly if default ordering of factors was used

    for(i in seq_len(ncol(x)))
        if(typeof(var.levels[[i]]) == "character")
            x[[i]] <- factor(x[[i]], labels=var.levels[[i]])
    x
}
# following is used by plotmo.y.gbm and plotmo.y.GBMFit
plotmo.y.gbm.aux <- function(y, x.order)
{
    stopifnot(!is.null(y))
    stopifnot(!is.null(x.order) && !is.null(dim(x.order)))
    ntrain <- nrow(x.order)
    y[seq_len(ntrain)]
}
plotmo.x.gbm <- function(object, ...)
{
    plotmo.x.gbm.aux(object$data$x, object$data$x.order, object$var.levels)
}
plotmo.y.gbm <- function(object, ...)
{
    plotmo.y.gbm.aux(object$data$y, object$data$x.order)
}
plotmo.predict.gbm <- function(object, newdata, type, ..., TRACE)
{
    # TODO I've only tested the distributions listed below although more may work
    dist <- gbm.short.distribution.name(object)
    if(!(dist %in%  c("ga", "la", "td", "be", "hu", "ad")))
        stop0("gbm distribution=\"", object$distribution$name,
              "\" is not yet supported\n",
             "       (A direct call to plot_gbm may work)")

    # The following invokes predict.gbm.
    # predict.gbm doesn't do partial matching on type so we do it here with pmatch.
    # n.trees is defaulted so first time users can call plotmo(gbm.model) easily.
    type = match.choices(type, c("link", "response"), "type")
    n.trees <- gbm.n.trees(object)

    plotmo.predict.default(object, newdata,
        type=type, def.n.trees=n.trees, ..., TRACE=TRACE)
}
