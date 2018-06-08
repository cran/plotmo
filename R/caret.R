# caret.R: plotmo functions for caret objects
#
# TODO Currently only caret "train" objects have explicit support.

# sanity check that object a caret train object
# (since "train" is a quite generic name)
check.is.caret.train.object <- function(object)
{
    class <- class(object)[1]
    stopifnot.string(class)
    mod <- object[["finalModel"]]
    if(class != "train" || is.null(mod) || !is.list(mod))
        stop0("unrecognized \"train\" object ",
              "(was expecting a train object from the caret package)")
}
plotmo.prolog.train <- function(object, object.name, trace, ...)
{
    check.is.caret.train.object(object)
    # call plotmo.prolog for the finalModel for its side effects
    # (e.g. may attach plotmo.importance to the finalModel)
    finalModel <- try(plotmo.prolog(object$finalModel, object.name, trace, ...),
                      silent=trace < 2)
    is.err <- is.try.err(finalModel)
    trace1(trace, "plotmo.prolog(object$finalModel) %s\n",
           if(is.err) "failed, continuing anyway" else "succeeded")
    if(!is.err)
        object$finalModel <- finalModel
    object
}
plotmo.singles.train <- function(object, x, nresponse, trace, all1, ...)
{
    check.is.caret.train.object(object)
    singles <-
        try(plotmo.singles(object$finalModel, x, nresponse, trace, all1, ...),
            silent=trace < 2)
    is.err <- is.try.err(singles)
    trace2(trace, "plotmo.singles(object$finalModel) %s\n",
           if(is.err) "failed" else "succeeded")
    if(is.err)
        plotmo.singles.default(object, x, nresponse, trace, all1, ...)
    else
        singles
}
plotmo.pairs.train <- function(object, x, nresponse, trace, all2, ...)
{
    check.is.caret.train.object(object)
    pairs <-
        try(plotmo.pairs(object$finalModel, x, nresponse, trace, all2, ...),
            silent=trace < 2)
    is.err <- is.try.err(pairs)
    trace2(trace, "plotmo.pairs(object$finalModel) %s\n",
           if(is.err) "failed" else "succeeded")
    if(is.err)
        plotmo.pairs.default(object, x, nresponse, trace, all2, ...)
    else
        pairs
}
# determine "type" arg for predict()
plotmo.type.train <- function(object, ..., TRACE)
{
    "raw"

    # check.is.caret.train.object(object)
    # trace <- TRACE
    # type <- try(plotmo.type(object$finalModel, ..., TRACE=TRACE), silent=trace < 2)
    # is.err <- is.try.err(type)
    # trace2(trace, "plotmo.type(object$finalModel) %s\n",
    #        if(is.err) "failed" else "succeeded")
    # if(is.err)
    #     "raw"
    # else
    #     type
}
# determine "type" arg for residuals()
plotmo.residtype.train <- function(object, ..., TRACE)
{
    "raw"

    # check.is.caret.train.object(object)
    # trace <- TRACE
    # type <- try(plotmo.residtype(object$finalModel, ...), silent=trace < 2)
    # is.err <- is.try.err(type)
    # trace2(trace, "plotmo.residtype(object$finalModel) %s\n",
    #        if(is.err) "failed" else "succeeded")
    # if(is.err)
    #     "raw"
    # else
    #     type
}
