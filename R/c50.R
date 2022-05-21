# c50.R: plotmo functions for model objects from the C50 package

plotmo.prolog.C5.0 <- function(object, object.name, trace, ...) # invoked when plotmo starts
{
    # "imp" is a vector of variable indices (column numbers in x), most
    # important vars first, no variables with relative.influence < 1%.
    imp <- order.C5.0.vars.on.importance(object)
    attr(object, "plotmo.importance") <- imp
    if(trace > 0)
        cat0("importance: ",
             paste.trunc(object$predictors[imp], maxlen=120), "\n")

    object
}
order.C5.0.vars.on.importance <- function(object)
{
    imp <- C50::C5imp(object)
    stopifnot(is.data.frame(imp) && all(dim(imp) == c(object$dims[2], 1)))
    imp <- imp[imp >= 1, , drop=FALSE]
    stopifnot(length(imp) > 0)
    imp <- match(rownames(imp), object$predictors)
    stopifnot(!anyNA(imp))
    imp
}
plotmo.singles.C5.0 <- function(object, x, nresponse, trace, all1, ...)
{
    if(all1)
        return(1:length(object$predictors))
    importance <- attr(object, "plotmo.importance")
    stopifnot(!is.null(importance)) # uninitialized?
    # indices of vars with importance >= 1%, max of 10 variables
    # (10 becauses plotmo.pairs returns 6, total is 16, therefore 4x4 grid)
    importance[1: min(10, length(importance))]
}
plotmo.pairs.C5.0 <- function(object, ...)
{
    importance <- attr(object, "plotmo.importance")
    stopifnot(!is.null(importance)) # uninitialized?
    # choose npairs so a total of no more than 16 plots
    # npairs=5 gives 10 pairplots, npairs=4 gives 6 pairplots
    npairs <- if(length(importance) <= 6) 5 else 4
    form.pairs(importance[1: min(npairs, length(importance))])
}
