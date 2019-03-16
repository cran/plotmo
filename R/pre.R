# pre.R: plotmo functions for "pre" package

plotmo.prolog.pre <- function(object, object.name, trace, ...) # invoked when plotmo starts
{
    # importance is a vector of variable indices, most important vars first
    importance <- order.pre.vars.on.importance(object, trace)
    attr(object, "plotmo.importance") <- importance
    object
}
order.pre.vars.on.importance <- function(object, trace)
{
    varimps <- try(pre::importance(object, plot=FALSE)$varimps, silent=TRUE)
    if(is.try.err(varimps)) {
        cat("\n")
        warning0("pre::importance(pre.object) failed\n",
"(Will plot all variables regardless of importance. Use all2=TRUE to get degree2 plots.)\n")
        # NULL be will be treated as all vars by plotmo.single.pre,
        # and as no vars by plotmo.pairs.pre.
        return(NULL)
    }
    stopifnot(is.data.frame(varimps))
    if(NROW(varimps) == 0) { # based on code in importance function in pre.R
        warning0("importance(pre.object)$varimps is empty")
        return(NULL)
    }
    stopifnot(!is.null(varimps$varname))
    # following is needed for multiple response models
    # we get the combined importance across all responses
    if(is.null(varimps$imp))
        varimps$imp <- rowSums(varimps[,-1])
    stopifnot(!is.null(varimps$imp))
    # discard variables whose importance is less than 1% of max importance
    varname <- varimps[varimps$imp > .01 * varimps$imp[1], ]$varname
    # convert variable names to column indices
    allvarnames <- object$x_names
    stopifnot(!is.null(allvarnames) && length(allvarnames) > 0) # paranoia
    importance <- match(varname, allvarnames)
    if(any(is.na(importance) | (importance == 0))) { # sanity check
        warning0("could not get variable importances\n  varname=",
            paste.c(varname))
        return(NULL)
    }
    if(trace > 0)
        cat0("importance: ",
             paste.trunc(allvarnames[importance], maxlen=120), "\n")
    importance # return a vector of var indices, most important vars first
}
plotmo.singles.pre <- function(object, x, nresponse, trace, all1, ...)
{
    importance <- attr(object, "plotmo.importance")
    if(all1 || is.null(importance))
        return(seq_len(NCOL(x))) # all variables
    # 10 most important variables
    # (10 becauses plotmo.pairs returns 6, total is 16, therefore 4x4 grid)
    importance[seq_len(min(10, length(importance)))]
}
plotmo.pairs.pre <- function(object, x, ...)
{
    importance <- attr(object, "plotmo.importance")
    if(is.null(importance))
        return(NULL) # importances not available so don't plot any pairs
    # choose npairs so a total of no more than 16 plots
    # npairs=5 gives 10 pairplots, npairs=4 gives 6 pairplots
    npairs <- if(length(importance) <= 6) 5 else 4
    form.pairs(importance[1: min(npairs, length(importance))])
}
plotmo.pairs.gpe <- function(object, x, nresponse=1, trace=0, all2=FALSE, ...)
{
    return(NULL) # not yet supported because importance(gpe) not supported
}
