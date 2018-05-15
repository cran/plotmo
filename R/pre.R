# pre.R: plotmo functions for "pre" package

order.pre.vars.on.importance <- function(object, x, trace)
{
    varimps <- try(pre::importance(object, plot=FALSE)$varimps, silent=TRUE)
    if(is.try.err(varimps)) {
        warning0("pre::importance(pre.object) failed")
        return(NULL)
    }
    stopifnot(is.data.frame(varimps))
    stopifnot(!is.null(varimps$varname) && !is.null(varimps$imp))
    if(NROW(varimps) == 0) { # based on code in importance function in pre.R
        warning0("importance(pre.object)$varimps is empty")
        return(NULL)
    }
    # discard variables whose importance is less than 1% of max importance
    varname <- varimps[varimps$imp > .01 * varimps$imp[1], ]$varname
    # convert variable names to column indices
    importance <- match(varname, colnames(x))
    if(any(is.na(importance)| importance == 0)) { # sanity check
        warning0("could not get variable importances\n  varname=",
            paste.c(varname), " colnames(x)=", paste.c(colnames(x)))
        return(NULL)
    }
    importance # return a vector of var indices, most important vars first
}
plotmo.singles.pre <- function(object, x, nresponse, trace, all1, ...)
{
    importance <- order.pre.vars.on.importance(object, x, trace)
    if(all1 || is.null(importance))
        return(seq_len(NCOL(x))) # all variables
    # 10 most important variables
    # (10 becauses plotmo.pairs returns 6, total is 16, therefore 4x4 grid)
    importance[seq_len(min(10, length(importance)))]
}
plotmo.pairs.pre <- function(object, x, ...)
{
    importance <- order.pre.vars.on.importance(object, x, trace)
    if(is.null(importance))
        return(NULL)
    # pairs of four most important variables (i.e. 6 plots)
    form.pairs(importance[1: min(4, length(importance))])
}
plotmo.pairs.gpe <- function(object, x, nresponse=1, trace=0, all2=FALSE, ...)
{
    return(NULL) # not yet supported because importance(gpe) not supported
}
