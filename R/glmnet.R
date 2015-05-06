# glmnet.R: plotmo functions for glmnet objects

plotmo.prolog.glmnet <- function(object, object.name, trace, ...) # invoked when plotmo starts
{
    # stash (possibly user specified) s for use by plot.glmnetx and predict.glmnet
    s <- dot("predict.s", ...)
    check.numeric.scalar(s, na.ok=TRUE)
    if(is.na(s))
        s <- dot("s", ...)
    check.numeric.scalar(s, na.ok=TRUE)
    # if s is unspecified, use s=0 to match plotmo.predict.glmnet
    if(is.na(s))
        s <- 0
    attr(object, "s") <- s # stash it for later
    object
}
plotmo.predict.glmnet <- function(object, newdata, type, ..., TRACE)
{
    s <- attr(object, "s") # get the predict.glmnet s
    stopifnot(!is.null(s)) # uninitialized?
    # newx for predict.glmnet must be a matrix not a dataframe,
    # so here we use plotmo.predict.defaultm (not plotmo.predict.default)
    yhat <- plotmo.predict.defaultm(object, newdata, type=type, force.s=s,
                                    ..., TRACE=TRACE)
    if(length(dim(yhat) == 2) && NCOL(yhat) == 1) # paranoia, check that is matrix
        colnames(yhat) <- paste0("s=", signif(s,2))
    yhat
}
plotmo.singles.glmnet <- function(object, x, nresponse, trace, all1)
{
    # return the indices of the 25 biggest coefs, but exclude zero coefs
    s <- attr(object, "s") # get the predict.glmnet s
    stopifnot(!is.null(s)) # uninitialized?
    lambda.index <- which.min(abs(object$lambda - s)) # index into object$lambda
    trace2(trace, "plotmo.singles.glmnet: s %g lambda.index %g\n", s, lambda.index)
    beta <- as.vector(object$beta[, lambda.index]) # as.vector converts from dgCMatrix
    order <- order(abs(beta), decreasing=TRUE)
    max.nsingles <- if(all1) Inf else 25
    # extract the biggest coefs
    beta <- beta[order][1:min(max.nsingles, length(beta))]
    nsingles <- sum(abs(beta) > 1e-8) # drop zero coefs
    order[seq_len(nsingles)]
}
# Note: you have to manually assign x and y to the cv.glmnet object
# before calling plotmo, because cv.glmnet objects do not have a call
# field (checked in glmnet version 2.0.2).

plotmo.predict.cv.glmnet <- function(object, newdata, type, ..., TRACE)
{
    # newx for predict.glmnet must be a matrix not a dataframe,
    # so here we use plotmo.predict.defaultm (not plotmo.predict.default)
    plotmo.predict.defaultm(object, newdata, type=type, ..., TRACE=TRACE)
}
