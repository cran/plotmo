# glmnet.R: plotmo functions for glmnet and glmnetUtils objects

plotmo.prolog.glmnet <- function(object, object.name, trace, ...) # invoked when plotmo starts
{
    # stash (possibly user specified) s for use by plot_glmnet and predict.glmnet
    s <- dota("predict.s", ...)     # get predict.s from dots, NA if not in dots
    check.numeric.scalar(s, na.ok=TRUE)
    if(is.na(s))
        s <- dota("s", ...)         # get s from dots, NA if not in dots
    check.numeric.scalar(s, na.ok=TRUE)
    # if s is unspecified, use s=0 to match plotmo.predict.glmnet
    if(is.na(s))
        s <- 0
    attr(object, "plotmo.s") <- s   # stash it for later
    object
}
plotmo.predict.glmnet <- function(object, newdata, type, ..., TRACE)
{
    s <- attr(object, "plotmo.s") # get the predict.glmnet s
    stopifnot(!is.null(s)) # uninitialized?
    # newx for predict.glmnet must be a matrix not a dataframe,
    # so here we use plotmo.predict.defaultm (not plotmo.predict.default)
    yhat <- plotmo.predict.defaultm(object, newdata, type=type, force.s=s,
                                     ..., TRACE=TRACE)
    if(length(dim(yhat) == 2) && NCOL(yhat) == 1) # paranoia, check that is matrix
        colnames(yhat) <- paste0("s=", signif(s,2))
    yhat
}
plotmo.predict.glmnet.formula <- function(object, newdata, type, ..., TRACE) # glmnetUtils package
{
    # same as plotmo.predict.glmnet but doesn't convert newx to a matrix
    s <- attr(object, "plotmo.s") # get the predict.glmnet s
    stopifnot(!is.null(s)) # uninitialized?
    yhat <- plotmo.predict.default(object, newdata, type=type, force.s=s,
                                   ..., TRACE=TRACE)
    if(length(dim(yhat) == 2) && NCOL(yhat) == 1) # paranoia, check that is matrix
        colnames(yhat) <- paste0("s=", signif(s,2))
    yhat
}
plotmo.singles.glmnet <- function(object, x, nresponse, trace, all1, ...)
{
    # return the indices of the 25 biggest coefs, but exclude zero coefs
    s <- attr(object, "plotmo.s") # get the predict.glmnet s
    stopifnot(!is.null(s)) # uninitialized?
    lambda.index <- which.min(abs(object$lambda - s)) # index into object$lambda
    trace2(trace, "plotmo.singles.glmnet: s %g lambda.index %g\n", s, lambda.index)
    beta <- object$beta
    if(is.list(beta)) # multiple response model?
        beta <- beta[[nresponse]]
    beta <- as.vector(beta[, lambda.index]) # as.vector converts from dgCMatrix
    order <- order(abs(beta), decreasing=TRUE)
    max.nsingles <- if(all1) Inf else 25
    # extract the biggest coefs
    beta <- beta[order][1:min(max.nsingles, length(beta))]
    nsingles <- sum(abs(beta) > 1e-8) # drop zero coefs
    order[seq_len(nsingles)]
}
plotmo.prolog.cv.glmnet <- function(object, object.name, trace, ...) # invoked when plotmo starts
{
    # cv.glmnet objects don't have their call field in the usual place,
    # so fix that (tested on glmnet version 2.0-2).
    # Note that getCall() doesn't work on cv.glmnet objects.
    if(is.null(object[["call"]])) {
        object$call <- object$glmnet.fit$call
        stopifnot(!is.null(object$call), is.call(object$call))
    }
    object
}
plotmo.predict.cv.glmnet <- function(object, newdata, type, ..., TRACE)
{
    if(inherits(object, "cv.glmnet.formula")) { # glmnetUtils package
        plotmo.predict.default(object, newdata, type=type, ..., TRACE=TRACE)
    } else {                                    # glmnet package
        # newx for predict.glmnet must be a matrix not a dataframe,
        # so here we use plotmo.predict.defaultm (not plotmo.predict.default)
        plotmo.predict.defaultm(object, newdata, type=type, ..., TRACE=TRACE)
    }
}
# glmnet family="binomial", y is a vector of 1s and 2s.
# convert 1s and 2s to 0s and 1s to match predicted values
plotmo.y.lognet <- function(object, trace, naked, expected.len, nresponse, ...)
{
    # plotmo.y.default returns list(field=y, do.subset=do.subset)
    list <- plotmo.y.default(object, trace, naked, expected.len)
    # following is needed for glmnetUtils:glmnet.formula models (but not for glmnet xy models)
    if(is.data.frame(list$field))
        list$field <- list$field[[1]]
    stopifnot(!is.null(list$field)) # paranoia
    list$do.subset <- FALSE      # glmnet doesn't support subset so don't even try
    # TODO following only works correctly if default ordering of factor was used?
    list$field <-as.numeric(list$field) # as.numeric needed if y is a factor
    list$field - min(list$field)        # convert 1s and 2s to 0s and 1s
}
# glmnet family="multinomial"
plotmo.y.multnet <- function(object, trace, naked, expected.len, nresponse, ...)
{
    # plotmo.y.default returns list(field=y, do.subset=do.subset)
    list <- plotmo.y.default(object, trace, naked, expected.len)
    list$do.subset <- FALSE # glmnet doesn't support subset so don't even try
    if(is.null(nresponse))  # plotmo uses nresponse=NULL in initial checking
        nresponse <- 1
    if(NCOL(list$field) > 1) # if y is multiple columns assume it's an indicator matrix
        y <- list$field
    else {                   # else convert it to an indicator matrix
        # TODO following only works correctly if default ordering of factor was used?
        y1 <- as.numeric(list$field) # as.numeric needed if y is a factor
        stopifnot(min(y1) == 1 && max(y1) >  1) # sanity check
        # convert y1 to an indicator matrix of 0s and 1s (NA_real_ to avoid type convert)
        y <- matrix(NA_real_, nrow=length(y1), ncol=max(y1))
        for(i in 1:max(y1))
            y[,i] <- as.numeric(y1 == nresponse)
    }
    y
}
# glmnet family="mgaussian"
plotmo.y.mrelnet <- function(object, trace, naked, expected.len, nresponse, ...)
{
    plotmo.y.multnet(object, trace, naked, expected.len, nresponse, ...)
}
