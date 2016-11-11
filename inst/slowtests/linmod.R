# linmod.R: example S3 linear model
#
# Please see www.milbo.org/doc/modguide.pdf.
#
# This code can be used as a template for new models as follows:
# (i)  replace all occurrences of "linmod" with the new model name,
# (ii) modify linmod.fit and the method functions (predict.linmod and friends).
#
# The subset, weights and na.action arguments aren't supported.
# Multiple responses aren't supported (y must be a vector or have one column).
#
# Comprehensive tests for this code are in test.linmod.R in the plotmo package.

linmod <- function(...) UseMethod("linmod")

# internal function, not for the casual user
# for model with an intercept, first column of x must be the intercept (all 1s)

linmod.fit <- function(x = stop("no 'x' argument"),
                       y = stop("no 'y' argument"),
                       ...)
{
    stop.if.dot.arg.used(...)
    x <- check.linmod.x(x)
    y <- check.linmod.y(x, y)

    qx <- qr(x)                            # QR-decomposition of x
    if(qx$rank < ncol(x))
        stop("'x' is singular (it has ", ncol(x),
             " columns but its rank is ", qx$rank, ")")
    coef <- solve.qr(qx, y)                # compute (x'x)^(-1) x'y
    stopifnot(!anyNA(coef))                # should never happen
    df.residual <- nrow(x) - ncol(x)       # degrees of freedom
    stopifnot(df.residual > 0) # should have been caught by singular check above
    sigma2 <- sum((y - x %*% coef)^2) / df.residual # variance of residuals
    vcov <- sigma2 * chol2inv(qx$qr)       # covar mat is sigma^2 * (x'x)^(-1)
    fitted.values <- qr.fitted(qx, y)

    colnames(vcov) <- rownames(vcov) <- colnames(x)
    names(fitted.values) <- rownames(x)
    colnames(coef) <- colnames(y)

    fit <- list(coefficients  = coef,
                residuals     = y - fitted.values,
                fitted.values = fitted.values,
                vcov          = vcov,
                sigma         = sqrt(sigma2),
                df.residual   = df.residual)

    class(fit) <- "linmod"
    fit
}
linmod.default <- function(x = stop("no 'x' argument"),
                           y = stop("no 'y' argument"),
                           keep = FALSE,
                           ...)
{
    stop.if.dot.arg.used(...)
    x.original <- x
    xmat <- as.matrix(x)
    # use name "(Intercept)" here so coef names match linmod.formula
    x <- cbind("(Intercept)"=1, xmat)
    fit <- linmod.fit(x, y)
    fit$call <- match.call()
    if(keep) {
        fit$x <- xmat
        # save y as a matrix so can use colname to remember response name
        # (we already checked that it's safe to do this in check.linmod.y)
        colname <- deparse(substitute(y))[1]
        colname <- gsub(" ", "", substr(colname, 1, 100)) # strip spaces, truncate
        fit$y <- as.matrix(y, ncol=1)
        colnames(fit$y) <- colname
    }
    fit
}
linmod.formula <- function(formula = stop("no 'formula' argument"),
                           data = parent.frame(),
                           keep = FALSE,
                           ...)
{
    stop.if.dot.arg.used(...)
    # note that na.action=na.pass because we will catch NAs later
    # in linmod.fit, for uniformity with linmod.default
    mf <- model.frame(formula=formula, data=data, na.action=na.pass)
    terms <- attr(mf, "terms")
    x <- model.matrix(terms, mf)
    y <- model.response(mf)
    fit <- linmod.fit(x, y)
    fit$terms <- terms
    fit$xlevels <- .getXlevels(terms, mf) # will be used by predict.linmod
    fit$call <- match.call()
    if(keep) {
        # TODO reconsider how factors in x should be handled here
        varnames <- names(attr(terms, "dataClasses"))
        int.col <- attr(terms, "intercept") # intercept column nbr in x
        if(int.col != 0)                    # drop intercept column
            x <- x[, -int.col, drop=FALSE]  # for compat with linmod.default
        fit$x <- x # any factors will have been expanded by model.frame
        # save y as a matrix so can use colname to remember response name
        fit$y <- as.matrix(y, ncol=1)
        colnames(fit$y) <- varnames[attr(terms, "response")]
    }
    fit
}
check.linmod.x <- function(x)
{
    if(!is.matrix(x))
        stop("'x' is not a matrix or could not be converted to a matrix")
    if(NROW(x) == 0 || NCOL(x) == 0)
        stop("'x' is empty")
    if(anyNA(x))
        stop("NA in 'x'")
    # checking just the first column of x suffices because x is a matrix
    # is.logical allowed because qr etc. know how to deal with logical vars
    if(!is.numeric(x[,1]) && !is.logical(x[,1]))
        stop("non-numeric column in 'x'")
    # ensure all columns in x are named (needed for names in vcov etc.)
    # use the same naming convention as lm (prefix for unnamed cols is "V")
    missing.colnames <-
        if(is.null(colnames(x))) 1:NCOL(x)
        else                     nchar(colnames(x)) == 0
    colnames(x)[missing.colnames] <-
        c("(Intercept)",
          paste("V", seq_len(NCOL(x)-1), sep=""))[missing.colnames]
    duplicated <- which(duplicated(colnames(x)))
    if(length(duplicated))
        stop("column name \"", colnames(x)[duplicated[1]],
             "\" in 'x' is duplicated")
    x
}
check.linmod.y <- function(x, y)
{
    # as.vector(as.matrix(y)) is necessary when y is a data.frame
    # (as.vector alone on a data.frame returns a data.frame)
    y <- as.vector(as.matrix(y))
    if(length(y) == 0)
        stop("'y' is empty")
    if(anyNA(y))
        stop("NA in 'y'")
    if(!is.numeric(y) && !is.logical(y))
        stop("'y' is not numeric or logical")
    if(length(y) != nrow(x))
        stop("nrow(x) is ", nrow(x), " but length(y) is ", length(y))
    y
}
predict.linmod <- function(object = stop("no 'object' argument"),
                           newdata = NULL,
                           type = "response",
                           ...)
{
    stopifnot(inherits(object, "linmod"))
    stop.if.dot.arg.used(...)
    if(!identical(type, "response"))
        stop("the 'type' argument is not yet supported")
    if(is.null(newdata))
        yhat <- fitted(object)
    else {
        if(NROW(newdata) == 0)
            stop("'newdata' is empty")      # prevents obscure message later
        if(is.null(object$terms)) {         # model built with linmod.default?
            # columns in newdata must be in the same order as orig x
            x <- as.matrix(newdata)         # allows newdata to be a data.frame
            x <- cbind(1, x)                # intercept column
        } else                              # model built with linmod.formula
            x <- process.newdata(object, newdata)
        casenames <- rownames(x)
        # TODO The following tests suffice to catch all incorrect input,
        # but aren't ideal in that they don't always direct you to the
        # root cause of the problem.
        if(ncol(x) != length(object$coefficients))
            stop("ncol(newdata) is ", ncol(x)-1, " but should be ",
                 length(object$coefficients)-1) # -1 for intercept
        if(anyNA(x))
            stop("NA in 'newdata'")
        if(!is.numeric(x[,1]) && !is.logical(x[1,]))
            stop("non-numeric column in 'newdata' (after processing)")
        yhat <- as.vector(x %*% coef(object))
        names(yhat) <- casenames
    }
    yhat
}
process.newdata <- function(object, newdata) # for models built with linmod.formula
{
    terms <- object$terms
    dataClasses <- attr(terms, "dataClasses")
    newdata <- as.data.frame(newdata) # allows newdata to be a matrix

    # The code below preempts code in model.frame that issues
    #   Warning: 'newdata' had M rows but variables found have N rows
    # This code gives a clearer error message.
    # This check is necessary else model.frame can return bad data.
    varnames <- names(dataClasses)
    varnames <- varnames[-attr(terms, "response")]
    missing <- which(!(varnames %in% colnames(newdata)))
    if(length(missing))
        stop("variable '", varnames[missing[1]],
             "' used when building the model is not in colnames(newdata)")

    terms <- delete.response(terms)
    # na.action=na.pass because we will catch NAs after (for clearer error msg)
    newdata <- model.frame(terms, newdata, na.action=na.pass, xlev=object$xlevels)
    if(anyNA(newdata))
        stop("NA in 'newdata'")
    if(NROW(newdata) != NROW(newdata))    # paranoia, shouldn't be needed
        stop("newdata has ", NROW(newdata),
             " rows but model.frame returned ", NROW(newdata), " rows")
    .checkMFClasses(dataClasses, newdata) # check types in newdata match original data
    model.matrix(terms, newdata)
}
summary.linmod <- function(object = stop("no 'object' argument"), ...)
{
    stop.if.dot.arg.used(...)
    se <- sqrt(diag(object$vcov))
    t.value <- coef(object) / se

    coefficients <- cbind(Estimate = coef(object),
                          StdErr   = se,
                          t.value  = t.value,
                          p.value  = 2 * pt(-abs(t.value), df=object$df))

    retval <- list(call         = object$call,
                   coefficients = coefficients)

    class(retval) <- "summary.linmod"
    retval
}
print.linmod <- function(x = stop("no 'x' argument"), ...)
{
    stop.if.dot.arg.used(...)
    print.model.call(x)
    print(x$coefficients)
    invisible(x)
}
print.summary.linmod <- function(x = stop("no 'x' argument"), ...)
{
    stop.if.dot.arg.used(...)
    print.model.call(x)
    printCoefmat(x$coefficients, signif.stars=FALSE,
                 P.value=TRUE, has.Pvalue=TRUE)
    invisible(x)
}
print.model.call <- function(x)
{
    cat("Call: ") # lm has a newline here, but a space is more compact
    # use paste0 to convert vector of strings to single string if necessary
    cat(strwrap(paste0(deparse(x$call, control=NULL, nlines=5),
                       sep=" ", collapse=" "), exdent=6), sep="\n")
    cat("\n")
}
# stop.if.dot.arg.used will cause an error message if any args are passed to it.
# We use it to test if any dots arg of the calling function was used, for
# functions that must have a dots arg (to match the generic method) but don't
# actually use the dots.  This helps the user catch mistyped or illegal args.
# R version 3.3-0 or higher has a function chkDots which could be used instead.

stop.if.dot.arg.used <- function()
{
    NULL
}
