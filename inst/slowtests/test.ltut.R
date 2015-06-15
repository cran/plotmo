# test.ltut.R: test modified version of linmod example in
#              Friedrich Leisch "Creating R Packages: A Tutorial"
#
# Contains three version of linmod (grep for "### ")
#   1. original code from tutorial
#   2. minimal changes for plotmo
#   3. production version (includes error checks)

options(warn=2) # treat warnings as errors

if(!interactive())
    postscript(paper="letter")

# test that we got an error as expected from a try() call
expect.err <- function(object, expected.msg="")
{
    if(class(object)[1] == "try-error") {
        msg <- attr(object, "condition")$message[1]
        if(length(grep(expected.msg, msg, fixed=TRUE)))
            cat("Got error as expected from ",
                deparse(substitute(object)), "\n", sep="")
        else
            stop(sprintf("Expected: %s\n  Got:      %s",
                         expected.msg, substr(msg, 1, 1000)))
    } else
        stop("did not get expected error ", expected.msg)
}
check <- function(fit, ref, check.coef.names=TRUE)
{
    cat("check ", deparse(substitute(fit)), " vs ",
        deparse(substitute(ref)), "\n", sep="")

    stopifnot(coef(fit) == coef(ref))
    if(check.coef.names)
        stopifnot(names(coef(fit)) == names(coef(ref)))

    # following commented out because fit$coefficients is 3x1 and ref$coefficients is a vector.
    # Note that fit$coefficients is 3x1 to allow multiple response models in some cases, becomes 3x2.
    # stopifnot(identical(dim(fit$coefficients), dim(ref$coefficients)))
    stopifnot(length(fit$coefficients) == length(ref$coefficients))
    stopifnot(max(abs(fit$coefficients - ref$coefficients)) < 1e-10)

    # following commented out because fit$residuals is nx2 and ref$residuals is a vector
    # stopifnot(identical(dim(fit$residuals), dim(ref$residuals)))
    stopifnot(length(fit$residuals) == length(ref$residuals))
    stopifnot(max(abs(fit$residuals - ref$residuals)) < 1e-10)

    # stopifnot(identical(dim(fit$fitted.values), dim(ref$fitted.values)))
    stopifnot(length(fit$fitted.values) == length(ref$fitted.values))
    stopifnot(max(abs(fit$fitted.values - ref$fitted.values)) < 1e-10)

    if(!is.null(fit$vcov) && !is.null(ref$vcov)) {
        stopifnot(identical(dim(fit$vcov), dim(ref$vcov)))
        stopifnot(length(fit$vcov) == length(ref$vcov))
        stopifnot(max(abs(fit$vcov - ref$vcov)) < 1e-10)
    }
    # stopifnot(max(abs(fit$sigma - ref$sigma)) < 1e-10)
    stopifnot(max(abs(fit$df - ref$df)) < 1e-10)

    stopifnot(max(abs(fitted(fit) - fitted(ref))) < 1e-10)
    stopifnot(names(fitted(fit)) == names(fitted(ref)))

    stopifnot(max(abs(residuals(fit) - residuals(ref))) < 1e-10)
    stopifnot(names(residuals(fit)) == names(residuals(ref)))

    # TODO this doesn't test predict with newdata
    stopifnot(max(abs(predict(fit) - predict(ref))) < 1e-10)
    stopifnot(names(predict(fit)) == names(predict(ref)))
}
### original code from tutorial ####################################

linmodEst <- function(x, y)
{
    ## compute QR-decomposition of x
    qx <- qr(x)

    ## compute (x'x)^(-1) x'y
    coef <- solve.qr(qx, y)

    ## degrees of freedom and standard deviation of residuals
    df <- nrow(x)-ncol(x)
    sigma2 <- sum((y - x%*%coef)^2)/df

    ## compute sigma^2 * (x'x)^-1
    vcov <- sigma2 * chol2inv(qx$qr)
    colnames(vcov) <- rownames(vcov) <- colnames(x)

    list(coefficients = coef,
         vcov = vcov,
         sigma = sqrt(sigma2),
         df = df)
}
print.linmod <- function(x, ...)
{
    cat("Call:\n")
    print(x$call)
    cat("\nCoefficients:\n")
    print(x$coefficients)
}
summary.linmod <- function(object, ...)
{
    se <- sqrt(diag(object$vcov))
    tval <- coef(object) / se

    TAB <- cbind(Estimate = coef(object),
                 StdErr = se,
                 t.value = tval,
                 p.value = 2*pt(-abs(tval), df=object$df))

    res <- list(call=object$call,
                coefficients=TAB)

    class(res) <- "summary.linmod"
    res
}
print.summary.linmod <- function(x, ...)
{
    cat("Call:\n")
    print(x$call)
    cat("\n")

    printCoefmat(x$coefficients, P.value=TRUE, has.Pvalue=TRUE)
}

linmod <- function(...) { UseMethod("linmod") }

linmod.default <- function(x, y, ...)
{
    x <- as.matrix(x)
    y <- as.numeric(y)

    est <- linmodEst(x, y)

    est$fitted.values <- as.vector(x %*% est$coefficients)
    est$residuals <- y - est$fitted.values
    est$call <- match.call()

    class(est) <- "linmod"
    est
}
linmod.formula <- function(formula, data=list(), ...)
{
    mf <- model.frame(formula=formula, data=data)
    x <- model.matrix(attr(mf, "terms"), data=mf)
    y <- model.response(mf)

    est <- linmod.default(x, y, ...)
    est$call <- match.call()
    est$formula <- formula
    est
}
predict.linmod <- function(object, newdata=NULL, ...)
{
    if(is.null(newdata))
        y <- fitted(object)
    else{
        if(!is.null(object$formula)){
            ## model has been fitted using formula interface
            x <- model.matrix(object$formula, newdata)
        } else{
            x <- newdata
        }
        y <- as.vector(x %*% coef(object))
    }
    y
}

cat("==functions in the tutorial have issues with predict\n")
data(trees)
tr <- trees
rownames(tr) <- paste("tree", 1:nrow(trees), sep="")
fit1 <- linmod(Volume~., data=tr)
expect.err(try(predict(fit1, newdata=data.frame(Girth=10, Height=80))), "object 'Volume' not found")
expect.err(try(predict(fit1, newdata=as.matrix(tr[1:3,]))), "'data' must be a data.frame, not a matrix or an array")
library(plotmo)
expect.err(try(plotmo(fit1)), "object 'Volume' not found")
fit2 <- linmod(cbind(1, tr[,1:2]), tr[,3])
stopifnot(coef(fit1) == coef(fit2))
expect.err(try(predict(fit2, newdata=tr[,1:2])), "requires numeric/complex matrix/vector arguments")
expect.err(try(predict(fit2, newdata=data.frame(Girth=10, Height=80))), "requires numeric/complex matrix/vector arguments")
expect.err(try(predict(fit2, newdata=as.matrix(data.frame(Girth=10, Height=80)))), "non-conformable arguments")
expect.err(try(plotmo(fit2)), "requires numeric/complex matrix/vector arguments")

cat("==a plotmo method function can deal with the issues\n")
plotmo.predict.linmod <- function(object, newdata, ...)
{
    if(is.null(object$formula))                                # x,y interface?
        plotmo:::plotmo.predict.defaultm(object, newdata, ...) # pass matrix not data.frame
    else {
        # add dummy response column to newdata
        newdata[[as.character(as.list(object$formula)[[2]])]] <- 1
        plotmo:::plotmo.predict.default(object, newdata, ...)
    }
}
plotmo(fit1, pt.col=2, caption="fit1 with original tutorial code and plotmo.predict.linmod")
plotmo(fit2, pt.col=2, caption="fit2 with original tutorial code and plotmo.predict.linmod")
plotmo.predict.linmod <- NULL

### minimal changes version
## new version of linmod

linmod <- function(...) UseMethod("linmod")

linmod.fit <- function(x, y) # internal function, not for the casual user
{                            # first column of x is the intercept (all 1s)

    qx <- qr(x)                             # QR-decomposition of x
    coef <- solve.qr(qx, y)                 # compute (x'x)^(-1) x'y
    df.residual <- nrow(x) - ncol(x)        # degrees of freedom
    sigma2 <- sum((y - x %*% coef)^2) / df.residual  # variance of residuals
    vcov <- sigma2 * chol2inv(qx$qr)        # covar mat is sigma^2 * (x'x)^(-1)
    colnames(vcov) <- rownames(vcov) <- colnames(x)
    fitted.values <- qr.fitted(qx, y)

    fit <- list(coefficients  = coef,
                residuals     = y - fitted.values,
                fitted.values = fitted.values,
                vcov          = vcov,
                sigma         = sqrt(sigma2),
                df.residual   = df.residual)

    class(fit) <- "linmod"
    fit
}
linmod.default <- function(x, y, ...)
{
    x <- cbind("(Intercept)"=1, as.matrix(x))
    # as.matrix rather than as.vector allows multiple response models
    fit <- linmod.fit(x, as.matrix(y))
    fit$call <- match.call()
    fit
}
linmod.formula <- function(formula, data=parent.frame(), ...)
{
    mf <- model.frame(formula=formula, data=data)
    terms <- attr(mf, "terms")
    x <- model.matrix(terms, mf)
    y <- model.response(mf)
    fit <- linmod.fit(x, y)
    fit$terms <- terms
    fit$call <- match.call()
    fit
}
predict.linmod <- function(object, newdata=NULL, ...)
{
    if(is.null(newdata))
        y <- fitted(object)
    else {
        if(is.null(object$terms))              # x,y interface
            x <- cbind(1, as.matrix(newdata))  # columns must be in same order as orig x
        else {                                 # formula interface
            terms <- delete.response(object$terms)
            x <- model.matrix(terms, model.frame(terms, as.data.frame(newdata)))
        }
        y <- as.vector(x %*% coef(object))
    }
    y
}
cat("==with the new version we are ok\n")
fit1.form <- linmod(Volume~., data=tr)
cat("==print.default(fit1.form)\n")
print.default(fit1.form)
stopifnot(abs(predict(fit1.form, newdata=data.frame(Girth=10, Height=80)) - 16.234045) < 1e-5)
stopifnot(sum(abs(predict(fit1.form, newdata=as.matrix(tr[1:3,])) - c(4.8376597, 4.5538516, 4.8169813))) < 1e-5)

lm.tr <- lm(Volume~., data=tr)
check(fit1.form, lm.tr)

fit1.mat <- linmod(tr[,1:2], tr[,3]) # note no need for intercept term
cat("==print.default(fit1.mat)\n")
print.default(fit1.form)

stopifnot(abs(predict(fit1.mat, newdata=data.frame(Girth=10, Height=80)) - 16.234045) < 1e-5)
stopifnot(sum(abs(predict(fit1.mat, newdata=tr[1:3,1:2]) - c(4.8376597, 4.5538516, 4.8169813))) < 1e-5)
stopifnot(abs(predict(fit1.mat, newdata=as.matrix(data.frame(Girth=10, Height=80))) - 16.234045) < 1e-5)
check(fit1.mat, lm.tr)

cat("==example plots\n")

library(plotmo)
data(trees)

fit1.form <- linmod(Volume~., data=trees)
print(fit1.form)
print(summary(fit1.form))

fit1.mat <- linmod(trees[,1:2], trees[,3])
print(fit1.mat)
print(summary(fit1.mat))

plotmo(fit1.form)
plotmo(fit1.mat)

plotres(fit1.form)
plotres(fit1.mat)

cat("==test model building with different numeric args\n")

x <- tr[,1:2]
y <- tr[,3]
fit2.mat <- linmod(x, y)
check(fit2.mat, lm.tr)

# check consistency with lm
expect.err(try(linmod(y~x)), "invalid type (list) for variable 'x'")
expect.err(try(lm(y~x)), "invalid type (list) for variable 'x'")

fit3.mat <- linmod(as.matrix(x), as.matrix(y))
check(fit3.mat, lm.tr)

fit4.form <- linmod(y ~ as.matrix(x))
lm4 <- linmod(y ~ as.matrix(x))
check(fit4.form, lm4)
stopifnot(coef(fit4.form)  == coef(lm.tr),
          gsub("as.matrix(x)", "", names(coef(fit4.form)), fixed=TRUE)  == names(coef(lm.tr)))

xm <- as.matrix(x)
fit5.form <- linmod(y ~ xm)
lm5 <- linmod(y ~ xm)
check(fit5.form, lm5)
stopifnot(coef(fit5.form)  == coef(lm.tr),
          gsub("xm", "", names(coef(fit5.form)), fixed=TRUE)  == names(coef(lm.tr)))

cat("==test correct use of global x1 and y1\n")
x1 <- tr[,1]
y1 <- tr[,3]
lm1 <- linmod(y1~x1)

fit6.mat <- linmod(x1, y1)
check(fit6.mat, lm1, check.coef.names=FALSE)
# production version only:
# stopifnot(coef(fit6.mat) == coef(lm1),
#           names(coef(fit6.mat)) == c("(Intercept)", "V1")) # names(coef(lm1) are "(Intercept)" "x1"

fit6.form <- linmod(y1~x1)
check(fit6.form, lm1)

cat("==check integer input (sibsp is an integer) \n")

library(earth)
data(etitanic)
tit <- etitanic[seq(1, nrow(etitanic), by=60), ] # small set of data for tests (18 cases)
tit$survived <- tit$survived != 0 # convert to logical
rownames(tit) <- paste("pas", 1:nrow(tit), sep="")
cat(paste(colnames(tit), "=", sapply(tit, class), sep="", collapse=", "), "\n")

fit7.mat <- linmod(tit$age, tit$sibsp)
lm7 <- lm.fit(cbind(1, tit$age), tit$sibsp)
stopifnot(coef(fit7.mat) == coef(lm7))# coef names will differ

fit7.form <- linmod(sibsp~age, data=tit)
lm7.form  <- lm(sibsp~age, data=tit)
check(fit7.form, lm7.form)

fit8.mat <- linmod(tit$sibsp, tit$age)
lm8 <- lm.fit(cbind(1, tit$sibsp), tit$age)
stopifnot(coef(fit8.mat) == coef(lm8))# coef names will differ

fit8.form <- linmod(age~sibsp, data=tit)
lm8.form  <- lm(age~sibsp, data=tit)
check(fit8.form, lm8.form)

cat("==test model building with different non numeric args\n")

library(earth)
data(etitanic)
tit <- etitanic[seq(1, nrow(etitanic), by=60), ] # small set of data for tests (18 cases)
tit$survived <- tit$survived != 0 # convert to logical
rownames(tit) <- paste("pas", 1:nrow(tit), sep="")
cat(paste(colnames(tit), "=", sapply(tit, class), sep="", collapse=", "), "\n")

lm9 <- lm(survived~., data=tit)
fit9.form <- linmod(survived~., data=tit)
check(fit9.form, lm9)

options(warn=2) # treat warnings as errors
expect.err(try(linmod(tit[,c(1,3,4,5,6)], tit[,"survived"])), "NAs introduced by coercion")
options(warn=1)
expect.err(try(linmod(tit[,c(1,3,4,5,6)], tit[,"survived"])), "NA/NaN/Inf in foreign function call (arg 1)")

options(warn=2) # treat warnings as errors
expect.err(try(lm(pclass~., data=tit)), "using type = \"numeric\" with a factor response will be ignored")
# minimal version
expect.err(try(linmod(pclass~., data=tit)), "(converted from warning) NAs introduced by coercion")
expect.err(try(linmod(tit$pclass, tit$survived)), "(converted from warning) NAs introduced by coercion")
# # production version
# expect.err(try(linmod(pclass~., data=tit)), "y is not numeric or logical")
options(warn=1)

lm10 <- lm(pclass~., data=tit) # will give warnings
fit10.form <- linmod(as.numeric(pclass)~., data=tit)
stopifnot(coef(fit10.form) == coef(lm10))
stopifnot(names(coef(fit10.form)) == names(coef(lm10)))
# check(fit10.form, lm10) # fails because lm10 fitted is all NA

# production version: (minimal version just gives warnings and builds lousy model)
# expect.err(try(linmod(pclass~., data=tit)), "y is not numeric or logical")
# expect.err(try(linmod(tit[,-1], tit[,1])), "y is not numeric or logical")
# expect.err(try(linmod(1:10, paste(1:10))), "y is not numeric or logical")

fit10a.form <- linmod(survived~pclass, data=tit)
lm10a <- lm(survived~pclass, data=tit)
check(fit10a.form, lm10a)

expect.err(try(linmod(paste(1:10), 1:10)), "requires numeric/complex matrix/vector arguments")

lm11 <- lm(as.numeric(pclass)~., data=tit)
fit11.form <- linmod(as.numeric(pclass)~., data=tit)
check(fit10.form, lm11)

cat("==data.frame with strings\n")

df.with.string <-
    data.frame(1:5,
               c(1,2,-1,4,5),
               c("a", "b", "a", "a", "b"),
               stringsAsFactors=FALSE)
colnames(df.with.string) <- c("num1", "num2", "string")

fit30.form <- linmod(num1~num2, df.with.string)
lm30       <- lm(num1~num2, df.with.string)
check(fit30.form, lm30)

fit31.form <- linmod(num1~., df.with.string)
lm31       <- lm(num1~., df.with.string)
check(fit31.form, lm31)

# minimal version
expect.err(try(linmod(string~., df.with.string)), "non-numeric argument to binary operator")
# production version
# expect.err(try(linmod(string~., df.with.string)), "y is not numeric or logical")

vec <- c(1,2,3,4,3)
options(warn=2) # treat warnings as errors
expect.err(try(linmod(df.with.string, vec)), "NAs introduced by coercion")
options(warn=1)
# minimal version
expect.err(try(linmod(df.with.string, vec)), "NA/NaN/Inf in foreign function call (arg 1)")
# production version
# expect.err(try(linmod(df.with.string, vec)), "NA in x")

options(warn=2) # treat warnings as errors
expect.err(try(linmod(df.with.string, vec)), "NAs introduced by coercion")
options(warn=1)
# minimal version
expect.err(try(linmod(df.with.string, vec)), "NA/NaN/Inf in foreign function call (arg 1)")
# production version
# expect.err(try(linmod(df.with.string, vec)), "NA in x")

cat("==more variables  than cases\n")

set.seed(1)
x2 <- matrix(rnorm(6), nrow=2)
y2 <- c(1,2)
# production version
# expect.err(try(linmod(y2~x2)), "more variables than cases")
# minimal version
expect.err(try(linmod(y2~x2)), "'size' cannot exceed nrow(x) = 2")

x3 <- matrix(1:10, ncol=2)
y3 <- c(1,2,9,4,5)
# TODO following gives a lousy error message (should be "columns of x are collinear")
expect.err(try(linmod(y3~x3)), "singular matrix 'a' in 'solve'")

cat("==multiple response model\n")
# TODO multiple response models not fully tested

y.2 <- cbind(c(1,2,3,4,9), vec^2)
fit100.mat <- linmod(vec, y.2)
lm100 <- lm.fit(cbind(1, vec), y.2)
stopifnot(coef(fit100.mat) == coef(lm100))
# stopifnot(names(coef(fit100.mat)) == names(coef(lm100)))

### production version of linmod ###################################

linmod <- function(...) UseMethod("linmod")

linmod.fit <- function(x, y) # internal function, not for the casual user
{                            # first column of x is the intercept
    if(anyNA(x))
        stop("NA in x")
    if(!is.numeric(x[[1]]) && !is.logical(x[[1]]))
        stop("non-numeric column in x")
    if(anyNA(y))
        stop("NA in y")
    if(!is.numeric(y[[1]]) && !is.logical(y[[1]]))
        stop("y is not numeric or logical")

    # ensure all columns in x are named (needed for names in vcov etc.)
    missing.colnames <-
        if(is.null(colnames(x))) 1:NCOL(x)
        else                     nchar(colnames(x)) == 0
    colnames(x)[missing.colnames] <-
        c("(Intercept)",
          paste("V", seq_len(NCOL(x)-1), sep=""))[missing.colnames]

    qx <- qr(x)                            # QR-decomposition of x
    if(qx$rank < ncol(x))
        stop("x is singular")
    coef <- solve.qr(qx, y)                # compute (x'x)^(-1) x'y
    df.residual <- nrow(x) - ncol(x)       # degrees of freedom
    # TODO consider issuing a warning here if df.residual is 0
    sigma2 <- sum((y - x %*% coef)^2) / df.residual # stddev of residuals
    vcov <- sigma2 * chol2inv(qx$qr)       # covar mat is sigma^2 * (x'x)^-1
    fitted.values <- qr.fitted(qx, y)

    colnames(vcov) <- rownames(vcov) <- colnames(x)
    names(fitted.values) <- rownames(x)
    if(NCOL(coef) > 1)                     # multiple response model
        colnames(coef) <- colnames(x)
    else
        names(coef) <- colnames(x)

    fit <- list(coefficients  = coef,
                residuals     = y - fitted.values,
                fitted.values = fitted.values,
                vcov          = vcov,
                sigma         = sqrt(sigma2),
                df.residual   = df.residual)

    class(fit) <- "linmod"
    fit
}
linmod.default <- function(x, y, ...)
{
    x <- cbind("(Intercept)"=1, as.matrix(x))
    fit <- linmod.fit(x, as.matrix(y)) # as.matrix rather than as.vector allows multiple response models
    fit$call <- match.call()
    fit
}
linmod.formula <- function(formula, data=parent.frame(), ...)
{
    mf <- model.frame(formula=formula, data=data)
    terms <- attr(mf, "terms")
    x <- model.matrix(terms, mf)
    y <- model.response(mf)

    fit <- linmod.fit(x, y)
    fit$terms <- terms
    fit$call <- match.call()
    fit
}
predict.linmod <- function(object, newdata=NULL, ...)
{
    if(is.null(newdata))
        y <- fitted(object)
    else{
        if(is.null(object$terms)) {                 # x,y interface
            x <- cbind(1, as.matrix(newdata))       # columns must be in the same order as orig x
        } else {                                    # formula interface
            terms <- delete.response(object$terms)
            # Note that the following code can issue quite obscure
            # error messages for bad newdata.  For example
            #     predict(obj, newdata=1:3)
            # causes
            #     eval(expr, envir, enclos) : object 'varname' not found
            x <- model.matrix(terms, model.frame(terms, as.data.frame(newdata)))
        }
        # The following tests suffice to catch all incorrect input (I believe),
        # but aren't ideal in that they don't always lead you to the cause of
        # the problem.  For example, strings in newdata that get converted to
        # factors by model.matrix can cause the wrong number of columns in x.
        if(ncol(x) != length(object$coefficients))
            stop("ncol(x) is ", ncol(x), " but should be ",
                 length(object$coefficients)-1) # -1 for intercept
        if(anyNA(x))
            stop("NA in x")
        if(!is.numeric(x[[1]]) && !is.logical(x[[1]]))
            stop("non-numeric column in x")
        y <- as.vector(x %*% coef(object))
    }
    y
}
cat("==with the production version we are ok\n")
fit1.form <- linmod(Volume~., data=tr)
stopifnot(abs(predict(fit1.form, newdata=data.frame(Girth=10, Height=80)) - 16.234045) < 1e-5)
stopifnot(sum(abs(predict(fit1.form, newdata=as.matrix(tr[1:3,])) - c(4.8376597, 4.5538516, 4.8169813))) < 1e-5)

lm.tr <- lm(Volume~., data=tr)
check(fit1.form, lm.tr)

fit1.mat <- linmod(tr[,1:2], tr[,3]) # note no need for intercept term

stopifnot(abs(predict(fit1.mat, newdata=data.frame(Girth=10, Height=80)) - 16.234045) < 1e-5)
stopifnot(sum(abs(predict(fit1.mat, newdata=tr[1:3,1:2]) - c(4.8376597, 4.5538516, 4.8169813))) < 1e-5)
stopifnot(abs(predict(fit1.mat, newdata=as.matrix(data.frame(Girth=10, Height=80))) - 16.234045) < 1e-5)
check(fit1.mat, lm.tr)

cat("==example plots\n")

library(plotmo)
data(trees)

fit1.form <- linmod(Volume~., data=trees)
print(fit1.form)
print(summary(fit1.form))

fit1.mat <- linmod(trees[,1:2], trees[,3])
print(fit1.mat)
print(summary(fit1.mat))

plotmo(fit1.form)
plotmo(fit1.mat)

plotres(fit1.form)
plotres(fit1.mat)

cat("==test model building with different numeric args\n")

x <- tr[,1:2]
y <- tr[,3]
fit2.mat <- linmod(x, y)
check(fit2.mat, lm.tr)

# check consistency with lm
expect.err(try(linmod(y~x)), "invalid type (list) for variable 'x'")
expect.err(try(lm(y~x)), "invalid type (list) for variable 'x'")

fit3.mat <- linmod(as.matrix(x), as.matrix(y))
check(fit3.mat, lm.tr)

fit4.form <- linmod(y ~ as.matrix(x))
lm4 <- linmod(y ~ as.matrix(x))
check(fit4.form, lm4)
stopifnot(coef(fit4.form)  == coef(lm.tr),
          gsub("as.matrix(x)", "", names(coef(fit4.form)), fixed=TRUE)  == names(coef(lm.tr)))

xm <- as.matrix(x)
fit5.form <- linmod(y ~ xm)
lm5 <- linmod(y ~ xm)
check(fit5.form, lm5)
stopifnot(coef(fit5.form)  == coef(lm.tr),
          gsub("xm", "", names(coef(fit5.form)), fixed=TRUE)  == names(coef(lm.tr)))

cat("==test correct use of global x1 and y1\n")
x1 <- tr[,1]
y1 <- tr[,3]
lm1 <- linmod(y1~x1)

fit6.mat <- linmod(x1, y1)
check(fit6.mat, lm1, check.coef.names=FALSE)
# production version only:
stopifnot(coef(fit6.mat) == coef(lm1),
          names(coef(fit6.mat)) == c("(Intercept)", "V1")) # names(coef(lm1) are "(Intercept)" "x1"

fit6.form <- linmod(y1~x1)
check(fit6.form, lm1)

cat("==check integer input (sibsp is an integer) \n")

library(earth)
data(etitanic)
tit <- etitanic[seq(1, nrow(etitanic), by=60), ] # small set of data for tests (18 cases)
tit$survived <- tit$survived != 0 # convert to logical
rownames(tit) <- paste("pas", 1:nrow(tit), sep="")
cat(paste(colnames(tit), "=", sapply(tit, class), sep="", collapse=", "), "\n")

fit7.mat <- linmod(tit$age, tit$sibsp)
lm7 <- lm.fit(cbind(1, tit$age), tit$sibsp)
stopifnot(coef(fit7.mat) == coef(lm7))# coef names will differ

fit7.form <- linmod(sibsp~age, data=tit)
lm7.form  <- lm(sibsp~age, data=tit)
check(fit7.form, lm7.form)

fit8.mat <- linmod(tit$sibsp, tit$age)
lm8 <- lm.fit(cbind(1, tit$sibsp), tit$age)
stopifnot(coef(fit8.mat) == coef(lm8))# coef names will differ

fit8.form <- linmod(age~sibsp, data=tit)
lm8.form  <- lm(age~sibsp, data=tit)
check(fit8.form, lm8.form)

cat("==test model building with different non numeric args\n")

library(earth)
data(etitanic)
tit <- etitanic[seq(1, nrow(etitanic), by=60), ] # small set of data for tests (18 cases)
tit$survived <- tit$survived != 0 # convert to logical
rownames(tit) <- paste("pas", 1:nrow(tit), sep="")
cat(paste(colnames(tit), "=", sapply(tit, class), sep="", collapse=", "), "\n")

lm9 <- lm(survived~., data=tit)
fit9.form <- linmod(survived~., data=tit)
check(fit9.form, lm9)

expect.err(try(linmod(tit[,c(1,3,4,5,6)], tit[,"survived"])),
           "non-numeric column in x")
fit9a.form <- lm.fit(data.matrix(cbind("(Intercept)"=1, tit[,c(1,3,4,5,6)])), tit[,"survived"])
lm9 <- lm.fit(data.matrix(cbind("(Intercept)"=1, tit[,c(1,3,4,5,6)])), tit[,"survived"])
stopifnot(coef(fit9a.form) == coef(lm9))
stopifnot(names(coef(fit9a.form)) == names(coef(lm9)))

options(warn=2) # treat warnings as errors
expect.err(try(lm(pclass~., data=tit)), "using type = \"numeric\" with a factor response will be ignored")
expect.err(try(linmod(pclass~., data=tit)), "y is not numeric or logical")
options(warn=1)

lm10 <- lm(pclass~., data=tit) # will give warnings
fit10.form <- linmod(as.numeric(pclass)~., data=tit)
stopifnot(coef(fit10.form) == coef(lm10))
stopifnot(names(coef(fit10.form)) == names(coef(lm10)))
# check(fit10.form, lm10) # fails because lm10 fitted is all NA

expect.err(try(linmod(pclass~., data=tit)), "y is not numeric or logical")
expect.err(try(linmod(tit[,-1], tit[,1])), "non-numeric column in x")
expect.err(try(linmod(1:10, paste(1:10))), "y is not numeric or logical")

fit10a.form <- linmod(survived~pclass, data=tit)
lm10a <- lm(survived~pclass, data=tit)
check(fit10a.form, lm10a)

expect.err(try(linmod(tit[,"pclass"], tit[,"age"])), "non-numeric column in x")

expect.err(try(linmod(paste(1:10), 1:10)), "non-numeric column in x")

lm11 <- lm(as.numeric(pclass)~., data=tit)
fit11.form <- linmod(as.numeric(pclass)~., data=tit)
check(fit10.form, lm11)

cat("==data.frame with strings\n")

df.with.string <-
    data.frame(1:5,
               c(1,2,-1,4,5),
               c("a", "b", "a", "a", "b"),
               stringsAsFactors=FALSE)
colnames(df.with.string) <- c("num1", "num2", "string")

fit30.form <- linmod(num1~num2, df.with.string)
lm30       <- lm(num1~num2, df.with.string)
check(fit30.form, lm30)

fit31.form <- linmod(num1~., df.with.string)
lm31       <- lm(num1~., df.with.string)
check(fit31.form, lm31)

expect.err(try(linmod(string~., df.with.string)), "y is not numeric or logical")

vec <- c(1,2,3,4,3)
expect.err(try(linmod(df.with.string, vec)), "non-numeric column in x")
expect.err(try(linmod(tit$pclass, tit$survived)), "non-numeric column in x")

cat("==x is singular\n")

set.seed(1)
x2 <- matrix(rnorm(6), nrow=2)
y2 <- c(1,2)
expect.err(try(linmod(y2~x2)), "x is singular")

x3 <- matrix(1:10, ncol=2)
y3 <- c(1,2,9,4,5)
expect.err(try(linmod(y3~x3)), "x is singular")

cat("==NA in x\n")

x <- tr[,1:2]
y <- tr[,3]
x[2,2] <- NA
expect.err(try(linmod(x, y)), "NA in x")

x <- tr[,1:2]
y <- tr[,3]
y[9] <- NA
expect.err(try(linmod(x, y)), "NA in y")

cat("==misc tests with different kinds of data\n")

data3 <- data.frame(s=c("a", "b", "c", "a", "a"), num=c(1,9,4,2,6), y=c(1,2,3,5,3), stringsAsFactors=F)
stopifnot(sapply(data3, class) == c("character", "numeric", "numeric"))
a40 <- linmod(y~., data=data3)
stopifnot(sum(abs(a40$coef - c(2.571, -1.857, -0.143, 0.143))) < 0.001)
stopifnot(sum(abs(predict(a40, newdata=data3[1:3,]) - c(2.71429, 2.00000, 3.00000))) < 0.001)

data4 <- data.frame(s=c("a", "b", "c", "a", "a"), num=c(1,9,4,2,6), y=c(1,2,3,5,3), stringsAsFactors=T)
stopifnot(sapply(data4, class) == c("factor", "numeric", "numeric"))
expect.err(try(linmod(data4[,1:2], data4[,3])), "non-numeric column in x")

# following gives no error (and matches lm)
a41 <- linmod(y~., data=data4)
stopifnot(sum(abs(predict(a41, newdata=data3[1:3,]) - c(2.71429, 2.00000, 3.00000))) < 0.001)

data5 <- data.frame(s=c("a", "b", "c", "a", "a"), num=c(1,9,4,2,6), y=c(1,2,3,5,3), stringsAsFactors=F)
stopifnot(sum(abs(predict(a41, newdata=data5[1:3,1:2]) - c(2.71429, 2.00000, 3.00000))) < 0.001)

data6 <- data.frame(s=c("a", "b", "c", "a9", "a"), num=c(1,9,4,2,6), y=c(1,2,3,5,3), stringsAsFactors=T)
expect.err(try(predict(a41, newdata=data6[1:3,1:2])), "ncol(x) is 5 but should be 3")

expect.err(try(predict(a41, newdata=1:9)), "object 's' not found") # issued by model.matrix in predict.linmod

cat("==multiple response model\n")
# TODO multiple response models not fully tested

y.2 <- cbind(c(1,2,3,4,9), vec^2)
fit100.mat <- linmod(vec, y.2)
lm100 <- lm.fit(cbind(1, vec), y.2)
stopifnot(coef(fit100.mat) == coef(lm100))
# stopifnot(names(coef(fit100.mat)) == names(coef(lm100)))

if(!interactive()) {
    dev.off()        # finish postscript plot
    q(runLast=FALSE) # needed else R prints the time on exit (R2.5 and higher) which messes up the diffs
}
