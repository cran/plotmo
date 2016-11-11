# test.linmod.R: test example S3 model in linmod.R

options(warn=2) # treat warnings as errors
set.seed(2016)
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
almost.equal <- function(x, y, max=1e-8)
{
    stopifnot(max >= 0 && max < .01)
    length(x) == length(y) && max(abs(x - y)) < max
}
# check that fit model matches ref lm model in all essential details
check.lm <- function(fit, ref, newdata=trees[3:5,],
                     check.coef.names=TRUE,
                     check.casenames=TRUE,
                     check.newdata=TRUE)
{
    check.names <- function(fit.names, ref.names)
    {
        if(check.casenames &&
        # lm always adds rownames even if "1", "2", "3": this seems
        # wasteful and not particulary helpful, so linmod doesn't do
        # this, hence the first !isTRUE(all.equal) below
           !isTRUE(all.equal(ref.names, paste(1:length(ref.names)))) &&
           !isTRUE(all.equal(fit.names, ref.names))) {
            print(fit.names)
            print(ref.names)
            stop(deparse(substitute(fit.names)), " != ",
                 deparse(substitute(ref.names)))
        }
    }
    cat("check ", deparse(substitute(fit)), " vs ",
        deparse(substitute(ref)), "\n", sep="")

    stopifnot(coef(fit) == coef(ref))
    if(check.coef.names)
        stopifnot(identical(names(coef(fit)), names(coef(ref))))

    stopifnot(identical(dim(fit$coefficients), dim(ref$coefficients)))
    stopifnot(length(fit$coefficients) == length(ref$coefficients))
    stopifnot(almost.equal(fit$coefficients, ref$coefficients))

    stopifnot(identical(dim(fit$residuals), dim(ref$residuals)))
    stopifnot(length(fit$residuals) == length(ref$residuals))
    stopifnot(almost.equal(fit$residuals, ref$residuals))

    stopifnot(identical(dim(fit$fitted.values), dim(ref$fitted.values)))
    stopifnot(length(fit$fitted.values) == length(ref$fitted.values))
    stopifnot(almost.equal(fit$fitted.values, ref$fitted.values))

    if(!is.null(fit$vcov) && !is.null(ref$vcov)) {
        stopifnot(identical(dim(fit$vcov), dim(ref$vcov)))
        stopifnot(length(fit$vcov) == length(ref$vcov))
        stopifnot(almost.equal(fit$vcov, ref$vcov))
    }
    ref.sigma <- ref$sigma
    if(is.null(ref.sigma)) # in lm models, sigma is only available from summary()
        ref.sigma <- summary(ref)$sigma
    stopifnot(almost.equal(fit$sigma, ref.sigma))

    stopifnot(almost.equal(fit$df, ref$df))

    stopifnot(almost.equal(fitted(fit), fitted(ref)))
    check.names(names(fitted(fit)), names(fitted(ref)))

    stopifnot(almost.equal(residuals(fit), residuals(ref)))
    check.names(names(residuals(fit)), names(residuals(ref)))

    stopifnot(almost.equal(predict(fit), predict(ref)))
    check.names(names(predict(fit)), names(predict(ref)))
    if(check.newdata) {
        stopifnot(almost.equal(predict(fit, newdata=newdata),
                               predict(ref, newdata=newdata)))
        check.names(names(predict(fit, newdata=newdata)),
                    names(predict(ref, newdata=newdata)))
    }
}
source("linmod.R")

tr <- trees # trees data but with rownames
rownames(tr) <- paste("tree", 1:nrow(trees), sep="")

cat("==check that example issues with functions in the tutorial have gone\n")
fit1.form <- linmod(Volume~., data=tr)
cat("==print(summary(fit1.form))\n")
print(summary(fit1.form))
lm.tr <- lm(Volume~., data=tr)
check.lm(fit1.form, lm.tr)
stopifnot(almost.equal(predict(fit1.form, newdata=data.frame(Girth=10, Height=80)),
                       16.234045, max=1e-5))
stopifnot(almost.equal(predict(fit1.form, newdata=as.matrix(tr[1:3,])),
                       c(4.8376597, 4.5538516, 4.8169813), max=1e-5))

fit1.mat <- linmod(tr[,1:2], tr[,3]) # note no need for intercept term
cat("==print(summary(fit1.mat))\n")
print(summary(fit1.mat))
check.lm(fit1.mat, lm.tr, newdata=trees[3:5,1:2])
stopifnot(almost.equal(predict(fit1.mat, newdata=data.frame(Girth=10, Height=80)),
                       16.234045, max=1e-5))
stopifnot(almost.equal(predict(fit1.mat, newdata=tr[1:3,1:2]),
                       c(4.8376597, 4.5538516, 4.8169813), max=1e-5))
stopifnot(almost.equal(predict(fit1.mat, newdata=as.matrix(data.frame(Girth=10, Height=80))),
                       16.234045, max=1e-5))

cat("==print.default(fit1.form)\n")
print.default(fit1.form)

cat("==check single x variable\n")
fit1a.form <- linmod(Volume~Height, data=tr)
cat("==print(summary(fit1a.form))\n")
print(summary(fit1a.form))
lma.tr <- lm(Volume~Height, data=tr)
check.lm(fit1a.form, lma.tr)

stopifnot(almost.equal(predict(fit1a.form, newdata=data.frame(Height=80)),
                       36.34437, max=1e-5))
stopifnot(almost.equal(predict(fit1a.form, newdata=data.frame(Girth=99, Height=80)),
                       36.34437, max=1e-5))
stopifnot(almost.equal(predict(fit1a.form, newdata=as.matrix(tr[1:3,])),
                       c(20.91087, 13.19412, 10.10742), max=1e-5))

fit1a.mat <- linmod(tr[,2,drop=FALSE], tr[,3])
cat("==print(summary(fit1a.mat))\n")
print(summary(fit1a.mat))
check.lm(fit1a.mat, lma.tr, newdata=trees[3:5,2,drop=FALSE])
check.lm(fit1a.mat, lma.tr, newdata=trees[3:5,2,drop=TRUE],
         check.newdata=FALSE) # needed because predict.lm gives numeric 'envir' arg not of length one
stopifnot(almost.equal(predict(fit1a.mat, newdata=trees[3:5,2,drop=FALSE]),
                       predict(fit1a.mat, newdata=trees[3:5,2,drop=TRUE])))
stopifnot(almost.equal(predict(fit1a.mat, newdata=data.frame(Height=80)),
                       36.34437, max=1e-5))
stopifnot(almost.equal(predict(fit1a.mat, newdata=tr[1:3,2]),
                       c(20.91087, 13.19412, 10.10742), max=1e-5))
stopifnot(almost.equal(predict(fit1a.mat, newdata=as.matrix(data.frame(Height=80))),
                       36.34437, max=1e-5))

# check that rownames got propagated
stopifnot(names(fit1.form$residuals)[1] == "tree1")
stopifnot(names(fit1.form$fitted.values)[3] == "tree3")
stopifnot(names(fit1.mat$residuals)[1] == "tree1")
stopifnot(names(fit1.mat$fitted.values)[3] == "tree3")
stopifnot(!is.null(names(fit1.mat$residuals)))
stopifnot(!is.null(names(fit1.mat$fitted.values)))
cat("==print.default(fit1.mat)\n")
print.default(fit1.mat)

# check that we don't artificially add rownames when no original rownames
fit1a.mat <- linmod(trees[,1:2], trees[,3])
stopifnot(is.null(names(fit1a.mat$residuals)))
stopifnot(is.null(names(fit1a.mat$fitted.values)))

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

cat("==test keep arg\n")

fit.form.keep <- linmod(Volume~., data=trees, keep=TRUE)
print(summary(fit.form.keep))
print(head(fit.form.keep$x))
stopifnot(dim(fit.form.keep$x) == c(nrow(trees), 2))
stopifnot(class(fit.form.keep$x) == "matrix")
print(head(fit.form.keep$y))
stopifnot(dim(fit.form.keep$y) == c(nrow(trees), 1))
stopifnot(class(fit.form.keep$y) == "matrix")
fit.form.keep$call <- NULL # trick to force use of x and y in plotmo
plotmo(fit.form.keep, pt.col=3)
plotres(fit.form.keep)

fit.mat.keep <- linmod(trees[,1:2], trees[,3], keep=TRUE)
print(summary(fit.mat.keep))
print(head(fit.mat.keep$x))
stopifnot(dim(fit.mat.keep$x) == c(nrow(trees), 2))
stopifnot(class(fit.mat.keep$x) == "matrix")
print(head(fit.mat.keep$y))
stopifnot(dim(fit.mat.keep$y) == c(nrow(trees), 1))
stopifnot(class(fit.mat.keep$y) == "matrix")
fit.mat.keep$call <- NULL # trick to force use of x and y in plotmo
plotmo(fit.mat.keep, pt.col=3)
plotres(fit.mat.keep)

check.lm(fit.form.keep, fit.mat.keep, check.casenames=FALSE, check.newdata=FALSE)

# check consistency in the way x and y are kept
stopifnot(all(dim(fit.mat.keep$x) == dim(fit.form.keep$x)))
stopifnot(class(fit.mat.keep$x)   == class(fit.form.keep$x))
stopifnot(all(dim(fit.mat.keep$x) == dim(fit.form.keep$x)))
stopifnot(class(fit.mat.keep$y)   == class(fit.form.keep$y))

cat("==test keep arg with vector x\n")

n <- 20
fit.vecx.form.keep <- linmod(Volume~Height, data=trees[1:n,], keep=TRUE)
print(summary(fit.vecx.form.keep))
print(head(fit.vecx.form.keep$x))
stopifnot(dim(fit.vecx.form.keep$x) == c(n, 1))
stopifnot(class(fit.vecx.form.keep$x) == "matrix")
print(head(fit.vecx.form.keep$y))
stopifnot(dim(fit.vecx.form.keep$y) == c(n, 1))
stopifnot(class(fit.vecx.form.keep$y) == "matrix")
fit.vecx.form.keep$call <- NULL # trick to force use of x and y in plotmo
plotmo(fit.vecx.form.keep, pt.col=3)
plotres(fit.vecx.form.keep)

fit.vecx.mat.keep <- linmod(trees[1:n,2], trees[1:n,3], keep=TRUE)
print(summary(fit.vecx.mat.keep))
print(head(fit.vecx.mat.keep$x))
stopifnot(dim(fit.vecx.mat.keep$x) == c(n, 1))
stopifnot(class(fit.vecx.mat.keep$x) == "matrix")
print(head(fit.vecx.mat.keep$y))
stopifnot(dim(fit.vecx.mat.keep$y) == c(n, 1))
stopifnot(class(fit.vecx.mat.keep$y) == "matrix")
fit.vecx.mat.keep$call <- NULL # trick to force use of x and y in plotmo
plotmo(fit.vecx.mat.keep, pt.col=3)
plotres(fit.vecx.mat.keep)

check.lm(fit.vecx.form.keep, fit.vecx.mat.keep, newdata=trees[3:5,2,drop=FALSE],
         check.coef.names=FALSE, check.casenames=FALSE)

# check consistency in the way x and y are kept
stopifnot(class(fit.vecx.mat.keep$x)   == class(fit.vecx.form.keep$x))
stopifnot(all(dim(fit.vecx.mat.keep$x) == dim(fit.vecx.form.keep$x)))
stopifnot(all(fit.vecx.mat.keep$x == fit.vecx.form.keep$x))
stopifnot(class(fit.vecx.mat.keep$y)   == class(fit.vecx.form.keep$y))
stopifnot(all(dim(fit.vecx.mat.keep$x) == dim(fit.vecx.form.keep$x)))
stopifnot(all(fit.vecx.mat.keep$x == fit.vecx.form.keep$x))

cat("==test no intercept model\n")
fit.noint <- linmod(Volume~.-1, data=trees)
print(summary(fit.noint))
lm.noint <- lm(Volume~.-1, data=trees)
check.lm(fit.noint, lm.noint)

fit.noint.keep <- linmod(Volume~.-1, data=trees, keep=TRUE)
print(summary(fit.noint.keep))
check.lm(fit.noint, lm.noint)
stopifnot(class(fit.noint.keep$x)   == class(fit.form.keep$x))
stopifnot(all(dim(fit.noint.keep$x) == dim(fit.form.keep$x)))
stopifnot(all(fit.noint.keep$x == fit.form.keep$x))
stopifnot(class(fit.noint.keep$y)   == class(fit.form.keep$y))
stopifnot(all(dim(fit.noint.keep$x) == dim(fit.form.keep$x)))
stopifnot(all(fit.noint.keep$x == fit.form.keep$x))

cat("==test model building with different numeric args\n")

x <- tr[,1:2]
y <- tr[,3]
fit2.mat <- linmod(x, y)
check.lm(fit2.mat, lm.tr, newdata=trees[3:5,1:2])

# check consistency with lm
expect.err(try(linmod(y~x)), "invalid type (list) for variable 'x'")
expect.err(try(lm(y~x)), "invalid type (list) for variable 'x'")

fit3.mat <- linmod(as.matrix(x), as.matrix(y))
check.lm(fit3.mat, lm.tr, newdata=trees[3:5,1:2])

fit4.form <- linmod(y ~ as.matrix(x))
lm4 <- linmod(y ~ as.matrix(x))
check.lm(fit4.form, lm4, check.newdata=FALSE)
stopifnot(coef(fit4.form)  == coef(lm.tr),
          gsub("as.matrix(x)", "", names(coef(fit4.form)), fixed=TRUE)  == names(coef(lm.tr)))

xm <- as.matrix(x)
fit5.form <- linmod(y ~ xm)
lm5 <- linmod(y ~ xm)
check.lm(fit5.form, lm5, check.newdata=FALSE)
stopifnot(coef(fit5.form)  == coef(lm.tr),
          gsub("xm", "", names(coef(fit5.form)), fixed=TRUE)  == names(coef(lm.tr)))

cat("==test correct use of global x1 and y1, and of predict error handling\n")
x1 <- tr[,1]
y1 <- tr[,3]
linmod1 <- linmod(y1~x1)
lm1 <- lm(y1~x1)
fit6.mat <- linmod(x1, y1)

# following checks some confusing behaviour of predict.lm
options(warn=2) # treat warnings as errors
expect.err(try(predict(lm1,    newdata=trees[3:5,1,drop=FALSE])),
           "'newdata' had 3 rows but variables found have 31 rows")
expect.err(try(predict(lm1,    newdata=trees[3:5,1,drop=TRUE])),
           "numeric 'envir' arg not of length one")

# following checks that predict.linmod gives better error messages than predict.lm
expect.err(try(predict(linmod1, newdata=trees[3:5,1,drop=FALSE])),
           "variable 'x1' used when building the model is not in colnames(newdata)")
expect.err(try(predict(linmod1, newdata=trees[3:5,1,drop=TRUE])),
           "variable 'x1' used when building the model is not in colnames(newdata)")

newdata.x1 <- trees[3:5,1,drop=FALSE]
colnames(newdata.x1) <- "x1"
stopifnot(almost.equal(predict(linmod1, newdata=newdata.x1),
          c(7.63607739644657, 16.24803331528098, 17.26120459984973)))

check.lm(fit6.mat, linmod1, newdata=x1[3:5],
         check.newdata=FALSE, # TODO needed because linmod1 ignores newdata(!)
         check.coef.names=FALSE, check.casenames=FALSE)
print(predict(fit6.mat, newdata=x1[3:5]))
stopifnot(almost.equal(predict(fit6.mat, newdata=x1[3]), 7.63607739644657))

# production version only:
stopifnot(coef(fit6.mat) == coef(linmod1),
          names(coef(fit6.mat)) == c("(Intercept)", "V1")) # names(coef(linmod1) are "(Intercept)" "x1"

fit6.form <- linmod(y1~x1)
check.lm(fit6.form, linmod1, check.newdata=FALSE)

fit1.form <- linmod(Volume~., data=tr)
newdata <- trees[5:6,]
colnames(newdata) <- c("Girth", "Height", "Volume999") # doesn't matter what we call the response
stopifnot(identical(predict(fit1.form, newdata=newdata),
                    predict(fit1.form, newdata=trees[5:6,])))
newdata <- trees[5:6,3:1] # reverse columns and their colnames
colnames(newdata) <- c("Volume", "Height", "Girth")
stopifnot(identical(predict(fit1.form, newdata=newdata),
                    predict(fit1.form, newdata=trees[5:6,])))
newdata <- trees[5:6,2:1] # reverse columns and their colnames, delete response column
colnames(newdata) <- c("Height", "Girth")
stopifnot(identical(predict(fit1.form, newdata=newdata),
                    predict(fit1.form, newdata=trees[5:6,])))
stopifnot(identical(predict(fit1.form, newdata=as.matrix(trees[5:6,])), # allow matrix newdata
                    predict(fit1.form, newdata=trees[5:6,])))
newdata <- trees[5:6,]
colnames(newdata) <- c("Girth99", "Height", "Volume")
expect.err(try(predict(fit1.form, newdata=newdata)),
           "variable 'Girth' used when building the model is not in colnames(newdata)")
colnames(newdata) <- c("Girth", "Height99", "Volume")
expect.err(try(predict(fit1.form, newdata=newdata)),
           "variable 'Height' used when building the model is not in colnames(newdata)")

cat("==check integer input (sibsp is an integer) \n")

library(earth) # for etitanic data
data(etitanic)
tit <- etitanic[seq(1, nrow(etitanic), by=60), ] # small set of data for tests (18 cases)
tit$survived <- tit$survived != 0 # convert to logical
rownames(tit) <- paste("pas", 1:nrow(tit), sep="")
cat(paste(colnames(tit), "=", sapply(tit, class), sep="", collapse=", "), "\n")

fit7.mat <- linmod(tit$age, tit$sibsp)
lm7 <- lm.fit(cbind(1, tit$age), tit$sibsp)
stopifnot(coef(fit7.mat) == coef(lm7)) # coef names will differ

fit7.form <- linmod(sibsp~age, data=tit)
lm7.form  <- lm(sibsp~age, data=tit)
check.lm(fit7.form, lm7.form, newdata=tit[3:5,])

fit8.mat <- linmod(tit$sibsp, tit$age)
lm8 <- lm.fit(cbind(1, tit$sibsp), tit$age)
stopifnot(coef(fit8.mat) == coef(lm8)) # coef names will differ

fit8.form <- linmod(age~sibsp, data=tit)
lm8.form  <- lm(age~sibsp, data=tit)
check.lm(fit8.form, lm8.form, newdata=tit[3:5,])

# drop=FALSE so response is a data frame
fit1a.mat <- linmod(trees[,1:2], trees[, 3, drop=FALSE])
print(fit1a.mat)
print(summary(fit1.mat))
plotres(fit1a.mat) # plot caption shows response name "Volume"

cat("==test model building with different non numeric args\n")

library(earth) # for etitanic data
data(etitanic)
tit <- etitanic[seq(1, nrow(etitanic), by=60), ] # small set of data for tests (18 cases)
tit$survived <- tit$survived != 0 # convert to logical
rownames(tit) <- paste("pas", 1:nrow(tit), sep="")
cat(paste(colnames(tit), "=", sapply(tit, class), sep="", collapse=", "), "\n")

lm9 <- lm(survived~., data=tit)
fit9.form <- linmod(survived~., data=tit)
check.lm(fit9.form, lm9, newdata=tit[3:5,])

expect.err(try(linmod(tit[,c(1,3,4,5,6)], tit[,"survived"])),
           "non-numeric column in 'x'")
fit9a.form <- lm.fit(data.matrix(cbind("(Intercept)"=1, tit[,c(1,3,4,5,6)])), tit[,"survived"])
lm9 <- lm.fit(data.matrix(cbind("(Intercept)"=1, tit[,c(1,3,4,5,6)])), tit[,"survived"])
stopifnot(coef(fit9a.form) == coef(lm9))
stopifnot(names(coef(fit9a.form)) == names(coef(lm9)))

options(warn=2) # treat warnings as errors
expect.err(try(lm(pclass~., data=tit)), "using type = \"numeric\" with a factor response will be ignored")
expect.err(try(linmod(pclass~., data=tit)), "'y' is not numeric or logical")

options(warn=1) # print warnings as they occur
lm10 <- lm(pclass~., data=tit) # will give warnings
options(warn=2) # treat warnings as errors
fit10.form <- linmod(as.numeric(pclass)~., data=tit)
stopifnot(coef(fit10.form) == coef(lm10))
stopifnot(names(coef(fit10.form)) == names(coef(lm10)))
# check.lm(fit10.form, lm10) # fails because lm10 fitted is all NA

expect.err(try(linmod(pclass~., data=tit)), "'y' is not numeric or logical")
expect.err(try(linmod(tit[,-1], tit[,1])), "non-numeric column in 'x'")
expect.err(try(linmod(1:10, paste(1:10))), "'y' is not numeric or logical")

fit10a.form <- linmod(survived~pclass, data=tit)
lm10a <- lm(survived~pclass, data=tit)
check.lm(fit10a.form, lm10a, newdata=tit[3:5,])

expect.err(try(linmod(tit[,"pclass"], tit[,"age"])), "non-numeric column in 'x'")

expect.err(try(linmod(paste(1:10), 1:10)), "non-numeric column in 'x'")

lm11 <- lm(as.numeric(pclass)~., data=tit)
fit11.form <- linmod(as.numeric(pclass)~., data=tit)
check.lm(fit11.form, lm11, newdata=tit[3:5,])

# logical data (not numeric)
bool.data <- data.frame(x=rep(c(TRUE, FALSE, TRUE), length.out=10),
                        y=rep(c(TRUE, FALSE, FALSE), length.out=10))
lm12 <- lm(y~x, data=bool.data)
fit12.form <- linmod(y~x, data=bool.data)
check.lm(fit12.form, lm12, newdata=bool.data[3:5,1],
         check.newdata=FALSE) # needed because predict.lm gives invalid type (list) for variable 'x'
fit12.xy <- linmod(bool.data$x, bool.data$y)
# hack: delete mismatching names so check.lm() doesn't fail
names(lm12$coefficients) <- NULL     # were "(Intercept)" "xTRUE"
names(fit12.xy$coefficients) <- NULL # were "(Intercept)" "V1"
check.lm(fit12.xy, lm12, newdata=bool.data[3:5,1],
         check.newdata=FALSE, # needed because predict.lm gives invalid 'envir' argument of type 'logical'
         check.casenames=FALSE)

cat("==data.frame with strings\n")

df.with.string <-
    data.frame(1:5,
               c(1,2,-1,4,5),
               c("a", "b", "a", "a", "b"),
               stringsAsFactors=FALSE)
colnames(df.with.string) <- c("num1", "num2", "string")

fit30.form <- linmod(num1~num2, df.with.string)
lm30       <- lm(num1~num2, df.with.string)
check.lm(fit30.form, lm30, check.newdata=FALSE)

fit31.form <- linmod(num1~., df.with.string)
lm31       <- lm(num1~., df.with.string)
check.lm(fit31.form, lm31, check.newdata=FALSE)

expect.err(try(linmod(string~., df.with.string)), "'y' is not numeric or logical")

vec <- c(1,2,3,4,3)
expect.err(try(linmod(df.with.string, vec)), "non-numeric column in 'x'")
expect.err(try(linmod(tit$pclass, tit$survived)), "non-numeric column in 'x'")

cat("==x is singular\n")

set.seed(1)
x2 <- matrix(rnorm(6), nrow=2)
y2 <- c(1,2)
expect.err(try(linmod(y2~x2)), "'x' is singular (it has 4 columns but its rank is 2)")

x3 <- matrix(1:10, ncol=2)
y3 <- c(1,2,9,4,5)
expect.err(try(linmod(y3~x3)), "'x' is singular (it has 3 columns but its rank is 2)")

expect.err(try(linmod(trees[1,1:2], trees[1,3])), "'x' is singular (it has 3 columns but its rank is 1)")

cat("==nrow(x) does not match length(y)\n")

x4 <- matrix(1:10, ncol=2)
y4 <- c(1,2,9,4)
expect.err(try(linmod(x4, y4)), "nrow(x) is 5 but length(y) is 4")

x5 <- matrix(1:10, ncol=2)
y5 <- c(1,2,9,4,5,9)
expect.err(try(linmod(x5, y5)), "nrow(x) is 5 but length(y) is 6")

cat("==y has multiple columns\n")

vec <- c(1,2,3,4,3)
y2 <- cbind(c(1,2,3,4,9), vec^2)
expect.err(try(linmod(vec, y2)), "nrow(x) is 5 but length(y) is 10")
expect.err(try(linmod(y2~vec)), "nrow(x) is 5 but length(y) is 10")

cat("==NA in x\n")

x <- tr[,1:2]
y <- tr[,3]
x[2,2] <- NA
expect.err(try(linmod(x, y)), "NA in 'x'")

x <- tr[,1:2]
y <- tr[,3]
y[9] <- NA
expect.err(try(linmod(x, y)), "NA in 'y'")

cat("==misc tests with different kinds of data\n")

data3 <- data.frame(s=c("a", "b", "a", "c", "a"), num=c(1,5,1,9,2), y=c(1,3,2,5,3), stringsAsFactors=F)
stopifnot(sapply(data3, class) == c("character", "numeric", "numeric"))
a40 <- linmod(y~., data=data3)
print(summary(a40))
stopifnot(almost.equal(a40$coef, c(0, -4.5, -8.5, 1.5), max=0.001))
stopifnot(almost.equal(predict(a40, newdata=data3[2:3,]),
                       c(3.0, 1.5), max=0.001))

data4 <- data.frame(s=c("a", "b", "a", "c", "a"), num=c(1,5,1,9,2), y=c(1,3,2,5,3), stringsAsFactors=T)
stopifnot(sapply(data4, class) == c("factor", "numeric", "numeric"))
expect.err(try(linmod(data4[,1:2], data4[,3])), "non-numeric column in 'x'")

# following gives no error (and matches lm) even though col 1 of data3 is character not factor
a41 <- linmod(y~., data=data4)
print(summary(a41))
stopifnot(almost.equal(predict(a41, newdata=data3[2:3,]),
                       c(3.0, 1.5), max=0.001))

data5 <- data.frame(s=c("a", "b", "c", "a", "a"), num=c(1,9,4,2,6), y=c(1,2,3,5,3), stringsAsFactors=F)
stopifnot(almost.equal(predict(a41, newdata=data5[1:3,1:2]),
                        c(1.5, 9.0, -2.5), max=0.001))

data6 <- data.frame(s=c("a", "b", "c", "a9", "a"), num=c(1,9,4,2,6), y=c(1,2,3,5,3), stringsAsFactors=T)
expect.err(try(predict(a41, newdata=data6[1:3,1])), "variable 's' used when building the model is not in colnames(newdata)")
expect.err(try(predict(a41, newdata=data6[1:3,c(1,1)])), "variable 'num' used when building the model is not in colnames(newdata)")

expect.err(try(predict(a41, newdata=data.frame(s=1, num=2, y=3))), "variable 's' is not a factor")

expect.err(try(predict(a41, newdata=1:9)),
           "variable 's' used when building the model is not in colnames(newdata)")

expect.err(try(predict(a41, newdata=data.frame())), "'newdata' is empty")

tr.na <- trees
tr.na[9,3] <- NA
expect.err(try(linmod(Volume~.,data=tr.na)), "NA in 'y'")
expect.err(try(linmod(tr.na[,1:2], tr.na[,3])), "NA in 'y'")

tr.na <- trees
tr.na[10,1] <- NA
expect.err(try(linmod(Volume~.,data=tr.na)), "NA in 'x'")
expect.err(try(linmod(tr.na[,1:2], tr.na[,3])), "NA in 'x'")

a42 <- linmod(trees[,1:2], trees[, 3])
newdata1 <- data.frame(Girth=20)
expect.err(try(predict(a42, newdata=newdata1)), "ncol(newdata) is 1 but should be 2")
newdata3 <- data.frame(Girth=20, extra1=21, extra2=22)
expect.err(try(predict(a42, newdata=newdata3)), "ncol(newdata) is 3 but should be 2")
expect.err(try(predict(a42, newdata=data.frame())), "'newdata' is empty")
newdata.with.NA <- data.frame(Girth=20, Height=NA)
expect.err(try(predict(a42, newdata=newdata.with.NA)), "NA in 'newdata'")

a43 <- linmod(Volume~.,data=trees)
expect.err(try(predict(a43, newdata=newdata.with.NA)), "NA in 'newdata'")

y6 <- 1:5
x6 <- data.frame()
options(warn=1) # print warnings as they occur
expect.err(try(linmod(x6, y6)), "'x' is empty")
options(warn=2) # treat warnings as errors

y7 <- data.frame()
x7 <- 1:5
expect.err(try(linmod(x7, y7)), "'y' is empty")

# duplicated column names
data7 <- matrix(1:25, ncol=5)
colnames(data7) <- c("y", "x1", "x1", "x3", "x4")
expect.err(try(linmod(data7[,-1], data7[,1])), "column name \"x1\" in 'x' is duplicated")

colnames(data7) <- c("y", "x1", "x2", "x2", "x4")
expect.err(try(linmod(data7[,-1], data7[,1])), "column name \"x2\" in 'x' is duplicated")

colnames(data7) <- c("y", "x1", "x2", "x2", "x2")
expect.err(try(linmod(data7[,-1], data7[,1])), "column name \"x2\" in 'x' is duplicated")

# column name V2 will be created but it clashes with the existing column name
colnames(data7) <- c("y", "V2", "", "V3", "V4")
expect.err(try(linmod(data7[,-1], data7[,1])), "column name \"V2\" in 'x' is duplicated")

# missing column names
trees1 <- trees
colnames(trees1) <- NULL
cat("a52\n")
a52 <- linmod(trees1[,1:2], trees1[,3])
print(summary(a52))

trees1 <- trees
colnames(trees1) <- c("", "Height", "Volume") # was Girth Height Volume
cat("a53\n")
a53 <- linmod(trees1[,1:2], trees1[,3])
print(summary(a53))
cat("a53.formula\n")
expect.err(try(linmod(Volume~., data=trees1)), "attempt to use zero-length variable name")

# very long names to test formatting in summary.linmod
trees1 <- trees
colnames(trees1) <- c("Girth.a.very.long.name.in.fact.an.exceptionally.long.name",
                      "Height.a.very.long.name.in.fact.an.exceptionally.long.name",
                      "Volume.a.very.long.name.in.fact.an.exceptionally.long.name")
cat("a55\n")
a55 <- linmod(Volume.a.very.long.name.in.fact.an.exceptionally.long.name~
              Girth.a.very.long.name.in.fact.an.exceptionally.long.name+
              Height.a.very.long.name.in.fact.an.exceptionally.long.name,
              data=trees1)
print(summary(a55))

# intercept-only model
a56.form <- linmod(Volume~1, data=trees)
print(summary(a56.form))
stopifnot(length(coef(a56.form)) == 1)
plotres(a56.form)
expect.err(try(plotmo(a56.form)), "x is empty")
expect.err(try(linmod(rep(1, length.out=nrow(trees)), trees$Volume)), "'x' is singular (it has 2 columns but its rank is 1)")

# various tests for bad args
expect.err(try(linmod(trees[,1:2])), "no 'y' argument")

expect.err(try(linmod(Volume~., data=trees, nonesuch=99)), "unused argument (nonesuch = 99)")
expect.err(try(linmod(trees[,1:2], trees[,3], nonesuch=linmod)), "unused argument (nonesuch = function (...)")
expect.err(try(summary(linmod(trees[,1:2], trees[,3]), nonesuch=linmod)), "unused argument (nonesuch = function (...)")
expect.err(try(print(linmod(trees[,1:2], trees[,3]), nonesuch=linmod)), "unused argument (nonesuch = function (...)")
fit1.form <- linmod(Volume~., data=tr)
expect.err(try(predict(fit1.form, nonesuch=99)), "unused argument (nonesuch = 99)")
expect.err(try(predict(fit1.form, type="nonesuch")), "the 'type' argument is not yet supported")

# method functions

case.names.linmod <- function(object, ...)
{
    stop.if.dot.arg.used(...)
    names(residuals(object))
}
variable.names.linmod <- function(object, ...)
{
    stop.if.dot.arg.used(...)
    names(coef(object))
}
deviance.linmod <- function(object, ...)
{
    stop.if.dot.arg.used(...)
    sum(residuals(object)^2)
}
plot.linmod <- function(x, main=NULL, ...) # dots are passed to plot()
{
    call.as.char <- paste0(deparse(x$call, control=NULL, nlines=5),
                           sep=" ", collapse=" ")
    plot(fitted(x), residuals(x),
         main=if(is.null(main)) substr(call.as.char, 1, 50) else main,
         xlab="Fitted values", ylab="Residuals", ...)
    smooth <- lowess(fitted(x), residuals(x), f=.5)
    lines(smooth$x, smooth$y, col=2)
}
old.par <- par(no.readonly=TRUE)
par(mfrow=c(2,2))

fit1.form <- linmod(Volume~., data=tr)
fit1.lm <- lm(Volume~., data=tr)
stopifnot(almost.equal(coef(fit1.form), coef(fit1.lm)))
stopifnot(identical(names(coef(fit1.form)), names(coef(fit1.lm))))
stopifnot(almost.equal(fitted(fit1.form), fitted(fit1.lm)))
stopifnot(identical(names(fitted(fit1.form)), names(fitted(fit1.lm))))
stopifnot(identical(na.action(fit1.form), na.action(fit1.lm)))
stopifnot(almost.equal(residuals(fit1.form), residuals(fit1.lm)))
stopifnot(identical(names(residuals(fit1.form)), names(residuals(fit1.lm))))
stopifnot(identical(names(case.names(fit1.form)), names(case.names(fit1.lm))))
stopifnot(identical(names(variable.names(fit1.form)), names(variable.names(fit1.lm))))
stopifnot(identical(weights(fit1.form), weights(fit1.lm)))
stopifnot(almost.equal(df.residual(fit1.form), df.residual(fit1.lm)))
stopifnot(identical(names(df.residual(fit1.form)), names(df.residual(fit1.lm))))
# print(model.matrix(fit1.form)) # TODO doesn't work
stopifnot(almost.equal(deviance(fit1.form), deviance(fit1.lm)))
stopifnot(identical(names(deviance(fit1.form)), names(deviance(fit1.lm))))
stopifnot(identical(weights(fit1.form), weights(fit1.lm)))

plot(fit1.form)
plot(fit1.form, xlim=c(0,80), ylim=c(-10,10), pch=20, main="fit1.form: test plot args")

fit1.mat <- linmod(tr[,1:2], tr[,3]) # note no need for intercept term
stopifnot(almost.equal(coef(fit1.mat), coef(fit1.lm)))
stopifnot(identical(names(coef(fit1.mat)), names(coef(fit1.lm))))
stopifnot(almost.equal(fitted(fit1.mat), fitted(fit1.lm)))
stopifnot(identical(names(fitted(fit1.mat)), names(fitted(fit1.lm))))
stopifnot(identical(na.action(fit1.mat), na.action(fit1.lm)))
stopifnot(almost.equal(residuals(fit1.mat), residuals(fit1.lm)))
stopifnot(identical(names(residuals(fit1.mat)), names(residuals(fit1.lm))))
stopifnot(identical(names(case.names(fit1.mat)), names(case.names(fit1.lm))))
stopifnot(identical(names(variable.names(fit1.mat)), names(variable.names(fit1.lm))))
stopifnot(identical(weights(fit1.mat), weights(fit1.lm)))
stopifnot(almost.equal(df.residual(fit1.mat), df.residual(fit1.lm)))
stopifnot(identical(names(df.residual(fit1.mat)), names(df.residual(fit1.lm))))
# print(model.matrix(fit1.mat)) # TODO doesn't work
stopifnot(almost.equal(deviance(fit1.mat), deviance(fit1.lm)))
stopifnot(identical(names(deviance(fit1.mat)), names(deviance(fit1.lm))))
stopifnot(identical(weights(fit1.mat), weights(fit1.lm)))

plot(fit1.mat)
plot(fit1.mat, xlim=c(0,80), ylim=c(-10,10), pch=20, main="fit1.mat: test plot args")

par(old.par)

if(!interactive()) {
    dev.off()        # finish postscript plot
    q(runLast=FALSE) # needed else R prints the time on exit (R2.5 and higher) which messes up the diffs
}
