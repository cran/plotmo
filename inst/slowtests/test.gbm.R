# test.gbm.R: gbm tests for plotmo and plotres

library(gbm)
library(rpart.plot) # for ptitanic, want data with NAs for testing
library(plotmo)
library(earth) # for ozone1
data(ptitanic)
set.seed(2016)
options(warn=1) # print warnings as they occur
if(!interactive())
    postscript(paper="letter")

printf <- function(format, ...) cat(sprintf(format, ...), sep="") # like c printf

strip.space <- function(s) gsub("[ \t\n]", "", s)

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
cat("--- distribution=\"gaussian\", formula interface ----------------------------------\n")

set.seed(2016)
ptit <- ptitanic[sample(1:nrow(ptitanic), size=70), ] # small data for fast test
set.seed(2016)
# # TODO bug in gbm: following causes error: survived is not of type numeric, ordered, or factor
# ptit$survived <- ptit$survived == "survived"
ptit <- ptit[!is.na(ptit$age), ]
train.frac <- .8
set.seed(2016)
gbm.gaussian <- gbm(age~., data=ptit, train.frac=train.frac,
                   distribution="gaussian",
                   n.trees=50, shrinkage=.1, keep.data=FALSE)
expect.err(try(plotres(gbm.gaussian)),
           "use keep.data=TRUE in the call to gbm (object$data is NULL)")
#           "use keep_gbm_data=TRUE in the call to gbmt")
set.seed(2016)
gbm.gaussian <- gbm(age~., data=ptit, train.frac=train.frac,
                   distribution="gaussian",
                   n.trees=50, shrinkage=.1)
old.par <- par(no.readonly=TRUE)
par(mfrow=c(2,2), mar=c(3,3,4,1))
w1 <- plotres(gbm.gaussian, which=1, do.par=FALSE, w1.smooth=TRUE,
        w1.main="gbm.gaussian")
cat("w1 plot for gbm.gaussian returned (w1.smooth=TRUE):\n")
print(w1)
plot(0, 0) # dummy plot
w3 <- plotres(gbm.gaussian, which=3, do.par=FALSE, info=TRUE,
        smooth.col=0, col=ptit$sex, # ylim=c(-40,40),
        wmain="nresponse=1")
# compare to manual residuals
iused <- 1:(train.frac * nrow(ptit))
y <- ptit$age[iused]
n.trees <- plotmo:::gbm.n.trees(gbm.gaussian)
# TODO following fails in the new version of gbm (version 2.2) (you have to provide newdata)
# yhat <- predict(gbm.gaussian, type="response", n.trees=n.trees)
yhat <- predict(gbm.gaussian, newdata=ptit, type="response", n.trees=n.trees)
yhat <- yhat[iused]
plot(yhat, y - yhat,
     col=ptit$sex[iused], main="manual gaussian residuals",
     pch=20, ylim=c(-40,40))
abline(h=0, col="gray")
stopifnot(all(yhat == w3$x))
stopifnot(all(y - yhat == w3$y))
par(old.par)

w1 <- plotres(gbm.gaussian, predict.n.trees=13, w1.grid.col=1, trace=1, SHOWCALL=TRUE,
              w1.smooth=TRUE,
              w1.main="predict.n.trees=13 w1.grid.col=1")
cat("second w1 plot for gbm.gaussian returned (w1.smooth=TRUE):\n")
print(w1)
plotmo(gbm.gaussian, trace=-1, SHOWCALL=TRUE)
# plotmo(gbm.gaussian, trace=-1, all1=TRUE, SHOWCALL=TRUE)
# plotmo(gbm.gaussian, trace=-1, all2=TRUE, SHOWCALL=TRUE)

# test color argument
old.par <- par(no.readonly=TRUE)
par(mfrow=c(2,2), mar=c(3,3,4,1))
plotres(gbm.gaussian, which=1)
title("test color argument")
plotres(gbm.gaussian, which=1, w1.col=c(1,2,3,0))
plotres(gbm.gaussian, which=1, w1.col=c(1,0,0,4), w1.legend.x=40, w1.legend.y=.3)
plotres(gbm.gaussian, which=1, w1.col=c(2,3,4,1), w1.legend.x="topright")
par(old.par)

old.par <- par(no.readonly=TRUE)
par(mfrow=c(2,2), mar=c(3,3,4,1))
plot_gbm(gbm.gaussian)
title("test plot_gbm")
w1 <- plot_gbm(gbm.gaussian, col=c(1,2,3,0), grid.col=1, smooth=TRUE,
               main="col=c(1,2,3,0), grid.col=1")
cat("third w1 plot for gbm.gaussian returned (smooth=TRUE):\n")
print(w1)
par(old.par)

# test xlim and ylim
old.par <- par(no.readonly=TRUE)
par(mfrow=c(2,3), mar=c(3,3,4,1))
plot_gbm(gbm.gaussian,                  main="test xlim and ylim default")
plot_gbm(gbm.gaussian, ylim=NULL,       main="ylim=NULL")
plot_gbm(gbm.gaussian, xlim=c(5, 50),    main="xlim=c(5, 50)")
plot_gbm(gbm.gaussian, ylim=c(100, 250), main="ylim=c(100, 250)")
plot_gbm(gbm.gaussian, xlim=c(10, 25),
                       ylim=c(150, 170),  main="xlim=c(10, 25), ylim=c(150, 170)")
plot_gbm(gbm.gaussian, xlim=c(-10, 40), ylim=c(-10, 300), legend.x=NA,
                                        main="xlim=c(-10, 40), ylim=c(-10, 300)\nlegend.x=NA")
par(old.par)

# test the smooth argument
old.par <- par(no.readonly=TRUE)
par(mfrow=c(3,3), mar=c(3,3,4,1))
imin <- plot_gbm(gbm.gaussian,                    main="smooth=default")
imin.default <- imin
cat("smooth=default imin=c(", imin[1], ",", imin[2], ",", imin[3], ",", imin[4], ")\n", sep="")

imin <- plot_gbm(gbm.gaussian, smooth=c(1,0,0,0), main="smooth=c(1,0,0,0)")
cat("smooth=c(1,0,0,0) imin=c(", imin[1], ",", imin[2], ",", imin[3], ",", imin[4], ")\n", sep="")

imin <- plot_gbm(gbm.gaussian, smooth=c(0,1,0,0), main="smooth=c(0,1,0,0)")
cat("smooth=c(0,1,0,0) imin=c(", imin[1], ",", imin[2], ",", imin[3], ",", imin[4], ")\n", sep="")

imin <- plot_gbm(gbm.gaussian, smooth=c(0,0,1,0), main="smooth=c(0,0,1,0)")
cat("smooth=c(0,0,1,0) imin=c(", imin[1], ",", imin[2], ",", imin[3], ",", imin[4], ")\n", sep="")

imin <- plot_gbm(gbm.gaussian, smooth=c(0,0,0,1), main="smooth=c(0,0,0,1)\nsame as default")
cat("smooth=c(0,0,0,1) imin=c(", imin[1], ",", imin[2], ",", imin[3], ",", imin[4], ")\n", sep="")

imin <- plot_gbm(gbm.gaussian, smooth=c(0,0,0,0), main="smooth=c(0,0,0,0)")
cat("smooth=c(0,0,0,0) imin=c(", imin[1], ",", imin[2], ",", imin[3], ",", imin[4], ")\n", sep="")

imin <- plot_gbm(gbm.gaussian, smooth=c(0,0,1,1), main="smooth=c(0,0,1,1)")
cat("smooth=c(0,0,1,1) imin=c(", imin[1], ",", imin[2], ",", imin[3], ",", imin[4], ")\n", sep="")

imin <- plot_gbm(gbm.gaussian, smooth=1,          main="smooth=1") # gets recycled
cat("smooth=1          imin=c(", imin[1], ",", imin[2], ",", imin[3], ",", imin[4], ")\n", sep="")
imin.smooth <- imin

imin.noplot <- plot_gbm(gbm.gaussian, col=0) # will not be plotted
print(imin.default)
print(imin.noplot)
stopifnot(identical(imin.default, imin.noplot))

imin.noplot <- plot_gbm(gbm.gaussian, col=0, smooth=1) # will not be plotted
print(imin.smooth)
print(imin.noplot)
stopifnot(identical(imin.smooth, imin.noplot))

par(old.par)

cat("--- distribution=\"gaussian\", glm.fit interface ----------------------------------\n")

set.seed(2016)
ptit <- ptitanic[sample(1:nrow(ptitanic), size=70), ]
set.seed(2016)
ptit <- ptit[!is.na(ptit$age), ]
train.frac <- .8
set.seed(2016)
gbm.gaussian.fit <- gbm.fit(ptit[,-4], ptit[,4], nTrain=floor(train.frac * nrow(ptit)),
                   distribution="gaussian", verbose=FALSE,
                   n.trees=50, shrinkage=.1)
old.par <- par(no.readonly=TRUE)
par(mfrow=c(2,2), mar=c(3,3,4,1))
w1 <- plotres(gbm.gaussian.fit, which=1, do.par=FALSE, w1.smooth=TRUE,
        w1.main="gbm.gaussian.fit")

cat("w1 plot for gbm.gaussian.fit returned (w1.smooth=TRUE):\n")
print(w1)

plot(0, 0) # dummy plot

w3 <- plotres(gbm.gaussian.fit, which=3, do.par=FALSE, info=TRUE, trace=0,
        smooth.col=0, col=ptit$sex, # ylim=c(-40,40),
        wmain="nresponse=1")

# compare to manual residuals
iused <- 1:(train.frac * nrow(ptit))
y.fit <- ptit$age[iused]
n.trees <- plotmo:::gbm.n.trees(gbm.gaussian.fit)
# TODO following fails in the new version of gbm (version 2.2) (you have to provide newdata)
# yhat.fit <- predict(gbm.gaussian.fit, type="response", n.trees=n.trees)
yhat.fit <- predict(gbm.gaussian.fit, newdata=ptit[,-4], type="response", n.trees=n.trees)
yhat.fit <- yhat.fit[iused]
# plot(yhat.fit, y.fit - yhat.fit,
#      col=ptit$sex[iused], main="manual gaussian residuals\n(TODO gbm.fit don't match)",
#      pch=20, ylim=c(-40,40))
# abline(h=0, col="gray")
# --- TODO known issue, these fail ---
# compare to formual interface
# stopifnot(all(yhat.fit == yhat))
stopifnot(all(y.fit == y))
# # sanity check
# stopifnot(all(yhat.fit == w3$x))
# stopifnot(all(y.fit - yhat.fit == w3$y.fit))
plotmo(gbm.gaussian.fit, trace=-1, SHOWCALL=TRUE)
par(old.par)

cat("--- distribution=\"laplace\" ----------------------------------\n")

set.seed(2016)
ptit <- ptitanic[sample(1:nrow(ptitanic), size=70), ]
ptit <- ptit[!is.na(ptit$age), ]
ptit$survived <- ptit$parch <- ptit$sex <- NULL
train.frac <- .8
set.seed(2016)
gbm.laplace <- gbm(age~., data=ptit, train.frac=train.frac,
                   distribution="laplace",
                   n.trees=100, shrinkage=.1)
old.par <- par(no.readonly=TRUE)
par(mfrow=c(2,2), mar=c(3,3,4,1))
w1 <- plotres(gbm.laplace, which=1:2, do.par=FALSE, w1.smooth=TRUE,
        w1.main="gbm.laplace")

cat("w1 plot for gbm.laplace returned (w1.smooth=TRUE):\n")
print(w1)

w3 <- plotres(gbm.laplace, which=3, do.par=FALSE, info=TRUE)

# compare to manual residuals
iused <- 1:(train.frac * nrow(ptit))
y <- ptit$age[iused]
n.trees <- plotmo:::gbm.n.trees(gbm.laplace)
# TODO following fails in the new version of gbm (version 2.2) (you have to provide newdata)
# yhat <- predict(gbm.laplace, type="response", n.trees=n.trees)
yhat <- predict(gbm.laplace, newdata=ptit, type="response", n.trees=n.trees)
yhat <- yhat[iused]
plot(yhat, y - yhat,
     main="manual laplace residuals",
     pch=20, ylim=c(-40,40))
abline(h=0, col="gray")
stopifnot(all(yhat == w3$x))
stopifnot(all(y - yhat == w3$y))
plotmo(gbm.laplace, trace=-1, SHOWCALL=TRUE)
par(old.par)

cat("--- distribution=\"tdist\" ----------------------------------\n")

set.seed(2016)
ptit <- ptitanic[sample(1:nrow(ptitanic), size=70), ]
ptit <- ptit[!is.na(ptit$age), ]
ptit$survived <- ptit$parch <- ptit$sex <- NULL
train.frac <- .8
set.seed(2016)
gbm.tdist <- gbm(age~., data=ptit, train.frac=train.frac,
                   distribution="tdist",
                   n.trees=100, shrinkage=.1)
old.par <- par(no.readonly=TRUE)
par(mfrow=c(2,2), mar=c(3,3,4,1))
set.seed(2016)
w1 <- plotres(gbm.tdist, which=1:2, do.par=FALSE,
        w1.main="gbm.tdist")

cat("w1 plot for gbm.tdist returned (w1.smooth=default):\n")
print(w1)

set.seed(2016)
w3 <- plotres(gbm.tdist, which=3, do.par=FALSE, info=TRUE)

# compare to manual residuals
iused <- 1:(train.frac * nrow(ptit))
y <- ptit$age[iused]
n.trees <- plotmo:::gbm.n.trees(gbm.tdist)
# TODO following fails in the new version of gbm (version 2.2) (you have to provide newdata)
# yhat <- predict(gbm.tdist, type="response", n.trees=n.trees)
yhat <- predict(gbm.tdist, newdata=ptit, type="response", n.trees=n.trees)
yhat <- yhat[iused]
plot(yhat, y - yhat,
     main="manual tdist residuals",
     pch=20, ylim=c(-40,40))
abline(h=0, col="gray")
stopifnot(all(yhat == w3$x))
stopifnot(all(y - yhat == w3$y))
plotmo(gbm.tdist, trace=-1, SHOWCALL=TRUE)
par(old.par)

cat("--- distribution=\"bernoulli\" ----------------------------------\n")

set.seed(2016)
ptit <- ptitanic[sample(1:nrow(ptitanic), size=80), ]
ptit$survived <- ptit$survived == "survived"
temp <- ptit$pclass # put pclass at the end so can check ordering of importances
ptit$pclass <- NULL
ptit$pclass <- factor(as.numeric(temp), labels=c("first", "second", "third"))
train.frac <- .9
set.seed(2016)
gbm.bernoulli <- gbm(survived~., data=ptit, train.frac=train.frac,
                     distribution="bernoulli",
                     n.trees=100, shrinkage=.1, cv.folds=3)
old.par <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
par(mar=c(3.5, 3, 2, 0.5))  # small margins and text to pack figs in
par(mgp=c(1.5, .4, 0))      # squash axis annotations
w1 <- plotres(gbm.bernoulli, which=c(1,4),
        col=ptit$survived+2, trace=0, do.par=FALSE,
        w1.main="gbm.bernoulli")
cat("w1 plot for gbm.bernoulli with cv.folds=3 returned:\n")
print(w1)

w3 <- plotres(gbm.bernoulli, which=3, predict.n.trees=40,
        ylim=c(-.6, 1), xlim=c(.1, .6),
        col=ptit$sex, trace=0, do.par=FALSE, smooth.col=0)

# compare to manual residuals
iused <- 1:(train.frac * nrow(ptit))
y <- ptit$survived[iused]
# TODO following fails in the new version of gbm (version 2.2) (you have to provide newdata)
# yhat <- predict(gbm.bernoulli, type="response", n.trees=40)
yhat <- predict(gbm.bernoulli, newdata=ptit, type="response", n.trees=40)
yhat <- yhat[iused]
plot(yhat, y - yhat, col=ptit$sex,
     main="manual bernoulli residuals", pch=20, cex=1,
     ylim=c(-.6, 1), xlim=c(.1, .6))
abline(h=0, col="gray")
stopifnot(all(yhat == w3$x))
stopifnot(all(y - yhat == w3$y))
par(old.par)

old.par <- par(no.readonly=TRUE)
plotmo(gbm.bernoulli, do.par=2)
print(summary(gbm.bernoulli)) # will also plot
par(old.par)

cat("--- distribution=\"huberized\" ----------------------------------\n")

set.seed(2016)
ptit <- ptitanic[sample(1:nrow(ptitanic), size=100), ]
ptit$survived <- ptit$survived == "survived"
ptit$sibsp <- ptit$parch <- ptit$pclass <- NULL
train.frac <- 1
set.seed(2016)
gbm.huberized <- gbm(survived~., data=ptit, train.frac=train.frac,
                     distribution="huberized",
                     n.trees=200, shrinkage=.1)
old.par <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
par(mar=c(3.5, 3, 2, 0.5))  # small margins and text to pack figs in
par(mgp=c(1.5, .4, 0))      # squash axis annotations
w1 <- plotres(gbm.huberized, which=c(1,4),
        col=ptit$survived+2, trace=0, do.par=FALSE,
        w1.main="gbm.huberized")
cat("w1 plot for gbm.huberized returned (smooth=default):\n")
print(w1)

# TODO huberized residuals look weird
w3 <- plotres(gbm.huberized, which=3, predict.n.trees=40,
        col=ptit$sex, trace=0, do.par=FALSE, smooth.col=0)

# compare to manual residuals
iused <- 1:(train.frac * nrow(ptit))
y <- ptit$survived[iused]
# TODO following fails in the new version of gbm (version 2.2) (you have to provide newdata)
# yhat <- predict(gbm.huberized, type="response", n.trees=40)
yhat <- predict(gbm.huberized, newdata=ptit, type="response", n.trees=40)
yhat <- yhat[iused]
plot(yhat, y - yhat, col=ptit$sex, ylim=c(-2.5, 2.5),
     main="manual huberized residuals", pch=20)
abline(h=0, col="gray")
stopifnot(all(yhat == w3$x))
stopifnot(all(y - yhat == w3$y))
par(old.par)

old.par <- par(no.readonly=TRUE)
plotmo(gbm.huberized, do.par=2)
print(summary(gbm.huberized)) # will also plot
par(old.par)

cat("--- distribution=\"adaboost\" ----------------------------------\n")

set.seed(2016)
ptit <- ptitanic[sample(1:nrow(ptitanic), size=100), ]
ptit$survived <- ptit$survived == "survived"
ptit$sibsp <- ptit$parch <- ptit$pclass <- NULL
train.frac <- .8
set.seed(2016)
gbm.adaboost <- gbm(survived~., data=ptit, train.frac=train.frac,
                     distribution="adaboost",
                     n.trees=150, shrinkage=.01)
old.par <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
par(mar=c(3.5, 3, 2, 0.5))  # small margins and text to pack figs in
par(mgp=c(1.5, .4, 0))      # squash axis annotations
w1 <- plotres(gbm.adaboost, which=c(1,4),
        col=ptit$survived+2, trace=0, do.par=FALSE,
        w1.main="gbm.adaboost")
cat("w1 plot for gbm.adaboost returned (smooth=default):\n")
print(w1)

w3 <- plotres(gbm.adaboost, which=3, predict.n.trees=40,
        col=ptit$sex, trace=0, do.par=FALSE, smooth.col=0)

# compare to manual residuals
iused <- 1:(train.frac * nrow(ptit))
y <- ptit$survived[iused]
# TODO following fails in the new version of gbm (version 2.2) (you have to provide newdata)
# yhat <- predict(gbm.adaboost, type="response", n.trees=40)
yhat <- predict(gbm.adaboost, newdata=ptit, type="response", n.trees=40)
yhat <- yhat[iused]
plot(yhat, y - yhat, col=ptit$sex,
     main="manual adaboost residuals", pch=20)
abline(h=0, col="gray")
stopifnot(all(yhat == w3$x))
stopifnot(all(y - yhat == w3$y))
par(old.par)

old.par <- par(no.readonly=TRUE)
plotmo(gbm.adaboost, do.par=2)
print(summary(gbm.adaboost)) # will also plot
par(old.par)

# test gbm multinomial model, also test very small number of trees in plot_gbm

data(iris)
set.seed(2016)
gbm.iris <- gbm(Species~., data=iris, distribution="multinomial", n.tree=5)
expect.err(try(plotres(gbm.iris)),
           "gbm distribution=\"multinomial\" is not yet supported")
expect.err(try(plotmo(gbm.iris)),
           "gbm distribution=\"multinomial\" is not yet supported")
plot_gbm(gbm.iris)

# TODO following fails in the new version of gbm (version 2.2)
#   (distribution "multinomial" is no longer supported)
#
# cat("--- distribution=\"multinomial\" ----------------------------------\n")
#
# set.seed(2016)
# ptit <- ptitanic[sample(1:nrow(ptitanic), size=500), ]
# set.seed(2016)
# gbm.multinomial <- gbm(pclass~.,
#                        data=ptit, train.frac=.7,
#                        distribution="multinomial",
#                        n.trees=100, shrinkage=.1)
#
# w1 <- plot_gbm(gbm.multinomial, main="gbm.multinomial", smooth=T)
# cat("plot_gbm for gbm.multinomial returned (smooth=TRUE):\n")
# print(w1)
#
# expect.err(try(plotres(gbm.multinomial)),
#            "gbm distribution=\"multinomial\" is not yet supported")
#
# expect.err(try(plotmo(gbm.multinomial)),
#            "gbm distribution=\"multinomial\" is not yet supported")

# cat("--- gbmt distribution=\"Gaussian\", formula interface ----------------------------------\n")
#
# set.seed(2016)
# ptit <- ptitanic[sample(1:nrow(ptitanic), size=70), ] # small data for fast test
# set.seed(2016)
# # # TODO bug in gbm: following causes error: survived is not of type numeric, ordered, or factor
# # ptit$survived <- ptit$survived == "survived"
# ptit <- ptit[!is.na(ptit$age), ]
# # TODO change this to build same model as gbm.gaussian
# train_params <-
#      training_params(num_trees = 50,
#                      shrinkage = 0.1,
#                      bag_fraction = 0.5,
#                      num_train = round(.8 * nrow(ptit)))
# old.par <- par(no.readonly=TRUE)
# par(mfrow=c(2,2), mar=c(3,3,4,1))
# set.seed(2016)
# gbmt.gaussian <- gbmt(age~., data=ptit,
#             distribution=gbm_dist("Gaussian"),
#             train_params = train_params,
#             is_verbose = FALSE)
# expect.err(try(plotres(gbmt.gaussian)),
#            "use keep.data=TRUE in the call to gbm (object$data is NULL)")
# #           "use keep_gbm_data=TRUE in the call to gbmt")
# set.seed(2016)
# gbmt.gaussian <- gbmt(age~., data=ptit,
#             distribution=gbm_dist("Gaussian"),
#             train_params = train_params,
#             is_verbose = FALSE,  keep_gbm_data=TRUE)
# w1 <- plotres(gbmt.gaussian, which=1, do.par=FALSE, w1.smooth=TRUE,
#               w1.main="gbmt.gaussian")
# cat("w1 plot for gbmt.gaussian returned (w1.smooth=TRUE):\n")
# print(w1)
# plot(0, 0) # dummy plot
# set.seed(2016)
# w3 <- plotres(gbmt.gaussian, which=3, do.par=FALSE, info=TRUE,
#         smooth.col=0, col=ptit$sex, # ylim=c(-40,40),
#         wmain="nresponse=1")
#
# # compare to manual residuals
# iused <- 1:(train.frac * nrow(ptit))
# y <- ptit$age[iused]
# n.trees <- plotmo:::gbm.n.trees(gbmt.gaussian)
# # TODO following fails in the new version of gbm (version 2.2) (you have to provide newdata)
# # yhat <- predict(gbmt.gaussian, type="response", n.trees=n.trees)
# yhat <- predict(gbmt.gaussian, newdata=ptit, type="response", n.trees=n.trees)
# yhat <- yhat[iused]
# plot(yhat, y - yhat,
#      col=ptit$sex[iused], main="manual gaussian residuals",
#      pch=20, ylim=c(-40,40))
# abline(h=0, col="gray")
# stopifnot(all(yhat == w3$x))
# stopifnot(all(y - yhat == w3$y))
# par(old.par)
#
# w1 <- plotres(gbmt.gaussian, predict.n.trees=13, w1.grid.col=1, trace=1, SHOWCALL=TRUE,
#               w1.smooth=TRUE,
#               w1.main="predict.n.trees=13 w1.grid.col=1")
# cat("second w1 plot for gbmt.gaussian returned (w1.smooth=TRUE):\n")
# print(w1)
# plotmo(gbmt.gaussian, trace=-1, SHOWCALL=TRUE)
#
# par(old.par)
#
# cat("--- distribution=\"bernoulli\" ----------------------------------\n")
#
# set.seed(2016)
# ptit <- ptitanic[sample(1:nrow(ptitanic), size=80), ]
# ptit$survived <- ptit$survived == "survived"
# temp <- ptit$pclass # put pclass at the end so can check ordering of importances
# ptit$pclass <- NULL
# ptit$pclass <- factor(as.numeric(temp), labels=c("first", "second", "third"))
# # TODO change this to build same model as gbm.bernoulli
# train_params <-
#      training_params(num_trees = 100,
#                      shrinkage = 0.1,
#                      bag_fraction = 0.5,
#                      num_train = round(.8 * nrow(ptit)))
# set.seed(2016)
# gbmt.bernoulli <- gbmt(survived~., data=ptit,
#             distribution=gbm_dist("Bernoulli"),
#             train_params = train_params,
#             cv_folds = 3,
#             is_verbose = FALSE,  keep_gbm_data=TRUE)
# old.par <- par(no.readonly=TRUE)
# par(mfrow=c(2,2))
# par(mar=c(3.5, 3, 2, 0.5))  # small margins and text to pack figs in
# par(mgp=c(1.5, .4, 0))      # squash axis annotations
# w1 <- plotres(gbmt.bernoulli, which=c(1,4),
#         col=ptit$survived+2, trace=0, do.par=FALSE,
#         w1.main="gbmt.bernoulli")
# cat("w1 plot for gbmt.bernoulli with cv.folds=3 returned:\n")
# print(w1)
#
# w3 <- plotres(gbmt.bernoulli, which=3, predict.n.trees=40,
#         ylim=c(-.6, 1), xlim=c(.1, .6),
#         col=ptit$sex, trace=0, do.par=FALSE, smooth.col=0)
#
# # compare to manual residuals
# iused <- 1:(train.frac * nrow(ptit))
# y <- ptit$survived[iused]
# # TODO following fails in the new version of gbm (version 2.2) (you have to provide newdata)
# # yhat <- predict(gbmt.bernoulli, type="response", n.trees=40)
# yhat <- predict(gbmt.bernoulli, newdata=ptit, type="response", n.trees=40)
# yhat <- yhat[iused]
# plot(yhat, y - yhat, col=ptit$sex,
#      main="manual bernoulli residuals", pch=20, cex=1,
#      ylim=c(-.6, 1), xlim=c(.1, .6))
# abline(h=0, col="gray")
# stopifnot(all(yhat == w3$x))
# stopifnot(all(y - yhat == w3$y))
# par(old.par)
#
# old.par <- par(no.readonly=TRUE)
# plotmo(gbmt.bernoulli, do.par=2)
# print(summary(gbmt.bernoulli)) # will also plot
# par(old.par)

if(!interactive()) {
    dev.off()         # finish postscript plot
    q(runLast=FALSE)  # needed else R prints the time on exit (R2.5 and higher) which messes up the diffs
}
