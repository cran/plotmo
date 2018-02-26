# test.non.earth.R: test plotmo on non-earth models
# Stephen Milborrow, Basley KwaZulu-Natal Mar 2011

library(plotmo)
library(earth)
data(ozone1)
data(etitanic)
options(warn=1) # print warnings as they occur
if(!interactive())
    postscript(paper="letter")
set.seed(2016)
dopar <- function(nrows, ncols, caption = "")
{
    cat("                             ", caption, "\n")
    par(mfrow=c(nrows, ncols))
    par(oma = c(0, 0, 3, 0))
    par(mar = c(3, 3, 1.7, 0.5))
    par(mgp = c(1.6, 0.6, 0))
    par(cex = 0.7)
}
expect.err <- function(obj) # test that we got an error as expected from a try() call
{
    if(class(obj)[1] == "try-error")
        cat("Got error as expected\n")
    else
        stop("did not get expected error")
}
library(caret)
set.seed(2015)
caret.earth.mod <- train(O3~., data=ozone1, method="earth",
                         tuneGrid=data.frame(degree=2, nprune=10))
plotmo(caret.earth.mod, trace=1, SHOWCALL=TRUE)
# but the pairs are plotted here
plotmo(caret.earth.mod$finalModel, trace=1, SHOWCALL=TRUE)
plotres(caret.earth.mod, trace=1, SHOWCALL=TRUE)
# plotres(caret.earth.mod$finalModel, trace=1, SHOWCALL=TRUE)

set.seed(2015)
bag <- bagEarth(O3~., data=ozone1, degree=2, B=3)
print(bag$fit)
# pairs are plotted correctly (I think)
plotmo(bag, type="response", trace=1, SHOWCALL=TRUE)
plotres(bag, type="response", trace=1, SHOWCALL=TRUE)

set.seed(2015)
a.bag1 <- bagEarth(trees[,-3], trees[,3], degree=2, B = 3)
plotmo(a.bag1, trace=1, SHOWCALL=TRUE, all2=TRUE, caption="bagEarth, trees")
plotres(a.bag1, trace=1, SHOWCALL=TRUE)

# TODO following doesn't work properly, factors are plotted as continuous?
# trace=1 to display "Fixed rank deficient bx by removing 1 term" messages
a.bag3 <- bagEarth(survived~., data=etitanic, degree=2, B=3, trace=1)
plotmo(a.bag3, clip=F, caption="bagEarth, etitanic", trace=1, SHOWCALL=TRUE)
plotres(a.bag3, clip=F, trace=1, SHOWCALL=TRUE)

# example by Max Kuhn on stackoverflow
set.seed(2015)
etit <- etitanic
etit$survived <- factor(ifelse(etit$survived == 1, "yes", "no"),
                       levels = c("yes", "no"))
# TODO pairs are not plotted
caret.earth.mod2 <- train(survived ~ .,
            data = etit,
            method = "earth",
            tuneGrid = data.frame(degree = 2, nprune = 9),
            trControl = trainControl(method = "none",
                                     classProbs = TRUE))
plotmo(caret.earth.mod2, trace=1, SHOWCALL=TRUE)
plotres(caret.earth.mod2, trace=1, SHOWCALL=TRUE)

data(ozone1)
a <- train(O3 ~ ., data = ozone1,  method = "earth",
            tuneGrid = data.frame(degree = 2, nprune = 14))
plotmo(a, trace=1, SHOWCALL=TRUE)
plotres(a, trace=1, SHOWCALL=TRUE)

if(!interactive()) {
    dev.off()         # finish postscript plot
    q(runLast=FALSE)  # needed else R prints the time on exit (R2.5 and higher) which messes up the diffs
}
