# test.c50.R: c50 tests for plotmo and plotres

library(C50)
library(rpart.plot) # for ptitanic, want data with NAs for testing
library(plotmo)
library(earth) # for etitanic
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
data(etitanic)
get.tit <- function() # abbreviated titanic data
{
    tit <- etitanic
    pclass <- as.character(tit$pclass)
    # change the order of the factors so not alphabetical
    pclass[pclass == "1st"] <- "first"
    pclass[pclass == "2nd"] <- "class2"
    pclass[pclass == "3rd"] <- "classthird"
    tit$pclass <- factor(pclass, levels=c("class2", "classthird", "first"))
    # log age is so we have a continuous predictor even when model is age~.
    set.seed(2015)
    tit$logage <- log(tit$age) + rnorm(nrow(tit))
    tit$parch <- NULL
    # by=12 gives us a small fast model with an additive and a interaction term
    tit <- tit[seq(1, nrow(etitanic), by=12), ]
}
tit <- get.tit()

c50.tree.xy <- C5.0(x=tit[,-1], y=tit[,1]) # predict pclass
plotmo(c50.tree.xy, type="prob", nresponse="first", pmethod="apartdep")
plotmo(c50.tree.xy, type="class")
# TODO following gives error: type should be either 'class', 'confidence' or 'prob'
# try(plotmo(c50.tree.xy, type="confidence"))
plotres(c50.tree.xy, type="prob", nresponse="first")

c50.tree.form <- C5.0(pclass~., data=tit) # predict pclass
plotmo(c50.tree.form, type="prob", nresponse="first")
plotmo(c50.tree.form, type="class")
# TODO following gives error: type should be either 'class', 'confidence' or 'prob'
# try(plotmo(c50.tree.form, type="confidence"))
plotres(c50.tree.form, type="prob", nresponse="first")

tit$survived <- factor(ifelse(tit$survived == 1, "yes", "no"),
                       levels = c("yes", "no"))
c50.tree.survived <- C5.0(survived~., data=tit, trials=5) # predict survived
plotmo(c50.tree.survived, type="prob", nresponse="yes")
plotmo(c50.tree.survived, type="class")
# TODO following gives error: type should be either 'class', 'confidence' or 'prob'
# try(plotmo(c50.tree.survived, type="confidence"))
plotres(c50.tree.survived, type="prob", nresponse="yes")

if(!interactive()) {
    dev.off()         # finish postscript plot
    q(runLast=FALSE)  # needed else R prints the time on exit (R2.5 and higher) which messes up the diffs
}