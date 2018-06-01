# test.mlr.R: test the "mlr" package with plotmo and plotres

library(mlr)
library(plotmo)
library(rpart.plot)
library(earth)
options(warn=1) # print warnings as they occur

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
                         expected.msg, substr(msg[1], 1, 1000)))
    } else
        stop("Did not get expected error: ", expected.msg)
}
if(!interactive())
    postscript(paper="letter")

cat("==simple regression model with earth ============================================\n")

set.seed(2018)
library(earth)

task <- makeRegrTask(data=trees, target="Volume")
lrn <- makeLearner("regr.earth", degree=2)
regr.earth = train(lrn, task)

# compare to regular earth model
earth <- earth(Volume~., data=trees, degree=2)
set.seed(2018)
# SHOWCALL is just a testing thing, so we can see who created the plot on the plot itself
plotres(regr.earth$learner.model, SHOWCALL=TRUE)
set.seed(2018)
plotres(earth, SHOWCALL=TRUE)

plotmo(regr.earth$learner.model, trace=1, SHOWCALL=TRUE)
plotmo(earth, trace=1, SHOWCALL=TRUE)

# compare partial dependence plots from mlr and plotmo packages
plotmo(earth, degree1="Girth", degree2=0, pmethod="partdep", SHOWCALL=TRUE,
       pt.col=2, grid.col="lightgray")
set.seed(2018)
pd <- generatePartialDependenceData(regr.earth, task, "Girth")
print(plotPartialDependence(pd, data = getTaskData(task)))

cat("==regression model with rpart====================================================\n")

set.seed(2018)
library(earth) # for etitanic data
data(etitanic)
task <- makeRegrTask(data=etitanic, target="survived")
lrn = makeLearner("regr.rpart")
regr.rpart = train(lrn, task)

# compare to regular rpart model
rpart <- rpart(survived~., data=etitanic, method="anova")
# earth <- earth(survived~., data=etitanic, degree=2)
set.seed(2018)
plotres(regr.rpart$learner.model, SHOWCALL=TRUE)
set.seed(2018)
plotres(rpart, SHOWCALL=TRUE)

plotmo(regr.rpart$learner.model, trace=1, SHOWCALL=TRUE)
plotmo(rpart, trace=1, SHOWCALL=TRUE)

# compare partial dependence plots from mlr and plotmo packages
plotmo(rpart, degree1="pclass", degree2=0, pmethod="partdep")
set.seed(2018)
# TODO following fails
pd <- generatePartialDependenceData(regr.rpart, task, "pclass") # Warning: argument is not numeric or logical: returning NA
try(print(plotPartialDependence(pd, data = getTaskData(task)))) # Error: Discrete value supplied to continuous scale

cat("==classification model with rpart================================================\n")

set.seed(2018)
library(earth) # for etitanic data
data(etitanic)
task <- makeClassifTask(data=etitanic, target="survived")

# TODO following causes Error: classif.earth: Setting parameter glm without available description object
# lrn <- makeLearner("classif.earth", degree=2, glm=list(family=binomial))

lrn <- makeLearner("classif.rpart")
n <- nrow(etitanic)
train.set <- sample(n, size=2/3*n)
test.set <- setdiff(1:n, train.set)
classif.rpart <- train(lrn, task, subset=train.set)

# compare to regular rpart model
rpart <- rpart(survived~., data=etitanic, method="class")
set.seed(2018)
plotres(classif.rpart$learner.model, SHOWCALL=TRUE)
set.seed(2018)
plotres(rpart, type="prob", SHOWCALL=TRUE)

plotmo(classif.rpart$learner.model, trace=1, SHOWCALL=TRUE)
plotmo(rpart, trace=1, SHOWCALL=TRUE)

# compare partial dependence plots
plotmo(rpart, degree1="pclass", degree2=0, pmethod="partdep")
set.seed(2018)
# TODO following fails
pd <- generatePartialDependenceData(classif.rpart, task, "pclass") # Warning: argument is not numeric or logical: returning NA
try(print(plotPartialDependence(pd, data = getTaskData(task)))) # Error: Discrete value supplied to continuous scale

cat("==examples from plotmo-notes.pdf ===============================================\n")

#-- Regression model with mlr -------------------------------------------

library(mlr)
library(plotmo)
lrn <- makeLearner("regr.svm")
fit1 <- train(lrn, bh.task)

# generate partial dependence plots for all variables
# we use "apartdep" and not "partdep" to save testing time
plotmo(fit1$learner.model, pmethod="apartdep")

# generate partial dependence plot for just "lstat"
plotmo(fit1$learner.model,
       degree1="lstat",           # what predictor to plot
       degree2=0,                 # no interaction plots
       pmethod="partdep",         # generate partial dependence plot
       pt.col=2, grid.col="gray") # optional bells and whistles

# compare to the function provided by the mlr package
set.seed(2018)
pd <- generatePartialDependenceData(fit1, bh.task, "lstat")
print(plotPartialDependence(pd, data = getTaskData(bh.task)))
# # TODO following fails: Error: Discrete value supplied to continuous scale
# pd <- generatePartialDependenceData(fit1, bh.task, "chas")
# plotPartialDependence(pd, data = getTaskData(bh.task))

#-- Classification model with mlr ---------------------------------------

lrn <- makeLearner("classif.rpart", predict.type = "prob", minsplit = 10)
fit2 <- train(lrn, iris.task)

# generate partial dependence plots for all variables
# TODO plotmo can plot the response for only one class at a time
plotmo(fit2$learner.model,
       nresponse="virginica",     # what response to plot
       type="prob",               # type gets passed to predict.rpart
       pmethod="apartdep")        # generate partial dependence plot

# generate partial dependence plot for just "Petal.Length"
plotmo(fit2$learner.model,
       degree1="Petal.Length",    # what predictor to plot
       degree2=0,                 # no interaction plots
       nresponse="virginica",     # what response to plot
       type="prob",               # type gets passed to predict.rpart
       pmethod="apartdep")        # generate partial dependence plot

# compare to the function provided by the mlr package
set.seed(2018)
pd <- generatePartialDependenceData(fit2, iris.task, "Petal.Length")
print(plotPartialDependence(pd, data = getTaskData(iris.task)))

cat("==lda example from mlr documentation, and plotmo error handling =================\n")

set.seed(2018)
data(iris)
task <- makeClassifTask(data=iris, target="Species")
lrn <- makeLearner("classif.lda")
n <- nrow(iris)
train.set <- sample(n, size=2/3*n)
test.set <- setdiff(1:n, train.set)
classif.lda <- train(lrn, task, subset=train.set)
expect.err(try(plotres(classif.lda$learner.model)), "plotres does not (yet) support type=\"class\" for \"lda\" objects")
expect.err(try(plotres(classif.lda$learner.model, type="response")), "predict.lda returned multiple columns (see above) but nresponse is not specified")
expect.err(try(plotres(classif.lda$learner.model, type="response", nresponse="nonesuch")), "nresponse=\"nonesuch\" is not allowed")
expect.err(try(plotres(classif.lda$learner.model, type="response", nresponse=0)), "nresponse=0 but it should be at least 1")
expect.err(try(plotres(classif.lda$learner.model, type="response", nresponse=99)), "nresponse is 99 but the number of columns is only 2")
expect.err(try(plotmo(classif.lda)), "Call plotmo like this: plotmo(classif.lda$learner.model, ...)")
expect.err(try(plotres(classif.lda)), "Call plotres like this: plotres(classif.lda$learner.model, ...)")
plotres(classif.lda$learner.model, type="response", nresponse="LD2", SHOWCALL=TRUE)
plotmo(classif.lda$learner.model, pmethod="apartdep", SHOWCALL=TRUE)
plotmo(classif.lda$learner.model, type="response", nresponse="LD2", all2=TRUE, SHOWCALL=TRUE)

if(!interactive()) {
    dev.off()         # finish postscript plot
    q(runLast=FALSE)  # needed else R prints the time on exit (R2.5 and higher) which messes up the diffs
}
