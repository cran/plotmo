# type.R: plotmo functions for getting the default type arg for predict() and residuals()
#         this is used when plotmo's argument "type" is NULL (the default)

# get the type for predict()
plotmo.type <- function(object, ..., TRACE)
{
    UseMethod("plotmo.type")
}
plotmo.type.default <- function(object, ..., TRACE)
{
    "response"
}
plotmo.type.nnet <- function(object, ..., TRACE)
{
    "raw"
}
plotmo.type.knn3 <- function(object, ..., TRACE)
{
    "prob"
}
plotmo.type.tree <- function(object, ..., TRACE) # tree package
{
    "vector"
}
plotmo.type.fda <- function(object, ..., TRACE) # mda package
{
    "class"
}
# get the type for residuals()
plotmo.residtype <- function(object, ..., TRACE)
{
    UseMethod("plotmo.residtype")
}
plotmo.residtype.default <- function(object, ..., TRACE)
{
    plotmo.type(object, ..., TRACE=TRACE) # use the predict type
}
# TRUE if we are predicting probabilities.
# This is used for setting the default ylim to c(0,1).
# Not always reliable (but if wrong, the user can override with explicit ylim arg).
# It can save a call get.ylim.by.dummy.plots, and also works for objects
# for which get.ylim.by.dummy.plots doesn't automatically figure out c(0,1)

is.yaxis.a.probability.aux <- function(object, type, trace)
{
    if(inherits(object, "WrappedModel")) { # mlr package
        # will be we be predicting probabilities?
        # TODO this will be wrong if use say nresponse="response" in call to plotmo
        call <- object[["call"]]
        if(!is.null(call)) {
            # TODO assumes environment for learner is available and correct
            learner <- eval(call[["learner"]])
            if(!is.null(learner)) {
                predict.type <- mlr::getLearnerPredictType(learner)
                if(substr(predict.type[1], 1, 1) == "p") # prob
                    return(TRUE)
            }
        }
        # continue processing, but use the learner.model
        object <- object$learner.model
    }
    type.firstchar <- substr(type[1], 1, 1) # type argument to predict()

    substr(type[1], 1, 4) == "prob" || # catchall

    (inherits(object, "rpart") &&
        object$method[1] == "class" &&
         type.firstchar == "p") ||

    # following not strictly necessary for earth models because
    # get.ylim.by.dummy.plots can also figure this out
    # The "r" below is for "response"
    (inherits(object, "earth") &&
        is.nomial(object$glm.list[[1]]) && type.firstchar == "r") ||

    # the "r" below is for "response"
    (inherits(object, c("glm", "glmnet", "pre")) &&
        is.nomial(object) && type.firstchar == "r") ||

    (inherits(object, "cv.glmnet") &&
         !is.null(object$glmnet.fit$classnames)) ||

    (inherits(object, "randomForest") &&
        is.character(object$type) &&
        object$type[1] == "classification" &&
        type.firstchar == "p") ||

    (inherits(object, "C5.0") &&
         type.firstchar == "p")
}
is.yaxis.a.probability <- function(object, type, trace)
{
    # This wrapper exists because we don't want plotmo to completely stop
    # (issue an error) if a package changes the model fields.  (This function is
    # vulnerable to changes because it accessess internal fields in multiple
    # different models.)

    is.prob <- try(is.yaxis.a.probability.aux(object, type, trace), silent=trace < 2)
    if(is.try.err(is.prob))
        FALSE
    else
        is.prob
}
