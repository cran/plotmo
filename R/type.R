# type.R: plotmo functions for getting the default type arg for predict() and residuals()
#         this is used when plotmo's argument "type" is NULL (the default)

# get the type for predict()
plotmo.type <- function(object, ...)
{
    UseMethod("plotmo.type")
}
plotmo.type.default <- function(object, ...)
{
    "response"
}
plotmo.type.nnet <- function(object, ...)
{
    "raw"
}
plotmo.type.knn3 <- function(object, ...)
{
    "prob"
}
plotmo.type.tree <- function(object, ...) # tree package
{
    "vector"
}
plotmo.type.fda <- function(object, ...) # mda package
{
    "class"
}
# get the type for residuals()
plotmo.residtype <- function(object, ...)
{
    UseMethod("plotmo.residtype")
}
plotmo.residtype.default <- function(object, ...)
{
    plotmo.type(object, ...) # use the predict type
}
# TRUE if predict.modelname returns probabilities.
# This is used for setting the default ylim to c(0,1).
# It can save a call get.ylim.by.dummy.plots, and also works for objects
# for which get.ylim.by.dummy.plots doesn't automatically figure out c(0,1)

is.predict.prob <- function(object, type)
{
    type.firstchar <- substr(type[1], 1, 1) # type argument to predict()

    (inherits(object, "rpart") &&
        object$method[1] == "class" &&
         type.firstchar == "p") ||

    # following not strictly necessary for earth models because
    # get.ylim.by.dummy.plots can also figure this out
    (inherits(object, "earth") &&
         !is.null(object$glm.list[[1]]) &&
         (object$glm.list[[1]]$family$family == "binomial" ||
          object$glm.list[[1]]$family$family == "quasibinomial") &&
         type.firstchar == "r") || # "r" for response

    (inherits(object, "randomForest") &&
         object$type[1] == "classification" &&
         type.firstchar == "p") ||

    (inherits(object, "C5.0") &&
         type.firstchar == "p") ||

    (inherits(object, "pre") &&
         (object$family[1] == "binomial" || object$family[1] == "multinomial") &&
         type.firstchar == "r") # response
}
is.predict.prob.wrapper <- function(object, type)
{
    # this wrapper exists because if the maintainer of a package changes the model
    # fields, we don't want plotmo to completely stop working with an error message
    is.prob <- try(is.predict.prob, silent=TRUE)
    if(is.try.err(is.prob))
        FALSE
    else
        is.prob(object, type)
}
