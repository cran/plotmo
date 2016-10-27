# gbm.backcompat.R:
#
# TODO change name of this module? this is actually for new functions (not back compat funcs)
#
# The following functions were added in Oct 2016 for
# Paul Metcalfe's changes to glm (version 2.2 and higher).
#
# The idea is that we work with both the old and the new gbm models, and
# give error messages appropriate to the object (not to an object
# converted by to_old_gbm).

plotmo.prolog.GBMFit <- function(object, ...)
{
    if(is.null(object$gbm_data_obj))
        stop0("use keep_gbm_data=TRUE in the call to gbmt ",
              "(object$gbm_data_obj is NULL)")

    # "importance" is a vector of variable indices (column numbers in x), most
    # important vars first, no variables with relative.influence < 1%.  We attach
    # it to the object to avoid calling summary.gbm twice (it's expensive).

    attr(object, "plotmo.importance") <- order.GBMFit.vars.on.importance(object)

    object
}
order.GBMFit.vars.on.importance <- function(object)
{
    # order=FALSE so importances correspond to orig variable indices
    importance <- summary(object, plot_it=FALSE,     # calls summary.GBMFit
                          order=FALSE, normalize=TRUE)$rel_inf
    stopifnot(!is.null(importance))
    # NA assignment below so order() drops vars with importance < .01
    importance[importance < .01] <- NA
    importance <- order(importance, decreasing=TRUE, na.last=NA)
    # return a vector of variable indices, most important vars first
    importance[!is.na(importance)]
}
plotmo.singles.GBMFit <- function(object, x, nresponse, trace, all1, ...)
{
    plotmo.singles.gbm(object, x, nresponse, trace, all1, ...)
}
plotmo.pairs.GBMFit <- function(object, ...)
{
    plotmo.pairs.gbm(object, ...)
}
plotmo.x.GBMFit <- function(object, ...)
{
    plotmo.x.gbm.aux(object$gbm_data_obj$x,
                     object$gbm_data_obj$x_order,
                     object$variables$var_levels)
}
plotmo.y.GBMFit <- function(object, ...)
{
    plotmo.y.gbm.aux(object$gbm_data_obj$y, object$gbm_data_obj$x_order)
}
plotmo.predict.GBMFit <- function(object, newdata, type, ..., TRACE)
{
    plotmo.predict.gbm(object, newdata, type, ..., TRACE=TRACE)
}
gbm.short.distribution.name <- function(obj)
{
    substr(tolower(obj$distribution$name), 1, 2)
}
gbm.n.trees <- function(obj)
{
    if(!is.null(obj$n.trees)) # paranoia
        stopifnot(obj$n.trees == length(obj$trees))
    length(obj$trees)
}
gbm.train.fraction <- function(obj)
{
    train.fraction <-
        if(is.null(obj$train.fraction)) {
            # TODO following returns the wrong results
            # obj$params$train_fraction
            # TODO work around
            if(is.null(obj$gbm_data_obj))
                stop0("use keep_gbm_data=TRUE in the call to gbmt ",
                    "(obj$gbm_data_obj is NULL)")
            stopifnot(!is.null(obj$gbm_data_obj$original_data))
            train.fraction <- obj$params$num_train /
                              NROW(obj$gbm_data_obj$original_data)
            # check.numeric.scalar(train.fraction, min=0, max=1)
            # stopifnot(train.fraction > 0)
            train.fraction
        } else
            obj$train.fraction
    check.numeric.scalar(train.fraction, min=0, max=1)
    train.fraction
}
gbm.bag.fraction <- function(obj)
{
    bag.fraction <-
        if(is.null(obj$bag.fraction))
            obj$params$bag_fraction
        else
            obj$bag.fraction
    check.numeric.scalar(bag.fraction, min=0, max=1)
    bag.fraction
}
gbm.cv.folds <- function(obj)
{
    cv.folds <-
        if(is.null(obj$cv.folds))
            obj$cv_folds
        else
            obj$cv.folds
    check.numeric.scalar(cv.folds, min=1, null.ok=TRUE)
    cv.folds
}
gbm.train.error <- function(obj)
{
    train.error <- obj$train.error
    stopifnot(!is.null(train.error))
    stopifnot(is.numeric(train.error))
    stopifnot(length(train.error) == gbm.n.trees(obj))
    train.error
}
gbm.valid.error <- function(obj)
{
    valid.error <- obj$valid.error
    if(!is.null(valid.error)) {
        stopifnot(is.numeric(valid.error))
        stopifnot(length(valid.error) == gbm.n.trees(obj))
    }
    valid.error
}
gbm.oobag.improve <- function(obj)
{
    oobag.improve <- obj$oobag.improve
    if(!is.null(oobag.improve)) {
        stopifnot(is.numeric(oobag.improve))
        stopifnot(length(oobag.improve) == gbm.n.trees(obj))
    }
    oobag.improve
}
gbm.cv.error <- function(obj)
{
    cv.error <-
        if(is.null(obj$cv.error))
            obj$cv_error
        else
            obj$cv.error
    if(!is.null(cv.error)) {
        stopifnot(is.numeric(cv.error))
        stopifnot(length(cv.error) == gbm.n.trees(obj))
    }
    cv.error
}
