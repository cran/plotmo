# plotmo.gbm.R: plotmo method functions for gbm objects
#               See the descriptions of the methods in plotmo.methods.R.
#
# TODO Add support for plotmo's level argument (quantile regression).

# gbm.importance is a vector of variable indices (column numbers in x),
# most important vars first, no variables with relative.influence < 1%.
# We keep a global copy of this variable to avoid calling summary.gbm
# twice (it's expensive).
#
gbm.importance <- NULL

plotmo.prolog.gbm <- function(object, ...) # called when plotmo starts
{
    order.vars.on.importance <- function(object)
    {
        # order=FALSE so importances correspond to orig variable indices
        importance <- summary(object, plotit=FALSE,     # calls summary.gbm
                              order=FALSE, normalize=TRUE)$rel.inf
        # NA assignment below so order() drops vars with importance < .01
        importance[importance < .01] <- NA
        importance <- order(importance, decreasing=TRUE, na.last=NA)
        # return a vector of variable indices, most important vars first
        importance[!is.na(importance)]
    }
    if(is.null(object$data)) # TODO could do more if object had a call component
        stop0("object has no data component, ",
              "(use keep.data=TRUE in the call to gbm)")

    unlockBinding("gbm.importance", asNamespace("plotmo"))
    gbm.importance <<- order.vars.on.importance(object)     # note <<- not <-
    lockBinding("gbm.importance", asNamespace("plotmo"))
}

get.plotmo.singles.gbm <- function(object, ...)
{
    gbm.importance    # indices of vars with importance >= 1%
}

get.plotmo.pairs.gbm <- function(object, ...)
{
    # pairs of four most important variables
    form.pairs(gbm.importance[1: min(4, length(gbm.importance))])
}

get.plotmo.x.gbm <- function(object, ...)
{
    # Return the first ntrain rows of the x matrix.  The x matrix is stored
    # with the gbm object as a vector, so we must convert it back to
    # a data.frame here, one column for each variable.
    # The use of as.integer here is copied from gbm.R.

    ntrain <- as.integer(object$train.fraction * nrow(object$data$x.order))
    x <- matrix(object$data$x, ncol=ncol(object$data$x.order))
    colnames(x) <- colnames(object$data$x.order)
    x <- data.frame(x[1:ntrain, ])

    # convert numeric columns that are actually factors
    # TODO this won't work for ordered factors?

    for(i in 1:ncol(x))
        if(typeof(object$var.levels[[i]]) == "character")
            x[[i]] <- factor(x[[i]], labels=object$var.levels[[i]])
    x
}

get.plotmo.y.gbm <- function(object, ...)
{
    ntrain <- as.integer(object$train.fraction * nrow(object$data$x.order))
    object$data$y[1:ntrain]
}

plotmo.predict.gbm <- function(object, newdata, type, ...)
{
    # predict.gbm doesn't do partial matching on type so do it here
    allowed.types <- c("link","response")
    predict(object, newdata, n.trees=object$n.trees, # calls predict.gbm
            type=allowed.types[match.choices(type, allowed.types, "type")])
}
