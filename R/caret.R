# caret.R: plotmo functions for caret objects
#
# TODO Currently only caret "train" objects have explicit support.

# sanity check that object a caret train object
# (since "train" is a quite generic name)
check.is.caret.train.object <- function(object)
{
    class <- class(object)[1]
    stopifnot.string(class)
    mod <- object[["finalModel"]]
    if(class != "train" || is.null(mod) || !is.list(mod))
        stop0("unrecognized \"train\" object ",
              "(was expecting a train object from the caret package)")
}
# determine "type" arg for predict()
plotmo.type.train <- function(object, ...)
{
    check.is.caret.train.object(object)
    "raw"
}
# determine "type" arg for residuals()
plotmo.residtype.train <- function(object, ...)
{
    check.is.caret.train.object(object)
    "raw"
}
