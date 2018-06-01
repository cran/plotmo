# mlr.R
#
# TODO Currently only mlr "train" objects have explicit support.

plotmo.prolog.WrappedModel <- function(object, object.name, trace, ...)
{
    object.name <- gsub("'", "", object.name) # remove beginning and ending quotes
    callers.name <- callers.name(n=3)
    stopf("Call %s like this: %s(%s$learner.model, ...)",
          callers.name, callers.name, object.name)
}
