# xgboost.R:

plotmo.prolog.xgb.Booster <- function(object, object.name, trace, ...) # xgboost model
{
    stop0("xgboost models are not yet supported by plotmo and plotres")
    # if(is.null(object$call))
    #     stopf("xgb.Booster object '%s' does not have a 'call' field", object.name)
    # stopifnot(is.null(object[["x"]]))
    # stopifnot(is.null(object[["y"]]))
    # watchlist <- get.data.from.object.call.field(object, "watchlist", trace,
    #                 check.is.good.data=FALSE)
    # is.good.data(watchlist$train, "watchlist$train", trace)
    # object$x
    # stopifnot(!is.null(object$x))
    object
}
