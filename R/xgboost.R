# xgboost.R:

plotmo.prolog.xgb.Booster <- function(object, object.name, trace, ...) # xgboost model
{
    stop0("xgboost models do not conform to standard S3 model guidelines ",
          "and are thus not supported by plotmo and plotres")
}
