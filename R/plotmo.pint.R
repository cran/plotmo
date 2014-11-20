# plotmo.pint.R: plotmo method functions for prediction intervals

plotmo.pint.lm <- function(object, newdata, type, level, trace)
{
    # lm objects with weights do not support confidence intervals on new data
    if(!is.null(object$weights))
        stop0("the level argument is not supported on lm objects with weights")
    pints <- predict(object, newdata, interval="prediction", level=level)
    cints <- predict(object, newdata, interval="confidence", level=level)
    data.frame(
        lwr      = pints[,"lwr"], # intervals for prediction of new data
        upr      = pints[,"upr"],
        cint.lwr = cints[,"lwr"], # intervals for prediction of mean response
        cint.upr = cints[,"upr"])
}
plotmo.pint.glm <- function(object, newdata, type, level, trace)
{
    if(!is.null(object$weights) && !all(object$weights == object$weights[1]))
        warning0("the level argument may not be properly supported on this object built with weights")

    quant <- 1 - (1 - level) / 2 # .95 becomes .975

    predict <- predict(object, newdata, type=type, se.fit=TRUE)

    # special handling for where use used gam::gam instead of mgcv::gam
    if(class(predict) == "numeric" &&
            "package:gam" %in% search()) {
        cat("\n")
        stop0("gam objects in the \"gam\" package do not support ",
              "confidence intervals on new data")
    }
    data.frame(cint.lwr = predict$fit - quant * predict$se.fit,
               cint.upr = predict$fit + quant * predict$se.fit)
}
plotmo.pint.gam <- function(object, newdata, type, level, trace)
{
    plotmo.pint.glm(object, newdata, type, level, trace)
}
plotmo.pint.quantregForest <- function(object, newdata, type, level, trace)
{
    q0 <- (1 - level) / 2   # .95 becomes .025
    q1 <- 1 - q0            # .975

    predict <- predict(object, newdata, quantiles=c(q0, q1))

    data.frame(lwr = predict[,1], upr = predict[,2])
}
plotmo.pint.earth <- function(object, newdata, type, level, trace)
{
    if(is.null(object$cv.list))
        stop0("level arg: earth object has no cv.list (Remedy: run earth with nfold)")
    if(is.null(object$varmod))
        stop0("level arg: earth object has no varmod (Remedy: run earth with varmod.method)")
    predict(object, newdata=newdata, interval="pint", level=level)
}
