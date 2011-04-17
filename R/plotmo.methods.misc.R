# plotmo.methods.misc.R: plotmo method functions for miscellaneous objects
#
# See the descriptions of the methods in plotmo.methods.R.
#
# TODO add a lars test to the plotmo test suite

get.plotmo.pairs.randomForest <- function(object, env, x, ...)
{
    order.randomForest.vars.on.importance <- function(object, x)
    {
        importance <- object$importance
        if(!is.matrix(importance) ||           # sanity checks
                nrow(importance) == 0 ||
                !identical(row.names(importance), colnames(x))) {
            warning0("randomForest object has an invalid ",
                     "\"importance\" field, try all2=T")
            return(NULL)
        }
        # vector of var indices, most important vars first
        order(importance[,1], decreasing=TRUE)
    }
    if(is.null(object$forest))
        stop0("object has no \"forest\" component ",
              "(use keep.forest=TRUE in the call to randomForest)")
    importance <- order.randomForest.vars.on.importance(object, x)
    if(is.null(importance))
        return(NULL)
    # pairs of four most important variables
    form.pairs(importance[1: min(4, length(importance))])
}
get.plotmo.default.type.lars <- function(obj, ...)
{
    "fit"
}
plotmo.predict.lars <- function(object, newdata, type, se.fit, ...)
{
    if(se.fit)
        stop0("predict.lars does not support \"se\"")

    if(pmatch(type, "coefficients", 0))
        stop0("predict.lars type=\"coefficients\" cannot be used with plotmo")

    # just a skeleton for now, s and mode not specified
    predict(object, newx=newdata, type=type)$fit
}
get.plotmo.default.type.bruto <- function(obj, ...)
{
    "fitted"
}
plotmo.predict.bruto <- function(object, newdata, type, se.fit, ...)
{
    if(se.fit)
        stop0("predict.bruto does not support \"se\"")

    # TODO fails: predict.bruto returned a response of the wrong length (got 31 expected 27)
    predict(object, newx=as.matrix(newdata), type=type)
}
plotmo.predict.lda <- function(object, newdata, type, se.fit, trace)
{
    if(se.fit)
        stop0("predict.lda does not support \"se\"")
    y <- predict(object, newdata, type=type)    # calls predict.lda
    get.lda.yhat(object, y, type, trace)
}
plotmo.predict.qda <- function(object, newdata, type, se.fit, trace)
{
    if(se.fit)
        stop0("predict.qda does not support \"se\"")
    y <- predict(object, newdata, type=type)    # calls predict.qda
    get.lda.yhat(object, y, type, trace)
}
# Special handling for MASS lda and qda predicted response, which
# is a data.frame with columns "x", "class", and "posterior".
# Here we use plotmo's type argument to choose a column.

get.lda.yhat <- function(object, yhat, type, trace)
{
    yhat1 <- switch(match.choices(type,
                         c("response", "ld", "class", "posterior"), "type"),
           yhat$x,              # response (default)
           yhat$x,              # ld
           yhat$class,          # class
           yhat$posterior)      # posterior

    if(is.null(yhat1)) {
        msg <- paste0(
            if(!is.null(yhat$x)) "type=\"response\" " else "",
            if(!is.null(yhat$class)) "type=\"class\" " else "",
            if(!is.null(yhat$posterior)) "type=\"posterior\" " else "")
        stop0("type=\"", type, "\" is illegal for predict.", class(object)[1], ".  ",
              if(nchar(msg)) paste("Use one of:", msg) else "",
              "\n")
    }
    yhat1
}
