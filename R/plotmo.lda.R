# plotmo.lda.R: plotmo methods for lda and qda objects

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
