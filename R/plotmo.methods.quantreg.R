# plotmo.quantreg.R: plotmo method functions for the quantreg package.
#
# Currently we support only rq (which for some reason
# returns objects of class "rqs", so we support both
# "rq" and"rsq",

plotmo.predict.rq <- function(object, newdata, type, ...) # quantreg package
{
    if(type != "response")
        warning0("plotmo.predict.rq: ignored type=\"", type, "\"")
    if(is.null(object$tau))
        stop0("rq object has no \"tau\" field")
    # the following may return multiple responses,
    # this is handled later in plotmo.get.nresponse.rq
    predict(object, newdata=newdata, type="none")
}

plotmo.predict.rqs <- function(object, newdata, type, ...) # quantreg package
{
    plotmo.predict.rq(object, newdata, type, ...)
}

# quantreg::predict.rq returns a column for each value in the tau arg
# in the call to rq.  Select the column corresponding to tau=.5

plotmo.get.nresponse.rq <- function(object, nresponse, y)
{
    if(is.na(nresponse) && NCOL(y) > 1) {
        nresponse <- which(object$tau == .5)
        if(length(nresponse) == 0) { # no tau=.5?
            nresponse <- length(object$tau) %/% 2
            warning0(
                "rq object has multiple taus, none are tau=.5, so plotting tau=",
                object$tau[nresponse])
        }
        nresponse <- nresponse[1] # needed if tau=.5 specified twice in call to rq
    }
    nresponse
}

plotmo.get.nresponse.rqs <- function(object, nresponse, y)
{
    plotmo.get.nresponse.rq(object, nresponse, y)
}

plotmo.pint.rq <- function(object, newdata, type, level, trace) # quantreg package
{
    if(length(object$tau) == 1)
        stop0("object was built with single tau (tau=", object$tau, ")\n",
              "Plotmo needs multiple taus to plot confidence bands, ",
              "something like tau=c(.05,.5,.95)")
    q0 <- (1 - level) / 2   # .95 becomes .025
    q1 <- 1 - q0            # .975
    tau <- object$tau
    i0 <- which(abs(tau - q0) < 1e-8) # 1e-8 allows limited precision
    i1 <- which(abs(tau - q1) < 1e-8)
    if(length(i0) == 0 || length(i1) == 0) {
        i0 <- 1
        i1 <- length(tau)
        warning0(
            "You specified level=", level, " but rq was called with tau=",
            if(length(tau) == 1)
                tau
            else
                sprintf("c(%s)", paste(tau, collapse=", ")),
            "\n         Try plotmo level=", 1 - 2 * tau[1],
            " to make this warning go away",
            "\n         Continuing anyway, with confidence bands for tau=",
            tau[i0], " and ", tau[i1])
    }
    predict <- predict(object, newdata=newdata, type="none")
    data.frame(lwr = predict[,i0], upr = predict[,i1])
}

plotmo.pint.rqs <- function(object, newdata, type, level, trace) # quantreg package
{
    plotmo.pint.rq(object, newdata, type, level, trace)
}
