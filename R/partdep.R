# partdep.R: functions for partial dependence plots

# get the dataframe of variables we integrate over for partdeps
get.partdep.x <- function(pmethod, x, y, n.apartdep)
{
    switch(pmethod,
        "plotmo"   = NA,   # don't need partdep.x
        "partdep"  = x,    # full x
        "apartdep" = {     # possible subset of x
            if(nrow(x) <= n.apartdep)
                x
            else {
                stopifnot(nrow(x) == NROW(y))
                # order on y with sample_int randomly break ties in y
                index <- order(as.numeric(y), sample.int(NROW(y)))
                # select n.apartdep equally spaced rows
                index <- index[seq.int(1, nrow(x), length.out=n.apartdep)]
                x[index, , drop=FALSE]
            }
        })
}
check.grid.class <- function(x1, xgrid, predname) # paranoia
{
    class.x1 <- class(x1)[1]
    class.xgrid <- class(xgrid)[1]
    # the integer check is necessary because plotmo converts
    # integer predictors to a numeric range
    if(!(class.x1 == class.xgrid ||
        (class.x1 == "integer" && class.xgrid == "numeric"))) {
        cat("\n")
        stopf("class(%s) == \"%s\" but class(xgrid) == \"%s\"",
             predname, class.x1, class.xgrid)
    }
}
degree1.partdep.yhat <- function(object,
    type, nresponse, pmethod, inverse.func, trace,   # plotmo args
    partdep.x, xframe, ipred, pred.names, resp.levs, # internal args
    ...)
{
    if(trace >= 0)
        printf("calculating %s for %s%s",
               pmethod, pred.names[ipred],
               if (trace >= 2) "\n" else " ")
    xgrid <- xframe[[ipred]] # grid of values for predictor
    nxgrid <- length(xgrid)
    stopifnot(nxgrid >= 1)
    check.grid.class(partdep.x[[ipred]], xgrid, pred.names[ipred])
    # For efficiency, predict for all values in xgrid at once.
    # This reduces the number of calls to plotmo_predict, but requires more memory.
    expanded.partdep.x <- partdep.x[rep(1:nrow(partdep.x), times=nxgrid), ]
    expanded.partdep.x[[ipred]] <- rep(xgrid, each=nrow(partdep.x)) # gets recyled
    # plotmo_predict always returns a numeric 1 x n matrix
    yhats <- plotmo_predict(object, expanded.partdep.x, nresponse,
                            type, resp.levs, trace, inverse.func, ...)$yhat
    if(trace >= 0)
        printf("\n")
    colMeans(matrix(yhats, ncol=nxgrid), na.rm=TRUE)
}
degree2.partdep.yhat <- function(object,
    type, nresponse, pmethod, inverse.func, trace, # plotmo args
    partdep.x, x1grid, ipred1, x2grid, ipred2,     # internal args
    pred.names, resp.levs,
    ...)
{
    if(trace >= 0)
        printf("calculating %s for %s:%s %s",
               pmethod, pred.names[ipred1], pred.names[ipred2],
               if(trace >= 0 && trace < 2) "0" else if (trace >= 2) "\n")

    n1 <- length(x1grid)
    stopifnot(n1 >= 1)
    check.grid.class(partdep.x[[ipred1]], x1grid, pred.names[ipred1])

    n2 <- length(x2grid)
    stopifnot(n2 >= 1)
    check.grid.class(partdep.x[[ipred2]], x2grid, pred.names[ipred2])

    # For efficiency, predict for all values of xgrid2 for each value of xgrid1.
    # This reduces the number of calls to plotmo_predict, but requires more memory.
    yhat <- matrix(0., nrow=n1, ncol=n2) # will store predictions in here
    pacifier.i <- n1 / 10 # for pacifier
    pacifier.digit <- -1
    expanded.partdep.x <- partdep.x[rep(1:nrow(partdep.x), times=n2), ]
    for(i in 1:n1) {
        while(pacifier.i < i) { # print pacifier
            if(trace >= 0 && pacifier.digit != floor(10 * pacifier.i / n1)) {
                pacifier.digit <- floor(10 * pacifier.i / n1)
                cat(pacifier.digit)
            }
            pacifier.i <- pacifier.i + n1 / 10
        }
        expanded.partdep.x[[ipred1]] <- x1grid[i] # whole columm all the same value
        expanded.partdep.x[[ipred2]] <- rep(x2grid, each=nrow(partdep.x)) # gets recyled
        # plotmo_predict always returns a numeric 1 x n matrix
        yhats <- plotmo_predict(object, expanded.partdep.x, nresponse,
                                type, resp.levs, trace, inverse.func, ...)$yhat
        yhats <- matrix(yhats, ncol=n2)
        yhat[i,] <- colMeans(yhats, na.rm=TRUE)
        if(trace > 0)
            trace <- 0 # only show the first call to plotmo_predict
    }
    if(trace >= 0)
        cat("0\n") # print final 0 for pacifier
    matrix(yhat, nrow=n1 * n2, ncol=1)
}
