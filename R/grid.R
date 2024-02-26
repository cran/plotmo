# grid.R: functions for creating the grid of values to be plotted in plotmo graphs

# Get the x matrix (actually a data.frame) with median values (or first level
# for factors), ngrid1 rows, all rows identical, nrow(xgrid) is ngrid1.

get.degree1.xgrid <- function(x, grid.func, grid.levels, pred.names, ngrid1)
{
    stopifnot(!is.null(pred.names))
    check.grid.levels.arg(x, grid.levels, pred.names)
    xgrid <- data.frame(matrix(0, ngrid1, ncol(x), byrow=TRUE))
    for(ipred in seq_len(ncol(x)))
        xgrid[[ipred]] <- get.fixed.gridval(x[[ipred]], pred.names[ipred],
                                            grid.func, grid.levels)
    warn.if.not.all.finite(xgrid, "'xgrid' for degree1 plots")
    colnames(xgrid) <- pred.names
    xgrid
}
# Update xgrid for the predictor currently being plotted.
# That is, replace this predictor's column with a range of values.
# For factors or discrete variables, we shorten the frame to match the nbr of levels.

get.degree1.xframe <- function(xgrid, x, ipred, ngrid1,
                               ndiscrete, ux.list, extend, mean)
{
    x1 <- x[[ipred]]
    # uxlist is a list, each elem is the unique levels for corresponding column of x
    u1 <- ux.list[[ipred]]
    if(is.factor(x1) && length(u1) > ngrid1)
        stop0("ngrid1=", ngrid1, " is less than the number ",
              "of levels ",  length(u1), " in '",
              colnames(x)[ipred],
              "'\n       Workaround: call plotmo with ngrid1=", length(u1))
    if(is.factor(x1) || is.logical(x1) || length(u1) <= ndiscrete) {
        levels <- get.all.levs(x1, u1)
        xframe <- xgrid[1:length(levels), , drop=FALSE] # shorten xframe
        xframe[[ipred]] <- levels
    } else {
        xframe <- xgrid
        xrange <- range1(x1)
        if(extend != 0) {
            # extend the range of x (TODO consider allowing extend with discrete vars)
            stopifnot(xrange[2] >= xrange[1])
            ext <- extend * (xrange[2] - xrange[1])
            xrange[1] <- xrange[1] - ext
            xrange[2] <- xrange[2] + ext
        }
        xval <- seq(from=xrange[1], to=xrange[2], length.out=ngrid1)
        # # following commented out because it causes cliffs to slope more than necessary
        # # e.g. test.fac.R plotmo(rpart(survived ~ pclass.num+parch.int, data=et))
        # if(is.integer(x1)) {
        #     xval <- unique(as.integer(xval))
        #     if(length(xval) < ngrid1)
        #         xframe <- xframe[1:length(xval), , drop=FALSE] # shorten xframe
        # }
        xframe[[ipred]] <- xval
    }
    xframe
}
# We want to display discrete variables in degree1 plots as quantized.
# (Factors get handled elsewhere.)  So if a variable is discrete, then
# modify the xframe and yhat to do so.  For example, an xframe that was
#
#      pclass yhat
#           1  1.1
#           2  2.2
#           3  3.3
#
# becomes
#
#      pclass yhat
#         1.0  1.1
#         1.5  1.1
#         1.5  2.2
#         2.5  2.2
#         2.5  3.3
#         3.0  3.3

blockify.degree1.frame <- function(xframe, yhat, intervals,
                                   ipred, ux.list, ndiscrete)
{
    u1 <- ux.list[[ipred]]
    # TODO the integral check is necessary for compatibility with blockify.degree2.frame
    # (the code here can handle non integers but the code in blockify.degree2.frame can't)
    if(length(u1) <= ndiscrete && !is.factor(xframe[[ipred]]) &&
            !inherits(u1, "Date") && is.integral(u1)) {
        # discrete, so duplicate each elem in yhat
        yhat <- rep(yhat, each=2)
        if(!is.null(intervals)) {
            new.intervals <- data.frame(
                                lwr = rep(intervals$lwr, each=2),
                                upr = rep(intervals$upr, each=2))
            if(!is.null(intervals$fit))
                new.intervals$fit <- rep(intervals$fit, each=2)
            if(!is.null(intervals$cint.lwr)) {
                new.intervals$cint.lwr <- rep(intervals$cint.lwr, each=2)
                new.intervals$cint.upr <- rep(intervals$cint.upr, each=2)
            }
            intervals <- new.intervals
        }
        # duplicate each row of xframe, except the first and last row
        xframe <- xframe[rep(seq_len(nrow(xframe)), each=2), , drop=FALSE]
        if(nrow(xframe) >= 4) {
            x1 <- xframe[[ipred]]
            for(i in seq(2, length(x1)-1, by=2))
                x1[i] <- x1[i+1] <- (x1[i] + x1[i+1]) / 2
            xframe[[ipred]] <- x1
        }
    }
    list(xframe=xframe, yhat=yhat, intervals=intervals)
}
# Get the x matrix (actually a data.frame) to plot in degree2 plots.
# Each row of xgrid is identical (the medians).

get.degree2.xgrid <- function(x, grid.func, grid.levels, pred.names, ngrid2)
{
    check.grid.levels.arg(x, grid.levels, pred.names)
    xgrid <- list(ncol(x))
    for(ipred in seq_len(ncol(x)))
        xgrid[[ipred]] <- get.fixed.gridval(x[[ipred]], pred.names[ipred],
                                            grid.func, grid.levels)
    warn.if.not.all.finite(xgrid, "'xgrid' for degree2 plots")
    xgrid <- as.data.frame(xgrid)
    colnames(xgrid) <- pred.names
    xgrid[seq_len(ngrid2^2), ] <- xgrid
    xgrid
}
# Update xgrid for the predictor pair currently being plotted (ipred1
# and ipred2 are column numbers in x).  That is, replace two columns
# with a range of values.
#
# This will also shorten xgrid if possible (i.e. if predictor is discrete
# with number of discrete values less than ngrid2, typically because
# predictor is a factor.)  This shortening is for efficiency later,
# because it means we avoid duplicate cases in xgrid.

get.degree2.xframe <- function(xgrid, x, ipred1, ipred2,
                               ngrid2, xranges, ux.list, ndiscrete)
{
    ret1 <- get.degree2.xframe.aux(xgrid, x, ipred1,
                                   ngrid2, xranges, ux.list, ndiscrete)
    ret2 <- get.degree2.xframe.aux(xgrid, x, ipred2,
                                   ngrid2, xranges, ux.list, ndiscrete)
    # pack x1grid and x2grid into xgrid
    if(ret1$n != ngrid2 || ret2$n != ngrid2)
        xgrid <- xgrid[1:(ret1$n * ret2$n), , drop=FALSE] # shorten xgrid
    xgrid[[ipred1]] <- ret1$xgrid # will recycle
    xgrid[[ipred2]] <- rep(ret2$xgrid, each=ret1$n)
    list(xframe=xgrid, x1grid=ret1$xgrid, x2grid=ret2$xgrid)
}
get.degree2.xframe.aux <- function(xgrid, x, ipred1,
                                   ngrid2, xranges, ux.list, ndiscrete)
{
    n1 <- ngrid2 # will change if ipred1 is discrete
    u1 <- ux.list[[ipred1]]
    nlevs1 <- length(u1)
    if(is.factor(x[[ipred1]]) && nlevs1 > ngrid2)
        stop0("ngrid2=", ngrid2, " is less than the number",
              " of levels ", nlevs1, " in '",
              colnames(x)[ipred1],
          "'\n       Workaround: call plotmo with ngrid2=", length(u1))
    x1 <- x[[ipred1]]
    x1grid <-
        if(is.factor(x1) || is.logical(x1) || nlevs1 <= ndiscrete) { # discrete?
            n1 <- nlevs1
            x1grid <- get.all.levs(x1, u1)
        } else
            seq(from=xranges[1,ipred1], to=xranges[2,ipred1], length.out=ngrid2)
    if(is.integer(x1)) {
        x1grid <- unique(as.integer(x1grid))
        n1 <- length(x1grid)
    }
    list(xgrid=x1grid, n=n1)
}
# we want to draw discrete variables in persp and contour plots using "blocks"

blockify.degree2.frame <- function(x, yhat, x1grid, x2grid,
                                   ipred1, ipred2, ux.list, ndiscrete)
{
    is.discrete2 <- function(ipred, x1grid)
    {
        if(is.factor(x[[ipred]]))
            return(TRUE)
        u1 <- ux.list[[ipred]]
        # the integral check is necessary with the current
        # implementation which adds/subtracts a hardcoded .499
        # TODO make this like blockify.degree1.frame (which can handle non integers)
        length(u1) <= ndiscrete && is.integral(x1grid)
    }
    if(is.discrete2(ipred1, x1grid)) {
        yhat <- rep(yhat, each=2)                     # duplicate each elem in yhat
        x1grid <- rep(x1grid, each=2)                 # duplicate each elem in x1grid
        is.even <- (1:length(x1grid)) %% 2 == 0
        x1grid[!is.even] <- x1grid[!is.even] - .499   # sub .5 from odd elems
        x1grid[is.even]  <- x1grid[is.even]  + .499   # add .5 to even elems
    }
    if(is.discrete2(ipred2, x2grid)) {
        # duplicate each block in yhat (each block has n1 elements)
        y.old <- yhat
        yhat <- double(2 * length(yhat))
        n1 <- length(x1grid)
        for(i in 1:length(x2grid)) {
            start <- n1 * (i-1)
            end   <- n1 * i
            yhat[(2 * start + 1): (2 * end)] <- y.old[(start + 1): end]
        }
        x2grid <- rep(x2grid, each=2)                 # duplicate each elem in x2grid
        is.even <- (1:length(x2grid)) %% 2 == 0
        x2grid[!is.even] <- x2grid[!is.even] - .499   # sub .5 from odd elems
        x2grid[is.even]  <- x2grid[is.even]  + .499   # add .5 to even elems
    }
    list(yhat=yhat, x1grid=x1grid, x2grid=x2grid)
}
# if x is a factor
#   return a factor vector with nlevs elements, e.g. pclass1, pclass2, pclass3.
# else
#   return a vector with all unique values in x, e.g. 1,2,3 or FALSE, TRUE

get.all.levs <- function(x, levels)
{
    if(!is.factor(x))
        return(levels)

    # TODO Sanity check, quite expensive, make sure no gaps in factor coding
    #      Could remove this if convert levels to factors in a better way below?
    range <- range(as.numeric(x), na.rm=TRUE)
    if(range[1] < 1 || range[2] > length(levels))
        stop0("internal error: illegal factor range ", range[1], " ",  range[2],
              " for levels ", quotify(levels))

    if(is.ordered(x))
        ordered(1:length(levels), labels=levels)
    else
        factor(1:length(levels), labels=levels)
}
# Print the grid values, must do some finagling for a nice display
print_grid_values <- function(xgrid, trace)
{
    trace1(trace, "\n") # extra space when tracing
    row <- xgrid[1, , drop=FALSE]
    names(row) <- c(paste("plotmo grid:   ", names(row)[1]), names(row)[-1])
    rownames(row) <- ""
    print(row)
    trace1(trace, "\n")
}
