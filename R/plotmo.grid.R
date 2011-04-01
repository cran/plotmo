# plotmo.grid.R: functions for creating the grid of values to be plotted in plotmo graphs.

# Get the x matrix (actually a data.frame) widh median values (or
# first level for factors), ngrid1 rows, all rows identical.

get.degree1.xgrid <- function(x, grid.func, grid.levels, pred.names, ngrid1)
{
    stopif(is.null(pred.names))
    check.grid.levels.arg(x, grid.levels, pred.names)
    xgrid <- data.frame(matrix(0, ngrid1, ncol(x), byrow=TRUE))
    for(ipred in 1:ncol(x))
        xgrid[[ipred]] <- get.grid.lev(x[[ipred]], pred.names[ipred],
                                       grid.func, grid.levels)
    warn.if.not.all.finite(xgrid, "\"xgrid\" for degree1 plots")
    colnames(xgrid) <- pred.names
    xgrid
}
# Update xgrid for the predictor currently being plotted.
# Do this by replacing this predictor's column with a range of values.
# For factors we return a shorter frame, with nlevels rows.

get.degree1.xframe <- function(xgrid, x, ipred, ngrid1)
{
    xframe <- xgrid
    xcol <- x[[ipred]]
    if(is.factor(xcol)) {
        levels <- get.all.factor.levels(xcol)
        xframe <- xframe[1:nlevels(levels), ]
        xframe[[ipred]] <- levels
    } else {
        xrange <- range1(xcol, finite=TRUE)
        xframe[[ipred]] <- seq(from=xrange[1], to=xrange[2], length=ngrid1)
    }
    xframe
}
# Get the x matrix (actually a data.frame) to plot in degree2 plots.
# Each row of xgrid is identical.  For numeric predictors, each column
# is the column median for that row of x.  For factors, each column is the first
# level of the factor.  (That may be changed with grid.func and grid.levels.)

get.degree2.xgrid <- function(x, grid.func, grid.levels, pred.names, ngrid2)
{
    stopif(is.null(pred.names))
    check.grid.levels.arg(x, grid.levels, pred.names)
    xgrid <- list(ncol(x))
    for(ipred in 1:ncol(x))
        xgrid[[ipred]] <- get.grid.lev(x[[ipred]], pred.names[ipred],
                                       grid.func, grid.levels)
    warn.if.not.all.finite(xgrid, "\"xgrid\" for degree2 plots")
    names(xgrid) <- pred.names
    xgrid <- as.data.frame(xgrid)
    xgrid[1:ngrid2^2, ] <- xgrid
    xgrid
}
# Update xgrid for the predictor pair currently being plotted (ipred1
# and ipred2 are column numbers in x).  That is, replace two columns
# with a range of values.

get.degree2.xframe <- function(xgrid, x, ipred1, ipred2, ngrid2, xranges)
{
    n1 <- ngrid2 # will change only if ipred1 is a factors
    if(is.factor(xgrid[[ipred1]])) {
        grid1 <- get.all.factor.levels(xgrid[[ipred1]])
        n1 <- nlevels(grid1)
        xgrid <- xgrid[1:(n1 * ngrid2), ]
    } else
        grid1 <- seq(from=xranges[1,ipred1], to=xranges[2,ipred1], length=ngrid2)

    xgrid[[ipred1]] <- grid1 # will recycle

    if(is.factor(xgrid[[ipred2]])) {
        grid2 <- get.all.factor.levels(xgrid[[ipred2]])
        ngrid2 <- nlevels(grid2)
        xgrid <- xgrid[rep(1:n1, times=ngrid2), ]
    } else
        grid2 <- seq(from=xranges[1,ipred2], to=xranges[2,ipred2], length=ngrid2)

    xgrid[[ipred2]] <- rep(grid2, each=n1)

    list(xframe=xgrid, grid1=as.numeric(grid1), grid2=as.numeric(grid2))
}
# We want to draw factors in persp plots using "blocks".  So if factors
# in x then duplicate elements and tweak so will plot as blocks.

fix.frame.for.persp <- function(x, y.predict, grid1, grid2, ipred1, ipred2)
{
    indices <- 1:length(y.predict)
    if(is.factor(x[[ipred1]])) {
        y.predict <- rep(y.predict, each=2) # duplicate each elem in y.predict
        grid1 <- rep(grid1, each=2)         # duplicate each elem in grid1
        is.even <- (1:length(grid1)) %% 2 == 0
        grid1[!is.even] <- grid1[!is.even] - .499   # sub .5 from odd elems
        grid1[is.even]  <- grid1[is.even]  + .499   # plus .5 to odd elems
    }
    if(is.factor(x[[ipred2]])) {
        # duplicate each block in y.predict (each block has n1 elements)
        y.old <- y.predict
        y.predict <- double(2 * length(y.predict))
        n1 <- length(grid1)
        for(i in 1:length(grid2)) {
            start <- n1 * (i-1)
            end   <- n1 * i
            y.predict[(2 * start + 1): (2 * end)] <- y.old[(start + 1): end]
        }
        grid2 <- rep(grid2, each=2)         # duplicate each elem in grid2
        indices <- 1:length(grid2)
        is.even <- (1:length(grid2)) %% 2 == 0
        grid2[!is.even] <- grid2[!is.even] - .499   # sub .5 from odd elems
        grid2[is.even]  <- grid2[is.even]  + .499   # plus .5 to odd elems
    }
    list(x=x, y.predict=y.predict, grid1=grid1, grid2=grid2)
}
# Check grid.levels arg passed in by the user.  This checks that the names
# of the list elements are indeed predictor names.  The actual levels will
# be checked later in get.grid.lev.

check.grid.levels.arg <- function(x, grid.levels, pred.names)
{
    if(!is.null(grid.levels)) { # null is the default value
        if(!is.list(grid.levels))
            stop0("grid.levels must be a list.  ",
                  "Example: grid.levels=list(sex=\"male\")")
        for(name in names(grid.levels))
            if(!pmatch(name, pred.names, 0))
                stop0("illegal variable name \"", name, "\" in grid.levels")
    }
}
# xcol is a column in the data matrix x

get.grid.lev <- function(xcol, pred.name, grid.func, grid.levels)
{
    lev <- NA
    ilev <- 1
    if(!is.null(grid.levels)) {
        # look for pred.name in the grid.levels list, if found use its value
        iname <- which(pmatch(names(grid.levels), pred.name, duplicates.ok=TRUE) == 1)
        if(length(iname) > 1)
            stop0("bad grid.levels argument (\"",
                  names(grid.levels)[iname[1]], "\" and \"",
                  names(grid.levels)[iname[2]],
                  "\" both match \"", pred.name, "\")")
        else if(length(iname)) {
            lev <- grid.levels[[iname]]
            if(length(lev) > 1 || is.na(lev))
                stop0("illegal value for ", pred.name, " in grid.levels")
            if(is.factor(xcol)) {
                lev.name <- grid.levels[[iname]]
                if(!is.character(lev.name) || length(lev.name) != 1)
                    stop0("illegal level for \"",
                          names(grid.levels)[iname], "\" in grid.levels ",
                          "(specify factor levels with a string)")
                lev.names <- levels(xcol)
                ilev <- pmatch(lev.name, lev.names, 0)
                if(!ilev)
                    stop0("illegal level \"", lev.name, "\" for \"",
                          pred.name, "\" in grid.levels (allowed levels are ",
                          paste.quoted.names(lev.names), ")")
            }
            else if(!(is.numeric(lev) || is.logical(lev)) || !is.finite(lev))
                stop0("illegal value for ", pred.name, " in grid.levels")
        }
    }
    if(is.factor(xcol)) {
        lev.names <- levels(xcol)
        lev <- factor(lev.names, levels=lev.names)[ilev]
    } else if(is.na(lev))
        lev <- grid.func(xcol, na.rm=TRUE)
    lev
}
# return a factor vector with nlevels elements, e.g. pclass1, pclass2, pclass3.

get.all.factor.levels <- function(x)
{
    levels <- levels(x)

    # TODO sanity check, quite expensive, make sure no gaps in numbering
    range <- range(as.numeric(x))
    if(range[1] < 1 || range[2] > length(levels))
        stop0("internal error: bad factor range ", range[1], " ",  range[2],
              " for levels ", paste.quoted.names(levels))

    factor(1:length(levels), labels=levels)
}
# Print the grid values, must do some finangling for a nice display

print.grid.values <- function(xgrid)
{
    cat("\n")
    row <- xgrid[1, , drop=FALSE]
    names(row) <- c(paste("grid:   ", names(row)[1]), names(row)[-1])
    rownames(row) <- ""
    print(row)
    cat("\n")
}
