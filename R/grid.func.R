# grid.func:  apply grid.levels or grid.func to x (a column from the input x mat)
#             to get a scalar value for the given background variable

get.fixed.gridval <- function(x, pred.name, grid.func, grid.levels)
{
    gridval.method <- "grid.levels" # used only in warning messages
    gridval <- get.fixed.gridval.from.grid.levels.arg(x, pred.name, grid.levels)
    if(is.na(gridval)) { # pred.name is not in grid.levels?
        gridval.method <- "grid.func"
        if(is.null(grid.func)) {
            grid.func <- default.grid.func
            gridval.method <- "default.grid.func"
        }
        check.grid.func(grid.func)
        if(length(x) == 0) # paranoia
            stop0("length(", pred.name, ") is zero")
        x <- x[!is.na(x)]
        if(length(x) == 0) # paranoia
            stop0("all values of ", pred.name, " are NA")
        gridval <- try(grid.func(x, na.rm=TRUE), silent=TRUE)
    }
    check.fixed.gridval(gridval, gridval.method, x, pred.name) # returns gridval
}
default.grid.func <- function(x, ...)
{
    if(inherits(x, "integer"))      # return median rounded to integer
        return(as.integer(round(median(x))))
    if(inherits(x, "logical"))      # return most common value
        return(median(x) > .5)
    if(inherits(x, "factor")) {     # return most common value
        lev.names <- levels(x)
        ilev <- which.max(table(x))
        if(is.ordered(x))
            return(ordered(lev.names, levels=lev.names)[ilev])
        return(factor(lev.names, levels=lev.names)[ilev])
    }
    median(x)                       # default to median
}
# Check grid.levels arg passed in by the user.  This checks that the names
# of the list elements are indeed predictor names.  The actual levels will
# be checked later in get.fixed.gridval.from.grid.levels.arg.

check.grid.levels.arg <- function(x, grid.levels, pred.names)
{
    if(!is.null(grid.levels)) { # null is the default value
        if(!is.list(grid.levels))
            stop0("grid.levels must be a list.  ",
                  "Example: grid.levels=list(sex=\"male\")")
        for(name in names(grid.levels)) {
            if(nchar(name) == 0)
                stop0(
"All elements of grid.levels must be named\n       You have grid.levels=",
                      as.char(grid.levels))
            if(!pmatch(name, pred.names, 0))
                stop0("illegal variable name '", name, "' in grid.levels")
        }
    }
}
# this returns NA if pred.name is not in grid.levels

get.fixed.gridval.from.grid.levels.arg <-function(x, pred.name, grid.levels)
{
    if(is.null(grid.levels))
        return(NA)
    gridval <- NA
    names.grid.levels <- names(grid.levels)
    # look for pred.name in the grid.levels list, if found use its value
    iname <- which(pmatch(names.grid.levels, pred.name, duplicates.ok=TRUE) == 1)
    if(length(iname) == 0) # no match?
        return(NA)
    if(length(iname) > 1) # more than one match?
        stop0("illegal grid.levels argument (\"",
              names.grid.levels[iname[1]], "\" and \"",
              names.grid.levels[iname[2]], "\" both match \"",
              pred.name, "\")")
    # a name in grid.levels matches pred.name
    stopifnot(length(iname) == 1)
    gridval <- grid.levels[[iname]]
    if(length(gridval) > 1)
        stop0("length(", pred.name, ") in grid.levels is not 1")
    if(is.na(gridval))
        stop0(pred.name, " in grid.levels is NA")
    if(is.numeric(gridval) && !all(is.finite(gridval)))
        stop0(pred.name, " in grid.levels is infinite")
    if(is.factor(x)) {
        lev.name <- grid.levels[[iname]]
        if(!is.character(lev.name) || length(lev.name) != 1 || !nzchar(lev.name))
            stop0("illegal level for \"", pred.name, "\" in grid.levels ",
                  "(specify factor levels with a string)")
        lev.names <- levels(x)
        ilev <- pmatch(lev.name, lev.names, 0)
        if(ilev == 0)
            stop0("illegal level \"", lev.name, "\" for \"",
                  pred.name, "\" in grid.levels (allowed levels are ",
                  quotify(lev.names), ")")
        gridval <- if(is.ordered(x))
                        ordered(lev.names, levels=lev.names)[ilev]
                   else
                        factor(lev.names, levels=lev.names)[ilev]
    }
    # do type conversions for some common types
    # (e.g. allow 3 instead of 3L for integer variables)
    class.gridval <- class(gridval)[1]
    class.x <- class(x)[1]
    if(class.gridval != class.x) {
        if(class.gridval == "numeric" && class.x == "integer")
            gridval <- as.integer(round(gridval))
        else if(class.gridval == "integer" && class.x == "numeric")
            gridval <- as.numeric(gridval)
        else if(class.x == "logical") {
            if(!is.logical(gridval) && !is.numeric(gridval))
                stop0("expected a logical value in grid.levels for ", pred.name)
            gridval <- gridval > .5
        }
    }
    return(gridval)
}
check.grid.func <- function(grid.func)
{
    if(!is.function(grid.func))
        stop0("'grid.func' is not a function");
    formals <- names(formals(grid.func))
    # check grid.func signature, we allow argname "na.rm" for mean and median
    if(length(formals) < 2 || formals[1] != "x" ||
            (!any(formals == "na.rm") && formals[2] != "..."))
        stop0("The formal arguments of 'grid.func' should be 'x' and '...'\n",
              "       Your 'grid.func' has ",
              if(length(formals) == 0)      "no formal arguments"
              else if(length(formals) == 1) "a single formal argument "
              else                          "formal arguments ",
              if(length(formals) > 0) paste0("'", formals, "'", collapse=" ")
              else "")
}
check.fixed.gridval <- function(gridval, gridval.method, x, pred.name)
{
    if(is.try.err(gridval)) {
        if(inherits(x, "logical") || inherits(x, "factor"))
            warning0(gridval.method, " failed for ", pred.name,
                     ", so will use the most common value of ", pred.name)
        else
            warning0(gridval.method, " failed for ", pred.name,
                     ", so will use the default grid.func for ", pred.name)
       gridval <- default.grid.func(x)
    }
    if(length(gridval) != 1) {
        warning0(gridval.method, " returned multiple values for ", pred.name,
                 ", so will use the default grid.func for ", pred.name)
        gridval <- default.grid.func(x) # revert to default.grid.func
    }
    if(is.na(gridval)) {
        warning0(gridval.method, " returned NA for ", pred.name,
                 ", so will use the default grid.func for ", pred.name)
        gridval <- default.grid.func(x) # revert to default.grid.func
    }
    # possibly type convert gridval
    class.gridval <- class(gridval)[1]
    if(class.gridval != class(x)[1]) {
        if(inherits(x, "integer"))        # silently fix so e.g. grid.func=mean works
            gridval <- as.integer(round(median(gridval)))
        else if(inherits(x, "logical")) { # silently fix if possible
            if(!is.logical(gridval) && !is.numeric(gridval))
                stop0("expected a logical value in grid.levels for ", pred.name)
            gridval <- gridval > .5
        }
        else if(inherits(x, "factor")) {
            warning0(gridval.method, " returned class \"", class.gridval,
                     "\" for ", pred.name,
                     ", so will use the most common value of ", pred.name)
            gridval <- default.grid.func(x)
        } else {
            warning0(gridval.method, " returned class \"", class.gridval,
                     "\" for ", pred.name,
                     ", so will use the default grid.func for ", pred.name)
            gridval <- default.grid.func(x)
        }
    }
    gridval
}
# this retunrs NA if pred.name is not in grid.levels

get.fixed.gridval.for.partdep <- function(x, ipred, pred.name, grid.levels)
{
    gridval <- get.fixed.gridval.from.grid.levels.arg(x, pred.name, grid.levels)
    # common type conversions were already done in get.fixed.gridval.from.grid.levels.arg
    # check here if that wasn't possible
    if(!is.na(gridval)[1] && class(gridval)[1] != class(x)[1])
        stop0("the class \"", class(gridval)[1], "\" of \"", pred.name,
              "\" in grid.levels does not match its class \"",
              class(x)[1],
              "\" in the input data")
    gridval
}
