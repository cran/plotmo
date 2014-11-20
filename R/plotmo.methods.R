# plotmo.methods.R:  default method functions for plotmo

#------------------------------------------------------------------------------
# All these methods functions have an argument "env", which is the environment
# in which the model function was originally called.  If that is not available
# from the object, arg is the environment in which plotmo was called.

plotmo.prolog <- function(object, env, object.name) # gets called at the start of plotmo
{
    UseMethod("plotmo.prolog")
}
plotmo.prolog.default <- function(object, env, object.name)
{
    # here we just establish with some sort of plausibility that object
    # is a model object, to minimize confusing messages later
    # (e.g. Error in object$terms : $ operator not defined for this S4 class)

    if(!is.list(object))
        stop0("'", object.name, "' is not an S3 model object")
}
#------------------------------------------------------------------------------
# Get the type to be used when plotmo's argument "type" is NULL (the default)

get.plotmo.default.type <- function(obj, env)
{
    UseMethod("get.plotmo.default.type")
}
get.plotmo.default.type.default <- function(obj, ...)
{
    "response"
}
get.plotmo.default.type.nnet <- function(obj, ...)
{
    "raw"
}
get.plotmo.default.type.knn3 <- function(obj, ...)
{
    "prob"
}
#------------------------------------------------------------------------------
# Get the y limits to be used when ylim=NULL.  Return a two elem vector or
# NA.  If NA, plotmo will use the range of the predicted response as the ylim
# (it gets the predicted response by calling plot.degree1 and plot.degree2
# to run through the plotting code, but without actually plotting).

get.plotmo.ylim <- function(object, env, type, trace)
{
    UseMethod("get.plotmo.ylim")
}
get.plotmo.ylim.default <- function(object, env, type, ...)
{
    if(predictions.are.probabilities(object, type))
        return(c(0, 1))
    NA
}
#------------------------------------------------------------------------------
# Return a two elem vector or NA.  If NA, plotmo will use the range of the
# original response as the clip limits.

get.plotmo.clip.limits <- function(object, env, type, y, trace)
{
    UseMethod("get.plotmo.clip.limits")
}
get.plotmo.clip.limits.default <- function(object, env, type, y, trace)
{
    no.clip.limits <- function(object, type)
    {
        pmatch(type, "class", 0) ||
        # assume no clip limits if is glm object and predict type is "link"
        (!is.null(get.glm.family(object)) && pmatch(type, c("link"), 0))
    }
    #--- get.plotmo.clip.limits.default starts here
    if(predictions.are.probabilities(object, type))
        return(c(0, 1))
    else if(no.clip.limits(object, type))
        return(c(-Inf, Inf))
    NA
}
#------------------------------------------------------------------------------
# Return a vector of indices of predictors for degree1 plots, e.g, c(1,3,4).
# The indices are col numbers in the x matrix.  The caller will sort the
# returned vector and remove duplicates.  The default method simply
# returns the indices of all predictors.  The object specific methods
# typically return only the predictors actually used in the model.

get.plotmo.singles <- function(object, env, x, trace, all1)
{
    UseMethod("get.plotmo.singles")
}
get.plotmo.singles.default <- function(object, env, x, ...)
{
    stopifnot(NCOL(x) >= 1)
    1 : ncol(x)
}
#------------------------------------------------------------------------------
# Get the pairs of predictors to be displayed in degree2 plots.
# Each row of the returned pairs matrix is the indices of two predictors
# for a degree2 plot. Example (this was returned from get.plotmo.pairs.rpart):
#
#    1    2
#    1    2
#    2    1
#
# The indices are col numbers in the x matrix.  The caller will remove
# duplicated pairs and re-order the pairs on the order of the predictors
# in the original call to the model function.  The above example will
# become simply
#
#    1    2
#
# It is ok to return NULL or a matrix with zero rows.

get.plotmo.pairs <- function(object, env, x, trace, all2)
{
    if(all2)
        return(get.all.pairs(object, env, x, trace)) # TODO include all1 in this
    UseMethod("get.plotmo.pairs")
}
# Predictors x1 and x2 are considered paired if they appear in the formula
# in forms such as x1:x2 or I(x1*x2) or s(x1,x2)

get.plotmo.pairs.default <- function(object, env, x, trace, ...)
{
    pairs <- NULL
    if(!is.null(object$call$formula)) {
        form <- object$call$formula
        # following "if" is needed for: form <- Volume ~ .; lm(form, data=trees)
        if(typeof(form) != "language")
            form <- eval(form, env)
        form <- as.formula(form)
        data <- get.data.for.formula(object, env, object$call$data, "x", trace)
        terms <- terms(form, data=data)
        term.labels <- NULL
        if(!is.null(terms))
            term.labels <- attr(terms, "term.labels")
        if(!is.null(term.labels) && length(term.labels))
            pairs <- get.plotmo.pairs.from.term.labels(term.labels, colnames(x), trace)
        else if(trace > 0)
            cat0("no degree2 plots because no object$call$formula$term.labels\n")
    }
    pairs
}
get.all.pairs <- function(object, env, x, trace, max.pairs=10) # 10 is arbitrary
{
    singles <- get.plotmo.singles(object, env, x, trace, all1=TRUE)
    if(length(singles) == 0)
        return(NULL) # no pairs (must be an intercept only model)
    singles <- unique(singles)
    if(length(singles) > max.pairs) {
        warning0("too many variables to plot all pairs,\n           ",
                 "so plotting degree2 plots for just the first ",
                 max.pairs, " variables")
        singles <- singles[1:max.pairs]
    }
    form.pairs(singles)
}
form.pairs <- function(varnames) # return a two column matrix, each row is a pair
{
    col1 <- rep(varnames, times=length(varnames))
    col2 <- rep(varnames, each=length(varnames))
    pairs <- cbind(col1, col2)
    pairs[col1 != col2, , drop=FALSE]
}
#------------------------------------------------------------------------------
# This is called once for each degree1 and degree2 plot.  The newdata
# argument is a data.frame of predictor values to be displayed in the
# current plot.  The other args are copies of the args passed to plotmo.
# The trace flag here will be TRUE if trace=2 was used in the call to plotmo.

plotmo.predict <- function(object, newdata, type, trace)
{
    UseMethod("plotmo.predict")
}
plotmo.predict.default <- function(object, newdata, type, trace)
{
    predict(object, newdata=newdata, type=type, trace=trace)
}
#------------------------------------------------------------------------------
# Some models return multiple columns from predict that don't corresponsd
# to the multiple responses of a multiple response model.  So we have to
# modify the nresponse argument to select the appropriate column.

plotmo.get.nresponse <- function(object, nresponse, y)
{
    UseMethod("plotmo.get.nresponse")
}
plotmo.get.nresponse.default <- function(object, nresponse, y)
{
    nresponse
}
#------------------------------------------------------------------------------
# Handle plotmo's "level" argument.  Return a prediction interval dataframe
# with either or both of the following sets of columns.  What columns get
# returned depends on the capabilities of the object's predict method.
# For example, predict.lm allows us to return both i and ii,  and for
# earth models we can return only i.
#
#  (i)  lwr, upr               intervals for prediction of new data
#
#  (ii) cint.lwr, cint.upr     intervals for prediction of mean response

plotmo.pint <- function(object, newdata, type, level, trace)
{
    if(trace > 0)
        cat("\n--plotmo.pint for", class(object)[1], "object\n\n")
    UseMethod("plotmo.pint")
}
plotmo.pint.default <- function(object, newdata, type, level, trace)
{
    stop0("\"level\" is not supported for ",
          paste.quoted.names(class(object)[1]), " objects\n",
          "You need to supply a plotmo.pint.", class(object)[1], " method")
}
#------------------------------------------------------------------------------
# Return the data matrix for the given object with the response deleted.

get.plotmo.x <- function(object, env, trace)
{
    if(trace > 0)
        cat("\n--get.plotmo.x for", class(object)[1], "object\n\n")
    UseMethod("get.plotmo.x")
}
# The default function tries hard to get x regardless of the model.
#
# Note that the alternative approach of simply calling the standard model.matrix
# wouldn't get us what we want here because, among other reasons, it can return
# columns with headings like "ns(x3,4)" whereas we want the "naked" predictor x3.

get.plotmo.x.default <- function(object, env, trace)
{
    badx <- function(x, check.colnames)
    {
        is.null(x) || is.try.error(x) || NROW(x) == 0 ||
            (check.colnames && is.null(colnames(x)))
    }
    #--- get.plotmo.x.default starts here

    # This check suffices to prevent most downstream error messages for objects
    # that don't have the fields required for the default plotmo methods.
    if(is.null(object$call) && is.null(object[["x"]]))
        stop0(paste.quoted.names(class(object)[1]),
              " objects are not supported by plotmo")

    try.error.message <- NULL
    x <- object.x <- object[["x"]] # use [["x"]] rather than $x to prevent partial match
    if(!badx(x, check.colnames=TRUE) && trace > 0)
        cat("got x with colnames from object$x\n")
    if(badx(x, check.colnames=TRUE)) {
        x <- formula.x <- get.data.from.formula("x", object, env, trace)
        if(!badx(x, check.colnames=TRUE) && trace > 0)
            cat("got x with colnames from object$call$formula\n")
    }
    if(badx(x, check.colnames=TRUE)) {
        x <- call.x <- try(eval(object$call[["x"]], env), silent=TRUE)
        if(!badx(x, check.colnames=TRUE) && trace > 0)
            cat("got x with colnames from object$call$x\n")
        if(is.try.error(x))
            try.error.message <- x
    }
    # if don't have an x with colnames look for one without colnames
    if(badx(x, check.colnames=TRUE)) {
        x <- object.x
        if(!badx(x, check.colnames=FALSE)) {
            if(trace > 0)
                cat("got x without colnames from object$x\n")
        } else {
            x <- formula.x
            if(!badx(x, check.colnames=FALSE) && trace > 0)
                cat("got x without colnames from object$call$formula\n")
        }
        if(badx(x, check.colnames=FALSE)) {
            x <- call.x
            if(!badx(x, check.colnames=FALSE) && trace > 0)
                cat("got x without colnames from object$call$x\n")
            if(is.try.error(x))
                try.error.message <- x
        }
    }
    if(badx(x, check.colnames=FALSE)) {
        if(trace > 0) {
            cat("Looked unsuccessfully for an x in the following places:\n")
            cat("\n(i) object$x:\n")
            print(head(object$x, 3))
            cat("\n(ii) object$call$formula:\n")
            dput(object$call$formula)
            cat("\n(iii) object$call$x:\n")
            if(is.null(try.error.message))
                print(head(eval(object$call$x, env), 3))
            else
                cat(gsub("Error in ", "", try.error.message[1]))
            cat("\n")
        }
        stop0("get.plotmo.x.default cannot get the x matrix\n",
              "       (tried object$x, object$call$formula, and object$call$x)")
    }
    x
}
#------------------------------------------------------------------------------
# get.plotmo.y is similar to model.response but can deal with models
# created without a formula.

get.plotmo.y <- function(object, env, y.column, expected.len, trace)
{
    if(trace > 0)
        cat("\n--get.plotmo.y for", class(object)[1], "object\n\n")
    UseMethod("get.plotmo.y")
}
# TODO this allows earth to call get.plotmo.y.default without use of :::
get.plotmo.y_default <- function(object, env, y.column, expected.len, trace)
{
    get.plotmo.y.default(object, env, y.column, expected.len, trace)
}
get.plotmo.y.default <- function(object, env, y.column, expected.len, trace)
{
    bady <- function(y)
    {
        is.null(y) || is.try.error(y)
    }
    #--- get.plotmo.y.default starts here
    try.error.message <- NULL
    y <- object[["y"]] # use [["y"]] rather than $y to prevent partial matching
    if(!bady(y) && trace > 0)
        cat("got y from object$y\n")
    if(bady(y)) {
        y <- get.data.from.formula("y", object, env, trace)
        if(!bady(y) && trace > 0)
            cat("got y from object$call$formula\n")
    }
    if(bady(y)) {
        y <- try(eval(object$call[["y"]], env), silent=TRUE)
        if(!bady(y) && trace > 0)
            cat0("got y from object$call$y\n")
        if(is.try.error(y))
            try.error.message <- y
    }
    try.2nd.arg <- FALSE
    if(bady(y) && length(object$call) >= 3 &&
            !pmatch("formula", names(object$call), 0)) {
        # final resort --- try the second arg in the call to the model function
        try.2nd.arg <- TRUE
        y <- try(eval(object$call[[3]], env), silent=TRUE)
        if(!(is.vector(y) || is.factor(y)) || length(y) != expected.len)
            y <- NULL
        if(!bady(y) && trace > 0)
            cat0("got y from the second argument to the model function\n")
    }
    if(bady(y)) {
        if(trace > 0) {
            cat("Looked unsuccessfully for y in the following places:\n")
            cat("\n(i) object$y:\n")
            print(head(object$y, 3))
            cat("\n(ii) object$call$formula:\n")
            dput(object$call$formula)
            cat("\n(iii) object$call$y:\n")
            if(is.null(try.error.message))
                print(head(eval(object$call$y, env), 3))
            else
                cat(gsub("Error in ", "", try.error.message[1]))
            if(try.2nd.arg)
                cat("\n(iv) second arg to model function\n")
            cat("\n")
        }
        stop0("get.plotmo.y.default: cannot get y\n",
              "       (tried object$call$formula, object$call$y, ",
              "object$y, and second arg to model function)")
    }
    y
}
#------------------------------------------------------------------------------
# If object has a formula, use that formula to get x or y (field is "x" or "y").

get.data.from.formula <- function(field, object, env, trace)
{
    print.unevaluated.model.frame <- function(msg, mf)
    {
        mf$na.action <- NULL # don't want to print the na.action
        s <- format(mf)
        if(length(s) > 8) {
            s <- s[1:8]
            s[8] <- paste(s[8], "\n...")
        }
        s <- gsub("[ \t\n]", "", s) # remove white space
        s <- gsub(",", ", ", s)     # replace comma with comma space
        s <- paste(s, collapse="\n    ", sep="")
        cat0(msg, s, "\n")
    }
    get.iformula <- function() # get the index of the formula in object$call
    {
        iformula <- match(c("formula"), names(call), 0)
        if(iformula == 0) {
            # no field named "formula" in call, so look for a formula
            # anywhere in the call and name it "formula", then try again
            # TODO for which models is this actually necessary?
            call.list <- as.list(call)
            for(i in 1:length(call))
                if(typeof(call.list[[i]]) == "language" &&
                        as.list(call.list[[i]])[[1]] == "~") {
                    if(trace > 0)
                        cat0("no field named \"formula\" in call, ",
                             "but found a formula anyway\n")
                    names <- names(call)
                    names[i] <- "formula"
                    names(call) <- names
                    break
                }
            # try again
            iformula <- match(c("formula"), names(call), 0)
        }
        iformula
    }
    #--- get.data.from.formula starts here
    call <- object$call
    if(is.null(call))
        return(NULL)    # error will be reported later
    iformula <- get.iformula()
    if(iformula == 0)   # no formula?
        return(NULL)    # error will be reported later
    idata <- match(c("data"), names(call), 0) # may be 0, that's ok
    mf <- call[c(1, iformula, idata)]
    mf[[1]] <- as.name("model.frame")
    form <- mf[[2]]
    # following "if" is needed for: form <- Volume ~ .; earth(form, data=trees)
    # fixes bug reported by Martin Maechler and Michael Amrein
    if(typeof(form) != "language")
        form <- eval(form, env)
    formula.as.string <- paste(format(form), collapse=" ")
    stripped.formula <- strip.formula.string(formula.as.string)
    if(trace >= 2)
        cat0("formula ", formula.as.string, "\n",
             "stripped formula ", stripped.formula, "\n")
    mf$formula <- try(parse(text=stripped.formula)[[1]])
    if(is.try.error(mf$formula)) # should never happen
        stop0("plotmo cannot parse the model formula ", formula.as.string)
    mf$data <- get.data.for.formula(object, env, mf$data, field, trace)
    # TODO following is a hack for rpart's (special but useful) NA handling
    mf$na.action <- if(inherits(object, "rpart")) na.pass else na.fail
    if(trace >= 2)
        print.unevaluated.model.frame("about to eval ", mf)
    evaluated.mf <- try(eval(mf, env))
    if(is.try.error(evaluated.mf)) {
        if(trace >= 2)
            cat("eval(mf, env) failed\n")
        return(NULL) # error will be reported later
    }
    if(field == "x") {
        if(length(grep(".+~", stripped.formula)))         # has response?
            evaluated.mf <- evaluated.mf[,-1, drop=FALSE] # drop the response
        else
            warning0("formula has no response variable, formula is ",
                      stripped.formula)

        # TODO code for the following and similar
        # (why is this happening? c.f. error.bad.ylen):
        # library(ElemStatLearn); x <- mixture.example$x;
        # g <- mixture.example$y; x.mod <- lm(g ~ x)
        if(inherits(evaluated.mf[[1]], what=c("data.frame", "matrix"), which=FALSE)) {
            if(trace >= 2)
                cat("entire x matrix is stored as the first element of evaluated.mf\n")
            evaluated.mf <- evaluated.mf[[1]]
        }
        # Prevent misleading error msg later: bad index (missing column in x?)
        # caused by the following code (NIR has class "AsIs"):
        #    data(gasoline, package='pls')
        #    a <- earth(octane ~ NIR, data=gasoline)
        #    plotmo(a)
        # There appears to be no easy fix for this (July 2011).
        if(class(evaluated.mf[[1]])[1] == "AsIs")
            stop0("the class of the rhs of the formula ", formula.as.string,
                  " is \"AsIs\", which is not supported by plotmo")
    } else if(field == "y")
        evaluated.mf <- model.response(evaluated.mf, type="any")
    else
        stop("internal error, bad field: ", field)
    evaluated.mf
}
get.data.for.formula <- function(object, env, data.arg, field, trace)
{
    data.is.good <- function(...)
    {
        # The length test is necessary for lm which saves data as an
        # empty list if its data arg is FALSE, don't know why
        # The is.xxx tests are to minimize false positives.
        good <- !is.null(data) && length(data) &&
                    (is.data.frame(data) || is.matrix(data) ||
                     is.vector(data) || is.factor(data))
        if(good && trace > 0)
            cat("get.data.for.formula: using", field, "from", paste0(...), "\n")
        good
    }
    #--- get.data.for.formula starts here
    data <- object[["data"]]
    if(!data.is.good("object$data")) {
        data <- object[[field]]
        if(!data.is.good("object$", field) && !is.null(data.arg)) {
            # try data.arg (i.e. object$call$data or mf$data)
            data <- eval(data.arg, env)
            data.arg.as.character <- strip.white.space(deparse(data.arg)[1])
            if(!data.is.good(paste0("\"", data.arg.as.character,
                    "\" passed to ", class(object)[1]))) {
                msg <-
                    if(inherits(object, "earth"))
                        " (use keepxy=TRUE in the call to earth?)"
                    else if(inherits(object, "lm"))
                        paste0(" (use ", field, "=TRUE in the call to lm?)")
                    else
                        ""
                stop0("the data \"", data.arg.as.character, "\" passed to ",
                      class(object)[1], " is no longer available", msg,
                      "\n       (tried object$data, object$", field,
                      " and call$", data.arg.as.character, ")")
            }
        }
    }
    # Following needed if original data to model was a matrix not a data.frame.
    # Else get Error in model.frame.default: 'data' must be a data.frame.
    if(class(data)[1] == "matrix")
        data <- as.data.frame(data)
    if(is.null(data) && trace >= 2)
        cat("no explicit data for formula\n")
    data
}
# Given the term.labels, return a npairs x 2 matrix specifying which predictors
# are paired. The elements in the returned matrix are column indices of x.
#
# This routine is not infallible but works for the commonly used formulas.
# It works by extracting substrings in each term.label that looks like a
# predictor pair.  The following combos of x1 and x2 for example are
# considered pairs: x1*x2, x1:x2, s(x1,x2), and similar.

get.plotmo.pairs.from.term.labels <- function(term.labels, pred.names, trace)
{
    if(trace > 0)
        cat("term.labels:", term.labels, "\n")
    pairs <- matrix(0, nrow=0, ncol=2)          # no pairs initially
    for(i in 1:length(term.labels)) {
        s <- strip.white.space(term.labels[i])
        s <- gsub("[+*/,]", ":", s)             # replace + * / , with :
        s <- gsub("=[^,)]+", "", s)             # delete "=any"

        # get the indices of expressions of the form "ident1:ident2"
        igrep <- gregexpr(
            "[a-zA-Z._][a-zA-Z._0-9$]*:[a-zA-Z._][a-zA-Z._0-9$]*", s)[[1]]

        if(trace > 0)
            cat("considering", s)

        if(igrep[1] > 0) for(i in seq_along(igrep)) {
            # extract the i'th "ident1:ident2" into pair
            start <- igrep[i]
            stop <- start + attr(igrep, "match.length")[i] - 1
            pair <- substr(s, start=start, stop=stop)
            pair <- strsplit(pair, ":")[[1]]    # pair is now c("ident1","ident2")
            # are the variables in the candidate pair in pred.names?
            ipred1 <- which(pred.names == pair[1])
            ipred2 <- which(pred.names == pair[2])
            if(trace > 0)
                cat("->", pair, "at", if(length(ipred1)) ipred1 else NA,
                    if(length(ipred2)) ipred2 else NA)
            if(length(ipred1) == 1 && length(ipred2) == 1 && pair[1] != pair[2])
                pairs <- c(pairs, ipred1, ipred2)
        }
        if(trace > 0)
            cat("\n")
    }
    matrix(pairs, ncol=2, byrow=TRUE)
}
# Given a formula (as string), return a string with the "naked" predictors.
#
# Example: y ~ x9 + ns(x2,4) + s(x3,x4,df=4) + x5:sqrt(x6)
# becomes: y ~ x9 + x2 + x3 + x4 + x5 + x6
# which will later result in a model.matrix with columns x9 x2 x3 x4 x5 x6.
#
# This routine is not infallible but works for the commonly used formulas.

strip.formula.string <- function(form)
{
    gsubi <- function(pat, rep, x, perl=FALSE)
    {
        gsub(pat, rep, x, ignore.case=TRUE, perl=perl)
    }
    #--- strip.formula.string starts here
    igrep <- grep("[a-zA-Z._][a-zA-Z._0-9]\\$", form)   # check for "ident$"
    if(length(igrep)) {
        # TODO formula has vars with $, this confuses predict() later, why?
        # they cause "Warning: after calling plotmo.predict, y has the wrong length"
        stop0("plotmo: names with \"$\" are not yet supported.\n",
              "The offending formula is ", form)
    }
    form <- strip.white.space(form)
    args <- gsubi(".*~", "", form)                  # extract everything after ~

    # We don't want to mess with anything in [square brackets]
    # Doing that with regular expressions is tricky, so we adopt this approach:
    # change "+" "-" "," in square brackets to #PLUS# #MINUS# #COMMA# to protect them

    args <- gsubi("(\\[[.\\$a-z0-9]*)\\+([.\\$a-z0-9]*\\])", "\\1#PLUS#\\2", args)
    args <- gsubi("(\\[[.\\$a-z0-9]*)\\-([.\\$a-z0-9]*\\])", "\\1#MINUS#\\2", args)
    args <- gsubi("(\\[[.\\$a-z0-9]*)\\,([.\\$a-z0-9]*\\])", "\\1#COMMA#\\2", args)

    args <- gsubi("[-*/:]", "+", args)              # replace - / * : with +

    # next two gsubs allow us to retain "x=x1" but drop "df=99" from "bs(x=x1, df=99)"

    args <- gsubi("\\([a-z._0-9$]+=", "(", args)    # replace "(ident=" with "("
    args <- gsubi("[a-z._0-9$]+=[^,)]+", "", args)  # delete "ident=any"

    # replace ",ident" with ")+f(ident", thus "s(x0,x1)" becomes "s(x0)f(x1)"

    args <- gsubi(",([a-z._])", ")+s(\\1", args)
    args <- gsubi("[a-z._0-9$]*[(]", "", args)      # remove "ident("
    args <- gsubi("[,)][^+-]*", "", args)           # remove remaining ",arg1,arg2)"

    # change #PLUS# etc. back to what they where
    args <- gsubi("#MINUS#", "-", args)
    args <- gsubi("#PLUS#", "+", args)
    args <- gsubi("#COMMA#", ",", args)

    # workaround for "error: invalid type (list) for variable 'trees[, -3]'"
    # for a <- earth(trees[,3] ~ as.matrix(trees[,-3])); plotmo(a)
    #
    # TODO removed because although it fixes that problem we still get
    # Warning: 'newdata' had 10 rows but variable(s) found have 31 rows
    #
    # if(is.list(eval(parse(text=args, env))))
    #   args<-paste("as.matrix(", args, ")")

    response <- ""
    if(length(grep("~", form)))                     # if there is a response
        response <- gsubi("~.*", "~", form)         # then extract all before ~

    # FIXED 7 Dec 2007 reported by Joe Retzer
    # collapse possible multiple element response and args into a single string

    strip.white.space(paste(response[1], paste(args, collapse=" "), collapse=" "))
}
# Return TRUE if predictions are probabilities (thus their range is 0 ... 1).
# Just a guess really, works for common models
# The intent is to minimize "predicted values are out of ylim" messages.

predictions.are.probabilities <- function(object, type)
{
    #--- predictions.are.probabilities starts here
    if(pmatch(type, c("probability", "posterior"), 0))
        return(TRUE)
    is.binom <- FALSE
    if(pmatch(type, "response", 0)) {
        family <- get.glm.family(object)
        is.binom <- !is.null(family) &&
                        pmatch(family, c("binomial", "quasibinomial"), nomatch=0)
    }
    is.binom
}
# get object's glm family if there is one, returns a string or NULL
# TODO how to specify the 2nd or later model in earth's glm.list?

get.glm.family <- function(object)
{
    family <- NULL
    if(!is.null(object$glm.list[[1]])) # object class is "earth"
        family <- object$glm.list[[1]]$family$family
    else if(!is.null(object$family) && is.list(object$family))
        family <- object$family$family # object class is "glm" or similar
    family
}
