# plotmo.methods.R:  method functions for plotmo e.g. get.plotmo.singles.default

#------------------------------------------------------------------------------
plotmo.prolog <- function(object, object.name) # gets called at the start of plotmo
{
    UseMethod("plotmo.prolog")
}
plotmo.prolog.default <- function(object, object.name)
{
    # here we just establish with some sort of plausibility that object
    # is a model object, to minimize confusing messages later

    if(!is.list(object))
        stop0("'", object.name, "' is not a model object")

	# removed the following, causes too many false alarms
    # if(length(coef(object)) == 1)
    #     warning0("'", object.name, "' appears to be an intercept only model")
}
#------------------------------------------------------------------------------
get.default.plotmo.type <- function(obj)
{
    UseMethod("get.default.plotmo.type")
}
get.default.plotmo.type.default <- function(obj)
{
    "response"
}
#------------------------------------------------------------------------------
# Get the y limits to be used when ylim=NULL.  Return a two elem vector or
# NA.  If NA, plotmo will use the range of the predicted response as the ylim
# (it gets the predicted response by calling plot.degree1 and plot.degree2
# to go through the plotting code, but without actually plotting).

get.plotmo.auto.ylim <- function(object, type, trace)
{
    UseMethod("get.plotmo.auto.ylim")
}
get.plotmo.auto.ylim.default <- function(object, type, trace)
{
    if(predictions.are.probabilities(object, type))
        return(c(0, 1))
    NA
}
#------------------------------------------------------------------------------
# Return a two elem vector or NA.  If NA, plotmo will use the range of the
# original response as the clip limits.

get.plotmo.clip.limits <- function(object, type, y, trace)
{
    UseMethod("get.plotmo.clip.limits")
}
get.plotmo.clip.limits.default <- function(object, type, y, trace)
{
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
# The env argument is the environment from which plotmo was called.

get.plotmo.singles <- function(object, env, x, trace, all1)
{
    UseMethod("get.plotmo.singles")
}
get.plotmo.singles.default <- function(object, env, x, ...)
{
    ifirst <- if(colnames(x)[1] == "(Intercept)") 2 else 1 # delete intercept
    ifirst:ncol(x)
}
#------------------------------------------------------------------------------
# Get the pairs of predictors to be displayed in degree2 plots.
# Each row of the returned pairs matrix is the indices of two predictors
# for a degree2 plot. Example (this was returned from get.pairs.rpart):
#
#    1    2
#    1    2
#    2    1
#    2    1
#
# The indices are col numbers in the x matrix.  The caller will remove
# duplicates pairs and re-order the pairs on the order in the call to the
# model function.  The above example will become simply
#
#   1    2
#
# It is ok to return NULL or a matrix with zero rows.
# The env argument is the environment from which plotmo was called.

get.plotmo.pairs <- function(object, env, x, trace, all2)
{
    get.all.pairs <- function(object, env, x, trace)
    {
        singles <- get.plotmo.singles(object, env, x, trace, all1=TRUE)
        if(length(singles) == 0)
            return(NULL) # no pairs (must be an intercept only model)
        singles <- unique(singles)
        if(length(singles) > 10) { # 10 is arbitrary
            warning0("too many variables to plot all pairs,\n         ",
                     "so plotting degree2 plots for just the first 10 variables")
            singles <- singles[1:10]
        }
        form.pairs(singles)
    }
    #--- get.plotmo.pairs starts here
    if(all2)
        return(get.all.pairs(object, env, x, trace))
    UseMethod("get.plotmo.pairs")
}
form.pairs <- function(varnames) # return a two column matrix, each row is a pair
{
    col1 <- rep(varnames, times=length(varnames))
    col2 <- rep(varnames, each=length(varnames))
    pairs <- cbind(col1, col2)
    pairs[col1 != col2, , drop=FALSE]
}
# Predictors x1 and x2 are considered paired if they appear in the formula
# in forms such as x1:x2 or I(x1*x2) or s(x1,x2)

get.plotmo.pairs.default <- function(object, env, x, trace, ...)
{
    pairs <- matrix(0, nrow=0, ncol=2)  # no pairs
    term.labels <- NULL
    if(!is.null(object$call$formula)) {
        form <- object$call$formula
        # following "if" is needed for: form <- Volume ~ .; lm(form, data=trees)
        if(typeof(form) != "language")
            form <- eval(form, env)
        form <- as.formula(form)
        data <- get.formula.data(object, env, object$call$data, FALSE, trace)
        terms <- terms(form, data=data)
        if(!is.null(terms))
            term.labels <- attr(terms, "term.labels")
    }
    if(!is.null(term.labels))
        pairs <- get.plotmo.pairs.from.term.labels(term.labels, colnames(x), trace)
    else if(trace)
         cat("no degree2 plots because no $call$formula$term.labels, use all2=TRUE\n")
    pairs
}
#------------------------------------------------------------------------------
# This is called once for each degree1 and degree2 plot.  The newdata
# argument is a data.frame of predictor values to be displayed in the
# plot.  The other args are copies of the args passed to plotmo (trace will
# have been set FALSE by plotmo if this particular call should not be traced)

plotmo.predict <- function(object, newdata, type, se.fit, trace)
{
    UseMethod("plotmo.predict")
}
plotmo.predict.default <- function(object, newdata, type, se.fit, trace)
{
    if(se.fit)
        predict(object, newdata=newdata, type=type, se.fit=TRUE, trace=trace)
    else
        predict(object, newdata=newdata, type=type, trace=trace)
}
#------------------------------------------------------------------------------
# Return the data matrix for the given object with the response deleted.
# The env argument is the environment from which plotmo was called.

get.plotmo.x <- function(object, env, trace)
{
    if(trace)
        cat("\n--get.plotmo.x\n\n")
    UseMethod("get.plotmo.x")
}
# The default function tries hard to get x regardless of the model.
#
# Note that the alternative approach of simply calling the standard
# model.matrix wouldn't get us what we want here because it can return columns
# with headings like "ns(x3,4)" whereas we want the "naked" predictor x3.
#
# If the model has a call$formula, the columns of the returned matrix are
# in the same order as the predictors in the formula.

get.plotmo.x.default <- function(object, env, trace)
{
    badx <- function(x, check.colnames)
    {
        is.null(x) || is.try.error(x) || NROW(x) == 0 ||
            (check.colnames && is.null(colnames(x)))
    }
    #--- get.plotmo.x.default starts here ---
    try.error.message <- NULL
    x <- object[["x"]] # use [["x"]] rather than $x to prevent partial matching
    if(!badx(x, check.colnames=TRUE) && trace)
        cat("got x with colnames from object$x\n")
    if(badx(x, check.colnames=TRUE)) {
        x <- get.x.from.formula(object, env, trace)
        if(!badx(x, check.colnames=TRUE) && trace)
            cat("got x with colnames from object$call$formula\n")
    }
    if(badx(x, check.colnames=TRUE)) {
        x <- try(eval(object$call[["x"]], env), silent=TRUE)
        if(!badx(x, check.colnames=TRUE) && trace)
            cat("got x with colnames from object$call$x\n")
        if(is.try.error(x))
            try.error.message <- x
    }
    # if don't have an x with colnames look for one without colnames
    if(badx(x, check.colnames=TRUE)) {
        x <- object[["x"]]
        if(!badx(x, FALSE) && trace)
            cat("got x without colnames from object$x\n")
        if(badx(x, check.colnames=FALSE)) {
            x <- get.x.from.formula(object, env, trace)
            if(!badx(x, check.colnames=FALSE) && trace)
                cat("got x without colnames from object$call$formula\n")
        }
        if(badx(x, check.colnames=FALSE)) {
            x <- try(eval(object$call[["x"]], env), silent=TRUE)
            if(!badx(x, check.colnames=FALSE) && trace)
                cat("got x without colnames from object$call$x\n")
            if(is.try.error(x))
                try.error.message <- x
        }
    }
    if(badx(x, check.colnames=FALSE)) {
        if(trace) {
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
        stop0("get.plotmo.x.default cannot get the x matrix --- ",
              "tried object$x, object$call$formula, and object$call$x")
    }
    x
}
# get x by calling model.frame() with a stripped formula

get.x.from.formula <- function(object, env, trace)
{
    Call <- object$call
    if(is.null(Call))
        return(NULL)    # error will be reported later
    m <- match(c("formula", "data"), names(Call), 0)
    if(all(m == 0))
        return(NULL)
    Call <- Call[c(1, m)]
    Call[[1]] <- as.name("model.frame")

    # TODO it would be nice to use whatever NA handling the original model
    # function used, but there seems to be no general way of knowing what that is.
    # In the meantime the following hack seems to suffice.
    Call$na.action <- if(inherits(object, "rpart")) na.pass else na.fail

    form <- Call$formula
    if(is.null(form))
        return(NULL)
    # following "if" is needed for: form <- Volume ~ .; earth(form, data=trees)
    # fixes bug reported by Martin Maechler and Michael Amrein
    if(typeof(form) != "language")
        form <- eval(form, env)
    formula.as.string <- format(form)
    stripped.formula <- strip.formula.string(formula.as.string)
    if(trace > 1)
        cat0("formula ", formula.as.string, "\n",
             "stripped formula ", stripped.formula, "\n")
    Call$formula <- parse(text=stripped.formula)[[1]]
    Call$data <- get.formula.data(object, env, Call$data, FALSE, trace)
    if(trace > 1)
        my.print.call("about to call ", Call)
    if(length(grep(".+~", stripped.formula))) { # has response?
        x <- try(eval(Call, env)[,-1])   # then eval without the response
    } else {
        warning0("formula has no response variable, formula is ", stripped.formula)
        x <- try(eval(Call, env))
    }
    if(is.try.error(x)) {
        if(length(grep("missing", x)))
            stop0("get.x.from.formula: could not evaluate formula")
        else
            stop0("get.x.from.formula: could not evaluate formula ",
                  "(variables were deleted?)")
    }
    if(NCOL(x) == 1) {
        # if one predictor, model.matrix returns a vec with no colname, so fix it
        x <- data.frame(x)
        colnames(x) <- strip.formula.string(attr(object$terms, "term.labels")[1])
    }
    x
}
#------------------------------------------------------------------------------
# get.plotmo.y is similar to model.response but can deal with models
# created without a formula.  The env argument is the environment from
# which plotmo was called.

get.plotmo.y <- function(object, env, expected.len, trace)
{
    if(trace)
        cat("\n--get.plotmo.y\n\n")
    UseMethod("get.plotmo.y")
}
get.plotmo.y.default <- function(object, env, expected.len, trace)
{
    bady <- function(y)
    {
        is.null(y) || is.try.error(y)
    }
    #--- get.plotmo.y.default starts here ---
    try.error.message <- NULL
    y <- object[["y"]] # use [["y"]] rather than $y to prevent partial matching
    if(!bady(y) && trace)
        cat("got y from object$y\n")
    if(bady(y)) {
        y <- get.y.from.formula(object, env, trace)
        if(!bady(y) && trace)
            cat("got y from object$call$formula\n")
    }
    if(bady(y)) {
        y <- try(eval(object$call[["y"]], env), silent=TRUE)
        if(!bady(y) && trace)
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
        if(!bady(y) && trace)
            cat0("got y from second argument to model function\n")
    }
    if(bady(y)) {
        if(trace) {
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
get.y.from.formula <- function(object, env, trace)
{
    Call <- object$call
    if(is.null(Call))
        return(NULL)    # error will be reported later
    m <- match(c("formula", "data"), names(Call), 0)
    if(all(m == 0))
        return(NULL)
    Call <- Call[c(1, m)]

    # replace the formula with the stripped formula
    form <- Call$formula
    if(is.null(form))
        return(NULL)
    if(typeof(form) != "language")
        form <- eval(form, env)
    formula.as.string <- paste(format(form), collapse=" ")
    stripped.formula <- strip.formula.string(formula.as.string)
    Call$formula <- parse(text=stripped.formula)[[1]]
    if(trace > 1)
        cat0("formula ", formula.as.string, "\n",
             "stripped formula ", stripped.formula, "\n")

    Call$data <- get.formula.data(object, env, Call$data, TRUE, trace)
    Call[[1]] <- as.name("model.frame")
    # TODO following is a hack for rpart's (special but useful) NA handling
    Call$na.action <- if(inherits(object, "rpart")) na.pass else na.fail
    stripped.formula <- strip.formula.string(formula.as.string)
    if(trace > 1)
        my.print.call("about to call ", Call)
    Call <- try(eval(Call, env))
    if(!is.try.error(Call))
        model.response(Call, type="any")
}
#------------------------------------------------------------------------------
get.formula.data <- function(object, env, data.name, get.y, trace)
{
    data.is.good <- function(...)
    {
        # the length test is necessary for lm which saves x as an
        # empty list if its x arg is FALSE, don't know why
        good <- !is.null(x) && length(x)
        if(good && trace)
            cat("get.formula.data: got",
                if(get.y) "y" else "x", "from", paste0(...), "\n")
        good
    }
    #--- get.formula.data starts here ---
    xname <- if(get.y) "y" else "x"
    x <- object[["data"]]
    if(!data.is.good("object$data")) {
        x <- object[[xname]]
        if(!data.is.good("object$", xname)) {
            if(!is.null(data.name)) {
                x <- eval(data.name, env)
                if(!data.is.good("data passed to original call to ",
                                 class(object)[1])) {
                    stop0("the original data \"", data.name,
                          "\" is no longer available",
                          if(inherits(object, "earth"))
                                " (use keepxy=TRUE)"
                          else if(inherits(object, "lm"))
                                paste0(" (use ", xname, "=TRUE)")
                          else  "")
                }
            }
        }
    }
    x
}
#------------------------------------------------------------------------------
# Given the term.labels, return an npairs x 2 matrix specifying which
# predictors are pairs. The elements in the returned matrix are col indices of x.
#
# It works like this: extract substrings from each term.label that look
# like predictor pairs and qualify them as a valid pair if both predictors
# in the pair are in pred.names.
#
# The following combos of x1 and x2 are considered pairs: "x1*x2" "x1:x2" "s(x1,x2)"
#
# This routine is not infallible but works for the commonly used formulas.

get.plotmo.pairs.from.term.labels <- function(term.labels, pred.names, trace)
{
    if(trace)
        cat("term.labels:", term.labels, "\n")
    pairs <- matrix(0, nrow=0, ncol=2)          # no pairs
    for(i in 1:length(term.labels)) {
        s <- strip.white.space(term.labels[i])
        s <- gsub("[+*/,]", ":", s)             # replace + * / , with :
        s <- gsub("=[^,)]+", "", s)             # delete "=any"

        # get the indices of expressions of the form "ident1:ident2"
        igrep <- gregexpr(
            "[a-zA-Z._][a-zA-Z._0-9$]*:[a-zA-Z._][a-zA-Z._0-9$]*", s)[[1]]

        if(trace)
            cat("considering", s)

        if(igrep[1] > 0) for(i in seq_along(igrep)) {
            # extract the i'th "ident1:ident2" into Pair
            start <- igrep[i]
            stop <- start + attr(igrep, "match.length")[i] - 1
            Pair <- substr(s, start=start, stop=stop)
            Pair <- strsplit(Pair, ":")[[1]]    # Pair is now c("ident1","ident2")
            ipred1 <- which(pred.names == Pair[1])
            ipred2 <- which(pred.names == Pair[2])
            if(trace)
                cat("->", Pair, "at", if(length(ipred1)) ipred1 else NA,
                    if(length(ipred2)) ipred2 else NA)
            if(length(ipred1) == 1 && length(ipred2) == 1 && Pair[1] != Pair[2])
                pairs <- c(pairs, ipred1, ipred2)
        }
        if(trace)
            cat("\n")
    }
    matrix(pairs, ncol=2, byrow=TRUE)
}
#------------------------------------------------------------------------------
# Given a formula (as string), return a string with the "naked" predictors.
#
# Example: y ~ x9 + ns(x2,4) + s(x3,x4,df=4) + x5:sqrt(x6)
# becomes: y ~ x9 + x2 + x3 + x4 + x5 + x6
# which will later result in a model.matrix with columns x9 x2 x3 x4 x5 x6.
#
# This routine is not infallible but works for the commonly used formulas.

strip.formula.string <- function(form)
{
    gsubi <- function(pat, rep, x) gsub(pat, rep, x, ignore.case=TRUE)

    igrep <- grep("[a-zA-Z._][a-zA-Z._0-9]\\$", form)   # check for "ident$"
    if(length(igrep) > 0) {
        # TODO formula has vars with $, this confuses predict() later, why?
        # they cause "Warning: after calling plotmo.predict, y has the wrong length"
        stop0("plotmo: names with \"$\" are not yet supported\n",
            "The unacceptable formula is ", form)
    }
    form <- strip.white.space(form)
    args <- gsubi(".*~", "", form)                  # extract everything after ~

    # We don't want to mess with anything in [square brackets]
    # Doing that with regular expressions is tricky, so we adopt this approach:
    # change "+" "-" "," in square brackets to #PLUS# #MINUS# #COMMA# to protect them

    args <- gsubi("(\\[.*)\\+(.*\\])", "\\1#PLUS#\\2", args)
    args <- gsubi("(\\[.*)\\-(.*\\])", "\\1#MINUS#\\2", args)
    args <- gsubi("(\\[.*)\\,(.*\\])", "\\1#COMMA#\\2", args)

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
#------------------------------------------------------------------------------
# Return TRUE if predictions are probabilities (thus their range is 0 ... 1).
# Just a guess really, works for common models
# The intent is to minimize "predicted values are out of ylim" messages.

predictions.are.probabilities <- function(object, type)
{
    # get object's glm family if there is one, returns a string or NULL
    # TODO how to specify the 2nd or later model in earth's glm.list?
    get.glm.family <- function(object)
    {
        family <- NULL
        if(!is.null(object$glm.list[[1]])) # object class is "earth"
            family <- object$glm.list[[1]]$family$family
        else if(!is.null(object$family) && is.list(object$family))
            family <- object$family$family # object class is "glm" and similar
        family
    }
    #--- predictions.are.probabilities starts here ---
    if(pmatch(type, c("probability", "posterior"), 0))
        return(TRUE)
    family <- get.glm.family(object)
    is.binom <- !is.null(family) &&
                    pmatch(family, c("binomial", "quasibinomial"), nomatch=0)
    is.binom && pmatch(type, "response", 0)
}
no.clip.limits <- function(object, type)
{
    # assume no clip limits if is glm object and response is "link"
    inherits(object, "glm") && pmatch(type, c("link"), 0)
}
