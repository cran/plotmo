# xy.R: get a model's x or y (the plotmo_x and plotmo_y functions)
#
# Tracing is verbose and error messages are detailed throughout this
# file, to facilitate diagnosis when a model doesn't work with plotmo.
#------------------------------------------------------------------------------

# Return the "x" matrix for a model.  This returns a data.frame which
# always has column names.  It tries hard to get x regardless of the model.
# It can be used for models without a formula, provided that getCall(object)
# or model$x is available.
#
# The returned columns are for the "naked" predictors e.g. "x3" instead of
# "ns(x3,4)".  Column names are manufactured when necessary, as "x1",
# "x2", etc.  This is needed for example for rpart(x,y) where x does not
# have column names.
#
# It can handle sparse matrices from the Matrix package.  These get
# returned as a (non sparse) data.frame.
#
# If stringsAsFactors=FALSE, strings do not get converted to factors.

plotmo_x <- function(object, trace, stringsAsFactors=TRUE)
{
    trace2(trace, "--plotmo_x for %s object\n", class.as.char(object))

    x <- plotmo.x(object, trace)

    do.subset <- TRUE
    # plotmo.x.default returns list(field, do.subset), so handle that
    if(is.list(x) && !is.data.frame(x) && !is.null(x$do.subset)) {
        do.subset <- check.boolean(x$do.subset)
        x <- x$field
    }
    # Following are mainly for when plotmo.x didn't invoke plotmo.x.default.
    # It shouldn't be needed but is included here to make sure.
    x <- cleanup.x.or.y(object, x, "x", trace, check.naked=FALSE)
    stopifnot(is.good.data(x, "plotmo_x", check.colnames=FALSE))

    x <- my.data.frame(x, trace, stringsAsFactors)

    if(do.subset) {
        subset <- get.and.check.subset(x, object, trace)
        if(!is.null(subset)) {
            trace2(trace, "subset applied to x[%d,%d] ", NROW(x), NCOL(x))
            x <- x[subset, , drop=FALSE]
            trace2(trace, "to yield x[%d,%d]\n", NROW(x), NCOL(x))
        }
    }
    colnames(x) <- gen.colnames(x, "x", "x", trace)
    print_summary(x, "plotmo_x returned", trace)
    x
}
plotmo.x <- function(object, trace, ...)
{
    # returns x or list(field=x, do.subset=do.subset)
    UseMethod("plotmo.x")
}
plotmo.x.default <- function(object, trace, ...)
{
    # returns list(field=x, do.subset=do.subset)
    get.x.or.y(object, "x", trace, naked=TRUE)
}
# plotmo_y is similar to model.response but can handle models
# that were created without a formula.
#
# For more details on the args and return value, see process.y.
# If nresponse is not NULL we return the naked response variables
# e.g. Volume not log(Volume).
#
# If convert.glm.response=TRUE and the model is a glm model we may
# convert the response.  See convert.glm.response() for details.

plotmo_y <- function(object, nresponse=NULL, trace=0,
                     expected.len=NULL, resp.levs=NULL,
                     convert.glm.response=!is.null(nresponse))
{
    trace2(trace, "--plotmo_y with nresponse=%s for %s object\n",
           if(is.null(nresponse)) "NULL" else format(nresponse),
           class.as.char(object))
    y <- plotmo.y(object, trace, naked=FALSE, expected.len, nresponse)
    do.subset <- TRUE
    # plotmo.y.default returns list(field, do.subset), so handle that
    if(is.list(y) && !is.data.frame(y) && !is.null(y$do.subset)) {
        do.subset <- check.boolean(y$do.subset)
        y         <- y$field
    }
    if(convert.glm.response)
        y <- convert.glm.response(object, y, trace)
    if(do.subset) {
        subset <- get.and.check.subset(y, object, trace)
        if(!is.null(subset)) {
            trace2(trace, "subset applied to y[%d,%d] ", NROW(y), NCOL(y))
            y <- if(is.null(dim(y))) y[subset] else y[subset, , drop=FALSE]
            trace2(trace, "to yield y[%d,%d]\n", NROW(y), NCOL(y))
        }
    }
    process.y(y, object, type="response", nresponse,
              expected.len, resp.levs, trace, "plotmo_y")
}
# Note that the naked argument is irrelevant unless the response was
# specified with a wrapper function like log(Volume) instead of plain Volume.
#
# The default for nresponse allows this to work with old versions of earth
# (old plotmo.y.earth doesn't have a nresponse argument).

plotmo.y <- function(object, trace, naked, expected.len, nresponse=1, ...)
{
    # returns y or list(field=y, do.subset=do.subset)
    UseMethod("plotmo.y")
}
plotmo.y.default <- function(object, trace, naked, expected.len, ...)
{
    # returns list(field=y, do.subset=do.subset)
    get.x.or.y(object, "y", trace, try.object.x.or.y=TRUE,
               argn=2, nrows.argn=expected.len, naked)
}
# Get x or y from the given model object
# Returns list(field=x, do.subset=do.subset) where x is "x" or "y".

get.x.or.y <- function(
    object,                 # the model
    field,                  # "x" or "y"
    trace,
    try.object.x.or.y=TRUE, # FALSE if object[[field]] should be ignored
    argn=0,                 # if nonzero, consider argument nbr argn of the model call
    nrows.argn=NULL,        # expected NROWS of argument argn
    naked=TRUE)             # TRUE to return colnames like "x3" not "ns(x3,4)"
{
    ret.good.field <- function(x, do.subset=TRUE, source)
    {
        if(trace.call.global >= 1 && field == "y") {
            field <- if(field == "x") "predictors" else "response"
            if(grepl("model.frame(", source, fixed=TRUE))
                source <- sub(",", # insert newline after first comma
                    if(field == "response")
                        ",\n                                   "
                    else
                        ",\n                                 ", source)
            printf("got model %s from %s\n", field, source)
        }
        list(field=x, do.subset=do.subset)
    }
    stopifnot(is.list(object))
    stopifnot(field == "x" || field == "y")

    # try using object$x (where x is actually x or y throughout this file)

    object.x <- get.object.x.or.y.field(object, field, trace, try.object.x.or.y, naked)
    # object.x is object$x or NULL or an err msg
    if(is.good.data(object.x))
        return(ret.good.field(object.x, FALSE, sprint("object$%s", field)))

    call <- getCall(object)
    if(!is.null(call))
        trace2(trace, "\nobject call is %s\n", trunc.deparse(call, maxlen=80))

    # try getting x or y from the model formula and model frame

    temp <- get.x.or.y.from.model.frame(object, field, trace, naked)
        model.frame.x <- temp$x
        do.subset     <- temp$do.subset # TRUE when newdata is NULL
        source        <- temp$source

    # model.frame.x is now x or y or NULL or an err msg
    if(is.good.data(model.frame.x)) {
        formula.as.char <- paste.collapse(format(temp$formula))
        if(naked && grepl("\`", formula.as.char)) { # exception for hinge funcs etc
            trace2(trace, "setting check.naked=FALSE because backtick in formula\n")
            naked <- FALSE
        }
        model.frame.x <- cleanup.x.or.y(object, model.frame.x, field, trace,
                                        check.naked=naked && field != "y")
        if(!is.errmsg(model.frame.x))
            return(ret.good.field(model.frame.x, do.subset, source))
    }
    # try getCall(object)$x

    call.x <- get.data.from.object.call.field(object, field, trace)
    # call.x is getCall(object)$x or an error message
    if(is.good.data(call.x))
        return(ret.good.field(call.x, TRUE, sprint("getCall(object)$%s", field)))

    # else { # TODO may not want to do this if x is ok except for no colnames
    #     # try getCall(object)$X (note upper case "X")
    #     upfield <- toupper(field)
    #     call.x <- get.data.from.object.call.field(object, upfield, trace)
    #     # call.x is getCall(object)$X or an error message
    #     if(is.good.data(call.x)) {
    #         # paranoia, check that argument number is correct
    #         ifield <- if(field == "x") 2 else 3
    #         ok <- names(getCall(object))[ifield] == upfield
    #         if(!is.na(ok) && length(ok == 1) && ok)
    #             return(ret.good.field(call.x, TRUE,
    #                sprint("getCall(object)$%s", upfield)))
    #         else if(trace >= 2)
    #             printf("ignoring getCall(object)$%s because it isn't arg number %d\n",
    #                upfield, ifield)
    #     }
    # }

    trace2(trace, "\n")

    # consider argument number argn of the model call (ignoring its name)

    temp <- get.argn.from.call(argn, object, field, trace, nrows.argn)
        argn.x <- temp$x
        argn   <- temp$argn # may clear argn (for uncluttered errmsg later)
    # argn.x is the evaluated n'th arg or NULL or an err msg
    argn.name <- sprint("argument %g of the model call", argn)
    if(is.good.data(argn.x))
        return(ret.good.field(argn.x, TRUE, argn.name))

    # We don't have an x with colnames, so see if we have one without colnames.
    # We re-call is.errmsg() below to prevent re-issuing messages
    # in is.good.data() which we have already issued previously.

    if(try.object.x.or.y &&
           !is.errmsg(object.x) &&
           is.good.data(object.x, sprint("object$%s", field),
                        trace, check.colnames=FALSE))
        return(ret.good.field(object.x, FALSE, sprint("object$%s", field)))

    if(!is.errmsg(call.x) &&
           is.good.data(call.x, sprint("call$%s", field),
                        trace, check.colnames=FALSE))
        return(ret.good.field(call.x, TRUE, sprint("getCall(object)$%s", field)))

    if(argn && !is.errmsg(argn.x) &&
            is.good.data(argn.x, argn.name, trace, check.colnames=FALSE))
        return(ret.good.field(argn.x, TRUE, sprint("object$%s", field)))

    # unsuccessful

    errmsg.for.get.x.or.y(field, trace,
        try.object.x.or.y, argn, object.x,
        model.frame.x, call.x, argn.x)

    is.earth.cv.model <- is.null(object.x) &&
                         !is.null(object$ifold) &&
                         inherits(object, "earth")

    stopf("cannot get the original model %s%s",
          if(field == "x") "predictors" else "response",
          if(is.earth.cv.model) " (use keepxy=2 in the call to earth)" else "")
}
is.errmsg <- function(x)
{
    is.try.err(x) || (is.character(x) && length(x) == 1)
}
# Is the x argument a valid x or y for a model?
# This returns TRUE or FALSE, silently unless trace >= 2.

is.good.data <- function(x, xname="field", trace=0, check.colnames=TRUE)
{
    good <- !is.null(x) && !is.try.err(x) && NROW(x) >= 3
    has.colnames <- good && !is.null(colnames(x)) && !any(colnames(x) == "")
    if(trace >= 2)
        trace.data(good, has.colnames, x, xname, trace, check.colnames)
    good && (!check.colnames || has.colnames)
}
trace.data <- function(good, has.colnames,
                       x, xname, trace, check.colnames)
{
    stopifnot.string(xname)
    colnames.msg <-
        if(good && has.colnames) {
            sprint(" and has column name%s %s",
                   if(length(colnames(x)) == 1) "" else "s",
                   paste.trunc(colnames(x), maxlen=100))
        } else if(good)
            sprint(" but without colnames %s",
                   if(check.colnames) "so we will keep on searching"
                   else               "but we will use it anyway")
        else
            ""
    if(good)
        printf("%s is usable%s\n", xname, colnames.msg)
    else if(is.null(x))
        printf("%s is NULL%s\n", xname,
               if(check.colnames) " (and it has no colnames)" else "")
    else if(!is.character(x) && NROW(x) < 3)
        printf("%s has less than three rows\n", xname,
               if(check.colnames) " (and it has no colnames)" else "")
    else
        printf("%s is not usable%s\n", xname, colnames.msg)

    # print bad data, but only on the first go around for this data
    # (use check.colnames as an indicator of first go around)

    if(!is.null(x) && check.colnames) {
        if(!good)
            printf("%s:%s\n", xname, format_err_field(x, xname, trace))
        else if(trace >= 4) {
            printf("trace>=4: ")
            print_summary(x, xname, trace=2)
        }
    }
}
errmsg.for.get.x.or.y <- function(field, trace, try.object.x.or.y,
    argn, object.x, model.frame.x, call.x, argn.x)
{
    printf("\nLooked unsuccessfully for the original %s in the following places:\n",
           if(field == "x") "predictors" else "response")

    ifield <- 1
    if(try.object.x.or.y) {
        printf("\n(%d) object$%s:%s\n",
               ifield, field, format_err_field(object.x, field, trace))
        ifield <- ifield + 1
    }
    printf("\n(%d) model.frame:%s\n",
           ifield, format_err_field(model.frame.x, field, trace))
    ifield <- ifield + 1

    printf("\n(%d) getCall(object)$%s:%s\n",
           ifield, field, format_err_field(call.x, field, trace))
    ifield <- ifield + 1

    if(argn)
        printf("\n(%d) argument %d of the model call:%s\n",
               ifield, argn+1, format_err_field(argn.x, field, trace))

    printf("\n")
}
format_err_field <- function(x, xname, trace=0)
{
    if(is.try.err(x)) {
        errmsg <- sub(".* : *",    "",  x[1])   # strip prefix "Error in xxx : "
        errmsg <- gsub("\n *\\^",  "",  errmsg) # strip "    ^" in some err msgs
        errmsg <- gsub("[\n\t ]+", " ", errmsg) # collapse newlines and multiple spaces
        errmsg <- gsub("^ *| *$",  "",  errmsg) # delete remaining leading and trailing space
        sprint(" %s", errmsg)
    } else if(is.errmsg(x))
        sprint(" %s", x)
    else if(is.null(x))
        sprint(" NULL")
    else if(NROW(x) < 3)
        sprint(" less than three rows")
    else if(!is.null(dim(x))) {
        print_summary(x, xname, trace=2)
        sprint(" is not usable (see above)")
    } else
        sprint(" class \"%s\" with value %s",
               class(x), try(paste.trunc(format(x))[1]))
}
# Get object$x or object$y from the model.
# Return x (or y) or NULL or an error message.
#
# The approach taken in all helper routines for get.x.or.y
# (such as get.object.x.or.y.field) is that we issue trace messages
# here in the helper routine, and the caller silently checks
# the returned value for good data.
#
# For a model with a formula, the standard  path is to apply the
# naked formula to the data using model.frame().
# Example with argument field="x":
#
#   formula(object)    resp~num + sqrt(num) + bool + ord:num + fac
#   naked formula      resp~num + bool + ord + fac
#   data colnames      resp bool ord fac str num nx int date
#   returned colnames  num bool ord fac

get.object.x.or.y.field <- function( # get object$x or object$y
    object,                 # the model
    field,                  # "x" or "y"
    trace,
    try.object.x.or.y=TRUE, # FALSE if object[[field]] should be ignored
    naked=TRUE)             # TRUE for columns like "x3" not "ns(x3,4)"
{
    trace2(trace, "\nget.object.%s:\n", field)
    x <- NULL
    xname <- sprint("object$%s", field) # for tracing
    if(!try.object.x.or.y) # e.g. we must ignore object$x for mda::mars models
        trace2(trace, "ignoring %s for this %s object\n", xname, class.as.char(object))
    else {
        # note we use object[["x"]] rather than object$x to prevent partial
        # matching (but the error messages use object$x for readability)
        x <- object[[field]]
        if(is.good.data(x, xname, trace))
            x <- cleanup.x.or.y(object, x, field, trace,
                                check.naked=naked && field != "y")
        else if(!is.null(x) && !is.good.data(x, check.colnames=FALSE)) {
            # Issue a warning because predict.lm will probably crash
            # later when it internally accceses object$x.
            # We call is.good.data(check.colnames=FALSE) above to check if the
            # prior call to is.good.data() failed merely because of a colname
            # issue (if it's just a colname issue then don't issue warning).
            warnf("object$%s may be corrupt", field)
        }
    }
    x   # return x or NULL or an error message
}
# Get getCall(object)$x (or similar) from the model's call field.
# Return x (or similar) or NULL or an error message.

get.data.from.object.call.field <- function(object, field, trace,
                                            check.is.good.data=TRUE)
{
    trace2(trace, "\nget.data.from.object.call.field:\n")
    x <- NULL
    xname <- sprint("getCall(object)$%s", field)
    call <- getCall(object)
    if(is.null(call))
        trace2(trace, "getCall(object) is NULL so cannot get %s\n", xname)
    else if(!is.call(call))
        trace2(trace, "getCall(object) is not actually a call so cannot get %s", xname)
    else {
        x <- try.eval(call[[field]], model.env(object), trace=trace, expr.name=xname)
        if(is.errmsg(x))
            trace2(trace, "%s\n", x)
        else if(check.is.good.data) # invoke is.good.data purely for issuing trace messages
            is.good.data(x, xname, trace)
    }
    x
}
# Get the n'th arg in the call to the model function.
#
# This is for those model functions whose second argument is the
# response (what we call "y"), although that argument's name is
# not "y".  For example, argn=2 will select the "grouping" arg in
# qda(x=lcush[,2:3], grouping=lcush[,1]).
#
# Returns list(argn.x, argn)
# where argn.x is the evaluated n'th argument or NULL or an error message.
# and argn will be set 0 if routine processing says we should ignore argn.

get.argn.from.call <- function(argn, object, field, trace, nrows.argn)
{
    x <- NULL
    if(argn) {
        temp <- get.argn.from.call.aux(argn, object, field, trace, nrows.argn)
            x    <- temp$x
            argn <- temp$argn
        if(is.errmsg(x))
            trace2(trace, "%s\n", x)
        else # invoke is.good.data purely for issuing trace messages
            is.good.data(x, sprint("argument %d of the model call", argn), trace)
    }
    list(x=x, argn=argn)
}
# auxilary function for get.argn.from.call

get.argn.from.call.aux <- function(argn, object, field, trace, nrows.argn)
{
    ret <- function(x, argn)
    {
        list(x=x, argn=argn)
    }
    #--- get.argn.from.call.x starts here
    stopifnot(argn > 0)
    call <- getCall(object)
    if(is.null(call))
        return(ret("getCall(object) is NULL so cannot use argn", argn))
    if(!is.call(call))
        return(ret("getCall(object) is not actually a call so cannot use argn", argn))
    if(length(call) <= argn)
        return(ret(sprint(
            "cannot use argn %d because getCall(object) does not have %d arguments",
            argn, argn), argn))
    names.call <- names(call) # some names may be ""
    trace2(trace, "names(call) is %s\n", quotify(names.call))

    # If argn is field (i.e. "x" or "y"), don't process it here because
    # we process call$x and call$y elsewhere (in get.data.from.object.call.field).
    # This is a common case, so we clear argn for uncluttered message
    # later in errmsg.for.get.x.or.y.
    # If the arg name is "" in getCall(object) this won't work, not serious.

    if(identical(names.call[argn+1], field))
        return(ret(sprint(
            "the name of argument %d is \"%s\" so we will not process it with argn",
            argn, field),
            argn=0))

    # If an argument of the call is "formula" then return, because
    # any arg named "x" or "y" is unlikely to be model data.
    # This is a a common case, so clear argn.

    if(pmatch("formula", names.call[2], 0))
        return(ret(sprint(
            "ignoring argn %g because there is a formula argument", argn),
            argn=0))
    x <- try.eval(call[[argn+1]], model.env(object), trace=trace,
                  sprint("argument %d of the model call", argn))
    if(is.data.frame(x))
        x <- x[[1]]
    if(!(is.numeric(x[1]) || is.logical(x[1]) || is.factor(x[1])))
        return(ret(sprint(
            "cannot use argn %d because it is not numeric, logical, or a factor",
            argn), argn))
    if(is.null(nrows.argn)) # should never happen
        stop0("cannot use argn because the expected number of rows is unspecified")
    if(NROW(x) != nrows.argn)
        return(ret(sprint(
            "cannot use argn %g because it has %g rows but expected %g rows",
            argn, NROW(x), nrows.argn), argn))
    list(x=x, argn=argn)
}
# If object has a formula, use that formula to get x or y (field is "x" or "y").
# Returns list(x, do.subset, form.as.char, source) where x may be an err msg and source
# is a string describing where we got the data from (only used if no err msg).

get.x.or.y.from.model.frame <- function(object, field, trace, naked,
                                        na.action="auto", newdata=NULL)
{
    ret <- function(...)  # ... is an err msg in printf form
    {
        errmsg <- sprint(...)
        trace2(trace, "%s\n", errmsg)
        list(x=errmsg, do.subset=FALSE, formula=NULL, source="model frame")
    }
    #--- get.x.or.y.from.model.frame starts here
    stopifnot(field == "x" || field == "y")
    trace2(trace, "\nget.%s.from.model.frame:\n", field)
    mf <- get.model.frame(object, field, trace, naked, na.action, newdata)
    if(!is.good.data(mf$x))
        return(mf)
    model.frame <- mf$x
    if(field == "x") {
        # Check if any vars have $ in their name, this confuses predict() later.
        # They cause "Error in model.frame.default: variable lengths differ"
        # or "newdata had 50 rows but variables found have 330 rows"
        ibad <- grep("[._[:alnum:]]\\$", colnames(model.frame))
        if(any(ibad)) {
            warnf("%s: \"$\" in colnames(model.frame) is not supported by plotmo, %s",
                  colnames(model.frame)[ibad[1]],
                  "will try to get the data elsewhere")
            return(ret("\"$\" in colnames(model.frame)"))
        }
    }
    # got the model.frame, now get the column index(s) of the response in the model.frame
    iresponse.col <- get.iresponse.col(object, model.frame, mf$isFormula,
                            trace=if(field=="y") trace else 0) # reduce number of msgs
    if(field == "x") {
        # drop the response column(s)
        x <- model.frame[, -iresponse.col, drop=FALSE]
        if(!is.good.data(x, sprint("x=model.frame[,-%s]", paste.c(iresponse.col)), trace))
            return(ret("invalid model.frame[,-iresponse]"))
    } else { # field == "y"
        # select the response column(s)
        # we don't use model.response() here because that drops the column name
        x <- model.frame[, iresponse.col, drop=FALSE]
        if(!is.good.data(x, sprint("y=model.frame[,%s]", paste.c(iresponse.col)), trace))
            return(ret("invalid model.frame[,iresponse]"))
    }
    list(x=x, do.subset=mf$do.subset, formula=mf$formula, source=mf$source)
}
# The following is derived from stats::model.frame.default but tries to
# also handle models that didn't save the terms etc. in a standard way.
# It never uses parent.frame (as some model.frame methods do).
#
# We will use the given na.action.  But if na.action="auto" then get
# na.action from the model itself, and do a little special handling.
#
# Returns list(x, do.subste, formula, source, isFormula)
#   where x may be an err msg
#   source s a string describing where we got the data from (only used if no err msg)

get.model.frame <- function(object, field, trace, naked,
                            na.action="auto", newdata=NULL)
{
    ret <- function(x, do.subset=FALSE, formula=NULL,
                    source="model frame", isFormula=FALSE)
    {
        list(x=x, do.subset=do.subset, formula=formula,
             source=source, isFormula=isFormula)
    }
    #--- get.model.frame starts here
    # get.model.formula returns a Formula or formula with an environment, or an error string
    modform <- get.model.formula(object, trace, naked)
    formula <- modform$formula
    if(is.errmsg(formula))
        return(ret(formula)) # return errmsg
    isFormula <- inherits(formula, "Formula") # Formula vs formula
    trace2(trace, "formula is valid, now looking for data for the model.frame\n")
    if(!is.null(newdata)) {
        if(!is.good.data(newdata, "newdata", trace))
            return(ret("bad newdata")) # return errmsg
        data        <- newdata
        data.source <- "newdata"
    } else {
        # use object$model if possible (e.g. lm)
        # TODO the following code really belongs in get.data.for.model.frame?
        x <- object[["model"]]
        if(is.good.data(x, "object$model", trace)) {
            # Drop column named "(weights)" created by lm() if called with weights
            # (must drop else x will be rejected because non-naked colname).
            x <- x[, which(colnames(x) != "(weights)"), drop=FALSE]
            if(trace >= 3)
                print_summary(x, "model.frame", trace)
            # Note that we call check.naked even when the naked=FALSE.
            # Not essential, but gives more consistency so we select the same object$x,
            # getCall(object), or etc. regardless of whether naked is set or clear.
            if(is.null(check.naked(x, "object$model", trace))) # good object$model?
                return(ret(x, FALSE, formula, "object$model", isFormula))
        }
        temp <- get.data.for.model.frame(object, trace)
            data        <- temp$data
            data.source <- temp$source
        if(!is.good.data(data)) {
            # data is not usable (could be NULL)
            # following is for when no data argument when model was built
            data <- model.env(object)
            data.source <- "model.env(object)"
        }
    }
    if(is.character(na.action) && length(na.action) == 1 && na.action == "auto") {
        na.action <- na.action(object)
        class.na.action <- class(na.action)
        # following is for rpart's and ctree's (special but useful) NA handling
        if(is.null(na.action))
            na.action <-
                if(inherits(object, "rpart") || inherits(object, "party_plotmo"))
                    "na.pass"
                else
                    "na.fail"
        else if(length(class.na.action) == 2 && class.na.action[1] == "na.rpart")
            na.action <- paste0("na.", class(na.action)[2])
        else if(class.na.action[1] %in% c("exclude", "fail", "omit", "pass"))
            na.action <- paste0("na.", class(na.action)[1])
        trace2(trace, "na.action(object) is %s\n", as.char(na.action))
    }
    if(!is.function(na.action) && !is.character(na.action)) {
        errmsg <- sprint("bad na.action: %s", as.char(na.action))
        trace2(trace, "%s\n", errmsg)
        return(ret(errmsg))
    }
    if(trace >= 3) {
        printf("model.env is %s\n", environment.as.char(model.env(object)))
        print_summary(data, "data", trace)
    }
    data.source <-
        if(is.environment(data)) environment.as.char(data)
        else if(is.null(data))   "NULL"
        else                     data.source

    mfcall.as.char <- sprint("model.frame(%s, data=%s, na.action=%s)",
                             paste.trunc(modform$form.as.char, maxlen=40),
                             data.source, trunc.deparse(na.action))

    trace2(trace, "stats::%s\n", mfcall.as.char)

    x <- try(do.call(stats::model.frame, # calls model.frame.default
                     args=list(formula=formula, data=data, na.action=na.action)),
            silent=trace < 2)

    if(trace >= 3)
        print_summary(x, "model.frame returned", trace)

    ret(x, if(is.null(newdata)) TRUE else FALSE, formula, mfcall.as.char, isFormula)
}
get.data.for.model.frame <- function(object, trace)
{
    ret <- function(errmsg, data=NULL, source="model frame")
    {
        if(!is.null(errmsg))
            trace2(trace, "%s\n", errmsg)
        list(data=data, source=source)
    }
    # try object$data e.g. earth models with formula and keepxy=T
    # the inherits check is becauses party objects for e.g. "medv ~ log(lstat) + rm^2"
    # save "log(lstat)" not "lstat" in object data, that confuses model.frame.default
    if(!inherits(object, "party_plotmo")) {
        data <- object[["data"]]
        if(is.good.data(data, "object$data", trace))
            return(ret(NULL, data, "object$data"))
    }
    # look for the data in getCall(object)
    call <- object[["call"]]
    if(is.null(call))
        return(ret("getCall(object) is NULL so cannot get the data from the call"))
    if(!is.call(call))
        return(ret("getCall(object) is not actually a call so cannot get the data from the call"))
    data <- NULL
    argname <- "NULL"
    # try getCall(object)$data
    idata <- match(c("data"), names(call), 0)[1]
    if(idata > 0) {
        trace2(trace, "argument %g of the call is 'data'\n", idata-1)
        argname <- "call$data"
        # Mar 2019: TODO this doesn't work (if model was built internally to another
        # function?) because  it tries to get data from .RGlobalEnv (which in that
        # environment is a function "data").  Perhaps failure is because terms(mf) seems
        # to generate a terms field ".GlobalEnv" regardless of where the mf was evaluated.
        # Workaround for earth models: use keepxy=TRUE (to avoid this code)
        data <- try(eval.trace(call[[idata]], model.env(object),
                               trace=trace, expr.name=argname),
                    silent=FALSE) # so user can see what went wrong
        is.good.data(data, argname, trace) # purely for tracing
    } else {
        # no getCall(object)$data, search for an arg that looks like good data
        trace2(trace,
"getCall(object) has no arg named 'data', will search for an arg that looks like data\n")
        if(length(call) >= 3) { # start at 3 to ignore fname and first arg (the formula)
            for(icall in 3:length(call)) {
                arg <- call[[icall]]
                if(class(arg)[1] == "name") { # paranoia, will always be true?
                    argname <- sprint("call$%s", quotify(as.character(arg)))
                    data <- eval.trace(arg, model.env(object), trace=trace, expr.name=argname)
                    if(is.good.data(data, argname, trace=trace)) {
                        trace2(trace, "%s appears to be the model data\n", argname)
                        idata <- icall
                        break
                    } else {
                        trace2(trace, "%s is not the model data\n", argname)
                        data <- NULL
                    }
                }
            }
        }
    }
    if(is.good.data(data, argname)) {
        # following needed for e.g. nnet(O3~., data=scale(ozone1), size=2)
        # Else get Error in model.frame.default: 'data' must be a data.frame.
        if(!is.data.frame(data)) {
            data <- try(my.data.frame(data, trace))
            # invoke is.good.data purely for issuing trace messages
            is.good.data(data, sprint(
                "%s converted from \"%s\" to \"data.frame\"",
                argname, class(data)[1]), trace)
        }
    }
    ret(NULL, data, argname)
}
# get the column index(s) of the response in the model.frame, return 1 if can't (best guess is 1)
get.iresponse.col <- function(object, model.frame, isFormula, trace)
{
    assuming <- sprint("assuming \"%s\" in the model.frame is the response, because",
                       gen.colnames(model.frame, prefix="model.frame", trace=trace)[1])
    iresponse.col <- 1
    terms <- try(terms(object), silent=TRUE)
    if(is.null(terms)) { # e.g. bagEarth.formula and nn
        trace1(trace, "%s terms(object) is NULL\n", assuming)
        return(1) # assume iresponse.col is 1
    }
    if(is.try.err(terms)) {
        trace1(trace, "%s terms(object) did not return the terms\n", assuming)
        return(1)
    }
    # object seems to have a valid terms field
    iresponse.col <- attr(terms, "response")
    if(is.null(iresponse.col) || !is.numeric(iresponse.col) || length(iresponse.col) != 1) {
        trace1(trace, "%s attr(terms, \"response\") is invalid\n", assuming)
        return(1)
    }
    if(iresponse.col != 0) {
        if(isFormula) {
            trace1(trace, "%s object used Formula (not formula) yet attr(terms, \"response\") is nonzero\n", assuming)
            return(1)
        }
        iresponse.col <- try(check.index(iresponse.col,
                                         "attr(terms, \"response\")", model.frame,
                                         is.col.index=TRUE, allow.negatives=FALSE))
        }
    else { # iresponse.col == 0
        if(!isFormula) {
            trace1(trace, "%s attr(terms, \"response\") is 0\n", assuming)
            return(1)
        }
        # isFormula
        iresponse.col <- attr(terms, "Response")
        if(is.null(iresponse.col)) {
            # will happen for any model that uses Formula (not formula), except earth
            trace1(trace, "%s the model was built with Formula (not formula)\n", assuming)
            return(1)
        }
        if(is.null(iresponse.col) || !is.numeric(iresponse.col)) {
            trace1(trace, "%s attr(terms, \"Response\") is invalid\n", assuming)
            return(1)
        }
        iresponse.col <- try(check.index(iresponse.col,
                                         "attr(terms, \"Response\")", model.frame,
                                         is.col.index=TRUE, allow.negatives=FALSE))
    }
    if(is.try.err(iresponse.col)) {
        trace1(trace, "%s calculated index was invalid\n", assuming)
        iresponse.col <- 1
    }
    iresponse.col
}
isa.formula <- function(x)
{
    (typeof(x) == "language" && as.list(x)[[1]] == "~") ||
    (is.character(x) && length(x) == 1 && grepany("~", x))
}
get.index.of.formula.arg.in.call <- function(call, trace)
{
    iform <- match(c("formula"), names(call), 0)
    if(iform)
        return(iform)
    # no arg named "formula" in call, so look for a formula elsewhere in call
    # TODO for which model was this code added? I think it's needed if formula arg is unnamed?
    call <- as.list(call)
    # start at 2 to skip call[1] which is the function name
    for(iform in 2:length(call)) {
        if(isa.formula(call[[iform]])) {
            # warning0("the formula in the model call is not named 'formula'")
            trace2(trace, "argument %d in getCall(object) is a formula\n", iform)
            return(iform) # note return
        }
    }
    0 # no formula
}
# return a Formula or formula with an environment, or an error string

get.model.formula <- function(object, trace, naked)
{
    ret <- function(...)      # ... is an err msg in printf form
    {
        errmsg <- sprint(...)
        trace2(trace, "%s\n", errmsg)
        list(formula=errmsg, form.as.char="formula")
    }
    #--- get.model.formula starts here
    # try getting the formula from the terms field (object used formula)
    terms <- try(terms(object), silent=TRUE)
    if(is.null(terms))
        trace2(trace, "terms(object) is NULL, will look for the formula elsewhere\n")
    else if(is.try.err(terms))
        trace2(trace, "terms(object) did not return the terms, will look for the formula elsewhere\n")
    else { # object has a valid terms field
        # TODO Sep 2020 ask Formula package people to extend
        # (currently only earth supports attr(terms, "Formula") and "Response"
        form <- attr(terms, "Formula")
        isFormula <- !is.null(form) # "Formula" vs "formula"
        if(isFormula) {
            trace1(trace, "object created with Formula (not formula): using attr(terms, \"Formula\")\n")
            form <- formula_as_char_with_check(form, "attr(terms, \"Formula\")", trace)
        } else {
            form <- try(formula(terms), silent=TRUE)
            form <- formula_as_char_with_check(form, "formula(object)", trace)
        }
        if(!is.null(form$form.as.char))
            return(process.formula(object, form$form.as.char, isFormula, trace, naked))
        # if there was a $ in the form.as.char there is no point in looking at the call
        # formula, so to avoid issuing the same warning twice, we return
        # immediately here
        if(grepl("\"$\"", form$errmsg, fixed=TRUE))
            return(ret(form$errmsg))
    }
    # try getting the formula from getCall(object)
    call <- object[["call"]]
    if(is.null(call))
        return(ret("getCall(object) is NULL so cannot get the formula from the call"))
    if(!is.call(call))
        return(ret("getCall(object) is not actually a call so cannot get the formula from the call"))
    iform <- get.index.of.formula.arg.in.call(call, trace)
    if(iform == 0) # no formula?
        return(ret("no formula in getCall(object)"))

    # nasty name change, else model.frame.default: invalid type (language)
    # TODO clean this up, this won't work because it doesn't change the calling obj
    # names.call <- names(call)
    # names.call[iform] <- "formula"
    # names(call) <- names.call # note <<- not <-
    form.name <- sprint("model call argument %d", iform-1)
    form <- eval(call[[iform]], model.env(object))
    form <- formula_as_char_with_check(form, form.name, trace)
    if(is.null(form$form.as.char))
        return(ret(form$errmsg))
    # TODO More classes could be added to the following assignment to isFormula
    # (and remember we can only get here if object doesn't have a terms field,
    # and I believe the objects below do in fact have a terms field)
    isFormula <- inherits(object, c("pre"))
    process.formula(object, form$form.as.char, isFormula=isFormula, trace, naked)
}
# convert the formula to character, and also check it

formula_as_char_with_check <- function(form, form.name, trace)
{
    ret.null <- function(...) # ... is an err msg in printf form
    {
        errmsg <- sprint(...)
        trace2(trace, "%s\n", errmsg)
        list(form.as.char=NULL, errmsg=errmsg)
    }
    if(is.try.err(form))
        return(ret.null("%s did not return a formula", form.name))
    if(is.null(form))
        return(ret.null("%s is NULL", form.name))
    if(class(form)[1] != "formula" && !class(form)[1] == "Formula" &&
            !(is.character(form) && length(form) == 1))
        return(ret.null("%s is not a formula or Formula (its class is \"%s\")",
               form.name, class(form)[1]))
    form.as.char <- paste.collapse(format(form))
    trace2(trace, "%s is %s\n", form.name, paste.trunc(form.as.char))
    if(!grepl("[^ \t]+.*~", form.as.char))
        return(ret.null("%s has no response",  form.name))
    # Check if any vars have $ in their name, this confuses predict() later.
    # TODO Following comments are no longer accurate?
    # We do this check in get.x.or.y.from.model.frame but pre-emptively also here
    # (where we have the formula) for a slightly more informative error message.
    # (The other message kicks in if we get the model.frame from object$model.)
    rhs <- gsub(".*~ *", "", form.as.char)
    if(grepany("[._[:alnum:]]\\$", rhs)) { # check for "ident$"
        warnf("\"$\" in the formula is not supported by plotmo, %s\n         formula: %s",
              "will try to get the data elsewhere",
              rhs)
        return(ret.null("%s: \"$\" in formula is not allowed", form.name))
    }
    list(form.as.char=form.as.char, errmsg=NULL)
}
# Return a formula with an environment.  Also process naked.
# TODO this includes Height in Volume~Girth-Height, it shouldn't

process.formula <- function(object, form.as.char, isFormula, trace, naked)
{
    stopifnot(is.character(form.as.char))
    stopifnot(length(form.as.char) == 1)
    if(naked)
        form.as.char <- naken.formula.string(form.as.char, trace)
    form <- try(formula(form.as.char, env=model.env(object)), silent=TRUE)
    if(isFormula && !is.try.err(form))
        form <- try(Formula::Formula(form))
    if(is.try.err(form)) {
        # prepend "formula(%s) failed" for a clearer msg in format_err_field later
        form <- sprint("%s(%s) failed%s",
                       if(isFormula) "Formula" else "formula",
                       quotify(form.as.char),
                       # only append err msg if tracing because err msgs can be obscure
                       if(trace >= 1) sprint("(%s)", cleantry(form)) else "")
        trace2(trace, "%s\n", form)
        form <- sprint("Error : %s", form)
    }
    list(formula=form, form.as.char=form.as.char)
}
# Given a formula (as string), return a string with the "naked" predictors.
# This is used for getting the data to pass to predict.
#
# Example: log(y) ~ x9+ns(x2,4) + s(x3,x4,df=4) + x5:sqrt(x6)
# becomes: log(y) ~ x9 + x2 + x3 + x4 + x5 + x6
# which will later result in a model.matrix with columns x9 x2 x3 x4 x5 x6.
#
# Note that we don't naken the response (so for
# example in the above log(y) remains unchanged).
#
# This routine is not infallible but works for the commonly used formulas.
# It's a hack that relies on regular expressions.

naken.formula.string <- function(form.as.char, trace)
{
    stopifnot(is.character(form.as.char))
    form.as.char <- paste.collapse(form.as.char)
    old.form.as.char <- form.as.char
    naked <- gsub(".*~", "", form.as.char)          # extract everything after ~
    naked <- naken.collapse(naked, warn.if.minus=TRUE)
    if(grepl("~", form.as.char)) {
        response <- gsub("~.*", "", form.as.char)   # extract up to the ~
        response <- gsub("^ +| +$", "", response)   # trim leading and trailing spaces
        if(nchar(response))
            response <- paste0(response, " ~")
        naked <- paste.collapse(response, naked)
    }
    trace2(trace,
           if(strip.space(naked) == strip.space(old.form.as.char))
               "naked formula is the same%.0s\n" # e.g. O3~vh+wind
           else
               "naked formula is %s\n", naked)
    naked
}
is.naked <- function(colnames) # returns a logical vector
{
    naked <- logical(length(colnames))
    for(i in seq_len(length(colnames))) {
        colname <- strip.space(colnames[i])
        naked[i] <- colname == naken.collapse(colname)
    }
    naked
}
# Return an err msg if colnames(x) is not "naked".
# Return NULL if everything is ok.
#
# Example: in lm(Volume~poly(Height, degree=3), data=trees, x=T),
#   object$x, object$data, and object$model have
#   colnames like "poly(Height, degree = 3)1"
#   where plotmo (actually model.frame.default) gives "Error: object 'x1' not found"
#   unless we preempt that obscure error message here.

check.naked <- function(x, xname, trace)
{
    errmsg <- NULL

    colnames <- colnames(x)

    # column name "(Intercept)" must be considered naked
    colnames <- sub("(Intercept)", "Intercept", colnames, fixed=TRUE)

    is.naked <- is.naked(colnames)
    if(any(!is.naked)) {
        # e.g. lm(formula=log(doy)~vh, ...)
        errmsg <- sprint(
            "%s cannot be used because it has%s non-naked column name%s %s",
            xname,
            if(sum(!is.naked) > 1) "" else " a",
            if(sum(!is.naked) > 1) "s" else "",
            quotify.trunc(colnames[!is.naked]))
        trace2(trace, "%s\n", errmsg)
    }
    errmsg
}
# Returns x or an error message (currrently an error message
# is returned only if naked=TRUE but colnames are not naked).

cleanup.x.or.y <- function(object, x, field, trace, check.naked)
{
    x <- handle.nonvector.vars(object, x, field, trace)

    # remove column "(Intercept)"  e.g. object$x for lm(y~x1+x2, x=TRUE)
    if(!is.na(i <- match("(Intercept)", colnames(x)))) {
        trace2(trace, "dropped \"(Intercept)\" column from %s\n", field)
        x <- x[,-i, drop=FALSE]
    }
    if(check.naked) {
        errmsg <- check.naked(x, field, trace)
        if(!is.null(errmsg))
            return(errmsg)
    }
    x
}
# This tries to clean up columns of x that are themselves matrices or data.frames.
#
# Example (where the actual values in the x and y are not important):
#   x <- matrix(c(1,3,2,4,5,6,7,8,9,10,
#                 2,3,4,5,6,7,8,9,8,9), ncol=2)
#   colnames(x) <- c("c1", "c2")
#   y <- 3:12
#   a <- lm(y~x) # seems natural, but lm doesn't handle it as we might expect
# Cannot get predict to work with newdata on above lm model
# Causes for example 'newdata' had 8 rows but variables found have 10 rows
#
# Another example:
#   library(ElemStatLearn); x <- mixture.example$x;
#   g <- mixture.example$y; a <- lm(g ~ x)
#
# This routine also prevents a misleading error msg later in plot_degree1
# (illegal index, missing column in x) caused by the following code:
#    data(gasoline, package='pls')
#    plotmo(earth(octane ~ NIR, data=gasoline))
# where NIR has class "AsIs" and is a matrix.
# There appears to be no easy fix for this (July 2011).

handle.nonvector.vars <- function(object, x, field, trace)
{
    if(!is.data.frame(x))
        return(x)

    ndims.of.each.var <- sapply(x, function(x) NCOL(x))
    if(all(ndims.of.each.var == 1)) {
        # we are ok: NCOL is 1 for all variables (even though some
        # may not be vectors i.e. they could be single column mats)
        return(x)
    }
    format <- paste0("%s variable on the %s side of the formula is a matrix or data.frame\n",
                     "         plotmo often cannot process such variables")
    msg <- sprint(format,
        if(ncol(x) == 1) "the" else "a",
        if(field == "x") "right" else "left")

    if(field == "x") {
        # We issue the warning only if this is the rhs, because we seem to be able
        # to recover when the lhs is a non vector.  Thus we correctly don't issue
        # warnings for valid models like earth(cbind(O3,doy)~., data=ozone1) and
        # glm(cbind(damage, 6-damage)~temp, family=binomial, data=orings).
        warning0(msg)
    } else if(trace >= 2) {
        printf("%s\n", msg)
        printf("the number of dimensions of each variable in %s is %s and ",
               field, paste.trunc(ndims.of.each.var))
        # details is 1 not 2 below else huge output
        print_summary(x, sprint("%s is ", field), trace, details=-1)
    }
    # Attempt to fix the problem by replacing x with x[[1]].  However
    # for the rhs this only sometimes works --- there may be downstream
    # problems, typically in predict (because the column names are wrong?).

    if(ndims.of.each.var[1] > 1) { # first variable is not a vector
        trace2(trace, "replacing %s with %s[[1]]%s\n", field, field,
               if(length(ndims.of.each.var) == 1) ""
               else ", ignoring remaining columns")
        org.colnames <- colnames(x)
        x <- x[[1]]
        # add column names (helps keep track later)
        if(is.null(colnames(x))) {
            safe.org.colnames <-
                if(is.null(org.colnames)) # can never happen, but best to be sure
                    field
                else
                    org.colnames
            if(NCOL(x) > 1)
                colnames(x) <- paste0(safe.org.colnames[1], "[,", 1:NCOL(x), "]")
            else # e.g. glm(formula=response~temp, family="binomial", data=...)
                colnames(x) <- safe.org.colnames[1]
            trace2(trace, "%s colnames were %s and now %s\n",
                field,
                if(is.null(org.colnames)) "NULL"
                else quotify.trunc(org.colnames),
                quotify.trunc(colnames(x)))
        }
    }
    x
}
# Detect if the model is a glm model, and if so possibly convert the
# response.  We do this in the same way as glm() does internally:
#
# o A factor response get converted to indicator column of
#   ones and zeros (first level vs the rest).
#
# o Two column binomial responses get converted to a single
#   column of fractions.
#
# Note that responses for earth models are handled independently
# in plotmo.y.earth (two level factor to single numeric column,
# three of more level factors to three or more indicator columns).

convert.glm.response <- function(object, y, trace)
{
    # check if y is is factor, or first column of y is a factor
    is.factor <- is.factor(y) ||
                 (length(dim(y) == 2) && ncol(y) == 1 && is.factor(y[,1]))
    if(is.factor)
        y <- convert.glm.response.factor(object, y, trace)
    else if(NCOL(y) == 2) # possibly a two column binomial model
        y <- possibly.convert.glm.two.column.response(object, y, trace)
    y
}
is.nomial <- function(object)
{
    is.nomial.string <- function(family) {
        family[1] == "binomial" ||
        family[1] == "quasibinomial" ||
        family[1] == "multinomial"
    }
    if(!is.list(object))
        return(FALSE)

    family <- object$family
    if(is.character(family)) # glmnet models
        return(is.nomial.string(family))

    fam <- try(family(object), silent=TRUE)
    if(inherits(fam, "family")) { # lm, glm, etc models
        family <- fam$family
        if(is.character(family))
            return(is.nomial.string(family))
    }
    FALSE
}
convert.glm.response.factor <- function(object, y, trace)
{
    if(!is.nomial(object)) {
        # e.g. rpart(formula=Kyphosis~., data=kyphosis)
        trace2(trace,
            "the response is a factor but could not get the family of the %s model\n",
            class.as.char(object))
    } else {
        # e.g. glm(formula=sex~., family=binomial, data=etitanic)
        if(!is.null(dim(y)))  {  # data.frame or matrix
            levels <- levels(y[,1])
            y[,1] <- y[,1] != levels[1]
        } else {                 # vector
            levels <- levels(y)
            y <- y != levels[1]
            y <- data.frame(y)
        }
        # column naming helps us keep track that we did this manipulation of x
        colnames(y) <- if(length(levels) > 1) paste0("is", levels[2])
                                              else paste0("not", levels[1])
        trace2(trace, "generated indicator column \"%s\" from levels %s\n",
               colnames(y)[1], paste.trunc(levels))
    }
    y
}
possibly.convert.glm.two.column.response <- function(object, y, trace)
{
    if(is.nomial(object)) {
        # following are sanity checks
        # note also that here we treat a two column multinom model as a binom model
        stopifnot(NCOL(y) == 2)
        if(!is.numeric(y[,1]) || !is.numeric(y[,2]))
            warning0("non-numeric two column response for a binomial model")
        else if(any(y[,1] < 0) || any(y[,2] < 0))
            warning0("negative values in the two column response ",
                     "for a binomial model")
        # example 1 glm(formula=response~temp, family="binomial", data=orings)
        # example 2 glm(formula=cbind(damage,6-damage)~temp, family="bi...)
        org.colnames <- colnames(y)
        y <- bpairs.yfrac(y[,1:2], trace=(trace!=0))
        y <- data.frame(y)
        # column naming helps us keep track that we did this manipulation of x
        if(!is.null(org.colnames)) {
            colnames(y) <- # gsub deletes things like "[,2]"
                paste0(gsub("\\[.*\\]", "", org.colnames[1]), ".yfrac")
            trace2(trace,
                  "created column \"%s\" from two column binomial response\n",
                  colnames(y))
        }
    }
    y
}
get.and.check.subset <- function(x, object, trace)
{
    is.valid <- function(subset)
    {
        !is.null(subset) && (is.numeric(subset) || is.logical(subset))
    }
    #--- get.and.check.subset starts here
    subset <- object$subset
    if(is.valid(subset))
        msg <- "object$subset"
    else {
        subset <- try(eval(getCall(object)$subset, model.env(object)), silent=TRUE)
        if(is.try.err(subset))
            subset <- NULL
        else
            msg <- "getCall(object)$subset"
    }
    if(!is.valid(subset))
        subset <- NULL
    else {
        # duplicates are allowed in subsets so user can specify a bootstrap sample
        check.index(subset, "subset", x, allow.dups=TRUE, allow.zeros=TRUE)
        if(trace >= 2) {
            cat0("got subset from ", msg, " length " , length(subset))
            print_first_few_elements_of_vector(subset, trace)
        }
    }
    subset
}
