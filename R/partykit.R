# partykit.R: hackery for plotmo to support the partykit package

plotmo.prolog.party <- function(object, object.name, trace, ...) # called when plotmo starts
{
    check.mob.object(object)

    # Attach plotmo.importance (a character vector) to the model.

    object <- attach.party.plotmo.importance(object, trace)

    # Following is necessary because we will shortly change the class of the object
    # (and therefore getCall.party won't work, we must rely on getCall.default).
    # We need the call to get the data used to build the model.  (We can't use
    # object$data because that may contain "variable names" like "log(lstat)".)

    object$call <- getCall(object)

    # The meaning of "[[" is redefined for party objects i.e. the partykit
    # package defines "[[.party".  Since in the plotmo code we need [[ to do
    # things like object[["x"]], we change the class of the object here, so
    # [[ has its standard meaning for the object while we are in plotmo.

    trace2(trace,
        "changing class of %s from %s to \"party_plotmo\" for standard \"[[\"\n",
        object.name, quote.with.c(class(object)))

    original.class <- class(object) # save for plotmo.predict.party_plotmo
    class(object) <- "party_plotmo"
    object$original.class <- original.class
    object
}
plotmo.predict.party_plotmo <- function(object, newdata, type, ..., TRACE)
{
    stopifnot(is.character(object$original.class))
    class(object) <- object$original.class
    plotmo.predict(object, newdata, type=type,  ..., TRACE=TRACE)
}
# attach plotmo.importance (a character vector) to the model
attach.party.plotmo.importance <- function(object, trace)
{
    varimp <- try(varimp(object), silent=TRUE)
    if(is.try.err(varimp)) { # only some party objects support varimp
        # the variable(s) before the | in the formula
        varnames <- colnames(attr(object$info$terms$response, "factors"))
        # append variables actually used in the tree, in order of importance
        varnames <- c(varnames, names(varimp_party(object)))
    } else
        varnames <- names(sort(varimp, decreasing=TRUE))
    varnames.original <- varnames
    for(i in seq_along(varnames))
        varnames[i] <- naken(varnames[i]) # e.g. log(lstat) becomes lstat
    if(trace >= 1)
        cat("variable importance:", varnames, "\n")
    attr(object, "plotmo.importance") <- varnames
    object
}
# Like varimp.constparty but works for all party trees, including mob trees.
# Splits that affect more observations get more weight.
# Splits near the root get slightly more weight (lower depth).
# (This is to disambiguate vars that have equal importance otherwise.)
varimp_party <- function(object)
{
    init.varimp <- function(node, varimp, depth)
    {
        # update varimp for tree starting at node by walking the tree
        varid <- node$split$varid
        if(!is.null(varid)) {
            check.index(varid, "varid", varimp) # paranoia
            nobs <- if(!is.null(node$info$nobs)) node$info$nobs else 1
            varimp[varid] <- varimp[varid] + nobs - .0001 * depth
        }
        knodes <- partykit::kids_node(node)
        for(node in knodes)
            if(!is.null(node))
                varimp <- init.varimp(node, varimp, depth+1) # recurse
        varimp
    }
    #--- varimp_party starts here
    varnames <- colnames(object$data)
    varimp <- repl(0, length(varnames))
    names(varimp) <- varnames
    varimp <- init.varimp(object$node, varimp, depth=0)
    sort(varimp[varimp != 0], decreasing=TRUE) # discard vars not in tree, sort
}
plotmo.singles.party_plotmo <- function(object, x, nresponse, trace, all1, ...)
{
    all <- seq_along(colnames(x))
    if(all1)
        return(all)
    varnames <- attr(object, "plotmo.importance")
    stopifnot(!is.null(varnames))
    i <- match(varnames, colnames(x))
    ina <- which(is.na(i)) # sanity check
    if(length(ina)) {
        warnf(
"could not find \"%s\" in %s\nWorkaround: use all1=TRUE to plot all variables",
             varnames[ina[1]], quote.with.c(colnames(x)))
        i <- i[!is.na(i)]
    }
    if(length(i) == 0) {
        warnf("could not estimate variable importance")
        i <- seq_along(length(colnames(x))) # something went wrong, use all vars
    }
    # indices of important variables, max of 10 variables
    # (10 becauses plotmo.pairs returns 6, total is 16, therefore 4x4 grid)
    i[1: min(10, length(i))]
}
plotmo.pairs.party_plotmo <- function(object, x, nresponse, trace, all2, ...)
{
    singles <- plotmo.singles(object, x, nresponse, trace, all1=FALSE, ...)
    # choose npairs so a total of no more than 16 plots
    # npairs=5 gives 10 pairplots, npairs=4 gives 6 pairplots
    npairs <- if(length(singles) <= 6) 5 else 4
    form.pairs(singles[1: min(npairs, length(singles))])
}
# Check the mob object formula and issue a work-around message when
# the formula won't work for predictions with new data.
# This prevents err msg: 'newdata' had 1 row but variables found have 167 rows
check.mob.object <- function(object)
{
    call.fit <- getCall(object)$fit # was a fit func passed to the model building func?
    if(is.null(call.fit))
        return()
    # it's a mob object
    func <- eval(call.fit)
    stopifnot(inherits(func, "function"))
    func <- deparse(func, width.cutoff=500)
    # Is there a "(" followed by "~" followed by a lone "x," in the function body?
    # Or a "(" followed by "~" followed by "x - 1,".
    regex1 <- "\\(.*\\~.*[^a-zA-Z0-9_\\.]x,"
    regex2 <- "\\(.*\\~.*x \\- 1,"
    regex <- paste0(regex1, "|", regex2)
    grepl <- grepl(regex, func)
    if(any(grepl)) {
        # Issue the following message (details will vary depending on the fit func):
        #
        # The following formula in the mob fit function is not supported by plotmo:
        #
        #     glm(y ~ 0 + x, family = binomial, start = start, ...)
        #
        # Possible workaround: Replace the fit function with:
        #
        #     function (y, x, start = NULL, weights = NULL, offset = NULL, ...)
        #     {
        #         glm(as.formula(paste("y ~ ", paste(colnames(x)[-1], collapse="+"))),
        #            family = binomial, start = start, ...)
        #     }
        #
        # Error: The formula in the mob fit function is not supported by plotmo (see above)

        printf("\nThe following formula in the mob fit function is not supported by plotmo:\n\n")
        ifunc <- which(grepl)[1]
        cat(func[ifunc])
        regex <- "\\([^,]+,"
        func[ifunc] <- sub(regex,
"(as.formula(paste(\"y ~ \", paste(colnames(x)[-1], collapse=\"+\"))),\n            data=x,",
                          func[ifunc])
        printf("\n\nPossible workaround: Replace the fit function with:\n\n")
        printf("    %s <- ", as.character(call.fit))
        for(i in 1:length(func))
            printf("%s\n    ", func[i])
        printf("\n")
        stop0("The formula in the mob fit function is not supported by plotmo (see above)")
    }
}
# cforest objects
plotmo.prolog.parties <- function(object, object.name, trace, ...) # called when plotmo starts
{
    attr(object, "plotmo.importance") <-
        order.parties.vars.on.importance(object, trace) # a char vector
    object
}
order.parties.vars.on.importance <- function(object, trace) # a char vector
{
    varimp <- try(varimp(object), silent=TRUE)
    varnames <- if(is.try.err(varimp))
                    colnames(object$data)[-1] # -1 to drop response TODO is this reliable?
                else
                    names(sort(varimp, decreasing=TRUE))
    if(trace >= 1)
        cat("variable importance:", varnames, "\n")
    varnames
}
plotmo.singles.parties <- function(object, x, nresponse, trace, all1, ...)
{
    plotmo.singles.party_plotmo(object, x, nresponse, trace, all1, ...)
}
plotmo.pairs.parties <- function(object, x, nresponse, trace, all2, ...)
{
    plotmo.pairs.party_plotmo(object, x, nresponse, trace, all2, ...)
}
