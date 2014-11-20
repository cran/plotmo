# lib.R: miscellaneous function for plotmo

stop0 <- function(...) stop(..., call.=FALSE)

# set options(warn=2) and traceback() to see where the warning occurred
warning0 <- function(...) warning(..., call.=FALSE)

# The function stopif is intended for catching programmer errors.
# For user errors, we try to give a more informative message.

stopif <- function(...) stopifnot(!(...))

printf <- function(format, ...) cat(sprintf(format, ...)) # like c printf

paste0 <- function(...) paste(..., sep="") # paste with no added spaces

cat0 <- function(...) cat(..., sep="") # cat with no added spaces

paste.quoted.names <- function(names) # add quotes and comma seperators
    paste0("\"", paste(names, collapse="\" \""), "\"")

printf <- function(format, ...) cat(sprintf(format, ...)) # like c printf

strip.white.space <- function(s) gsub("[ \t\n]", "", s)

any1 <- function(x) any(x != 0) # like any but no warning if x not logical

is.try.error <- function(obj) class(obj)[1] == "try-error"

is.zero <- function(x) length(x) == 1 && x == 0

is.integral <- function(x) all(floor(x) == x)

stopifnot.boolean <- function(b) # b==0 or b==1 is also ok
{
    if(length(b) != 1 || !(is.logical(b) || is.numeric(b)) ||
            is.na(b) || !(b == 0 || b == 1))
        stop0("the ", deparse(substitute(b)),
              " argument is not FALSE or TRUE or 0 or 1")
}
stopifnot.integer <- function(i, logical.acceptable=TRUE)
{
    if(!(is.numeric(i) || (logical.acceptable && is.logical(i))) ||
            length(i) != 1 || is.na(i) || !is.finite(i) || floor(i) != i)
        stop0("the ", deparse(substitute(i)), " argument is not an integer")
}
stopifnot.scalar <- function(x, logical.acceptable=TRUE)
{
    if(!(is.numeric(x) || (logical.acceptable && is.logical(x))) ||
             is.na(x) || !is.finite(x) || length(x) != 1)
        stop0("the ", deparse(substitute(x)), " argument is not scalar")
}
my.center <- function(x, trace=FALSE)
{
    if(!is.null(x) && !is.factor(x)) {
        if(trace > 0)
            name <- deparse(substitute(x))
        x <- x - mean(x[is.finite(x)], na.rm=TRUE)
        if(trace > 0) {
            name <- paste0("centered ", name)
            cat(name, "length ", length(x))
            print.first.few.elements.of.vector(x, trace, name)
        }
    }
    x
}
# warn.if.not.all.finite helps preempt confusing message from code later.
# Return TRUE if warning issued.

warn.if.not.all.finite <- function(x, text="unknown")
{
    is.factors <- sapply(x, is.factor)
    if(any(is.factors)) {
        if(NCOL(x) == 1 || all(is.factors)) # TODO suspect
            return(FALSE)
        x <- x[, !is.factors]  # remove factor columns before is.finite check
    }
    if(any(sapply(x, is.na))) {
        warning0("NA in ", text)
        return(TRUE)
    }
    if(!all(sapply(x, is.finite))) {
        warning0("non finite value in ", text)
        return(TRUE)
    }
    FALSE
}
# Check that an index vector specified by the user is ok to index an object.
# We want to preclude confusing R messages or behaviour later.
# An example is when max(indexVec) > len(object) which quietly returns NA
# and can cause confusing downstream behaviour.
# This returns a vector suitable for indexing into object (will be identical
# to indexVec unless indexVec is a character vector).

check.index.vec <- function(index.name, indexVec, object, colnames=NULL,
                        check.empty = FALSE, use.as.col.index=FALSE,
                        allow.negative.indices = TRUE,
                        allow.duplicates = FALSE,
                        allow.zeroes = FALSE, treat.NA.as.one=FALSE)
{
    check.logical.index.vec <- function()
    {
        if(check.empty) {
            if(length(indexVec) == 0)
                stop0("length(", index.name, ") == 0")
            if(length(indexVec[indexVec == TRUE]) == 0)
                stop0("\"", index.name, "\" is all FALSE")
        }
        # note that a single FALSE or TRUE is ok regardless of length(object)
        if(length(indexVec) > len && length(indexVec) != 1) {
            stop0("logical index vector \"", index.name, "\" is too long.\n",
                  "       Its length is ", length(indexVec),
                  " and the max allowed length is ", len)
        }
        indexVec
    }
    check.numeric.index.vec <- function()
    {
        if(check.empty) {
            if(length(indexVec) == 0)
                stop0("length(", index.name, ") == 0")
            else if(all(indexVec == 0))
                if(length(indexVec) == 1)
                    stop0("\"", index.name, "\" is 0")
                else
                    stop0("\"", index.name, "\" is all zeroes")
        }
        if(!is.integral(indexVec))
            stop0(index.name, " is not an integer")
        if(any(indexVec < 0) && any(indexVec > 0))
            stop0("mixed negative and positive values in \"", index.name, "\"")
        if(!allow.zeroes && any(indexVec == 0) && length(indexVec) != 1)
            warning0("zero in \"", index.name, "\"")
        if(!allow.duplicates && any(duplicated(indexVec)))
            warning0("duplicates in \"", index.name, "\"")
        if(!allow.negative.indices && any(indexVec < 0))
            stop0("negative value in \"", index.name, "\"")
        if(any(abs(indexVec) > len)) {
            if(len != 1)
                stop0("out of range value in \"", index.name,
                      "\" (allowed index range is 1:",  len, ")")
            else if(treat.NA.as.one)
                stop0("out of range value in \"", index.name,
                      "\", the only legal value is 1 (or NA)")
            else
                stop0("out of range value in \"", index.name,
                      "\" (the only legal value is 1)")
        }
        indexVec
    }
    check.character.index.vec <- function()
    {
        # like match but return multiple matches if present
        matchm <- function(x, tab)
        {
             matches <- integer(0)
             for(ix in seq_along(x)) {
                this.x <- x[ix]
                for(itab in 1:length(tab))
                    if(this.x == tab[itab])
                        matches <- c(matches, itab)
             }
             matches
        }
        if(length(colnames) == 0 || !is.character(colnames))
            stop0(index.name,
                  " specifies variable names but variable names are unavailable")
        index <- NULL
        for(name in indexVec) {
            i <- grep(name, colnames)
            if(length(i) == 0)
                warning0("\"", name, "\" in ", index.name,
                         " does not match any variables")
            else
                index <- c(index, i)
        }
        if(length(dim(object)) == 0)            # vector?
            indexVec <- matchm(index, object)
        else if(length(dim(object)) == 2)       # matrix?
            indexVec <- c(matchm(index, object[,1]),
                          matchm(index, object[,2]))
        else
            stop("internal error")              # not yet supported
        unique(indexVec[!is.na(indexVec)])
    }
    #--- check.index.vec starts here
    if(is.null(indexVec)) {
        if(check.empty)
           stop0("\"", index.name,
                 "\" is NULL and cannot be used as an index vector")
        return(NULL)
    }
    if(any(is.na(indexVec)))
        stop0("NA in \"", index.name, "\"")
    if(treat.NA.as.one && (is.na(indexVec)[1] && length(indexVec) == 1))
        indexVec <- 1
    if(!(NROW(indexVec) == 1 || NCOL(indexVec) == 1))
        stop0("\"", index.name, "\" must be a vector not a matrix ",
              "(\"", index.name, "\" has dimensions ",
              NROW(indexVec), " x ", NCOL(indexVec), ")")

    if(use.as.col.index)
        len <- NCOL(object)         # index is for cols of object
    else if(is.vector(object))
        len <- length(object)
    else
        len <- NROW(object)         # index is for rows of object

    if(is.logical(indexVec))
        indexVec <- check.logical.index.vec()
    else if(is.numeric(indexVec))
        indexVec <- check.numeric.index.vec()
    else if(is.character(indexVec))
        indexVec <- check.character.index.vec()
    else
        warning0("index vector \"", index.name,
            "\" has an unusual class \"", class(indexVec)[1], "\"")
    indexVec
}
exists.and.not.null <- function(object, mode="any", argname="")
{
    # following "if" is like is.null(object) but no error msg if object doesn't exist

    if(paste0("'", object, "'") == "'NULL'")
        return(FALSE)

    if(paste0("'", object, "'") == "'NA'")
        if(length(argname))
            stop0(argname, "=NA")
        else
            stop0(object, "illegal NA")

    # TODO removed until I can get this to work reliably
    #
    #   if(!exists(paste0("'", object, "'"), where=parent.frame(), mode=mode))
    #       if(length(argname))
    #           stop0("you specified ", argname, "=", object,
    #               " but there is no such ", if(mode=="any") "object" else mode)
    #       else
    #           stop0(object, ": no such", if(mode=="any") "object" else mode)

    return(TRUE)
}
print.object.call <- function(object)
{
    if(is.null(object$call))
        cat("object$call is null\n\n")
    else
        cat("object$call:",
            paste0(deparse(object$call), collapse=""), "\n\n")
}
print.first.few.elements.of.vector <- function(x, trace, name=NULL)
{
    try(cat(" min", min(x), "max", max(x)), silent=TRUE)
    spaces <- "               "
    if(!is.null(name))
        spaces <- sprintf("%*s", nchar(name), " ")  # nchar spaces
    cat0("\n", spaces, " value ")
    len <- if(trace >= 3) length(x) else min(10, length(x))
    for(i in 1:len)
        cat(x[i], "")
    if(length(x) > len)
        cat("...")
   cat("\n")
    if(trace >= 3) {
        cat("\n")
        print(summary(x))
    }
}
# print first few rows and last row of x
# if trace >= 3, then print all rows of x, up to 1e4 rows

print.first.few.rows <- function(x, trace, msg="x", msg2="")
{
    stopifnot(length(dim(x)) == 2)
    cat0(msg, "[", nrow(x), ",", ncol(x), "]", msg2, ":\n")
    if(is.null(colnames(x)))
        colnames(x) <- paste("V", 1:ncol(x))
    xnew <- x
    nprint <- if(trace >= 3) 1000 else 5
    if(nprint < nrow(x)) {
        xnew <- x[c(1:(nprint-1), nrow(x)), , drop=FALSE]
        if(is.null(rownames(xnew)))
            rownames(xnew) <- c(1:(nprint-1), nrow(x))
        rownames(xnew)[nprint-2+1] <- "..."
    }
    print(xnew)
    is.fac <- sapply(x, is.factor)
    if(any(is.fac)) {
        names <- paste0(colnames(x),
                        ifelse(sapply(x, is.ordered), "(ordered)", ""))
        cat("\nfactors:", strip.white.space(names[is.fac]), "\n")
    }
    if(trace >= 3) {
        cat("\n")
        print(summary(x))
    }
    cat("\n")
}
match.arg1 <- function(arg)     # match.arg1 returns an integer
{
    formal.args <- formals(sys.function(sys.parent()))
    arg.name=deparse(substitute(arg))
    match.choices(arg[1], choices=eval(formal.args[[arg.name]]),  arg.name=arg.name)
}
match.choices <- function(arg, choices, arg.name) # choices is a vector of strings
{
    if(!is.character(arg) || length(arg) != 1)
         stop0("bad \"", arg.name, "\" argument.\n",
               "Choose one of: ", paste.quoted.names(choices))
    imatch <- pmatch(arg, choices)
    if(any(is.na(imatch))) {
        imatch <- NULL
        for(i in 1:length(choices))
            if(pmatch(arg, choices[i], nomatch=0))
                imatch <- c(i, imatch)
        if(length(imatch) == 0)
            stop0(arg.name, "=\"", arg, "\" is illegal.\n",
                  "Choose one of: ", paste.quoted.names(choices))
        if(length(imatch) > 1)
            stop0(paste0(arg.name, "=\"", arg, "\" is ambiguous.\n",
                         "Choose one of: ", paste.quoted.names(choices)))
    }
    imatch
}
# Call this only after a plot is on the screen to avoid
# an error message "plot.new has not been called yet"

show.caption <- function(caption, trim=TRUE, show=TRUE, cex=1)
{
    if(!is.null(caption) && nchar(caption)) {
        if(trim) {
            # trim each line of caption to fit
            caption <- strsplit(caption, "\n")[[1]]
            for(i in seq_along(caption)) {
                nchar.org <- nchar(caption[i])
                caption[i] <- substr(caption[i], 1, 60)
                if(nchar(caption[i]) < nchar.org) # append ellipsis if chars deleted
                    caption[i] <- paste0(caption[i], " ...")
            }
            caption <- paste(caption, collapse="\n")
        }
        if(show)
            mtext(caption, outer=TRUE, font=2, line=1, cex=cex)
    }
    caption
}
# the make.space functions should only be called if do.par is TRUE
# (otherwise par is not restored correctly)

make.space.for.bottom.axis <- function()
{
    mar <- par("mar")
    if(mar[1] < 3) {
        mar[1] <- 3
        par(mar=mar)
    }
}
make.space.for.left.axis <- function()
{
    mar <- par("mar")
    if(mar[2] < 3) {
        mar[2] <- 3
        par(mar=mar)
    }
}
get.cex.points <- function(npoints, len)
{
    n <- if(npoints <= 0) len else min(npoints, len)

    cex.points <-
        if     (n >= 5000) .2
        else if(n >= 3000) .4
        else if(n >= 1000) .6
        else if(n >= 300)  .8
        else               1

    cex.points / par("cex")
}
