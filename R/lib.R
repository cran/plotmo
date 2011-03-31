# lib.R: miscellaneous definitions for plotmo

stop0 <- function(...)
    stop(..., call.=FALSE)

warning0 <- function(...) # set options(warn=2) and traceback() to see the call
    warning(..., call.=FALSE)

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

# We use identical() and not is.na() below because is.na(x) gives warnings
# for certain x's, e.g if x is a function, and x == 0 gives warnings if x
# is a vector or a function etc.

is.na.or.zero <- function(x) identical(x, NA) || identical(x, 0)

stopifnot.boolean <- function(b) # b==0 or b==1 is also ok
{
    if(length(b) != 1 || !(is.logical(b) || is.numeric(b)) ||
            is.na(b) || !(b == 0 || b == 1)) {
        name <- deparse(substitute(b))
        cat0("\n", name, ": ")
        print(b)
        cat("\n")
        stop0("the ", name,
              " argument is not FALSE or TRUE or 0 or 1 (see above print)")
    }
}
check.trace.arg <- function(trace) # make sure trace is a one element vector
{
    if(!is.vector(trace))
        warning0("bad \"trace\" argument")
    if(length(trace) != 1)
        warning0("\"trace\" argument has more than one element")
    as.numeric(trace[1])
}
# warn.if.not.all.finite is intended to help clarify possibly confusing
# messages from within the bowels of calls to other functions later
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

check.index.vec <- function(index.name, indexVec, object,
                        check.empty = FALSE, use.as.col.index=FALSE,
                        allow.negative.indices = TRUE,
                        allow.duplicates = FALSE,
                        allow.zeroes = FALSE, treat.NA.as.one=FALSE)
{
    if(is.null(indexVec)) {
        if(check.empty)
            stop0("\"", index.name, "\" is NULL and cannot be used as an index vector")
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

    if(is.logical(indexVec)) {
        if(check.empty) {
            if(length(indexVec) == 0)
                stop0("length(", index.name, ") == 0")
            if(length(indexVec[indexVec == TRUE]) == 0)
                stop0("\"", index.name, "\" is all FALSE")
        }
        # note that a single FALSE or TRUE is ok regardless of length(object)
        if(length(indexVec) > len && length(indexVec) != 1) {
            stop0("logical index vector \"", index.name, "\" is too long\n",
                "       Its length is ", length(indexVec),
                " and the max allowed length is ", len)
        }
    } else if(is.numeric(indexVec)) {
        if(check.empty) {
            if(length(indexVec) == 0)
                stop0("length(", index.name, ") == 0")
            else if(all(indexVec == 0))
                if(length(indexVec) == 1)
                    stop0("\"", index.name, "\" is 0")
                else
                    stop0("\"", index.name, "\" is all zeroes")
        }
        if(any(floor(indexVec) != indexVec))
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
                    "\" (the only legal value is NA or 1)")
            else
                stop0("out of range value in \"", index.name,
                    "\" (the only legal value is 1)")
        }
    } else
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
# Example of my.print.call:
#
# Call: earth(formula=O3~., data=ozone1, trace=4, linpreds=c(3,
#       8), degree=2)
#
# Note that the 2nd line is horizontally aligned with the first.

my.print.call <- function(msg, Call)
{
    # don't print x or y if they are too long
    # TODO there must be a better way of checking length
    if(!is.null(Call$x)) {
        x. <- Call$x
        if(length(paste(substitute(x.))) > 100)
            Call$x <- paste0("[", NROW(Call$x), ",", NCOL(Call$x),
                             "]-too-long-to-display")
    }
    if(!is.null(Call$y)) {
        y. <- Call$y
        if(length(paste(substitute(y.))) > 100)
            Call$y <- paste0("[", NROW(Call$y), ",", NCOL(Call$y),
                             "]-too-long-to-display")
    }
    Call$na.action <- NULL # don't want to print the na.action
    s <- format(Call)
    if(length(s) > 8) {
        s <- s[1:8]
        s[8] <- paste(s[8], "\netc.")
    }
    s <- gsub("[ \t\n]", "", s)                 # remove white space

    # add newlines and prefix (spaces prefix all lines except the first)
    spaces. <- sprintf("%*s", nchar(msg), " ")   # nchar spaces

    s <- gsub(",", ", ", s)                     # replace comma with comma space
    s <- paste(s, collapse=paste("\n", spaces., sep=""), sep="")
    cat0(msg, s, "\n")
}
match.arg1 <- function(arg)     # match.arg1 returns an integer
{
    formal.args <- formals(sys.function(sys.parent()))
    arg.name=deparse(substitute(arg))
    match.choices(arg, choices=eval(formal.args[[arg.name]]),  arg.name=arg.name)
}
match.choices <- function(arg, choices, arg.name)   # choices is a vector of strings
{
    stopifnot(is.character(arg))
    if(all(arg == choices))
        return(1)
    i <- pmatch(arg, choices)
    if(any(is.na(i)))
        stop0(paste0("bad \"", arg.name, "\" argument \"", arg, "\"\n",
              "Choose one of: ", paste.quoted.names(choices)))
    if(i == 0)
        stop0(paste("the \"", arg.name, "\" argument is ambiguous\n",
              "Choose one of: ", paste.quoted.names(choices)))
    i
}
# Call this only after a plot is on the screen to avoid
# an error message "plot.new has not been called yet"
# TODO the trimming code sometimes overtrims

show.caption <- function(caption, trim=0, show=TRUE, cex=1)
{
    if(!is.null(caption) && (len.caption <- nchar(caption))) {
        if(trim) {
            if(is.logical(trim))
                trim <- 1
            # trim caption to fit
            len <- len.caption * trim / strwidth(caption, "figure")
            caption <- substr(caption, 1, len)
            # append ellipsis if chars deleted
            if(len < len.caption)
                caption <- paste0(caption, "...")
        }
        if(show)
            mtext(caption, outer=TRUE, font=2, line=1.5, cex=cex)
    }
    caption
}
# the make.space functions should only be called if do.par is TRUE
# (otherwise par is not restored correctly)

make.space.for.caption <- function(caption)
{
    oma <- par("oma")
    if(nchar(caption) > 0 && oma[3] < 3) {
        oma[3] <- 3
        par(oma=oma)
    }
}
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
