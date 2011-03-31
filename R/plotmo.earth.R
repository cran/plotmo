# plotmo.rpart.R: plotmo methods for earth objects

get.plotmo.singles.earth <- function(object, env, x, trace, all1)
{
    if(length(coef(object)) == 1)
        warning0("the earth model appears to be an intercept only model")
    if(all1) {
        # user wants all used predictors, not just those appearing in additive terms
        used.vars <- NULL
        selected <- object$selected.terms[earth:::reorder.earth(object, decomp="anova")]
        if(length(selected) > 0)
            used.vars <- unique(
                        which(object$dirs[selected, , drop=FALSE] != 0, arr.ind=TRUE)[,2])
        return(used.vars)
    }
    dataClasses <- attr(object$terms, "dataClasses")
    if(any((dataClasses == "factor") | (dataClasses == "ordered"))) {
        # NOV 2008: new code, only use if factors in x
        # TODO this can give extra predictors if variable names alias
        #      e.g. "x" and "x1" are both variable names
        used.colnames <- apply(object$dirs, 2, any1)
        colnames <- colnames(object$dirs)[used.colnames]
        used.preds <- NULL
        for(ipred in seq_along(object$namesx.org)) {
            if(is.factor(x[,ipred])) {
                # This knows how to deal with expanded factor names because
                # it e.g. looks for "^pclass" in "pclass3rd"
                if(length(grep(paste0("^", object$namesx.org[ipred]), colnames)) > 0)
                    used.preds <- c(used.preds, ipred)
            } else {
                # exact match
                if(length(grep(paste0("^", object$namesx.org[ipred], "$"), colnames)) > 0)
                    used.preds <- c(used.preds, ipred)
                used.preds <- c(used.preds, ipred)
            }
        }
        singles <- used.preds
    } else {
        # original code, use if no factors in x
        singles <- NULL
        selected <- object$selected.terms[  # selected is all degree 1 terms
                        earth:::reorder.earth(object, decomp="anova", degree=1, min.degree=1)]
        if(length(selected) > 0)
            singles <- which(object$dirs[selected, , drop=FALSE] != 0, arr.ind=TRUE)[,2]
    }
    singles
}
get.plotmo.pairs.earth <- function(object, env, x, trace, ...)
{
    pairs <- matrix(0, nrow=0, ncol=2)      # no pairs
    selected <- object$selected.terms[      # selected is all degree 2 terms
                    earth:::reorder.earth(object, decomp="anova", degree=2, min.degree=2)]
    pairs <- vector(mode="numeric")
    for(i in selected)                      # append indices of the two preds in term i
        pairs <- c(pairs, which(object$dirs[i,] != 0))
    pairs <- unique(matrix(pairs, ncol=2, byrow=TRUE))
    if(nrow(pairs) > 0 && any(sapply(x, is.factor))) { # any columns in x are factors?
        # pairs works off expanded factor names, so replace each name
        # with index of original variable name
        # TODO this can give wrong results if variable names alias
        #      e.g. if "x" and "x1" are both variable names this takes the LAST
        #      of the matching names so correct with "x" "x1" but not "x1" "x"
        dir.colnames <- colnames(object$dirs)
        prednames <- object$namesx.org
        prednames.hat <- paste0("^", prednames)
        for(i in 1:nrow(pairs))
            for(j in 1:2) {
                ipred1 <- 0
                for(ipred in seq_along(prednames.hat))
                    if(length(grep(prednames.hat[ipred], dir.colnames[pairs[i, j]])) > 0)
                        ipred1 <- ipred
                if(ipred1 == 0)
                    stop0("internal error: illegal ipred1 in get.plotmo.pairs.earth")
                pairs[i, j] <- ipred1
            }
    }
    pairs
}
get.plotmo.pairs.bagEarth <- function(object, env, pred.names, x, trace, all2)
{
    pairs <- matrix(0, nrow=0, ncol=2)
    for(i in 1:length(object$fit))
        pairs <- rbind(pairs,
                       get.plotmo.pairs.earth(object$fit[[i]], env, x, trace))
    pairs[order(pairs[,1], pairs[,2]),]
}
