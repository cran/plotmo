# bx.R: plotres functions for accessing a model's basis matrix

# TODO turn this into a method function

plotmo_bx <- function(object, trace, msg, versus)
{
    if(inherits(object, "mars") || inherits(object, "earth")) {
        if(inherits(object, "mars"))
            bx <- object[["x"]]
        else
            bx <- object[["bx"]]
        if(is.null(bx) || NCOL(bx) == 0)
            stopf("versus=\"b:\": no basis matrix for this %s object",
                  class.as.char(object, quotify=TRUE))
        else if(NCOL(bx) == 1) { # intercept only?
            bx <- bx
            icolumns <- 1
        } else {
            bx <- bx[, -1, drop=FALSE] # drop the intercept
            if(is.null(colnames(bx))) # mars model?
                colnames(bx) <- paste0("bx", seq_len(NCOL(bx)))
            icolumns <- check.index(versus, "versus", seq_len(NCOL(bx)), colnames=colnames(bx))
        }
    } else if(inherits(object, "Gam") || # package gam version 1.15 or higher
              # the additive.predictors check below is to ensure mda:gam (not mgcv:gam)
              # (applies only to package gam version less than 1.15)
              (inherits(object, "gam") && !is.null(object[["additive.predictors"]]))) {
        bx <- model.matrix(object)
        if(is.null(bx) || NCOL(bx) == 0)
            stopf("versus=\"b:\": model.matrix(object) for this %s object returned NULL",
                  class.as.char(object, quotify=TRUE))
        else if(NCOL(bx) == 1) { # intercept only?
            bx <- bx
            icolumns <- 1
        } else {
            bx <- bx[, -1, drop=FALSE] # drop the intercept
            icolumns <- check.index(versus, "versus", seq_len(NCOL(bx)), colnames=colnames(bx))
        }
    } else
        stopf("versus=\"b:\" is not supported for this %s object",
              class.as.char(object, quotify=TRUE))
    list(bx=bx, icolumns=icolumns)
}
