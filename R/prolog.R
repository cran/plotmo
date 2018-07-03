# prolog.R: plotmo.prolog functions, called at the start of plotmo and plotres

# gets called at the start of plotmo and plotres
plotmo.prolog <- function(object, object.name, trace, ...)
{
    trace2(trace, "--plotmo_prolog for %s object %s\n",
           class(object)[1], object.name)
    UseMethod("plotmo.prolog")
}
plotmo.prolog.default <- function(object, object.name, ...)
{
    # prevent confusing downstream errors by doing an initial check here
    if(is.null(getCall(object)) && is.null(object[["x"]]))
        stopf("%s does not have a 'call' field or %s",
              object.name,
              if(is.null(object[["y"]])) "'x' and 'y' fields"
              else                       "an 'x' field")
    object
}
