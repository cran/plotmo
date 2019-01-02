# mlr.R
#
# TODO WrappedModels need to save the call (and ideally the calling environment too).
#      Then we can work directly with the WrappedModel,
#      not with the learner.model.  Then predictions etc. are handled with
#      the mlr predict interface (more consistent for mlr users)
#
# TODO In documentation mention that NAs in model-building data will
#      often be a problem for plotmo
#
# TODO In documentation mention that plotres with prob models usually isn't helpful.
#
# TODO WrappedModels need a residuals() method? (using probabilities if available)

plotmo.prolog.WrappedModel <- function(object, object.name, trace, ...)
{
    object.name <- gsub("'", "", object.name) # remove begin and end quotes
    callers.name <- callers.name(n=3) # TODO this is fragile
    call <- getCall(object)
    if(is.null(call))
        stopf(
"getCall(%s) failed.\n       Possible  workaround: call %s like this: %s(%s$learner.model, ...)",
          object.name, callers.name, callers.name, object.name)

    # make x and y available for get.plotmo.x.default and get.plotmo.y.default

    # TODO This eval gets the object called "task" in the parent.frame.
    #      If that environment doesn't match the environment when the model
    #      was built, then we may get the wrong task object.
    task <- eval(call[["task"]])
    if(is.null(task))
        stop0("object call does not have a \"task\" field")
    stopifnot(inherits(task, "Task"))
    stopifnot.string(task$task.desc$id)
    trace2(trace,
           "task$task.desc$id for '%s' is \"%s\"\n",
            object.name, task$task.desc$id)
    data <- mlr::getTaskData(task)
    if(!inherits(data, "data.frame")) # sanity checks
        stop0("getTaskData(task) did not return a data.frame")

    stopifnot(!is.null(object[["subset"]]))
    subset <- object[["subset"]]
    stopifnot(NROW(subset) == object$task.desc$size)

    stopifnot(is.null(object[["x"]])) # check no pre-existing field x
    stopifnot(is.null(object[["y"]]))

    object$x <- get.xy.WrappedModel(data, object$features, subset,
                                    object.name, task$task.desc$id, trace)

    object$y <- get.xy.WrappedModel(data, task$task.desc$target, subset,
                                    object.name, task$task.desc$id, trace)

    # recursive call to plotmo.prolog to possibly update learner.model
    # (because for some models, plotmo.prolog adds var imp etc. fields to model)

    object <- plotmo.prolog_learner.model(object, object.name, trace, ...)

    object
}
get.xy.WrappedModel <- function(data, names, subset, object.name, task.desc.id, trace)
{
    # sanity checks
    check.index(names, index.name=deparse(substitute(names)), object=data,
                is.col.index=2) # exact match on column name
    check.index(index=subset, index.name="object$subset", object=data)

    x <- try(data[subset, names, drop=FALSE], silent=trace < 2)
    if(is.try.err(x))
        stopf("Could not get the original data from %s with %s",
                object.name, task.desc.id)
    x
}
get.learner.field <- function(object) # returns a string
{
    if(identical(class(object),
            c("ClassificationViaRegressionModel", "BaseWrapperModel", "WrappedModel")) ||
       identical(class(object),
            c("FilterModel", "ChainModel", "WrappedModel")) ||
       identical(class(object),
            c("FilterModel", "BaseWrapperModel", "WrappedModel")))

        "$learner.model$next.model$learner.model"

    else

        "$learner.model"
}
plotmo.prolog_learner.model <- function(object, object.name, trace, ...)
{
    learner.field <- get.learner.field(object)
    learner.model <- eval(parse(text=sprint("object%s", learner.field)))
    if(is.null(learner.model[["call"]])) # preempt error in try()
        trace2(trace, "%s object %s%s does not have a \"call\" field\n",
                      class(learner.model)[1], object.name, learner.field)
    else {
        learner.model <-
            try(plotmo.prolog(learner.model,
                              sprint("object%s", learner.field),
                              trace, ...),
                silent=trace < 0)

        if(!is.try.err(learner.model)) {
            # update the learner model
            # TODO these assignments are clumsy
            if(learner.field == "$learner.model")
                object$learner.model <- learner.model
            else if(learner.field == "$learner.model$next.model$learner.model")
                object$learner.model$next.model$learner.model <- learner.model
        } else
             trace0(trace, "plotmo.prolog(object%s) failed, continuing anyway\n",
                    learner.field)
        trace2(trace, "Done recursive call in plotmo.prolog for learner.model\n")
    }
    object
}
plotmo.predict.WrappedModel <- function(object, newdata, type, ..., TRACE)
{
    predict <- predict(object, newdata=newdata)$data
    stopifnot(is.data.frame(predict))
    predict
}
plotmo.singles.WrappedModel <- function(object, x, nresponse, trace, all1, ...)
{
    learner.field <- get.learner.field(object)
    learner.model <- eval(parse(text=sprint("object%s", learner.field)))
    singles <-
        try(plotmo.singles(learner.model, x, nresponse, trace, all1, ...),
            silent=trace < 2)
    is.err <- is.try.err(singles)
    trace2(trace, "plotmo.singles(object%s) %s\n", learner.field,
           if(is.err) "failed" else "succeeded")
    if(is.err)
        plotmo.singles.default(object, x, nresponse, trace, all1, ...)
    else
        singles
}
plotmo.pairs.WrappedModel <- function(object, x, nresponse, trace, all2, ...)
{
    learner.field <- get.learner.field(object)
    learner.model <- eval(parse(text=sprint("object%s", learner.field)))
    pairs <-
        try(plotmo.pairs(learner.model, x, nresponse, trace, all2, ...),
            silent=trace < 2)
    is.err <- is.try.err(pairs)
    trace2(trace, "plotmo.pairs(object%s) %s\n", learner.field,
           if(is.err) "failed" else "succeeded")
    if(is.err)
        plotmo.pairs.default(object, x, nresponse, trace, all2, ...)
    else
        pairs
}
