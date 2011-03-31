# plotmo.rpart.R: plotmo methods for randomForest objects

get.plotmo.pairs.randomForest <- function(object, env, x, trace, ...)
{
    order.randomForest.vars.on.importance <- function(object, x)
    {
        importance <- object$importance
        if(!is.matrix(importance) ||           # sanity checks
                nrow(importance) == 0 ||
                !identical(row.names(importance), colnames(x))) {
            warning0("randomForest object has an invalid ",
                     "\"importance\" field, try all2=T")
            return(NULL)
        }
        # vector of var indices, most important vars first
        order(importance[,1], decreasing=TRUE)
    }
    if(is.null(object$forest))
        stop0("object has no \"forest\" component ",
              "(use keep.forest=TRUE in the call to randomForest)")
    importance <- order.randomForest.vars.on.importance(object, x)
    if(is.null(importance))
        return(NULL)
    # pairs of four most important variables
    form.pairs(importance[1: min(4, length(importance))])
}

