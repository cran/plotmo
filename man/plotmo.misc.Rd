\name{plotmo.misc}
\alias{check.index}
\alias{get.plotmo.pairs}
\alias{get.plotmo.singles}
\alias{get.plotmo.type.wrapper}
\alias{get.plotmo.x.wrapper}
\alias{get.plotmo.y.default}
\alias{get.plotmo.y.wrapper}
\alias{get.plotmo.y}
\alias{plotmo.convert.na.nresponse}
\alias{plotmo.pairs.default}
\alias{plotmo.pairs}
\alias{plotmo.pint}
\alias{plotmo.predict}
\alias{plotmo.prolog}
\alias{plotmo.residtype}
\alias{plotmo.singles.default}
\alias{plotmo.singles}
\alias{plotmo.type}
\alias{plotmo.x}
\alias{plotmo.y.default}
\alias{plotmo.y}
\alias{plotmo_cum}
\alias{plotmo_fitted}
\alias{plotmo_nresponse}
\alias{plotmo_predict}
\alias{plotmo_prolog}
\alias{plotmo_resplevs}
\alias{plotmo_response}
\alias{plotmo_rinfo}
\alias{plotmo_rsq}
\alias{plotmo_standardizescale}
\alias{plotmo_type}
\alias{plotmo_y}
\title{Ignore}
\description{
Miscellaneous functions exported for internal use by \code{earth}
and other packages.
You can ignore these.
}
\usage{
# for earth
plotmo_fitted(object, trace, nresponse, type, ...)
plotmo_cum(rinfo, info, nfigs=1, add=FALSE,
           cum.col1, grid.col, jitter=0, cum.grid="percentages", ...)
plotmo_nresponse(y, object, nresponse, trace, fname, type="response")
plotmo_rinfo(object, type=NULL, residtype=type, nresponse=1,
    standardize=FALSE, delever=FALSE, trace=0,
    leverage.msg="returned as NA", expected.levs=NULL, labels.id=NULL, ...)
plotmo_predict(object, newdata, nresponse,
    type, expected.levs, trace, inverse.func=NULL, ...)
plotmo_prolog(object, object.name, trace, ...)
plotmo_resplevs(object, plotmo_fitted, yfull, trace)
plotmo_rsq(object, newdata, trace=0, nresponse=NA, type=NULL, ...)
plotmo_standardizescale(object)
plotmo_type(object, trace, fname="plotmo", type, ...)
plotmo_y(object, nresponse=NULL, trace=0, expected.len=NULL,
    resp.levs=NULL, convert.glm.response=!is.null(nresponse))
\method{plotmo.pairs}{default}(object, x, nresponse, trace, all2)
\method{plotmo.singles}{default}(object, x, nresponse, trace, all1)
\method{plotmo.y}{default}(object, trace, naked, expected.len)
# plotmo methods
plotmo.convert.na.nresponse(object, nresponse, yhat, type="response")
plotmo.pairs(object, x, nresponse, trace, all2)
plotmo.pint(object, newdata, type, level, trace)
plotmo.predict(object, newdata, type, ..., TRACE)
plotmo.prolog(object, object.name, trace, ...)
plotmo.residtype(object, ...)
plotmo.singles(object, x, nresponse, trace, all1)
plotmo.type(object, ...)
plotmo.x(object, trace)
plotmo.y(object, trace, naked, expected.len)
}
\arguments{
\item{\dots}{-}
\item{add}{-}
\item{all1}{-}
\item{all2}{-}
\item{convert.glm.response}{-}
\item{cum.col1}{-}
\item{cum.grid}{-}
\item{delever}{-}
\item{expected.len}{-}
\item{expected.levs}{-}
\item{fname}{-}
\item{grid.col}{-}
\item{info}{-}
\item{inverse.func}{-}
\item{jitter}{-}
\item{labels.id}{-}
\item{level}{-}
\item{leverage.msg}{-}
\item{naked}{-}
\item{newdata}{-}
\item{nfigs}{-}
\item{nresponse}{-}
\item{object.name}{-}
\item{object}{-}
\item{plotmo_fitted}{-}
\item{residtype}{-}
\item{resp.levs}{-}
\item{rinfo}{-}
\item{standardize}{-}
\item{TRACE}{-}
\item{trace}{-}
\item{type}{-}
\item{x}{-}
\item{yfull}{-}
\item{yhat}{-}
\item{y}{-}
}
