\name{plotmo.methods}
\title{Please ignore}
\alias{get.plotmo.nresponse}
\alias{get.plotmo.pairs}
\alias{get.plotmo.singles}
\alias{get.plotmo.type.wrapper}
\alias{get.plotmo.type}
\alias{get.plotmo.x.wrapper}
\alias{get.plotmo.x}
\alias{get.plotmo.y.wrapper}
\alias{get.plotmo.y_default}
\alias{get.plotmo.y}
\alias{get.and.check.subset}
\alias{check.index}
\description{Methods exported for use by \code{\link[earth]{earth}}.}
\usage{
get.plotmo.pairs(object, env, x, trace, all2)
get.plotmo.singles(object, env, x, trace, all1)
get.plotmo.type(object, type, func.name)
get.plotmo.type.wrapper(object, env, type, func.name="plotmo")
\method{get.plotmo}{x}(object, env, trace)
\method{get.plotmo}{x.wrapper}(object, env, trace)
get.plotmo.y(object, env, y.column, expected.len, trace)
get.plotmo.y_default(object, env, y.column, expected.len, trace)
get.plotmo.nresponse(object, nresponse, y)
get.and.check.subset(x, object, env, trace)
check.index(index, index.name, object, colnames,
    is.col.index, allow.empty, allow.zeroes,
    allow.negatives, allow.dups, treat.NA.as.one)

}
\arguments{
\item{object,all1,all2,env,expected.len,func.name}{See \code{plotmo.methods.R}.}

\item{nresponse,trace,type,x,y.column,y}{See \code{plotmo.methods.R}.}

\item{index, index.name, colnames,
is.col.index}{See \code{check.index.R}.}

\item{allow.empty, allow.zeroes, allow.negatives, allow.dups,
treat.NA.as.one}{See \code{check.index.R}.}
}
