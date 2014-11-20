\name{plotmo.methods}
\title{Please ignore}
\alias{get.plotmo.pairs}
\alias{get.plotmo.singles}
\alias{get.plotmo.type}
\alias{get.plotmo.type.wrapper}
\alias{get.plotmo.y}
\alias{get.plotmo.y_default}

\description{Methods exported for use by \code{\link[earth]{earth}}.}
\usage{
get.plotmo.pairs(object, env, x, trace, all2)
get.plotmo.singles(object, env, x, trace, all1)
get.plotmo.type(object, type, func.name)
get.plotmo.type.wrapper(object, env, type, func.name="plotmo")
get.plotmo.y(object, env, y.column, expected.len, trace)
get.plotmo.y_default(object, env, y.column, expected.len, trace)
}
\arguments{
\item{all1,all2,env,expected.len,func.name}{See \code{plotmo.methods.R}.}
\item{object,trace,type,x,y.column}{See \code{plotmo.methods.R}.}
}
