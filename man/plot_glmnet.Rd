\name{plot_glmnet}
\alias{plot_glmnet}
\title{Plot a glmnet model}
\description{
Plot the coefficient paths of a \code{\link[glmnet]{glmnet}} model.

An enhanced version of \code{\link[glmnet]{plot.glmnet}}.
}
\usage{
plot_glmnet(x = stop("no 'x' argument"),
           xvar = c("rlambda", "lambda", "norm", "dev"),
           label = 10, nresponse = NA, grid.col = NA, s = NA, ...)
}
\arguments{
\item{x}{
The \code{glmnet} model.
}
\item{xvar}{
What gets plotted along the x axis. One of:\cr
\bold{\code{"rlambda"}} (default) decreasing log lambda (lambda is the glmnet penalty)\cr
\bold{\code{"lambda"}} log lambda\cr
\bold{\code{"norm"}} L1-norm of the coefficients\cr
\bold{\code{"dev"}} percent deviance explained\cr\cr
The default \code{xvar} differs from \code{plot.glmnet} to allow
\code{s} to be plotted when this function is invoked by
\code{\link{plotres}}.
}
\item{label}{
Default \code{10}.
Number of variable names displayed on the right of the plot.
One of:\cr
\bold{\code{FALSE}} display no variables\cr
\bold{\code{TRUE}} display all variables\cr
\bold{\code{integer}} (default) number of variables to display (default is 10)\cr
}
\item{nresponse}{
Which response to plot for multiple response models.
}
\item{grid.col}{
Default \code{NA}.
Color of the optional grid, for example \code{grid.col="lightgray"}.
}
\item{s}{
For use by \code{\link{plotres}}.
The x position of the gray vertical line indicating the lambda
\code{s} passed by \code{plotres} to \code{predict.glmnet} to
calculate the residuals.
Plotres defaults to \code{s=0}.
}
\item{\dots}{
Dot arguments are passed internally to
\code{\link[graphics]{matplot}}.

Use \code{col} to change the color of curves; for example \code{col=1:4}.
The six default colors are intended to be distinguishable yet
harmonious (to my eye at least), with adjacent colors as different as
easily possible.
}
}
\note{
\bold{Limitations}

For multiple response models use the \code{nresponse} argument to
specify which response should be plotted.
(Currently each response must be plotted one by one.)

The \code{type.coef} argument of \code{\link[glmnet]{plot.glmnet}} is
currently not supported.

Currently \code{xvar="norm"} is not supported for multiple
response models (you will get an error message).

\bold{Interaction with \code{plotres}}

When invoking this function via \code{\link{plotres}}, prefix any
argument of \code{plotres} with \code{w1.} to tell \code{plotres} to
pass the argument to this function.
For example give \code{w1.col=1:4} to \code{plotres} (plain
\code{col=1:4} in this context gets passed to the residual plots).

\bold{Acknowledgments}

This function is based on \code{\link[glmnet]{plot.glmnet}} in the
\code{\link[glmnet]{glmnet}} package authored by Jerome Friedman,
Trevor Hastie, and Rob Tibshirani.
}
\seealso{
Chapter 6 in \href{../doc/plotres-notes.pdf}{plotres vignette} discusses
this function.
}
\examples{
if (require(glmnet)) {
    x <- matrix(rnorm(100 * 10), 100, 10)   # n=100 p=10
    y <- x[,1] + x[,2] + 2 * rnorm(100)     # y depends only on x[,1] and x[,2]
    mod <- glmnet(x, y)

    plot_glmnet(mod)

    # plotres(mod)                          # plot the residuals
}
}
