\name{plot_gbm}
\alias{plot_gbm}
\title{Plot a gbm model}
\description{
Plot a \code{\link[gbm]{gbm}} model showing the training and other
error curves.
}
\usage{
plot_gbm(object=stop("no 'object' argument"),
    smooth = c(0, 0, 0, 1),
    col = c(1, 2, 3, 4), ylim = "auto",
    legend.x = NULL, legend.y = NULL, legend.cex = .8,
    grid.col = NA,
    n.trees = NA, col.n.trees ="darkgray",
    ...)
}
\arguments{
\item{object}{
The \code{gbm} model.
}
\item{smooth}{
Four-element vector specifying if smoothing should be applied
to the train, test, CV, and OOB curves respectively.
When smoothing is specified, a smoothed curve is plotted and the
minimum is calculated from the smoothed curve.\cr
The default is c(0, 0, 0, 1) meaning apply smoothing only to the
OOB curve (same as \code{\link[gbm]{gbm.perf}}).\cr
Note that \code{smooth=1} (which gets recyled to \code{c(1,1,1,1)})
will smooth all the curves.
}
\item{col }{
Four-element vector specifying the colors for the train, test, CV, and OOB
curves respectively.\cr
The default is \code{c(1, 2, 3, 4)}.\cr
Use a color of \code{0} to remove the corresponding curve, e.g.
\code{col=c(1,2,3,0)} to not display the OOB curve.\cr
If \code{col=0} (which gets recycled to \code{c(0,0,0,0)}) nothing
will be plotted, but \code{plot_gbm} will return the number-of-trees
at the minima as usual (as described in the Value section below).
}
\item{ylim }{
The default \code{ylim="auto"} shows more detail around the minima.\cr
Use \code{ylim=NULL} for the full vertical range of the curves.\cr
Else specify \code{ylim} as usual.
}
\item{legend.x }{
The x position of the legend.
The default positions the legend automatically.\cr
Use \code{legend.x=NA} for no legend.\cr
See the \code{x} and \code{y} arguments of
\code{\link[grDevices]{xy.coords}} for other options,
for example \code{legend.x="topright"}.
}
\item{legend.y }{
The y position of the legend.
}
\item{legend.cex }{
The legend \code{cex} (the default is \code{0.8}).
}
\item{grid.col}{
Default \code{NA}.
Color of the optional grid, for example \code{grid.col=1}.
}
\item{n.trees}{
For use by \code{\link{plotres}}.\cr
The x position of the gray vertical line indicating the \code{n.trees}
passed by \code{plotres} to \code{predict.gbm} to calculate the residuals.
Plotres defaults to all trees.
}
\item{col.n.trees }{
For use by \code{\link{plotres}}.\cr
Color of the vertical line showing the \code{n.trees} argument.
Default is \code{"darkgray"}.
}
\item{\dots}{
Dot arguments are passed internally to
\code{\link[graphics]{plot.default}}.
}
}
\value{
This function returns a four-element vector specifying the number of trees at
the train, test, CV, and OOB minima respectively.

The minima are calculated after smoothing as specified by this
function's \code{smooth} argument.
By default, only the OOB curve is smoothed.
The smoothing algorithm for the OOB curve differs slightly
from \code{\link[gbm]{gbm.perf}}, so can give a slightly
different number of trees.
}
\note{
\bold{The OOB curve}

The OOB curve is artificially rescaled to force it into the plot.
See Chapter 7 in the \href{../doc/plotres-notes.pdf}{plotres
vignette}.

% The OOB minimum is determined after smoothing the curve,
% but the unsmoothed curve is displayed.
% Whereas the minima for the test and cross-validation curves are
% determined without smoothing.
% This calculation of minima is compatible with \code{gbm.perf}.

\bold{Interaction with \code{plotres}}

When invoking this function via \code{\link{plotres}}, prefix any
argument of \code{plotres} with \code{w1.} to tell \code{plotres} to
pass the argument to this function.
For example give \code{w1.ylim=c(0,10)} to \code{plotres} (plain
\code{ylim=c(0,10)} in this context gets passed to the residual
plots).

\bold{Acknowledgments}

This function is derived from code in the \code{\link[gbm]{gbm}}
package authored by Greg Ridgeway and others, and maintained by Harry
Southworth.
}
\seealso{
Chapter 7 in \href{../doc/plotres-notes.pdf}{plotres vignette} discusses
this function.
}
\examples{
if (require(gbm)) {
    n <- 100                            # toy model for quick demo
    x1 <- 3 * runif(n)
    x2 <- 3 * runif(n)
    x3 <- sample(1:4, n, replace=TRUE)
    y <- x1 + x2 + x3 + rnorm(n, 0, .3)
    data <- data.frame(y=y, x1=x1, x2=x2, x3=x3)
    mod <- gbm(y~., data=data, distribution="gaussian",
               n.trees=300, shrinkage=.1, interaction.depth=3,
               train.fraction=.8, verbose=FALSE)

    plot_gbm(mod)

    # plotres(mod)                      # plot residuals

    # plotmo(mod)                       # plot regression surfaces
}
}
