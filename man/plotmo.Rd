\name{plotmo}
\alias{plotmo}
\concept{partial dependence plot}
\title{Plot a model's response over a range of predictor values}
\description{
Plot model surfaces for a wide variety of models.

This function plots the model's response when varying one or two
predictors while holding the other predictors constant (a poor man's
partial-dependence plot).
It can also generate partial-dependence plots.

Please see the \href{../doc/plotmo-notes.pdf}{plotmo vignette}
(also available \href{http://www.milbo.org/doc/plotmo-notes.pdf}{here}).
}
\usage{
plotmo(object=stop("no 'object' argument"),
    type=NULL, nresponse=NA, pmethod="plotmo",
    pt.col=0, jitter=.5, smooth.col=0, level=0,
    func=NULL, inverse.func=NULL, nrug=0, grid.col=0,
    type2="persp",
    degree1=TRUE, all1=FALSE, degree2=TRUE, all2=FALSE,
    do.par=TRUE, clip=TRUE, ylim=NULL, caption=NULL, trace=0,
    grid.func=median, grid.levels=NULL, extend=0,
    ngrid1=50, ngrid2=20, ndiscrete=5, npoints=3000,
    center=FALSE, xflip=FALSE, yflip=FALSE, swapxy=FALSE, int.only.ok=TRUE,
    ...)
}
\arguments{
\item{object}{
The model object.
}
\item{type}{
Type parameter passed to \code{\link{predict}}.
For allowed values see the \code{predict} method for
your \code{object} (such as \code{\link[earth]{predict.earth}}).
By default, \code{plotmo} tries to automatically select a suitable
value for the model in question (usually \code{"response"})
but this will not always be correct.
Use \code{trace=1} to see the \code{type} argument passed to \code{predict}.
}
\item{nresponse}{
Which column to use when \code{predict} returns multiple columns.
This can be a column index, or a column name if the \code{predict}
method for the model returns column names.
The column name may be abbreviated, partial matching is used.
}
\item{pmethod}{
Plotting method.
One of:

\code{"plotmo"} (default)
Classic plotmo plots i.e. the background variables
are fixed at their medians (or first level for factors).

\code{"partdep"} Partial dependence plots, i.e. at each point the effect
of the background variables is averaged.
% See Hastie, Tibshirani and Friedman ``Elements of Statistical
% Learning'' 2nd Edition Section 10.13.2.
% Plotting these is slow and may require large amounts of memory.

\code{"apartdep"} Approximate partial dependence plots.
Faster than \code{"partdep"} especially for big datasets.
Like \code{"partdep"} but the background variables are averaged over a
subset of \code{ngrid1} cases (default 50), rather than all cases in
the training data.
The subset is created by selecting
rows at equally spaced intervals from the training data
after sorting the data on the response values
(ties are randomly broken).
% If \code{ngrid1} is greater then the number of cases than all cases
% are used, and \code{"apartdep"} is identical to \code{"partdep"}.
The same background subset of \code{ngrid1} cases is used for both
degree1 and degree2 plots.
}
\item{pt.col}{
The color of response points (or response sites in degree2 plots).
This refers to the response \code{y} in the data
used to build the model.
Note that the displayed points are jittered by default
(see the \code{jitter} argument).
\cr
Default is \code{0}, display no response points.
\cr
This can be a vector, like all such arguments, for example
\code{pt.col = as.numeric(survived)+2}.
\cr
You can modify the plotted points with
\code{pt.pch}, \code{pt.cex}, etc.
(these get passed via \code{plotmo}'s ``\code{...}'' argument).
To label the points, set \code{pt.pch} to a character vector.
}
\item{jitter}{
Applies only if \code{pt.col} is specified.\cr
The default is \code{jitter=.5}, automatically apply some jitter to the points.
Points are jittered horizontally and vertically.\cr
Use \code{jitter=0} to disable this automatic jittering.
Otherwise something like \code{jitter=1}, but the optimum value is data dependent.
}
\item{smooth.col}{
Color of smooth line through the response points.
(The points themselves will not be plotted unless \code{pt.col} is specified.)
Default is \code{0}, no smooth line.
\cr
Example:\preformatted{    mod <- lm(Volume~Height, data=trees)
    plotmo(mod, pt.color=1, smooth.col=2)}
You can adjust the amount of smoothing with \code{smooth.f}.
This gets passed as \code{f} to \code{\link[stats]{lowess}}.
The default is \code{.5}.
Lower values make the line more wiggly.
}
\item{level}{
Draw estimated confidence or prediction interval bands at the given \code{level},
if the predict method for the model supports them.\cr
Default is \code{0}, bands not plotted.
Else a fraction, for example \code{level=.95}.
See \dQuote{\emph{Prediction intervals}} in the \code{plotmo} vignette.
Example:\preformatted{    mod <- lm(log(Volume)~log(Girth), data=trees)
    plotmo(mod, level=.95)}
You can modify the color of the bands with \code{level.shade} and \code{level.shade2}.
}
\item{func}{
Superimpose \code{func(x)} on the plot.
Example:\preformatted{    mod <- lm(Volume~Girth, data=trees)
    estimated.volume <- function(x) .17 * x$Girth^2
    plotmo(mod, pt.col=2, func=estimated.volume)}
The \code{func} is called for each plot with a single argument which
is a dataframe with columns in the same order as the predictors
in the \code{formula} or \code{x} used to build the model.
Use \code{trace=2} to see the column names and first few rows of this dataframe.
}
\item{inverse.func}{
A function applied to the response before plotting.
Useful to transform a transformed response back to the original scale.
Example:\preformatted{    mod <- lm(log(Volume)~., data=trees)
    plotmo(mod, inverse.func=exp)    # exp() is inverse of log()

}
}
\item{nrug}{
Number of ticks in the \code{\link[graphics]{rug}} along the bottom of the plot
\cr
Default is \code{0}, no rug.
\cr
Use \code{nrug=TRUE} for all the points.
\cr
Else specify the number of quantiles
e.g. use \code{nrug=10} for ticks at the 0, 10, 20, ..., 100 percentiles.
\cr
Modify the rug ticks with \code{rug.col}, \code{rug.lwd}, etc.
\cr
The special value \code{nrug="density"} means plot the
density of the points along the bottom.
Modify the \code{\link[stats]{density}} plot with \code{density.adjust} (default is \code{.5}),
\code{density.col}, \code{density.lty}, etc.
}
\item{grid.col}{
Default is \code{0}, no grid.
Else add a background \code{\link[graphics]{grid}}
of the specified color to the degree1 plots.
The special value \code{grid.col=TRUE} is treated as \code{"lightgray"}.
}
\item{type2}{
Degree2 plot type.
One of \code{"\link[graphics]{persp}"} (default),
\code{"\link[graphics]{image}"}, or \code{"\link[graphics]{contour}"}.
You can pass arguments to these functions if necessary by using
\code{persp.}, \code{image.}, or \code{contour.} as a prefix.
Examples:\preformatted{    plotmo(mod, persp.ticktype="detailed", persp.nticks=2)
    plotmo(mod, type2="image")
    plotmo(mod, type2="image", image.col=heat.colors(12))
    plotmo(mod, type2="contour", contour.col=2, contour.labcex=.4)
    }
}
\item{degree1}{
An index vector specifying which subset of degree1 (main effect) plots to include
(after selecting the relevant predictors as described in
\dQuote{\emph{Which variables are plotted?}} in the \code{plotmo} vignette).
\cr
Default is \code{TRUE}, meaning all (the \code{TRUE} gets recycled).
To plot only the third plot use \code{degree1=3}.
For no degree1 plots use \code{degree1=0}.
\cr
\cr
Note that \code{degree1} indexes plots on the page,
not columns of \code{x}.
Probably the easiest way to use this argument (and \code{degree2}) is to
first use the default (and possibly \code{all1=TRUE})
to plot all figures.  This shows how the figures are numbered.
Then replot using \code{degree1} to select the figures you want,
for example \code{degree1=c(1,3,4)}.
\cr
\cr
Can also be a character vector
specifying which variables to plot. Examples:\cr
\code{degree1="wind"}\cr
\code{degree1=c("wind", "vis")}.
\cr
\cr
Variables names are matched with \code{\link[base]{grep}}.
Thus \code{"wind"} will match all variables with \code{"wind"}
anywhere in their name. Use \code{"^wind$"} to match only the variable
named \code{"wind"}.
}
\item{all1}{
Default is \code{FALSE}.
Use \code{TRUE} to plot all predictors,
not just those usually selected by \code{plotmo}.
\cr
The \code{all1} argument increases the number of plots;
the \code{degree1} argument reduces the number of plots.
}
\item{degree2}{
An index vector specifying which subset of degree2 (interaction) plots to include.
\cr
Default is \code{TRUE} meaning all
(after selecting the relevant interaction terms as described in
\dQuote{\emph{Which variables are plotted?}} in the \code{plotmo} vignette).
\cr
\cr
Can also be a character vector specifying which variables to plot
(\code{\link[base]{grep}} is used for matching).
Examples:
\cr
\code{degree2="wind"} plots all degree2 plots
for the \code{wind} variable.
\cr
\code{degree2=c("wind", "vis")} plots just the \code{wind:vis} plot.
}
\item{all2}{
Default is \code{FALSE}.
Use \code{TRUE} to plot all pairs of predictors,
not just those usually selected by \code{plotmo}.
}
\item{do.par}{One of \code{NULL}, \code{FALSE}, \code{TRUE}, or \code{2}, as follows:

\code{do.par=NULL}. Same as \code{do.par=FALSE} if the
number of plots is one; else the same as \code{TRUE}.

\code{do.par=FALSE}. Use the current \code{\link[graphics]{par}} settings.
You can pass additional graphics parameters in the ``\code{...}'' argument.

\code{do.par=TRUE} (default). Start a new page and call \code{\link[graphics]{par}} as
appropriate to display multiple plots on the same page.
This automatically sets parameters like \code{mfrow} and \code{mar}.
You can pass additional graphics parameters in the ``\code{...}'' argument.
% This sets the \emph{overall} look of the display; modify
% \emph{specific} plots by using prefixed arguments as described in the
% documentation for the \dots argument below.

\code{do.par=2}.  Like \code{do.par=TRUE} but don't restore
the \code{\link[graphics]{par}} settings to their original state when \code{plotmo} exits,
so you can add something to the plot.
\cr
}
\item{clip}{
The default is \code{clip=TRUE}, meaning ignore very outlying
predictions  when determining the automatic \code{ylim}.
This keeps \code{ylim} fairly compact while
still covering all or nearly all the data,
even if there are a few crazy predicted values.
See \dQuote{\emph{The \code{ylim} and \code{clip} arguments}} in the \code{plotmo} vignette.
\cr
Use \code{clip=FALSE} for no clipping.
}
\item{ylim}{Three possibilities:
\cr
\code{ylim=NULL} (default). Automatically determine a \code{ylim}
to use across all graphs.
\cr
\code{ylim=NA}. Each graph has its own \code{ylim}.
\cr
\code{ylim=c(ymin,ymax)}. Use the specified limits across all graphs.
\cr
}
\item{caption}{
Overall caption.  By default create the caption automatically.
Use \code{caption=""} for no caption.
(Use \code{main} to set the title of individual plots, can be a vector.)
}
\item{trace}{
Default is \code{0}.
\cr
\code{trace=1} (or \code{TRUE}) for a summary trace (shows how
\code{\link[stats]{predict}} is invoked for the current object).
\cr
\code{trace=2} for detailed tracing.
\cr
\code{trace=-1} inhibits the messages usually issued by \code{plotmo},
like the \code{plotmo grid:},
\code{calculating partdep},
and  \code{nothing to plot} messages.
Error and warning messages will be printed as usual.
\cr
\cr
}
\item{grid.func}{
Function applied to columns of the \code{x} matrix to pin the values of
variables not on the axes.
Default is \code{\link[stats]{median}}.
This argument is not related to the \code{grid.col} argument.
Examples:\preformatted{    plotmo(mod, grid.func=mean)
    grid.func <- function(x, ...) quantile(x)[2] # 25\% quantile
    plotmo(mod, grid.func=grid.func)}
This argument is ignored for factors.  The first level of
factors is used. That can be changed with \code{grid.levels}, see below.
}
\item{grid.levels}{
Default is \code{NULL}.
Else a list of variables and their fixed value to be used
when the variable is not on the axis.
Supersedes \code{grid.func} for variables in the list.
Names and values can be abbreviated, partial matching is used.
Example:\preformatted{    plotmo(mod, grid.levels=list(sex="m", age=21))

}
}
\item{extend}{
Amount to extend the horizontal axis in each plot.
The default is \code{0}, do not extend
(i.e. use the range of the variable in the training data).
Else something like \code{extend=.5}, which will extend both the lower
and upper \code{xlim} of each plot by 50\%.\cr
This argument is useful if you want to see how the model performs
on data that is beyond the training data;
for example, you want to see how a time-series model performs on future data.\cr
This argument is currently implemented only for degree1 plots.
Factors and discrete variables (see the \code{ndiscrete} argument)
are not extended.
}
\item{ngrid1}{
Number of equally spaced x values in each degree1 plot.
Default is \code{50}.
Also used as the number of background cases for \code{pmethod="apartdep"}.
}
\item{ngrid2}{
Grid size for degree2 plots (\code{ngrid2 x ngrid2} points are plotted).
Default is \code{20}.
\cr
The default will sometimes be too small for \code{contour} and \code{image} plots.
\cr
With large \code{ngrid2} values, \code{persp} plots look better with
\code{persp.border=NA}.
}
\item{npoints}{
Number of response points to be plotted
(a sample of \code{npoints} points is plotted).
Applies only if \code{pt.col} is specified.
\cr
The default is 3000 (not all, to avoid overplotting on large models).
Use \code{npoints=TRUE} or \code{-1} for all points.
}
\item{ndiscrete}{
Default \code{5} (a somewhat arbitrary value).
Variables with no more than \code{ndiscrete} unique values
are plotted as quantized in plots (a staircase rather than a curve).
Factors are always considered discrete.
}
\item{int.only.ok}{
Plot the model even if it is an intercept-only model (no predictors are
used in the model).
Do this by plotting a single degree1 plot for the first predictor.
\cr
The default is \code{TRUE}.
Use \code{int.only.ok=FALSE} to instead issue an error message for intercept-only models.
}
\item{center}{
Center the plotted response.
Default is \code{FALSE}.
}
\item{xflip}{
Default \code{FALSE}.
Use \code{TRUE} to flip the direction of the \code{x} axis.
This argument (and \code{yflip} and \code{swapxy}) is useful when comparing
to a plot from another source and you want the axes to be the same.
(Note that \code{xflip} and \code{yflip} cannot be used on the \code{persp} plots,
a limitation of the \code{persp} function.)
}
\item{yflip}{
Default \code{FALSE}.
Use \code{TRUE} to flip the direction of the y axis of the degree2 graphs.
}
\item{swapxy}{
Default \code{FALSE}.
Use \code{TRUE} to swap the x and y axes on the degree2 graphs.
\cr
\cr
}
\item{\dots}{
Dot arguments are passed to the predict and plot functions.
Dot argument names, whether prefixed or not, should be specified in full
and not abbreviated.
\cr
\cr
\dQuote{Prefixed} arguments are passed directly to the associated function.
For example the prefixed argument \code{persp.col="pink"} passes
\code{col="pink"} to \code{persp()}, overriding the global
\code{col} setting.
To send an argument to \code{predict} whose name may alias with
\code{plotmo}'s arguments, use \code{predict.} as a prefix.
Example:\preformatted{    plotmo(mod, s=1)           # error:  arg matches multiple formal args
    plotmo(mod, predict.s=1)   # ok now: s=1 will be passed to predict()
}
The prefixes recognized by \code{plotmo} are:\tabular{ll}{
\cr
\code{predict.} \tab passed to the \code{\link[stats]{predict}} method for the model
\cr
\code{degree1.} \tab modifies degree1 plots e.g. \code{degree1.col=3, degree1.lwd=2}
\cr
\code{persp.} \tab arguments passed to \code{\link[graphics]{persp}}
\cr
\code{contour.} \tab arguments passed to \code{\link[graphics]{contour}}
\cr
\code{image.} \tab arguments passed to \code{\link[graphics]{image}}
\cr
\code{pt.} \tab see the \code{pt.col} argument
(arguments passed to \code{\link[graphics]{points}} and \code{\link[graphics]{text}})
\cr
\code{smooth.} \tab see the \code{smooth.col} argument
(arguments passed to \code{\link[graphics]{lines}} and \code{\link[stats]{lowess}})
\cr
\code{level.} \tab see the \code{level} argument
(\code{level.shade}, \code{level.shade2}, and arguments for \code{\link[graphics]{polygon}})
\cr
\code{func.} \tab see the \code{func} argument
(arguments passed to \code{\link[graphics]{lines}})
\cr
\code{rug.} \tab see the \code{nrug} argument
(\code{rug.jitter}, and arguments passed to \code{\link[graphics]{rug}})
\cr
\code{density.} \tab see the \code{nrug} argument
(\code{density.adjust}, and arguments passed to \code{\link[graphics]{lines}})
\cr
\code{grid.} \tab see the \code{grid.col} argument
(arguments passed to \code{\link[graphics]{grid}})
\cr
\code{caption.} \tab see the \code{caption} argument
(arguments passed to \code{\link[graphics]{mtext}})
\cr
\code{par.} \tab arguments passed to \code{\link[graphics]{par}}
(only necessary if a \code{par} argument name clashes
with a \code{plotmo} argument)
}
The \code{cex} argument is relative, so
specifying \code{cex=1} is the same as not specifying \code{cex}.

For backwards compatibility, some dot arguments are supported but not
explicitly documented.  For example, the old argument \code{col.response}
is no longer in \code{plotmo}'s formal argument list, but is still
accepted and treated like the new argument \code{pt.col}.
}
}
\note{
In general this function won't work on models that don't save the call
and data with the model in a standard way.
For further discussion please see \dQuote{\emph{Accessing the model
data}} in the \href{../doc/plotmo-notes.pdf}{plotmo vignette}.
Package authors may want to look at
\href{../doc/modguide.pdf}{Guidelines for S3 Regression Models}
(also available \href{http://www.milbo.org/doc/modguide.pdf}{here}).

By default, \code{plotmo} tries to use sensible model-dependent
defaults when calling \code{predict}.
Use \code{trace=1} to see the arguments passed to \code{predict}.
You can change the defaults by using \code{plotmo}'s \code{type} argument,
and by using dot arguments prefixed with
\code{predict.} (see the description of ``\code{...}'' above).
}
\seealso{
Please see the \href{../doc/plotmo-notes.pdf}{plotmo vignette}
(also available \href{http://www.milbo.org/doc/plotmo-notes.pdf}{here}).
}
\examples{
if (require(rpart)) {
    data(kyphosis)
    rpart.model <- rpart(Kyphosis~., data=kyphosis)
    plotmo(rpart.model, type="prob", nresponse="present")
}
if (require(earth)) {
    data(ozone1)
    earth.model <- earth(O3 ~ ., data=ozone1, degree=2)
    plotmo(earth.model)
}
}
\keyword{partial dependence}
\keyword{regression}
