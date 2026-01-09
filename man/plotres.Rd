\name{plotres}
\alias{plotres}
\concept{residual plot}
\title{Plot the residuals of a regression model}
\description{
Plot the residuals of a regression model.

Please see the \href{../doc/plotres-notes.pdf}{plotres vignette}
(also available \href{http://www.milbo.org/doc/plotres-notes.pdf}{here}).
}
\usage{
plotres(object = stop("no 'object' argument"),
    which = 1:4, info = FALSE, versus = 1,
    standardize = FALSE, delever = FALSE, level = 0,
    id.n = 3, labels.id = NULL, smooth.col = 2,
    grid.col = 0, jitter = 0,
    do.par = NULL, caption = NULL, trace = 0,
    npoints = 3000, center = TRUE,
    type = NULL, nresponse = NA,
    object.name = quote.deparse(substitute(object)), ...)
}
\arguments{
\item{object}{
The model object.
}
\item{which}{
Which plots do draw.  Default is \code{1:4}.

\code{1}  Model plot.  What gets plotted here depends on the model class.
For example, for \code{earth} models this is a model selection plot.
Nothing will be displayed for some models.
For details, please see the
\href{../doc/plotres-notes.pdf}{plotres vignette}.

\code{2}  Cumulative distribution of abs residuals

\code{3}  Residuals vs fitted

\code{4}  QQ plot

\code{5}  Abs residuals vs fitted

\code{6}  Sqrt abs residuals vs fitted

\code{7}  Abs residuals vs log fitted

\code{8}  Cube root of the squared residuals vs log fitted

\code{9}  Log abs residuals vs log fitted
\cr
\cr
}
\item{info}{
Default is \code{FALSE}.
Use \code{TRUE} to print extra information as follows:
% For more information, please
% see the section \emph{\dQuote{The info argument of plot.earth}}
% in the \code{earth} package vignette
% \emph{\dQuote{Variance models in earth}}.

i) Display the distribution of the residuals along the bottom of the plot.

ii) Display the training R-Squared.

iii) Display the Spearman Rank Correlation of the absolute residuals
with the fitted values.
Actually, correlation is measured against the absolute values
of whatever is on the horizontal
axis --- by default this is the fitted response, but may be something
else if the \code{versus} argument is used.

iv) In the Cumulative Distribution plot (\code{which=2}),
display additional information on the quantiles.

v) Only for \code{which=5} or \code{9}.
Regress the absolute residuals against the fitted values
and display the regression slope.
Robust linear regression is used via \code{\link[MASS]{rlm}} in the MASS package.

vi) Add various annotations to the other plots.
\cr
\cr
}
\item{versus}{
    What do we plot the residuals against?  One of:

\code{1} Default. Plot the residuals versus the fitted values
(or the log values when \code{which=7} to \code{9}).

\code{2} Residuals versus observation number,
after observations have been sorted on the fitted value.
Same as \code{versus=1}, except that the residuals are spaced
uniformly along the horizontal axis.

\code{3} Residuals versus the response.

\code{4} Residuals versus the hat leverages.

\code{"b:"} Residuals versus the basis functions.
Currently only supported for \code{earth}, \code{mda::mars}, and \code{gam::gam} models.
A optional \code{\link{regex}} can follow the \code{"b:"} to specify a subset of the
terms, e.g. \code{versus="b:wind"} will plot terms with \code{"wind"} in
their name.

Else a character vector specifying which predictors to plot against.
\cr
Example 1: \code{versus=""} plots against all predictors (since the
regex \code{versus=""} matches anything).
\cr
Example 2: \code{versus=c("wind", "vis")} plots predictors
with \code{wind} or \code{vis} in their name.
\cr
Example 3: \code{versus=c("wind|vis")} equivalent to the above.
\cr
Note: These are \code{\link{regex}}s.
Thus \code{versus="wind"} will match all variables that have \code{"wind"}
in their names. Use \code{"^wind$"} to match only the variable named
\code{"wind"}.
\cr
\cr
}
\item{standardize}{
Default is \code{FALSE}.
Use \code{TRUE} to standardize the residuals.
Only supported for some models, an error message will be issued otherwise.
\cr
Each residual is divided by by \code{se_i * sqrt(1 - h_ii)},
where \code{se_i} is the standard error of prediction
and \code{h_ii} is the leverage (the diagonal entry of the hat matrix).
When the variance assumptions hold (for your type of model), the standardized residuals are
homoscedastic with unity variance.
\cr
The leverages are obtained using \code{\link{hatvalues}}.
(For \code{earth} models the leverages are
for the linear regression of the response on the basis matrix \code{bx}.)
A standardized residual with a leverage of 1 is plotted as a star on the axis.
\cr
This argument applies to all plots where the residuals are used
(including the cumulative distribution and QQ plots, and to
annotations displayed by the \code{info} argument).
}
\item{delever}{
Default is \code{FALSE}.
Use \code{TRUE} to \dQuote{de-lever} the residuals.
Only supported for some models, an error message will be issued otherwise.
\cr
Each residual is divided by \code{sqrt(1 - h_ii)}.
See the \code{standardize} argument for details.
}
\item{level}{
Draw estimated confidence or prediction interval bands at the given
\code{level}, if the model supports them.
\cr
Default is \code{0}, bands not plotted.
Else a fraction, for example \code{level=0.90}.
Example:\preformatted{    mod <- lm(log(Volume)~log(Girth), data=trees)
    plotres(mod, level=.90)}
You can modify the color of the bands with \code{level.shade} and \code{level.shade2}.
\cr
See also \dQuote{\emph{Prediction intervals}} in the
\href{../doc/plotmo-notes.pdf}{plotmo vignette}
(but note that \code{plotmo} needs prediction intervals on \emph{new}
data, whereas \code{plotres} requires only that the model supports
prediction intervals on the training data).
}
\item{id.n}{
The largest \code{id.n} residuals will be labeled in the plot.
Default is \code{3}.
Special values \code{TRUE} and \code{-1} or mean all.\cr
If \code{id.n} is negative (but not \code{-1})
the \code{id.n} most positive and most negative
residuals will be labeled in the plot.\cr
A current implementation restriction is that \code{id.n} is ignored
when there are more than ten thousand cases.
}
\item{labels.id}{
Residual labels.
Only used if \code{id.n > 0}.
Default is the case names, or the case numbers if the cases are unnamed.
}
\item{smooth.col}{
Color of the smooth line through the residual points.
Default is \code{2}, red. Use \code{smooth.col=0} for no smooth line.
\cr
You can adjust the amount of smoothing with \code{smooth.f}.
This gets passed as \code{f} to \code{\link[stats]{lowess}}.
The default is \code{2/3}.
Lower values make the line more wiggly.
}
\item{grid.col}{
Default is \code{0}, no grid.
Else add a background \code{\link[graphics]{grid}}
of the specified color to the degree1 plots.
The special value \code{grid.col=TRUE} is treated as \code{"lightgray"}.
}
% \item{cum.grid}{
% Grid type in the Cumulative Distribution plot. One of:
%
% \code{"none"} No grid.
%
% \code{"grid"} Add grid showing the 25\%, 50\%, 90\%, and 95\%
% quantiles.
%
% \code{"percentages"}  (default) Add grid and percentage labels.
% If \code{info=TRUE} also display quantiles on the right.
% \cr
% \cr
% }
\item{jitter}{
Default is \code{0}, no jitter.
Passed as \code{factor} to \code{\link[base]{jitter}}
to jitter the plotted points horizontally and vertically.
Useful for discrete variables and responses, where the residual points
tend to be overlaid.
}
\item{do.par}{One of \code{NULL}, \code{FALSE}, \code{TRUE}, or \code{2}, as follows:

\code{do.par=NULL} (default). Same as \code{do.par=FALSE} if the
number of plots is one; else the same as \code{TRUE}.

\code{do.par=FALSE}. Use the current \code{\link[graphics]{par}} settings.
You can pass additional graphics parameters in the ``\code{...}'' argument.

\code{do.par=TRUE}. Start a new page and call \code{\link[graphics]{par}} as
appropriate to display multiple plots on the same page.
This automatically sets parameters like \code{mfrow} and \code{mar}.
You can pass additional graphics parameters in the ``\code{...}'' argument.
% This sets the \emph{overall} look of the display; modify
% \emph{specific} plots by using prefixed arguments as described in the
% documentation for the \dots argument below.

\code{do.par=2}.  Like \code{do.par=TRUE} but don't restore the
\code{\link[graphics]{par}} settings to their original state when
\code{plotres} exits, so you can add something to the plot.
\cr
\cr
}
\item{caption}{
Overall caption.  By default create the caption automatically.
Use \code{caption=""} for no caption.
(Use \code{main} to set the title of an individual plot.)
}
\item{trace}{
Default is \code{0}.
\cr
\code{trace=1} (or \code{TRUE}) for a summary trace (shows how
\code{\link[stats]{predict}} and friends
are invoked for the model).
\cr
\code{trace=2} for detailed tracing.
\cr
}
\item{npoints}{
Number of points to be plotted.
A sample of \code{npoints} is taken; the sample includes the biggest
twenty or so residuals.
\cr
The default is 3000 (not all, to avoid overplotting on large models).
Use \code{npoints=TRUE} or \code{-1} for all points.
}
\item{center}{
Default is TRUE, meaning center the horizontal axis in the residuals plot,
so asymmetry in the residual distribution is more obvious.
}
\item{type}{
Type parameter passed first to \code{\link{residuals}} and
if that fails to \code{\link{predict}}.
For allowed values see the \code{residuals} and \code{predict} methods for
your \code{object}
(such as
\code{\link[rpart]{residuals.rpart}} or
\code{\link[earth]{predict.earth}}).
By default, \code{plotres} tries to automatically select a suitable
value for the model in question (usually \code{"response"}),
but this will not always be correct.
Use \code{trace=1} to see the \code{type} argument passed to
\code{residuals} and \code{predict}.
}
\item{nresponse}{
Which column to use when \code{residuals} or \code{predict} returns
multiple columns.
This can be a column index or column name
(which may be abbreviated, partial matching is used).
}
\item{object.name}{
The name of the \code{object} for error and trace messages.
Used internally by \code{plot.earth}.
\cr
\cr
}
\item{\dots}{
Dot arguments are passed to the plot functions.
Dot argument names, whether prefixed or not, should be specified in full
and not abbreviated.

\dQuote{Prefixed} arguments are passed directly to the associated function.
For example the prefixed argument \code{pt.col="pink"} passes
\code{col="pink"} to \code{points()}, overriding the global
\code{col} setting.
The prefixes recognized by \code{plotres} are:\tabular{ll}{
\code{residuals.} \tab passed to \code{\link[stats]{residuals}}
\cr
\code{predict.} \tab passed to \code{\link[stats]{predict}}
(\code{predict} is called if the call to \code{residuals} fails)
\cr
\code{w1.} \tab sent to the model-dependent plot for \code{which=1} e.g. \code{w1.col=2}
\cr
\code{pt.} \tab modify the displayed points
e.g. \code{pt.col=as.numeric(survived)+2} or \code{pt.cex=.8}.
\cr
\code{smooth.} \tab modify the  smooth line e.g. \code{smooth.col=0} or
\code{smooth.f=.5}.
\cr
\code{level.} \tab modify the interval bands, e.g. \code{level.shade="gray"} or \code{level.shade2="lightblue"}
\cr
\code{legend.} \tab modify the displayed \code{\link[graphics]{legend}} e.g. \code{legend.cex=.9}
\cr
\code{cum.} \tab modify the Cumulative Distribution plot
(arguments for \code{\link[stats]{plot.stepfun}})
\cr
\code{qq.} \tab modify the QQ plot, e.g. \code{qq.pch=1}
\cr
\code{qqline} \tab modify the \code{\link{qqline}} in the QQ plot, e.g. \code{qqline.col=0}
\cr
\code{label.} \tab modify the point labels, e.g. \code{label.cex=.9} or \code{label.font=2}
\cr
\code{cook.} \tab modify the Cook's Distance annotations.
This affects only the leverage plot
(\code{versus=3}) for \code{lm} models with \code{standardize=TRUE}.
e.g. \code{cook.levels=c(.5, .8, 1)} or \code{cook.col=2}.
\cr
\code{caption.} \tab modify the overall caption (see the \code{caption} argument)
e.g. \code{caption.col=2}.
\cr
\code{par.} \tab arguments for \code{\link[graphics]{par}}
(only necessary if a \code{par} argument name clashes
with a \code{plotres} argument)
}
The \code{cex} argument is relative, so
specifying \code{cex=1} is the same as not specifying \code{cex}.

For backwards compatibility, some dot
arguments are supported but not explicitly documented.
}
}
\value{
If the \code{which=1} plot was plotted, the return value of that
plot (model dependent).

Else if the \code{which=3} plot was plotted, return \code{list(x,y)}
where \code{x} and \code{y} are the coordinates of the points in that plot
(but without jittering even if the \code{jitter} argument was used).

Else return \code{NULL}.
}
\note{
This function is designed primarily for displaying standard
\code{response - fitted} residuals for models
with a single continuous response,
although it will work for a few other models.

In general this function won't work on models that don't save the call
and data with the model in a standard way.
It uses the same underlying mechanism to access the model data as
\code{\link{plotmo}}.
For further discussion please see \dQuote{\emph{Accessing the model
data}} in the \href{../doc/plotmo-notes.pdf}{plotmo vignette}
(also available \href{http://www.milbo.org/doc/plotmo-notes.pdf}{here}).
Package authors may want to look at
\href{../doc/modguide.pdf}{Guidelines for S3 Regression Models}
(also available \href{http://www.milbo.org/doc/modguide.pdf}{here}).
}
\seealso{
Please see the \href{../doc/plotres-notes.pdf}{plotres vignette}
(also available \href{http://www.milbo.org/doc/plotres-notes.pdf}{here}).

\code{\link[stats]{plot.lm}}

\code{\link[earth]{plot.earth}}
}
\examples{
# we use lm in this example, but plotres is more useful for models
# that don't have a function like plot.lm for plotting residuals

lm.model <- lm(Volume~., data=trees)

plotres(lm.model)
}
\keyword{partial dependence}
\keyword{regression}
