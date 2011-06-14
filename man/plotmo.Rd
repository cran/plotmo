\name{plotmo}
\alias{plotmo}
\concept{partial dependence plot}
\title{Plot a model's response over a range of predictor values}
\description{
Plot a model's response when varying one or two predictors while
holding the other predictors constant.  A poor man's partial
dependence plot.   See the ``Details'' section for an overview.
}
\usage{
plotmo(object = stop("no 'object' arg"),
       type=NULL, nresponse = NA, clip = TRUE, ylim = NULL,
       center = FALSE, ndiscrete = 5,
       degree1 = TRUE, all1=FALSE, degree2 = TRUE, all2=FALSE,
       grid.func = median, grid.levels = NULL,
       col.response = 0, cex.response = 1, pch.response = 1,
       jitter.response=0, npoints = -1,
       inverse.func = NULL, trace = FALSE,
       nrug = 0, col.degree1 = 1, lty.degree1 = 1, lwd.degree1 = 1,
       col.smooth = 0, lty.smooth = 1, lwd.smooth = 1,
       se = 0, col.shade = "lightgray", col.se = 0, lty.se = 2,
       func = NULL, col.func = "lightblue", lty.func = 1, lwd.func = 1,
       ngrid1 = 50,  grid=FALSE,
       type2 = "persp", ngrid2 = 20,
       col.image = gray(0:10/10), col.persp = "lightblue",
       theta = NA, phi = 30, dvalue = 1, shade = 0.5,
       do.par = TRUE, caption = NULL, main = NULL,
       xlab = "", ylab = "", cex = NULL, cex.lab = 1,
       xflip = FALSE, yflip = FALSE, swapxy = FALSE, ...)
}
\arguments{
To start off, look at the arguments
  \code{object},
  \code{type},
  \code{clip},
  and \code{col.response}.

  \item{object}{
     Model object.
  }
  \item{type}{
     Type parameter passed to \code{\link{predict}}.
     For legal values see the \code{predict} method for
     your \code{object} (such as
     \code{\link[earth]{predict.earth}} and \code{\link{predict.rpart}}).
     By default,
     \code{plotmo} tries to automatically select a suitable value
     (usually \code{"response"}; if not it will be printed in the caption).
  }
  \item{nresponse}{
     Which column to use when \code{predict} returns multiple columns.
     This can be a column index or column name
     (which may be abbreviated, partial matching is used).
     Ignored when \code{predict} returns a single column.
  }
  \item{clip}{
     Default is \code{TRUE}, meaning plot only predicted values that
     are in the expected range.
     Use \code{FALSE} to plot all values.
     See ``The \code{clip} argument'' section below for details.
  }
  \item{ylim}{Three possibilities:\cr

     (i) \code{NULL} (default) all y axes have same limits
     (where ``y'' is actually ``z'' on degree2 plots).
     The limits are the min and max values of the predicted response
     across all plots (after applying \code{clip}).

     (ii) \code{NA} each graph has its own y limits.\cr

     (iii) \code{c(ymin,ymax)} graphs have the specified y limits.\cr
\cr
  }
  \item{center}{
     Center the plotted response.
     Default is \code{FALSE}.
     (This is an initial implementation of centering and will change.)
  }
  \item{ndiscrete}{
     Default \code{5} (a somewhat arbitrary value).
     Variables with no more than \code{ndiscrete} unique values
     are plotted as quantized in plots (a staircase rather than a curve).
     Factors are always considered discrete.
  }
  \item{degree1}{
     Index vector specifying which main effect plots to include.
     Default is \code{TRUE}, meaning all degree1 plots (the \code{TRUE} gets recycled).
     Use \code{FALSE} (or \code{0}) for no degree1 plots.\cr
     The easiest way to use this argument (and \code{degree2}) is to
     first use the default (and possibly \code{all1=TRUE})
     to plot all figures.  This shows how the figures are numbered.
     Then replot using \code{degree1} to select the figures you want,
     for example, \code{degree1=c(1,3)}.
  }
  \item{all1}{
     Default is \code{FALSE}.
     Use \code{TRUE} to plot all predictors,
     not just those usually selected by \code{plotmo}.
     See ``Which variables are plotted?'' below.
     The \code{all1} argument increases the number of plots;
     the \code{degree1} argument reduces the number of plots.
  }
  \item{degree2}{
     Index vector specifying which interaction plots to include.
     Default is \code{TRUE}, meaning all degree2 plots.
  }
  \item{all2}{
     Default is \code{FALSE}.
     Use \code{TRUE} to plot all pairs of predictors,
     not just those usually selected by \code{plotmo}.
  }
  \item{grid.func}{
     Function applied to columns of the x matrix to fix the values of
     variables not on the axes.
     Default is \code{\link{median}}.
     (This argument is ignored for factors.  The first level of
     factors is used. That can be changed with \code{grid.levels}.)
     Example:\preformatted{
  grid.func <- function(x) quantile(x)[2] # 25\% quantile
  plotmo(fit, grid.func = grid.func)
  %spacer
}
  }
  \item{grid.levels}{
     Default is \code{NULL}.
     Else a list of variables and their fixed value to be used
     when the variable is not on the axis.
     Supersedes \code{grid.func} for variables in the list.
     Names and values can be abbreviated, partial matching is used.
     Example:\cr
     \code{plotmo(fit, grid.levels=list(sex="m", age=21))}.
     }
  \item{col.response}{
     Color of response points (or response sites in degree2 plots).
     This refers to the response \code{y} in the data
     used to build the model.
     Default is 0, don't plot the response.
     Can be a vector, for example,\cr
     \code{col.response=as.numeric(survived)+2}.
  }
  \item{cex.response}{
     Relative size of response points.
     Default is \code{1}.
     Applies only if \code{col.response!=0}.
  }
  \item{pch.response}{
     Plot character for response points.
     Default is \code{1}.
     Applies only if \code{col.response!=0}.
  }
  \item{jitter.response}{
     Amount to jitter the response points.
     Applies only if \code{col.response!=0}.
     Default \code{0}, no jitter.
     A typical useful value is \code{.3}, but it depends on the data.
     Points are jittered horizontally and vertically.
     Note: the points for factors and discrete variables and responses
     are always jittered (because unambiguous space is available),
     even when \code{jitter.response} is zero.
  }
  \item{npoints}{
     Number of response points to be plotted.
     Applies only if \code{col.response!=0}.
     Default is the special value \code{-1} meaning all.
     Otherwise a sample of \code{npoints} points is taken.
  }
  \item{inverse.func}{
     Default is \code{NULL}.
     Else a function applied to the predicted response before plotting.
     For example, you could use \code{inverse.func=exp} if your
     model formula is \code{log(y)~x}.
  }
  \item{trace}{
     Default is \code{FALSE}.
     Use \code{TRUE} to trace operation.
     Use values greater than \code{1} for more detailed tracing.\cr
\cr
\bold{The following arguments are for degree1 (main effect) plots}
  }
  \item{nrug}{
     Number of points in (jittered) rug.
     Default is \code{0}, no rug.
     Special value \code{-1} for all.
     Otherwise a sample of \code{nrug} points is taken.
  }
  \item{col.degree1}{
     Color of degree1 lines.  Default is \code{1}.
  }
  \item{lty.degree1}{
     Line type of degree1 lines.  Default is \code{1}.
  }
  \item{lwd.degree1}{
     Line width of degree1 lines.  Default is \code{1}.
  }
  \item{col.smooth}{
     Color of smoothed line through the response points.
     (The points themselves will not be plotted unless \code{col.response} is set.)
     This refers to the response \code{y} in the data
     used to build the model.
     Default is \code{0}, no line.
     Smoothing is done with \code{\link{lowess}},
     but for factors and discrete predictors (\code{<= ndiscrete} levels)
     the mean response at each level is plotted instead (no smoothing).
     Example:
\preformatted{  fit <- earth(O3~., data=ozone1)
  plotmo(fit, degree1=c(4,8), col.resp="gray", col.smooth=2)
  %spacer
}
  }
  \item{lty.smooth}{
     Default is \code{1}.
     Applies only if \code{col.smooth!=0}.
  }
  \item{lwd.smooth}{
     Default is \code{1}.
     Applies only if \code{col.smooth!=0}.
  }
  \item{se}{
     Draw standard error bands at plus and minus \code{se} times the pointwise standard errors.
     Default is \code{0}, no standard error bands.
     A typical value would be \code{2}.
     The predict method for the model \code{object} must support standard errors,
     i.e. be callable with \code{se.fit=TRUE}
     (such as \code{\link{predict.lm}} but not \code{\link[earth]{predict.earth}}).
     Example:
\preformatted{  fit <- lm(stack.loss~., stackloss)
  plotmo(fit, se=2, col.response=2, nrug=-1)
  %spacer
}
  }
  \item{col.shade}{
     Color of \code{se} shading.  Default is \code{"lightgray"}.
     Use \code{0} for no shading.
     Applies only if \code{se!=0}.
  }
  \item{col.se}{
     Color of \code{se} lines.  Default is \code{0}, no lines just shading.
     Applies only if \code{se!=0}.
  }
  \item{lty.se}{
     Line type of \code{se} lines.  Default is \code{2}.
  }
  \item{func}{
     Superimpose \code{func(x)} if \code{func} is not \code{NULL}.
     Default is \code{NULL}.
     This is useful if you are comparing the model to a known function.
     The \code{func} is called for each plot with a single argument which
     is a data frame with columns in the same order as the predictors
     in the \code{formula} or \code{x} used to build the model.
     Use \code{trace=TRUE} to see the column names and first few rows of this dataframe.
  }
  \item{col.func}{
     Color of \code{func} line.
     Default is \code{"lightblue"}.
  }
  \item{lwd.func}{
     Line width of \code{func} line.
     Default is 1.
  }
  \item{lty.func}{
     Line type of \code{func} line.
     Default is 1.
  }
  \item{ngrid1}{
     Number of points in degree1 plots.
     Default is \code{50}.
  }
  \item{grid}{
     Default \code{FALSE}.
     Use \code{TRUE} to add a \code{\link{grid}} to the degree1 plots.
     You can also specify a color here, e.g. \code{grid="darkgray"}.
  }
  \item{xlab}{
     Horizontal axis label on degree1 plots
     (for degree2 plots the labels are always the predictor names).
     Default is \code{""}, no label, which gives more plottable area.
     The special value \code{NULL} means use the current variable name as the label.
     (If you use \code{NULL}, you may want to use \code{main=""} to avoid
     redundant labeling.)
  }
  \item{ylab}{
     Vertical axis label.  Values as for \code{xlab}.
\cr
\cr
  \bold{The following arguments are for degree2 plots}
  }
  \item{type2}{
     Degree2 plot type.
     One of \code{"\link{persp}"} (default), \code{"\link{contour}"}, or \code{"\link{image}"}.
  }
  \item{ngrid2}{
     Grid size for degree2 plots (\code{ngrid2 x ngrid2} points are plotted,
      but less for factors and variables with less than \code{ngrid2} discrete values).
     Default is \code{20}.\cr
     Note 1: the default will often be too small for \code{contour} and \code{image} plots.\cr
     Note 2: with large \code{ngrid2} values, \code{persp} plots look better with \code{border=NA}.
  }
  \item{col.image}{
     Colors of \code{\link{image}} plot.
     Default is \code{gray(0:10/10)}, a range of grays.
     Clipped values will be displayed in blue (only applies if \code{clip=TRUE}).
  }
  \item{col.persp}{
     Color of \code{\link{persp}} surface. Default is \code{"lightblue"}.
     Use 0 for no color.
  }
  \item{theta}{
     Rotation parameter for \code{\link{persp}}.
     Default is \code{NA}, meaning automatically rotate each graph
     so the highest corner is furthest away.
     Use \code{trace=TRUE} to see the calculated value for \code{theta}.
     Higher values of \code{theta} rotate clockwise.
  }
  \item{phi}{
     Passed to \code{\link{persp}}. Default is \code{30}.
     Lower values to view from the side; higher to view from above.
  }
  \item{dvalue}{
     Passed to \code{\link{persp}} as \code{d}. Default is \code{1}.
     The name was changed from \code{d} to avoid partial matching problems.
  }
  \item{shade}{
     Passed to \code{\link{persp}}. Default is \code{0.5}.\cr
\cr
  \bold{The following are related to \code{par} and other graphical settings}.
  }
  \item{do.par}{
    Default is \code{TRUE}, meaning start a new page and call \code{\link{par}} as
    appropriate (this adjusts \code{mfrow}, \code{cex}, \code{mar}, and \code{mgp}).
    Use \code{FALSE} to use the current graphics settings.
    The value \code{2} means act like \code{TRUE} but do not restore
    the \code{par} settings to their original state (useful
    for slipping in a few more plots on the same page).
  }
  \item{caption}{
    Overall caption.  By default create captions automatically
    from the \code{type}, response name, and \code{call}.
  }
  \item{main}{
    A vector of titles, one for each plot.
    By default generate titles automatically from the variable names.
    See also \code{caption}, for the overall title.
  }
  \item{cex}{
    Character expansion.
  }
  \item{cex.lab}{
    Relative size of axis labels and text. Default \code{1}.
  }
  \item{xflip}{
     Default \code{FALSE}.
     Use \code{TRUE} to flip the direction of the x axis.
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
  }
  \item{\dots}{
     Extra arguments are passed on to the plotting functions.
     (For \code{persp} plots, \code{ticktype="d", nticks=2} is useful.)
  }
}
\details{
\code{Plotmo} can be used on a wide variety of regression models.
It plots a degree1 (main effect) plot by calling \code{\link{predict}} to
predict the response when changing one variable while holding all
other variables at their median values.
For degree2 (interaction) plots, two variables are changed while holding others
at their medians.
The first level is used instead of the median for factors.
You can change this with the \code{grid.func} and
\code{grid.levels} arguments.

Each graph shows only a thin slice of the data because most variables
are fixed.  Please be aware of that when interpreting the graph ---
over-interpretation is a temptation.

\code{Plotmo} was originally part of the \code{\link[earth]{earth}} package
and a few connections to that package still remain.

\bold{Limitations}

NAs are not yet supported.
To prevent confusing error messages from functions called by \code{plotmo},
it is safest to remove NAs before building your model.
(However, \code{\link[rpart]{rpart}} models are treated specially by
\code{plotmo}.  For these, \code{plotmo} predicts with \code{\link{na.pass}}
so \code{plotmo} can be used with \code{rpart}'s default NA handling.)

Keep the variable names in the original model formula simple.
Use temporary variables or \code{\link{attach}} rather than using
\code{$} and similar in formulas.

\code{Plotmo} evaluates the model data in the \code{\link{environment}}
used when the model was built, if that environment was saved with the model
(typically this is the case if the formula interface was used to the
model function).  If the environment was not saved with the model
(typically if the \code{x,y} interface was used),
the model data is evaluated in the environment in which \code{plotmo}
is called.

\bold{Alternatives}

An alternative approach is to use partial-dependence plots
(e.g. \emph{The Elements of Statistical Learning} 10.13.2).
\code{Plotmo} sets the ``other'' variables to their median
value, whereas in a partial-dependence plot at each plotted point the
effect of the other variables is averaged.
In general, partial-dependence plots and \code{plotmo} plots will
differ, but for additive models the \emph{shape} of the curves will
match identically.
Eventually \code{plotmo} will be enhanced to draw
partial-dependence plots.

% There appears to be no general-purpose R function similar to
% \code{plotmo} for drawing partial-dependence plots.
% An example special purpose function is \code{\link[randomForest]{partialPlot}} in the
% \code{\link[randomForest]{randomForest}} package.
% Averaging over the sample at every point is a slow process
% unless the effect of averaging can be
% determined without actually doing the calculation.
% That is not the case for most models (it is for CART trees).

\code{\link[=termplot]{Termplot}} is effective but can be used only on
models with a \code{predict} method that supports \code{type="terms"},
and it does not generate degree2 plots.

Some other possibilites for plotting the response on a per-predictor
basis are partial-residual plots, partial-regression variable plots, and 
marginal-model plots (e.g.  \code{\link[car]{crPlots}},
\code{\link[car]{avPlots}}, and \code{\link[car]{marginalModelPlot}} in the
\code{\link[car]{car-package}}).
These plots are orientated towards linear models.

\bold{Which variables are plotted?}

The set of variables plotted for some common objects is listed below.
This may leave out variables that you would like to see ---
in that case use \code{all1=TRUE} and \code{all2=TRUE}.
\describe{
\item{\bold{o}\code{ earth}}{\code{degree1}: variables in additive (non interaction) terms\cr
\code{degree2}: variables appearing together in interaction terms.}

\item{\bold{o}\code{ rpart}}{\code{degree1}: variables used in the tree\cr
\code{degree2}: parent-child pairs.}

\item{\bold{o}\code{ randomForest}}{\code{degree1}: all variables\cr
\code{degree2}: pairs of the four most important variables (ranked
on the first column of \code{object$importance)}.}

\item{\bold{o}\code{ gbm}}{\code{degree1}: variables with  \code{\link[gbm]{relative.influence} >= 1\%}\cr
\code{degree2}: pairs of the four variables with the largest relative influence.}

\item{\bold{o}\code{ lm, glm, gam, lda}, etc. are processed using \code{plotmo}'s default methods:}{\code{degree1}: all variables\cr
\code{degree2}: variables in formula terms
like \code{x1*x2}, \code{x1:x2} and \code{s(x1,x2)}.}
%spacer
}

\bold{The \code{clip} argument}

With the default \code{clip=TRUE}, predicted values out of the
expected range are not displayed.

Generally, the ``expected range'' is the range of the response
\code{y} used when building the model.
But that depends on the type of model, and \code{plotmo}
knows about some special cases.
For example, it knows that for some models we are predicting a
probability, and it scales the axes accordingly, \code{0} to \code{1}.
However, \code{plotmo} cannot know about every possible model and
prediction \code{type}, and will sometimes determine the expected response range
incorrectly.
In that case use \code{clip=FALSE}.

The default \code{clip} is \code{TRUE} because it is a useful sanity
check to test that the predicted values are in the expected range.
While not necessarily an error, predictions outside the expected range
are usually something we want to know about.
Also, with \code{clip=FALSE}, a few errant predictions can expand the
entire y-axis, making it difficult to see the shape of the other
predictions.

\bold{Using \code{plotmo} on various models}

Here are some examples which illustrate \code{plotmo} on various
objects.
(The models here are just for illustrating \code{plotmo} and
shouldn't be taken too seriously.)
\preformatted{
    # use a small set of variables for illustration
    library(earth) # for ozone1 data
    data(ozone1)
    oz <- ozone1[, c("O3", "humidity", "temp", "ibt")]

    lm.model <- lm(O3 ~ humidity + temp*ibt, data=oz)       # linear model
    plotmo(lm.model, se=2, col.response="gray", nrug=-1)

    library(rpart)                                          # rpart
    rpart.model <- rpart(O3 ~ ., data=oz)
    plotmo(rpart.model, all2=TRUE)

    library(randomForest)                                   # randomForest
    rf.model <- randomForest(O3~., data=oz)
    plotmo(rf.model)
    # partialPlot(rf.model, oz, temp) # compare to partial-dependence plot

    library(gbm)                                            # gbm
    gbm.model <- gbm(O3~., data=oz, dist="gaussian", inter=2, n.trees=1000)
    plotmo(gbm.model)
    # plot(gbm.model, i.var=2) # compare to partial-dependence plots
    # plot(gbm.model, i.var=c(2,3))

    library(mgcv)                                           # gam
    gam.model <- gam(O3 ~ s(humidity)+s(temp)+s(ibt)+s(temp,ibt), data=oz)
    plotmo(gam.model, se=2, all2=TRUE)

    library(nnet)                                           # nnet
    set.seed(4)
    nnet.model <- nnet(O3~., data=scale(oz), size=2, decay=0.01, trace=FALSE)
    plotmo(nnet.model, type="raw", all2=T)

    library(MASS)                                           # qda
    lcush <- data.frame(Type=as.numeric(Cushings$Type),log(Cushings[,1:2]))
    lcush <- lcush[1:21,]
    qda.model <- qda(Type~., data=lcush)
    plotmo(qda.model, type="class", all2=TRUE,
       type2="contour", ngrid2=100, nlevels=2, drawlabels=FALSE,
       col.response=as.numeric(lcush$Type)+1,
       pch.response=as.character(lcush$Type))

}
\bold{Extending plotmo}

\code{Plotmo} needs to access the data used to build the model.
It does that with the method functions listed below.
The default methods suffice for many objects.
However, the default methods don't work (\code{plotmo} will issue
an error message) if the model function did not save
the call or data with the object in a standard fashion.
Object-specific methods can usually be written to deal with such
issues.
See \code{plotmo.methods.gbm.R} in the \code{plotmo} source code for an example.
The methods are:

\describe{
\item{\code{plotmo.prolog}}{ called before plotting begins, sanity check of the \code{object}}
\item{\code{plotmo.predict}}{ invokes \code{predict} for each sub-plot}
\item{\code{get.plotmo.x}}{ the model matrix \code{x}}
\item{\code{get.plotmo.y}}{ the model response \code{y}}
\item{\code{get.plotmo.default.type}}{ the value of the \code{type} argument when not specified by the user}
\item{\code{get.plotmo.singles}}{ the vector of variables to be plotted in degree1 plots}
\item{\code{get.plotmo.pairs}}{ the array of pairs to be plotted in degree2 plots}
\item{\code{get.plotmo.ylim}}{ the value of \code{ylim} when not specified by the user}
\item{\code{get.plotmo.clip.limits}}{ the clip range when \code{clip=TRUE}}
}
\bold{Common error messages}

\bold{o}\code{ Error in match.arg(type): 'arg' should be one of ...}

The message is probably issued by the \code{predict} method for your
model \code{object}.
Set \code{type} to a legal value as described
on the help page for the \code{\link{predict}} method for your object.

%spacer
\bold{o}\code{ Error: predicted values are out of ylim, try clip=FALSE}

Probably \code{plotmo} has incorrectly determined the expected range
of the response, and hence also \code{ylim}.
Re-invoke \code{plotmo} with \code{clip=FALSE}.
See the section ``The \code{clip} argument''.

%spacer
\bold{o}\code{ Error: predict.lm(xgrid, type="response") returned the wrong length}

\bold{o}\code{ Warning: 'newdata' had 100 rows but variable(s) found have 30 rows}

\bold{o}\code{ Error: variable 'x' was fitted with type "nmatrix.2" but type "numeric" was supplied}

\bold{o}\code{ Error in model.frame: invalid type (list) for variable 'x[,3]'}

These and similar messages usually mean that \code{predict} is
misinterpreting the new data generated by \code{plotmo}.

The underlying issue is that many \code{predict} methods, including
\code{predict.lm}, seem to reject any reasonably constructed new data
for certain models.  The work-around is to simplify or standardize the
way the model function is called.  Use a formula and a data frame, or
at least explicitly name the the variables rather than passing a
matrix.  Use simple variable names (so \code{x1} rather than
\code{dat$x1}, for example).

If the symptoms persist after changing the way the model is called,
and the model is not one of those listed in ``Which variables are
plotted'', it is possible that the model class is not supported
by \code{plotmo}.  See ``Extending plotmo''.

%spacer
\bold{o}\code{ Error: get.plotmo.x.default cannot get the x matrix}

This and similar messages mean that \code{plotmo} cannot get the data
it needs from the model \code{object}.

You can try simplifying and standardizing the way the
model function is called, as described above.
Perhaps you need to use \code{keepxy} or similar in the call to the
model function, so the data is attached to the object and available for
\code{plotmo}.
Is a variable that was used to build the model no longer available in
the environment when \code{plotmo} is called?

%spacer
\bold{o}\code{ Error: this object is not supported by plotmo}

\code{Plotmo}'s default methods are insufficient for your model object.
See ``Extending plotmo'' above (and contact the author --- this is often
easy to fix).

%spacer
\bold{FAQ}

\bold{o} \emph{I am not seeing any interaction plots.  How can I change that?}

Use \code{all2=TRUE}.
By default, degree2 plots are drawn only for some types of model.
See the section ``Which variables are plotted?''.

\bold{o} \emph{The \code{persp} display is unnaturally jagged. How can I change that?}

Use \code{clip=FALSE}.
The jaggedness is probably an artifact of the way \code{\link{persp}} works
at the boundaries.
You can also try increasing \code{ngrid2}.

\bold{o} \emph{The \code{image} display has blue ``holes'' in it. What gives?}

The holes are areas where the predicted response is out-of-range.
Try using \code{clip=FALSE}.

\bold{o} \emph{I want to add lines or points to a plot created by \code{plotmo}.
and am having trouble getting my axis scaling right. Help?}

Use \code{do.par=FALSE} or \code{do.par=2}.
With the default \code{do.par=TRUE}, \code{plotmo} restores the
\code{\link{par}} parameters and axis scales to
their values before \code{plotmo} was called.
}
\author{Stephen Milborrow}
\seealso{
  \code{www.milbo.org/rpart-plot/prp.pdf} has a section on \code{plotmo}
}
\examples{
library(rpart)
data(kyphosis)
rpart.model <- rpart(Kyphosis~., data=kyphosis)
plotmo(rpart.model, type="prob", nresponse="present")
}
\keyword{partial dependence}
\keyword{regression}
