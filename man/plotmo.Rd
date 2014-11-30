\name{plotmo}
\alias{plotmo}
\concept{partial dependence plot}
\title{Plot a model's response over a range of predictor values}
\description{
Plot a model's response when varying one or two predictors while
holding the other predictors constant.  A poor man's partial
dependence plot.

See the package vignette
\dQuote{\href{../doc/plotmo-notes.pdf}{Notes on the \code{plotmo} package}}.
}
\usage{
plotmo(object = stop("no 'object' arg"),
       type=NULL, nresponse = NA, clip = TRUE, ylim = NULL,
       center = FALSE, ndiscrete = 5,
       degree1 = TRUE, all1=FALSE, degree2 = TRUE, all2=FALSE, int.only.ok = TRUE,
       grid.func = median, grid.levels = NULL,
       col.response = 0, cex.response = NULL, pch.response = 20,
       jitter.response=0, npoints = -1, inverse.func = NULL,
       level = 0, shade.pints = "mistyrose2", shade2.pints = "mistyrose4",
       trace = 0,
       nrug = 0, col.degree1 = 1, lty.degree1 = 1, lwd.degree1 = 1,
       col.smooth = 0, lty.smooth = 1, lwd.smooth = 1, smooth.f = .5,
       func = NULL, col.func = "lightblue", lty.func = 1, lwd.func = 1,
       ngrid1 = 50,  grid = FALSE,
       type2 = "persp", ngrid2 = 20,
       col.image = gray(0:10/10), col.persp = "lightblue",
       theta = NA, phi = 30, dvalue = 1, shade = 0.5,
       do.par = TRUE, caption = NULL, main = NULL,
       xlim = NULL, xlab = "", ylab = "", cex = NULL, cex.lab = 1,
       xflip = FALSE, yflip = FALSE, swapxy = FALSE, se = 0, ...)
}
\arguments{
\bold{To start off, look at the arguments
  \code{object},
  \code{type},
  \code{clip},
  \code{col.response} and
  \code{do.par}.}

  \item{object}{
     Model object.
  }
  \item{type}{
     Type parameter passed to \code{\link{predict}}.
     For allowed values see the \code{predict} method for
     your \code{object} (such as
     \code{\link[earth]{predict.earth}} and \code{\link{predict.rpart}}).
     By default,
     \code{plotmo} tries to automatically select a suitable value
     (usually \code{"response"}; if not it will be printed in the plot caption).
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
     See \dQuote{The \code{clip} argument} section below for details.
  }
  \item{ylim}{Three possibilities:\cr

     (i) \code{NULL} (default) all y axes have same limits
     (where \dQuote{y} is actually \dQuote{z} on degree2 plots).
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
     Index vector specifying which subset of main effect plots to include.
     Default is \code{TRUE}, meaning all degree1 plots (the \code{TRUE} gets recycled).
     Use \code{FALSE} (or \code{0}) for no degree1 plots.\cr
     Note that this indexes \code{plotmo} plots, not columns in \code{x}.
     Probably the easiest way to use this argument (and \code{degree2}) is to
     first use the default (and possibly \code{all1=TRUE})
     to plot all figures.  This shows how the figures are numbered.
     Then replot using \code{degree1} to select the figures you want,
     for example, \code{degree1=c(1,3)}.\cr
     \bold{New in version 1.3-0}: \code{degree1} may be a character vector
     specifying which variables to plot e.g. \code{degree1=c("wind", "vis")}.
     Note: \code{\link{grep}} is used for matching.  Thus \code{"wind"} will match
     all variables that have \code{"wind"} in their names. Use \code{"^wind$"}
     to match only the variable named \code{"wind"}.
  }
  \item{all1}{
     Default is \code{FALSE}.
     Use \code{TRUE} to plot all predictors,
     not just those usually selected by \code{plotmo}.
     See \dQuote{Which variables are plotted?} below.
     The \code{all1} argument increases the number of plots;
     the \code{degree1} argument reduces the number of plots.
  }
  \item{degree2}{
     Index vector specifying which subset of interaction plots to include.
     Default is \code{TRUE}, meaning all degree2 plots.\cr
     \bold{New in version 1.3-0}: \code{degree2} may be a character vector
     specifying which variables to plot
     (\code{\link{grep}} is used for matching).
  }
  \item{all2}{
     Default is \code{FALSE}.
     Use \code{TRUE} to plot all pairs of predictors,
     not just those usually selected by \code{plotmo}.
     The \code{all2} argument increases the number of plots;
     the \code{degree2} argument reduces the number of plots.
  }
  \item{int.only.ok}{
     \bold{New in version 2.0.0.}
     Plot the model even if it is an intercept-only model.
     Do this by plotting a single degree1 plot for the first predictor.\cr
     Use \code{int.only.ok=FALSE} to instead issue an error message
     for intercept-only models (which was the behaviour prior to version 2.0.0).
  }
  \item{grid.func}{
     Function applied to columns of the \code{x} matrix to fix the values of
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
     Plotting these points is most helpful only when the model has a single predictor.\cr
     Can be a vector, for example,\cr
     \code{col.response=as.numeric(survived)+2}.\cr
     Default is 0, don't plot the response.
  }
  \item{cex.response}{
     Relative size of response points.
     Default is \code{NULL}, meaning automatic.
     Applies only if \code{col.response!=0}.
  }
  \item{pch.response}{
     Plot character for response points.
     Default is \code{20}.
     Applies only if \code{col.response!=0}.
  }
  \item{jitter.response}{
     Amount to jitter the response points.
     Applies only if \code{col.response!=0}.
     Default \code{0}, no jitter.
     A typical useful value is \code{.3}, but it depends on the data.
     Points are jittered horizontally and vertically.
     Note: the points for factors and discrete variables (see the \code{ndiscrete} argument)
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
  \item{level}{
     \bold{New in version 2.0.0.}
     Draw estimated confidence or prediction interval bands at the given \code{level}.
     Default is 0, bands not plotted.
     Else a fraction, something like \code{level=.95}.
     See \dQuote{Prediction intervals} below.
  }
  \item{shade.pints}{
     Color of \code{level} bands. Default is \code{"mistyrose2"}, a pale pink.
     Applies only if \code{level > 0}.
  }
  \item{shade2.pints}{
     Color of additional \code{level} bands.  Default is \code{"mistyrose4"}.
     Applies only if \code{level > 0} and currently only for \code{lm} models.
  }
  \item{trace}{
     Default is \code{0}.\cr
     Use \code{1} (or \code{TRUE}) to trace operation.\cr
     Use values greater than \code{1} for more detailed tracing.\cr
     Use \code{-1} to inhibit the \code{grid:} message usually issued by \code{plotmo}.\cr
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
  \item{smooth.f}{
     \code{f} parameter in internal call to \code{\link{lowess}}.
     Default is \code{.5}.
     Applies only if \code{col.smooth!=0}.
  }
  \item{func}{
     Superimpose \code{func(x)} if \code{func} is not \code{NULL}.
     Default is \code{NULL}.
     This is useful if you are comparing the model to a known function.
     The \code{func} is called for each plot with a single argument which
     is a data frame with columns in the same order as the predictors
     in the \code{formula} or \code{x} used to build the model.
     Use \code{trace=1} to see the column names and first few rows of this dataframe.
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
     Use \code{trace=1} to see the calculated value for \code{theta}.
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
    appropriate to display multiple plots on the same page
    (this adjusts \code{mfrow}, \code{cex}, \code{mar}, and \code{mgp}).\cr
    Use \code{FALSE} to use the current graphics settings,
    for example if you want to add plots to an existing multifigure page.\cr
    The value \code{2} means act like \code{TRUE} but do not restore
    the \code{par} settings to their original state when \code{plotmo} exits.
    This is useful if you want to add a few more plots on the same page
    after running \code{plotmo}.
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
  \item{xlim}{
     Default is \code{NULL}, meaning automatically calculate \code{xlim} for each graph.
     Else used the given \code{xlim} for all \code{degree1} graphs.
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
  }
  \item{cex}{
    Character expansion.
  }
  \item{cex.lab}{
    Relative size of axis labels and text. Default \code{1}.
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
  }
  \item{se}{
     Deprecated.  Please use the \code{level} argument instead.
     So instead of \code{se=2} use \code{level=.95}.
  }
  \item{\dots}{
     Extra arguments are passed on to the plotting functions.
     (For \code{persp} plots, \code{ticktype="d", nticks=2} is useful.)
  }
}
\author{Stephen Milborrow}
\seealso{
Please see the package vignette
\dQuote{\href{../doc/plotmo-notes.pdf}{Notes on the \code{plotmo}} package}.

There is section on \code{plotmo} in the \code{rpart.plot} vignette
\dQuote{\href{http://www.milbo.org/rpart-plot/prp.pdf}{Plotting \code{rpart} trees with \code{prp}}}.
}
\examples{
if (require(rpart)) {
    data(kyphosis)
    rpart.model <- rpart(Kyphosis~., data=kyphosis)
    plotmo(rpart.model, type="prob", nresponse="present")
}
if (require(earth)) {
    data(ozone1)
    earth.model <- earth(O3 ~ ., data=ozone1, degree=2, nk=21)
    plotmo(earth.model)
}
}
\keyword{partial dependence}
\keyword{regression}
