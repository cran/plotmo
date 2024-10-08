Changes to the plotmo package
-----------------------------

## 3.6.4 Aug 29, 2024

Updates for R version 4.4.1

Fixed an issue with gbm3 version 3.0.
Thanks to Marcia Barbosa for help on this.

## 3.6.3 Feb 16, 2024

Updates for R version 4.3.2.
For example, had to change "sort.unique" to "sort_unique".

Removed dependency on possibly orphaned package TeachingDemos.

## 3.6.2 May 21, 2022

Minor updates for R version 4.2.0.

## 3.6.1 Jun 2, 2021

Minor updates for R version 4.1.0.
These updates quieten some warnings from sprintf when plotmo's trace flag is set.
Also updated some of the test scripts.

## 3.6.0 Sep 12, 2020

We now have better support for models with unusual variable names.
For example, variable names with spaces in them, and formula terms
like "as.numeric(x1)". This required a fairly large change to the
handling of formulas.

We now support models like "earth(formula, data=func(data))",
where the data argument is a function call.

Minor code change because base::range no longer seems to work with Date objects.

Better support for residuals plots for earth-glm models.

Support for the "ordinal" package ("clm" models).

Basic support for "parsnip" models.

Minor documentation updates.

Updated the libraries shared with the earth and plotmo packages.

Extended the test scripts and updated them for R version 4.0.2.

## 3.5.7 Apr 15, 2020

Added new dot arguments "prednames.abbreviate" and "prednames.minlength".
  o Use prednames.abbreviate=FALSE for full predictor names in graph axes.
    (The default is prednames.abbreviate=TRUE.)
  o The "prednames.minlength" argument is passed on internally to base::abbreviate().

Reinstated the tests for the emma package (were removed before because
emma gave the message "package 'clusterSim' could not be loaded").

## 3.5.6 Oct 26, 2019

The family of a model can now be a string (as well as a "family" object).
This allows better support of glmnet objects.

## 3.5.5 June 27, 2019

S4 models wrapped in caret models are now supported
e.g. train method="svmRadial" (which creates a kernlab ksvm model).

Modifications for glmnet models:
  The glmnet residuals plot now includes the predict arg "s" in the plot title.
  The default ylim for glmnet probability models is now c(0,1).
  For glmnet cv models: we now pass the predict.s argument to plotmo and plotres,
  and plotmo now by default plots a maximum of 25 coefs (the largest coefs).

Updated test scripts for the new random number generator
that came with R version 3.6.0.

## 3.5.4 Apr 6, 2019

Added a reminder to use keepxy=2 for earth if you want to use
plot.earth or plotmo on an earth cross-validation submodel.

Plotmo now requires R version at least 3.4.0.
Minor updates to libraries shared with earth and rpart.plot.

## 3.5.3 March 16, 2019

Extended plotmo to support earth version 5.0.0, which allows multiple
responses using the Formula package.  Plotmo now also has partial
support for other models also created using Formula (as well as those
that use formula). Added "Depends: Formula" to the DESCRIPTION.

Binomial pair responses are now more uniformly converted to a
"fraction true" before plotting.

If nresponse is not specified for multiple response models, plotmo now
defaults to nresponse=1 with a warning (whereas previous versions of
plotmo issued an error message).

Updates to the libraries shared with earth.

## 3.5.2  Jan 2, 2019

Improved support for models specified with a formula containing an offset term.

The grid.levels argument can now be used with pmethod="partdep".

## 3.5.1  Nov 23, 2018

Can now plot multinomial models from the "pre" package.

Tweaked linmod.R to better handle models with all-zero residuals,
and updated the documentation.

Minor changes to internal function calls to prevent warnings when
options(warnPartialMatchArgs=TRUE).

Added "LazyData: yes" to the DESCRIPTION file.

## 3.5.0  Aug 19, 2018

The default pegged value of background variables has changed in this
version, but only for logical and factor variables.  For these
variables the value occurring most often in the training data is used
as the background value.  (In previous plotmo versions, the first
level of factors was used.  But the majority level seems more
consistent with the median used for numerics.  Also, in previous
versions logicals and integers were sometimes incorrectly converted to
numeric.)
Note this change doesn't affect pmethod="partdep" and "apartdep",
which continue to behave as in previous versions.

We now support base::Date variables.

Plotmo now has better support for caret rpart models with factor predictors.

## 3.4.2  July 3, 2018

Added support for the partykit and evtree packages.
Thanks to Achim Zeilis for his help.

Plotmo is now more intelligent about maximizing the number of
degree2 plots in the 4x4 grid.

Minor updates to linmod.R and linmod.methods.R.

## 3.4.1  June 8, 2018

If plotting a probability and pt.col is specified, we now scale the
response range to 0...1 so the points are displayed on the probability
scale.

Expanded is.predict.prob() function for more models.

Fixed a minor bug in pmethod="partdep" which sometimes incorrectly
caused an error message under certain conditions when there is only
one predictor (added a missing drop=FALSE).

Enhanced support for the mlr package (but we can't support mlr
objects properly until the call is saved with WrappedModels).

Enhanced support for the caret package (we now use get.singles and
get.pairs on the submodel).

## 3.4.0  May 31, 2018

If predict.rpart is predicting a probability, plotmo now recognizes
that and sets ylim=c(0,1) appropriately.

Plotting of intercept-only models was slightly inconsistent. Fixed that.

We now attempt to better set the default nticks in persp plots.

We now position the labels in persp plots slightly better along
the axes (they were sometimes too far away from the front corner).

When degree2 is exactly two strings, we now assign the x1 and x2 axes
in the order specified in degree2 (although persp plots still get
rotated for optimum visibility of the surface, and this rotation can
reverse the order of the axes).

Added basic support for the mlr package (see test.mlr.R).

Documentation updates, especially to modguide.pdf and linmod.R.

## 3.3.7  May 15, 2018

Added a README file.

If degree2 is exactly two strings, plotmo now prints just that degree2
plot e.g. degree2=c("wind", "humidity").  We plot the variable pair
even that pair isn't used in the model (because we implicitly set
all2=TRUE if degree2 is two strings).

If degree1 is of type character, we now plot the variable even if it
isn't used in the model (because we implicitly set all1=TRUE if
degree1 is is of type character).

For the qq plot in plotres, changed the diagonal qq line to dotted
black.  This gives more compatibility with plot.lm, and also means
that the legend for the density subplot along the bottom of the qq
plot (with info=TRUE) isn't mistakenly assumed to apply to the main
plot.  For the old behavior use qqline.col="gray", qqline.lty=1.

Added basic support for the "pre" package (using the importance
function in that package).

Fixed minor bug: the plotmo grid wasn't printed if ylim was specified by the user.

The vignettes are now compressed with gs and qpdf as in tools::compactPDF,
(but that happens outside the standard CRAN build system).  It does mean
that the tar.gz file for plotmo is a little smaller (now 1155 kByte).

## 3.3.6  Mar 20, 2018

Minor documentation updates.

## 3.3.5  Feb 26, 2018

Added support for package gam version 1.15 and higher (the S3 class of
gam objects changed from "gam" to "Gam" to prevent clashes with the mgcv
package).  Plotmo now works with both the old and new versions of gam.

## 3.3.4  July 26, 2017

Added support for glmnetUtils objects.

## 3.3.3  May 4, 2017

Error "glmnet.formula must be called with use.model.frame=TRUE"
is now issued when necessary.

Tweaked test scripts because cosso models fail with R version 3.4.0.

## 3.3.2  Dec 2, 2016

Support for the C50 package.

Better handling of NA and 0 colors in plot_glmnet.

Better messages to the user for models with too many variables to fit on a page.

With all2=2, plotmo will now plot up to a maximum of all pairs of 20
variables (and as always, with all2=TRUE plotmo will plot a maximum
of all pairs of 7 variables).

## 3.3.1  Nov 24, 2016

When choosing which variables to plot for randomForest models,
variable importance is now calculated using a more correct measure,
viz. one of IncMSE or IncNodePurity (regression models), or
MeanDecreaseAccuracy or MeanDecreaseGini (classifications models).
The second option is used if importance=TRUE was used when building
the model.  Use trace=1 when calling plotmo to see which measure of
importance is used.

The plot_gbm function now displays the gray vertical line at the
correct position when n.trees is specified.

Documentation touchups.

## 3.3.0  Nov 11, 2016

Added support for partial dependence plots (the pmethod argument).

Extended the vignette with new chapters on partial dependence plots
and classification models.

Plotmo's nrug argument now supports quantiles.

The title on persp plots is now better aligned to the degree plot
titles.  The margins for persp plots are now more optimal (they now
give bigger plots when do.par=FALSE and there are also degree1 plots).

Added support for e1071::predict.svm decision.values and probability
arguments.

Fixed error message when plot_gbm was used on multinomial models.
Fixed warnings in plot_gbm when gbm.ntrees is very small (less than 10).

## 3.2.1  Oct 27, 2016

Added support for gbm package version 2.2.  See gbm.backcompat.R.

Extended linmod.R: support for no-intercept models, support for
'keep' argument, better handling of newdata in predict.linmod.
Also extended the tests for linmod.R in inst/slowtests.

## 3.2.0  Sep 7, 2016

The functions plot_gbm and plot_glmnet are now exported and available
for the user.  These functions have been enhanced for this version.

Improved support for gbm and glmnet and related models.

The plotres function now works better with caret "train" models
(but caret support is still a bit minimal).

We now print "plotmo grid:" instead of just "grid:" for context when
it's printed from within a body of code.

Removed deprecated interface functions like get.plotmo.pairs.

Updated dot library functions for eventual move to a dots package.

Revamped the vignettes.

## 3.1.5  Aug 26, 2016

The pt.cex argument now works correctly in plotres QQ plots.

Changed default colors in plot.glmnetx.  The colors stay in the order
they are passed to plot.glmnetx as we move down the rhs of the plot.

Extended test suite to include adabag package.

Fixed code in meta.R which assumed all.equal() always
returned TRUE or FALSE.

Merged the library source file lib.R with the earth
and rpart.plot packages's lib.R.

Updated and extended vignettes.

## 3.1.4  Jul 29, 2015

Added support for the adabag package.

Added imports for standard grDevices, stats, and utils
functions, as now required by CRAN check.

Documentation updates. Thanks to Achim Zeileis for his feedback.

## 3.1.3  Jun 24, 2015

Added plotmo.prolog.cv.glmnet (to handle missing "call" in cv.glmnet objects).
More work on the issue where vars on the rhs of formula are multidimensional.
Documentation updates.

## 3.1.2  Jun 15, 2015

Added the new vignette "Guidelines for S3 Regression Models".
Documentation touchups.

## 3.1.1  May 27, 2015

Removed references needed for old versions of earth.  Fixed a
gbm column naming issue.  Other minor code and document updates.

## 3.1.0  May 6, 2015

Removed references to functions in old versions of earth.

Simplified the way xlim and ylim are calculated internally.

Simplified the way jitter is handled.

If type="probability" or similar, and the response has two
columns, nresponse now automatically defaults to column 2.

Added support for biglm objects.  The predict.biglm method
(unnecessarily) requires that newdata has a response column, so
plotmo adds a dummy response column before calling predict.biglm.

We now find the data argument for formula models even if the argument
is unnamed.

## 3.0.0  Apr 29, 2015

Added the plotres function.

Reworked the internal functions that get the data from the model.

Reparameterized the argument list of plotmo, but maintained
backwards compatibility using the "dots" routines.

## 2.2.1  Jan 7, 2015

If pch.response has type character, we now plot the
response points as text.

Earth models with no degree1 terms but with degree2 terms were
incorrectly labelled as intercept-only models.  Fixed that.

Changes to match changes to earth's predict.varmod interval argument.

## 2.2.0  Dec 10, 2014

Fixed incorrect printing of some messages when trace=-1.

Expansions to check.index for earth.

Documentation touchups.

## 2.1.0  Nov 30, 2014

Added a vignette "Notes on the plotmo package".

Some more functions are now exported to allow
earth::plotmor to easily get the model data

Some documentation touchups as usual.

## 2.0.0  Nov 19, 2014

Plotting of prediction or confidence levels is now more comprehensive.
We now allow both prediction and confidence intervals to be plotted
for those predict methods that support it on new data (currently only
lm).  The "se" argument is now deprecated and superseded by the
"limit" argument (you will get a warning).

Plotmo will now plot the model even if it is an intercept-only model.
Use int.only.ok=FALSE for the old behaviour (i.e., issue an error for
intercept-only models).

The "grid:" message in now printed for only multiple predictor models.
Remember that you can always suppress this message in any case with
trace=-1.

The xlim argument is now supported.  Typically only useful
if only one degree1 plot.

Plotmo now supports quantreg and quantregForest objects.

Basic support for the AMORE package has been provided.  Thanks to
Bernard Nolan and David Lorenz for this.  But this has been commented
out in the source code to avoid having "suggests(AMORE)" in the plotmo
DESCRIPTION file.  To use functions, search for AMORE in the plotmo
source code, and cut and paste the commented-out code into your
environmemt.

The default pch.response is now 20 (was 1).

The default cex.response is now NULL (meaning automatic, was 1).

Minor other changes to fix formatting of captions etc.

## 1.3-3  Feb 4, 2014

Clerical changes to satisfy recent CRAN check requirements.

## 1.3-2  Dec 1, 2011

You can now use trace=-1 to inhibit the "grid: " message.

Removed a call to .Internal(persp)

## 1.3-1  Sep 16, 2011

Fixed an minor incorrect message introduced in the previous release.

## 1.3-0  Sep 15, 2011

You can now specify variables by name in degree1 and degree2.
Suppressed annoying "Warning: surface extends beyond the box".
We no longer issue an incorrect err msg if data frame has an "AsIs" field.

## 1.2-6

Removed an incorrect stopifnot.integer(y.column) in plotmo_y.wrapper

## 1.2-5 Jun 11, 2011

Fixed an incorrect stop when trace>0 and x had no column names.
We no longer print the plot index in the plot title when all1 or
   all2 specified but also degree1 and degree2.
Added get.plotmo.default.type.fda
Touchups to the documentation.

## 1.2-4 Apr 27, 2011

Removed hooks for the earth package (which are no longer necessary
with earth 2.6-2).  The file plotmo.methods.R was deleted.

Added the grid argument.

## 1.2-3 Apr 17, 2011

This package no longer needs the earth package.  However the current
  earth (2.6-1) needs some hooks in this package to build.
  After earth 2.6-2 is on CRAN that will no longer be necessary,
  and the hooks will be removed from this package.
We now have better error reporting for bad y's.
We now have better jittering of response points with a binary response.

## 1.2-0 Apr 12, 2011

Added ndiscrete arg (variables with a small number of levels
   are now plotted as "blocks", like factors).
Added smooth.col and related args (plotmo can add a loess line).
Made tweaks necessary because earth now imports this package.
Added dvalue and npoints args.
Added center arg (preliminary implementation).
Added basic support for lars, nnet, and knn3 models.
Jittering now works better.
We now jitter response points for factors and discrete variables by default.
plotmo is now faster:
   We cache the plot data to avoid calling predict twice for each plot
   For discrete vars and factors we only call predict for their original values
   ngrid1 is much smaller (ok to do that because of ndiscrete arg)
Better error reporting for illegal args.
Reduced the number of default colors (just grays and lightblue now).
Out-of-range values in image plots are now plotted in blue.
Fixed an issue where the wrong environment could be used.
Better error reporting for unsupported models.
Fixed handling of factors with non contiguous levels
Modified test scripts to conform to R 2.13.0's way of printing numbers
Numerous other document and code touch ups.

## 1.0-1 Apr 01, 2011

plotmo was printing degree1 graphs for all used earth predictors,
not just those appearing in degree1 terms.  Fixed that.

plotmo was not handling all1=TRUE correctly for earth models with
factor predictors.  Fixed that.

## 1.0-0 Mar 31, 2011

Initial release.  Moved plotmo from earth 2.5-1 to here.
