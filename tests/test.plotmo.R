# test.plotmo.R
# This does a basic sanity test of plotmo.
# For more comprehensive tests, see plotmo/src/tests.
library(plotmo)
library(earth)
library(rpart)
data(kyphosis)
rpart.model <- rpart(Kyphosis~., data=kyphosis)
plotmo(rpart.model, type="vec", trace=2)
