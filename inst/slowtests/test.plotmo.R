# test.plotmo.R: regression tests for plotmo
# Many of these tests are culled from man page examples and modified to try to confuse plotmo.
# Many of the plots are plotted twice so you can visually check by comparing
# plots in the same window, they should be substantially the same.
# Stephen Milborrow, Petaluma Jan 2007

print(R.version.string)
print(citation("rpart.plot"))

library(earth)
data(ozone1)
data(etitanic)
options(warn=1) # print warnings as they occur
if(!interactive())
    postscript(paper="letter")
Trace <- 0
dopar <- function(nrows, ncols, caption = "")
{
    cat("                             ", caption, "\n")
    earth:::make.space.for.caption(caption)
    par(mfrow=c(nrows, ncols))
    par(mar = c(3, 3, 1.7, 0.5))
    par(mgp = c(1.6, 0.6, 0))
    par(cex = 0.7)
}
expect.err <- function(obj) # test that we got an error as expected from a try() call
{
    if(class(obj)[1] == "try-error")
        cat("Got error as expected\n")
    else
        stop("did not get expected try error")
}
caption <- "basic earth test of plotmo"
a <- earth(O3 ~ ., data=ozone1, degree=2)
plotmo(a, degree1=2, degree2=4, caption=caption, trace=TRUE)

caption <- "test 5 x 5 layout"
dopar(1,1,caption)
a <- earth(O3 ~ ., data=ozone1, nk=51, pmethod="n", degree=2)
plotmo(a, xlab="", ylab="", caption=caption, trace=1)

caption <- "test 4 x 4 layout with ylab"
dopar(1,1,caption)
a <- earth(O3 ~ ., data=ozone1, nk=30, pmethod="n", degree=2)
plotmo(a, xlab="", ylab="ozone", caption=caption, trace=2)

caption <- "test 3 x 3 layout"
dopar(1,1,caption)
a <- earth(O3 ~ ., data=ozone1, nk=16, pmethod="n", degree=2)
plotmo(a, xlab="", ylab="", caption=caption, trace=3)

caption <- "test 2 x 2 layout"
dopar(1,1,caption)
a <- earth(O3 ~ ., data=ozone1, nk=9, pmethod="n", degree=2)
plotmo(a, xlab="", ylab="", caption=caption, trace=-1) # trace=-1 inhibits "grid:" message

caption <- "test 1 x 1 layout"
dopar(1,1,caption)
a <- earth(O3 ~ ., data=ozone1, nk=4, pmethod="n", degree=2)
plotmo(a, xlab="", ylab="", caption=caption, trace=Trace)

caption <- "test plotmo basic params"
a <- earth(O3 ~ ., data=ozone1, degree=2)
dopar(3,2,caption)
set.seed(1) # needed for reproducibility because of sample for rug in plotmo
plotmo(a, do.par=FALSE, degree1=1, nrug=-1, degree2=F, caption=caption,
        main="test main", xlab="test xlab", ylab="test ylab", trace=Trace)
set.seed(1)
plotmo(a, do.par=FALSE, degree1=F, degree2=4, grid.func=mean, col.persp="white", ngrid2=10, phi=40, trace=Trace)
set.seed(1)
plotmo(a, do.par=FALSE, degree1=1, lty.degree1=2, lwd.degree1=4, col.degree1=2, nrug=300, degree2=F, main="nrug=300", trace=Trace)
set.seed(1)
plotmo(a, do.par=FALSE, degree1=1, nrug=-1, degree2=F, main="nrug=-1", trace=Trace)
set.seed(1)
plotmo(a, do.par=FALSE, degree1=1, nrug=500, ngrid1=50, degree2=F, main="ngrid1=50 nrug=500", trace=Trace)
plotmo(a, do.par=FALSE, degree1=NA, degree2=1, phi=60, box=F, r=100) # dots args

caption <- "test plotmo xlim and ylim"
a <- earth(O3 ~ ., data=ozone1, degree=2)
dopar(5,3,caption)
plotmo(a, do.par=FALSE, degree1=2:3, degree2=4, caption=caption, xlab="ylim=default", trace=Trace)
plotmo(a, do.par=FALSE, degree1=2:3, degree2=4, ylim=NA, xlab="ylim=NA", trace=Trace)
plotmo(a, do.par=FALSE, degree1=2:3, degree2=4, ylim=c(0,20), xlab="ylim=c(0,20)", trace=Trace)
plotmo(a, do.par=FALSE, degree1=2:3, degree2=4, xlim=c(190,250))
plotmo(a, do.par=FALSE, degree1=2:3, degree2=4, xlim=c(190,250), ylim=c(11,18))

# term.plot calls predict.earth with an se parameter, even with termplot(se=FALSE)

caption <- "basic earth test against termplot"
dopar(4,4,caption)
earth:::make.space.for.caption("test caption1")
a <- earth(O3 ~ ., data=ozone1, degree=2)
plotmo(a, do.par=FALSE, ylim=NA, caption=caption, degree2=FALSE, trace=Trace)
cat("Ignore two warnings: predict.earth ignored argument \"se.fit\"\n")
termplot(a)

caption <- "test change order of earth predictors and cex"
dopar(4,4,caption)
a <- earth(doy ~ humidity + temp + wind, data=ozone1, degree=2)
plotmo(a, do.par=FALSE, ylim=NA, caption=caption, degree2=c(1,2), trace=Trace, cex=1)
termplot(a)

caption <- "test all1=TRUE"
a <- earth(doy ~ humidity + temp + wind, data=ozone1, degree=2)
plotmo(a, caption=caption, all1=TRUE)
caption <- "test all2=TRUE"
print(summary(a))
plotmo(a, caption=caption, all2=TRUE)

oz <- ozone1[150:200,c("O3","temp","humidity","ibh")]
a.glob <- earth(O3~temp+humidity, data=oz, degree=2)
ad.glob <- earth(oz[,2:3], oz[,1], degree=2)
func1 <- function()
{
    caption <- "test environments and finding the correct data"
    dopar(4,4,caption)

    plotmo(a.glob, do.par=FALSE, main="a.glob oz",
          degree1=1, all2=1, degree2=1, type2="im", trace=T,
          col.response=3, pch.response=20)
    mtext(caption, outer=TRUE, font=2, line=1.5, cex=1)

    plotmo(ad.glob, do.par=FALSE, main="ad.glob oz",
          degree1=1, all2=1, degree2=1, type2="im", trace=T,
          col.response=3, pch.response=20)

    a <- earth(O3~temp+humidity, data=oz, degree=2)
    plotmo(a, do.par=FALSE, main="a oz",
           degree1=1, all2=1, degree2=1, type2="im", trace=T,
           col.response=3, pch.response=20)

    ad <- earth(oz[,2:3], oz[,1], degree=2)
    plotmo(ad, do.par=FALSE, main="ad oz",
           degree1=1, all2=1, degree2=1, type2="im", trace=T,
           col.response=3, pch.response=20)

    oz.org <- oz
    oz10 <- 10 * oz # multiply by 10 so we can see by the axis labels if right data is being used
    oz <- oz10      # oz is now local to this function, but multiplied by 10
    a.oz10 <- earth(O3~temp+humidity, data=oz, degree=2)
    a.oz10.keep <- earth(O3~temp+humidity, data=oz, degree=2, keepxy=TRUE)
    plotmo(a.oz10, do.par=FALSE, main="a oz10",
           degree1=1, all2=1, degree2=1, type2="im", trace=T,
           col.response=3, pch.response=20)

    ad.oz10 <- earth(oz[,2:3], oz[,1], degree=2)
    ad.oz10.keep <- earth(oz[,2:3], oz[,1], degree=2, keepxy=TRUE)
    plotmo(ad.oz10, do.par=FALSE, main="ad oz10",
           degree1=1, all2=1, degree2=1, type2="im", trace=T,
           col.response=3, pch.response=20)

    func2 <- function() {
        a.func <- earth(O3~temp+humidity, data=oz10, degree=2)
        plotmo(a.func, do.par=FALSE, main="a.func oz10",
               degree1=1, all2=1, degree2=1, type2="im", trace=T,
               col.response=3, pch.response=20)

        ad.func <- earth(oz10[,2:3], oz10[,1], degree=2)
        plotmo(ad.func, do.par=FALSE, main="ad.func oz10",
               degree1=1, all2=1, degree2=1, type2="im", trace=T,
               col.response=3, pch.response=20)

        caption <- "test environments and finding the correct data, continued"
        dopar(4,4,caption)

        oz <- .1 * oz.org
        a.func <- earth(O3~temp+humidity, data=oz, degree=2)
        plotmo(a.func, do.par=FALSE, main="a.func oz.1",
               degree1=1, all2=1, degree2=1, type2="im", trace=T,
               col.response=3, pch.response=20)

        ad.func <- earth(oz[,2:3], oz[,1], degree=2)
        plotmo(ad.func, do.par=FALSE, main="ad.func oz.1",
               degree1=1, all2=1, degree2=1, type2="im", trace=T,
               col.response=3, pch.response=20)

        plotmo(a.oz10.keep, do.par=FALSE, main="func1:a.oz10.keep",
           degree1=1, all2=1, degree2=1, type2="im", trace=T,
           col.response=3, pch.response=20)

        plotmo(ad.oz10.keep, do.par=FALSE, main="func1:ad.oz10.keep",
           degree1=1, all2=1, degree2=1, type2="im", trace=T,
           col.response=3, pch.response=20)

        try <- try(plotmo(a.oz10, do.par=FALSE, main="func1:a.oz10",
           degree1=1, all2=1, degree2=1, type2="im", trace=T,
           col.response=3, pch.response=20, do.par=FALSE))
        expect.err(try)

        cat("Expect error msg (because get.plotmo.x calculated using oz.1 i.e. func2.oz)\n")
        try <- try(plotmo(ad.oz10, do.par=FALSE, main="func1:ad.oz10",
           degree1=1, all2=1, degree2=1, type2="im", trace=T,
           col.response=3, pch.response=20))
        expect.err(try)
    }
    func2()

    y  <- 3:11
    x1 <- c(1,3,2,4,5,6,6,6,6)
    x2 <- c(2,3,4,5,6,7,8,9,10)
    frame <- data.frame(y=y, x1=x1, x2=x2)
    foo <- function()
    {
        lm.18.out <- lm(y~x1+x2)
        x1[2] <- 18
        y[3] <- 19
        frame <- data.frame(y=y, x1=x1, x2=x2)
        list(lm.18.out   = lm.18.out,
             lm.18       = lm(y~x1+x2),
             lm.18.keep  = lm(y~x1+x2, x=TRUE, y=TRUE),
             lm.18.frame = lm(y~x1+x2, data=frame))
    }
    temp <- foo()
        lm.18.out   <- temp$lm.18.out
        lm.18       <- temp$lm.18
        lm.18.keep  <- temp$lm.18.keep
        lm.18.frame <- temp$lm.18.frame

    # following should all use the x1 and y inside foo

    cat("==lm.18.out\n")
    plotmo(lm.18.out,   trace=1, main="lm.18.out",
           do.par=FALSE, degree1=1, clip=FALSE, ylim=c(0,20),
           col.response=2, pch.response=20)

    cat("==lm.18\n")
    plotmo(lm.18,       trace=1, main="lm.18",
           do.par=FALSE, degree1=1, clip=FALSE, ylim=c(0,20),
           col.response=2, pch.response=20)

    cat("==lm.18.keep\n")
    plotmo(lm.18.keep,  trace=1, main="lm.18.keep",
           do.par=FALSE, degree1=1, clip=FALSE, ylim=c(0,20),
           col.response=2, pch.response=20)

    cat("==lm.18.frame\n")
    plotmo(lm.18.frame, trace=1, main="lm.18.frame",
           do.par=FALSE, degree1=1, clip=FALSE, ylim=c(0,20),
           col.response=2, pch.response=20)
}
func1()

caption <- "test earth formula versus x,y model"
dopar(4,4,caption)
mtext(caption, outer=TRUE, font=2, line=1.5, cex=1)
a <- earth(O3 ~ ., data=ozone1, degree=2)
plotmo(a, do.par=FALSE, caption=caption, trace=Trace)
a <- earth(ozone1[, -1], ozone1[,1], degree=2)
plotmo(a, do.par=FALSE, trace=Trace)

# single predictor
caption <- "test earth(O3~wind, data=ozone1, degree=2), single predictor"
dopar(2,2,caption)
a <- earth(O3~wind, data=ozone1, degree=2)
plotmo(a, trace=Trace)

caption = "se=2, earth(doy~humidity+temp+wind, data=ozone1) versus termplot (expect no se lines)"
dopar(3,2,caption)
mtext(caption, outer=TRUE, font=2, line=1.5, cex=1)
a <- earth(doy~humidity + temp + wind, data=ozone1, degree=2)
cat("Ignore warning: predict.earth ignored argument \"se\"\n")
termplot(a)
plotmo(a, do.par=FALSE, ylim=NA, degree2=c(1:2), clip=FALSE, caption=caption, trace=Trace)

# test fix to bug reported by Joe Retzer, FIXED Dec 7, 2007
N <- 650
set.seed(2007)
q_4    <- runif(N, -1, 1)
q_2102 <- runif(N, -1, 1)
q_2104 <- runif(N, -1, 1)
q_3105 <- runif(N, -1, 1)
q_3106 <- runif(N, -1, 1)
q_4104 <- runif(N, -1, 1)
q_6101 <- runif(N, -1, 1)
q_6103 <- runif(N, -1, 1)
q_7104 <- runif(N, -1, 1)
q_3109 <- runif(N, -1, 1)
q_4103 <- runif(N, -1, 1)
q_2111 <- runif(N, -1, 1)
q_3107 <- runif(N, -1, 1)
q_3101 <- runif(N, -1, 1)
q_3104 <- runif(N, -1, 1)
q_7107 <- runif(N, -1, 1)
depIndex <- sin(1.0 * q_4 + rnorm(650, sd=.8)) + sin(1.8 * q_2102 + rnorm(650, sd=.8)) + sin(1.3 * q_2104 + rnorm(650, sd=.8)) + sin(1.4 * q_3105 + rnorm(650, sd=.8)) +
            sin(1.5 * q_3106 + rnorm(650, sd=.8)) + sin(1.6 * q_4104 + rnorm(650, sd=.8)) + sin(1.8 * q_6101 + rnorm(650, sd=.8)) + sin(1.8 * q_6103 + rnorm(650, sd=.8)) +
            sin(1.9 * q_7104 + rnorm(650, sd=.8)) + sin(2.0 * q_3109 + rnorm(650, sd=.8))

regDatCWD <- as.data.frame(cbind(depIndex, q_4, q_2102, q_2104, q_3105, q_3106, q_4104, q_6101, q_6103, q_7104, q_3109, q_4103, q_2111, q_3107, q_3101, q_3104, q_7107))
earthobj <- earth(depIndex ~  q_4+q_2102+q_2104+q_3105+q_3106+q_4104+q_6101+q_6103+q_7104+q_3109+q_4103+q_2111+q_3107+q_3101+q_3104+q_7107, data=regDatCWD)
print(summary(earthobj, digits = 2))
plotmo(earthobj)

# long predictor names

a.rather.long.in.fact.very.long.name.q_4 <- q_4
a.rather.long.in.fact.very.long.name.q_2102 <- q_2102
a.rather.long.in.fact.very.long.name.q_2104 <- q_2104
a.rather.long.in.fact.very.long.name.q_3105 <- q_3105
a.rather.long.in.fact.very.long.name.q_3106 <- q_3106
a.rather.long.in.fact.very.long.name.q_4104 <- q_4104
a.rather.long.in.fact.very.long.name.q_6101 <- q_6101
a.rather.long.in.fact.very.long.name.q_6103 <- q_6103
a.rather.long.in.fact.very.long.name.q_7104 <- q_7104
a.rather.long.in.fact.very.long.name.q_3109 <- q_3109
a.rather.long.in.fact.very.long.name.q_4103 <- q_4103
a.rather.long.in.fact.very.long.name.q_2111 <- q_2111
a.rather.long.in.fact.very.long.name.q_3107 <- q_3107
a.rather.long.in.fact.very.long.name.q_3101 <- q_3101
a.rather.long.in.fact.very.long.name.q_3104 <- q_3104
a.rather.long.in.fact.very.long.name.q_7107 <- q_7107
a.rather.long.in.fact.very.long.name.for.the.response <- depIndex
a.rather.long.in.fact.very.long.name.for.the.dataframe <-
        as.data.frame(cbind(
                a.rather.long.in.fact.very.long.name.for.the.response,
                a.rather.long.in.fact.very.long.name.q_4,
                a.rather.long.in.fact.very.long.name.q_2102,
                a.rather.long.in.fact.very.long.name.q_2104,
                a.rather.long.in.fact.very.long.name.q_3105,
                a.rather.long.in.fact.very.long.name.q_3106,
                a.rather.long.in.fact.very.long.name.q_4104,
                a.rather.long.in.fact.very.long.name.q_6101,
                a.rather.long.in.fact.very.long.name.q_6103,
                a.rather.long.in.fact.very.long.name.q_7104,
                a.rather.long.in.fact.very.long.name.q_3109,
                a.rather.long.in.fact.very.long.name.q_4103,
                a.rather.long.in.fact.very.long.name.q_2111,
                a.rather.long.in.fact.very.long.name.q_3107,
                a.rather.long.in.fact.very.long.name.q_3101,
                a.rather.long.in.fact.very.long.name.q_3104,
                a.rather.long.in.fact.very.long.name.q_7107))

a.rather.long.in.fact.very.long.name.for.the.modelA <-
        earth(a.rather.long.in.fact.very.long.name.for.the.response ~
                a.rather.long.in.fact.very.long.name.q_4 +
                a.rather.long.in.fact.very.long.name.q_2102 +
                a.rather.long.in.fact.very.long.name.q_2104 +
                a.rather.long.in.fact.very.long.name.q_3105 +
                a.rather.long.in.fact.very.long.name.q_3106 +
                a.rather.long.in.fact.very.long.name.q_4104 +
                a.rather.long.in.fact.very.long.name.q_6101 +
                a.rather.long.in.fact.very.long.name.q_6103 +
                a.rather.long.in.fact.very.long.name.q_7104 +
                a.rather.long.in.fact.very.long.name.q_3109 +
                a.rather.long.in.fact.very.long.name.q_4103 +
                a.rather.long.in.fact.very.long.name.q_2111 +
                a.rather.long.in.fact.very.long.name.q_3107 +
                a.rather.long.in.fact.very.long.name.q_3101 +
                a.rather.long.in.fact.very.long.name.q_3104 +
                a.rather.long.in.fact.very.long.name.q_7107,
                data = a.rather.long.in.fact.very.long.name.for.the.dataframe)
print(summary(a.rather.long.in.fact.very.long.name.for.the.modelA, digits = 2))
plot(a.rather.long.in.fact.very.long.name.for.the.modelA)
plotmo(a.rather.long.in.fact.very.long.name.for.the.modelA)

a.rather.long.in.fact.very.long.name.for.the.modelC <-
        earth(x = a.rather.long.in.fact.very.long.name.for.the.dataframe[,-1],
          y = a.rather.long.in.fact.very.long.name.for.the.response,
                  degree = 3)
print(summary(a.rather.long.in.fact.very.long.name.for.the.modelC, digits = 2))
plot(a.rather.long.in.fact.very.long.name.for.the.modelC)
plotmo(a.rather.long.in.fact.very.long.name.for.the.modelC)

data(etitanic)
a <- earth(survived ~ pclass+sex+age, data=etitanic, degree=2)
print(summary(a))
plotmo(a, trace=Trace, caption="plotmo with facs: pclass+sex+age")
plotmo(a, trace=Trace, caption="plotmo with facs: pclass+sex+age, all1=T, grid=T", all1=T, grid=T)
plotmo(a, trace=Trace, caption="plotmo with facs: pclass+sex+age, all2=T, grid=\"green\"", all2=T, grid="green")
plotmo(a, trace=Trace, caption="plotmo with facs: pclass+sex+age, all1=T, all2=T, grid=2:3", all1=T, all2=T, grid=2:3)
plotmo(a, trace=Trace, clip=FALSE, degree2=FALSE, caption="plotmo (no degree2) with facs: pclass+sex+age")
plotmo(a, trace=Trace, clip=FALSE, grid.levels=list(pclass="2n", sex="ma"),
       caption="plotmo with grid.levels: pclass+sex+age")
# in above tests, all degree2 terms use facs
# now build a model with some degree2 term that use facs, some that don't
a <- earth(survived ~ pclass+age+sibsp, data=etitanic, degree=2)
print(summary(a))
plotmo(a, caption="plotmo with mixed fac and non-fac degree2 terms", border=NA)
plotmo(a, caption="plotmo with mixed fac and non-fac degree2 terms and grid.levels",
       grid.levels=list(pclass="2n", age=20), # test partial matching of grid levels, and numeric preds
       ticktype="d", nticks=2)

# check detection of illegal grid.levels argument
expect.err(try(plotmo(a, grid.levels=list(pcla="1", pclass="2"))))
expect.err(try(plotmo(a, grid.levels=list(pclass="1", pcla="2"))))
expect.err(try(plotmo(a, grid.levels=list(pcla=1))))
expect.err(try(plotmo(a, grid.levels=list(pcla=c("ab", "cd")))))
expect.err(try(plotmo(a, grid.levels=list(pcla=NA))))
expect.err(try(plotmo(a, grid.levels=list(pcla=Inf))))
expect.err(try(plotmo(a, grid.levels=list(pcla=9))))
expect.err(try(plotmo(a, grid.levels=list(age="ab"))))
expect.err(try(plotmo(a, grid.levels=list(age=NA))))
expect.err(try(plotmo(a, grid.levels=list(age=Inf))))
expect.err(try(plotmo(a, grid.lev=list(age=list(1,2)))))

# more-or-less repeat above, but with glm models
a <- earth(survived ~ pclass+age+sibsp, data=etitanic, degree=2, glm=list(family=binomial))
print(summary(a))
plotmo(a, ylim=c(0, 1), caption="plotmo glm with mixed fac and non-fac degree2 terms")
plotmo(a, ylim=c(0, 1), caption="plotmo glm with mixed fac and non-fac degree2 terms and grid.levels",
       grid.levels=list(pcl="2nd")) # test partial matching of variable name in grid levels
plotmo(a, type="earth", ylim=c(0, 1), caption="type=\"earth\" plotmo glm with mixed fac and non-fac degree2 terms")
plotmo(a, type="link", ylim=c(0, 1), clip=FALSE, caption="type=\"link\" plotmo glm with mixed fac and non-fac degree2 terms")
plotmo(a, type="class", ylim=c(0, 1), caption="type=\"class\" plotmo glm with mixed fac and non-fac degree2 terms")
plotmo(a, ylim=c(0, 1), caption="default type (\"response\")\nplotmo glm with mixed fac and non-fac degree2 terms")
# now with different type2s
mtext("different type2s", outer=TRUE, font=2, line=1.5, cex=1)
plotmo(a, do.par=FALSE, type2="persp",   theta=-20, degree1=FALSE, grid.levels=list(pclass="2nd"))
plotmo(a, do.par=FALSE, type2="contour", degree1=FALSE, grid.levels=list(pclass="2nd"))
plotmo(a, do.par=FALSE, type2="image",   degree1=FALSE, grid.levels=list(pclass="2nd"),
       col.response=as.numeric(etitanic$survived)+2, pch.response=20)
plotmo(a, do.par=FALSE, type="earth", type2="image", degree1=FALSE, grid.levels=list(pclass="2"))

# test vector main

a20 <- earth(O3 ~ humidity + temp + doy, data=ozone1, degree=2, glm=list(family=Gamma))

set.seed(1) # needed for nrug
plotmo(a20, nrug=-1)

set.seed(1) # needed for nrug and npoints
plotmo(a20, nrug=200, caption="Test plotmo with a vector main (and npoints=200)",
       main=c("Humidity", "Temperature", "Day of year", "Humidity: Temperature", "Temperature: Day of Year"),
       col.response="gray", pch.response=".", cex.response=3, npoints=200)

cat("Expect warning below (missing double titles)\n")
plotmo(a20, nrug=-1, caption="Test plotmo with a vector main (and plain smooth)",
       main=c("Humidity", "Temperature", "Day of year", "Humidity: Temperature"),
       col.smooth="indianred")

cat("Expect warning below (missing single titles)\n")
plotmo(a20, nrug=-1, caption="Test plotmo with a vector main (and smooth args)",
       main=c("Humidity", "Temperature"),
       col.smooth="indianred", lwd.smooth=2, lty.smooth=2,  smooth.f=.1,
       col.response="gray", npoints=500)

aflip <- earth(O3~vh + wind + humidity + temp, data=ozone1, degree=2)

# test all1 and all2, with and without degree1 and degree2
plotmo(aflip, all2=T, caption="all2=T")
plotmo(aflip, all2=T, degree2=c(4, 2), caption="all2=T, degree2=c(4, 2)")
plotmo(aflip, all1=T, caption="all1=T")
plotmo(aflip, all1=T, degree1=c(3,1), degree2=NA, caption="all1=T, degree1=c(3,1), degree2=NA")

expect.err(try(plotmo(aflip, no.such.arg=9))) # plotmo: illegal argument "no.such.arg"
expect.err(try(plotmo(aflip, ycolumn=1)))     # ycolumn is no longer legal, use nresponse instead
expect.err(try(plotmo(aflip, title="abc")))   # "title" is illegal, use "caption" instead
expect.err(try(plotmo(aflip, ticktype="d", ntick=3, tic=3, tick=9))) # duplicated arguments "ticktype" "tic" "tick"
expect.err(try(plotmo(aflip, ticktype="d", ntick=3, tic=3))) # duplicated arguments "ticktype" "tic"
expect.err(try(plotmo(aflip, ticktype="s", nt=3))) # nticks is illegal with ticktype="simple"
expect.err(try(plotmo(aflip, tic="s", nt=3))) # nticks is illegal with ticktype="simple"
expect.err(try(plotmo(aflip, tic="s", nt=3))) # nticks is illegal with ticktype="simple"
expect.err(try(plotmo(aflip, adj=8, adj=9))) # Error : duplicated arguments "adj" "adj"
expect.err(try(plotmo(aflip, adj1=8, adj2=9))) # Error : plotmo: illegal argument "adj1"
expect.err(try(plotmo(aflip, yc=8, x2=9))) # "ycolumn" is no longer legal, use "nresponse" instead
expect.err(try(plotmo(aflip, ticktype="d", ntick=3, ti=3))) # Error : "title" is illegal, use "caption" instead ("ti" taken to mean "title")
expect.err(try(plotmo(aflip, ticktype="d", ntick=3, title=3))) # Error : "title" is illegal, use "caption" instead
expect.err(try(plotmo(aflip, ticktype="d", ntick=3, tit=3, titl=7))) # Error : "title" is illegal, use "caption" instead ("tit" taken to mean "title")
expect.err(try(plotmo(aflip, zlab="abc"))) # "zlab" is illegal, use "ylab" instead
expect.err(try(plotmo(aflip, z="abc"))) # "zlab" is illegal, use "ylab" instead ("z" taken to mean "zlab")
expect.err(try(plotmo(aflip, degree1=c(4,1)))) # out of range value in degree2 (allowed index range is 1:3)
expect.err(try(plotmo(aflip, none.such=TRUE))) # illegal argument "all1"
expect.err(try(plotmo(aflip, ntick=3, type2="im"))) # the ntick argument is illegal for type2="image"
expect.err(try(plotmo(aflip, breaks=3, type2="persp"))) # the breaks argument is illegal for type2="persp"
expect.err(try(plotmo(aflip, breaks=99, type2="cont"))) #  the breaks argument is illegal for type2="contour"

# test character degree1 and degree2 (added in plotmo version 1.3-0)

a80 <- earth(O3~., data=ozone1, degree=2)
plotmo(a80, degree1="i", degree2="t",
       caption='degree1="i", degree2="t"')
plotmo(a80, degree1="^temp$", degree2="^dpg$",
       caption='degree1="^temp$", degree2="^dpg$"')
# Expect Warning: "nonesuch1" in degree1 does not match any variables, ditto for degree2
plotmo(a80, degree1=c("temp", "nonesuch1"), degree2=c("nonesuch2", "vis"),
       caption='degree1=c("temp", "nonesuch1"), degree2=c("nonesuch2", "vis")')
# Expect above warnings and also Warning: nothing to plot
plotmo(a80, degree1="nonesuch1", degree2="nonesuch2")

# Test error handling when accessing the original data

lm.bad <- lm.fit(as.matrix(ozone1[,-1]), as.matrix(ozone1[,1]))
expect.err(try(plotmo(lm.bad)))          # this object is not supported by plotmo
expect.err(try(plotmo(99)))              # '99' is not a model object

x <- matrix(c(1,3,2,4,5,6,7,8,9,10,
              2,3,4,5,6,7,8,9,8,9), ncol=2)

colnames(x) <- c("c1", "c2")
x1 <- x[,1]
x2 <- x[,2]
y <- 3:12
df <- data.frame(y=y, x1=x1, x2=x2)
foo1 <- function()
{
    a.foo1 <- lm(y~x1+x2)
    x1 <- NULL
    try <- try(plotmo(a.foo1)) # get.plotmo.x.default cannot get the x matrix
    expect.err(try)
}
foo1()
foo2 <- function()
{
    a.foo2 <- lm(y~x1+x2, data=df)
    df <- NULL
    try <- try(plotmo(a.foo2)) # the original data "df" is no longer available (use x=TRUE in the call to lm?)
    expect.err(try)
}
foo2()
foo3 <- function()
{
    a.foo3 <- lm(y~x) # lm() builds a.foo3 model for which predict doesn't work
    try <- try(plotmo(a.foo3)) # Error : variable 'x' was fitted with type "nmatrix.2" but type "numeric" was supplied
    expect.err(try)
}
foo3()
foo4 <- function()
{
    a.foo4 <- lm(y~x[,1]+x[,2])  # lm() builds a.foo4 model for which predict doesn't work
    try <- try(plotmo(a.foo4)) # Error : predict.lm(xgrid, type="response") returned a response of the wrong length.
    expect.err(try)
}
foo4()
foo5 <- function()
{
    a.foo5 <- lm(y~x1+x2)
    x1 <- c(1,2,3)
    try <- try(plotmo(a.foo5)) # get.plotmo.x.default cannot get the x matrix
    expect.err(try)
}
foo5()
foo6 <- function()
{
    a.foo6 <- lm(y~x1+x2)
    y[1] <- NA
    try <- try(plotmo(a.foo6, col.response=3)) # Error: get.plotmo.x.default cannot get the x matrix
    expect.err(try)
}
foo6()
foo7 <- function()
{
    a.foo7 <- lm(y~x1+x2)
    y[1] <- Inf
    plotmo(a.foo7, col.response=3) # Warning: non finite values returned by get.plotmo.y, see print above
}
foo7()
# TODO removed because this now works (why?)
# foo8 <- function()
# {
#     i <- 1
#     a.foo8 <- lm(y~x[,i]+x[,2])
#     try <- try(plotmo(a.foo8, trace=2)) # Error: predict.lm(xgrid, type="response") returned a response of the wrong length.
#     expect.err(try)
# }
# foo8()
foo9 <- function()
{
    my.list <- list(j=2)
    a.foo9 <- lm(y~x[,1]+x[,my.list$j])
    try <- try(plotmo(a.foo9, trace=2)) # Error: plotmo: names with "$" are not yet supported.
    expect.err(try)
}
foo9()

# TODO removed because this now works (why?)
# # test "entire x matrix is stored as the first element of evaluated.mf\n")
# x <- matrix(c(1,3,2,4,5,
#               2,3,4,5,6), ncol=2) # actual values not important
# y <- 3:7
# lm.model <- lm(y~x)
# try <- try(plotmo(lm.model, trace=2, caption="lm.model <- lm(y~x)")) # Expect predict.lm(xgrid, type="response") returned a response of the wrong length.
# expect.err(try)

set.seed(1235)
tit <- etitanic
tit <- tit[c(30:80,330:380,630:680), ]
a <- earth(survived~., data=tit, glm=list(family=binomial), degree=2)
plotmo(a, grid.levels=list(sex="ma"), caption="smooth: survived, sex=\"m\"",
       col.smooth="indianred", lwd.smooth=2,
       col.response=as.numeric(tit$survived)+2, pch.response=".", type2="im",
       cex.response=3, jitter.response=.3)
set.seed(1238)
a <- earth(pclass~., data=tit, degree=2)
plotmo(a, type="class", grid.levels=list(sex="ma"), caption="smooth: pclass, sex=\"m\"",
       col.smooth="indianred", lwd.smooth=2,
       col.response=as.numeric(tit$pclass)+1, type2="im",
       pch.response=".", cex.response=3, jitter.response=.3)

# intercept only models

dopar(2, 2, caption = "intercept-only models")
set.seed(1)
x <- 1:10
y <- runif(length(x))
earth.intercept.only <- earth(x, y)
plotmo(earth.intercept.only, do.par=FALSE, main="intercept-only model")
plotmo(earth.intercept.only, do.par=FALSE, col.response=1, pch.response=20)
library(rpart)
rpart.intercept.only <- rpart(y~x)
plotmo(rpart.intercept.only, do.par=FALSE)

if(!interactive()) {
    dev.off()         # finish postscript plot
    q(runLast=FALSE)  # needed else R prints the time on exit (R2.5 and higher) which messes up the diffs
}
