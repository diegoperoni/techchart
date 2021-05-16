Techchart: Technical Feature Extraction of Time Series Data
================
Prodipta Ghosh
2018-12-18

The R package `techchart` is a collection of tools to extract features from time series data for technical analysis and related quantitative applications. While R is not the most suitable platform for carrying out technical analysis with human inputs, this package makes it possible to extract and match technical features and patterns and use them to back-test trading ideas. At present, the package covers four major areas:

-   Perceptually Important Points (PIPs) identification
-   Supports/resistance identification (either based on PIPs or the old-fashioned Fibonacci method)
-   Change point analysis of trends and segmentation of time series based on underlying trend
-   Identification of technical envelopes (like trend channels or triangles) of a time series

Perceptually Important Points
-----------------------------

PIPs are an effort to algorithmically derive a set of important points as perceived by a human to describe a time series. This typically can be a set of minima or maxima points or a set of turning points which are important from a feature extraction perspective. Traditional technical analysis - like technical pattern identification - relies heavily on PIPs. In addition, a set of PIPs can be used to compress a time series in a very useful way. This compressed representation then can be used for comparing segments of time series (match finding) or other purposes. In this package, we have implemented the approach detailed [here](https://www.cs.cmu.edu/~eugene/research/full/search-series.pdf).

``` r
spx <- quantmod::getSymbols("^GSPC", auto.assign = FALSE)
spx <- spx["2014::2015"]
imppts <- techchart::find.imppoints(spx,2)
head(imppts)
```

    ##            pos sign   value
    ## 2014-02-03  22   -1 1741.89
    ## 2014-03-07  45    1 1878.52
    ## 2014-03-14  50   -1 1841.13
    ## 2014-04-03  64    1 1891.43

``` r
quantmod::chart_Series(spx)
points(as.numeric(imppts$maxima$pos),as.numeric(imppts$maxima$value),bg="green",pch=24,cex=1.25)
points(as.numeric(imppts$minima$pos),as.numeric(imppts$minima$value),bg="red",pch=25,cex=1.25)
```

<img src="unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

The function takes in a time series object (in xts format), and a tolerance level for extreme points identification (can be either a percentage or a multiple of standard deviation). It returns an object which has the list of all PIPs identified, marked by either a -1 (minima) or 1 (maxima), as well as the maxima and minima points separately as xts objects

Identification of Change Point in Linear (Deterministic) Trends
---------------------------------------------------------------

Change point analysis has recently become an increasingly important tools for both financial and non-financial time series. There are quite a few packages in R to implement the major algorithms. However, most of them is focused on stationary time series, where in most cases the typical price series encountered in financial market will be non-stationary. The `cpt.trend` function in this package implement a change point analysis for non-stationary time series to identify multiple changes in the deterministic linear trends. The implementation is based on identifying change in simple regression coefficients (with penalty) and extends to multiple change point identification using the popular binary segmentation methodology. See [here](https://arxiv.org/pdf/1101.1438.pdf) for a discussion on different methods. The function `find.major.trends` extends this functionality to automatically search a time series for the most top level changes in trends by starting with a high value of penalty and decreasing in each step till a set of trends found.

``` r
spx <- quantmod::getSymbols("^GSPC", auto.assign = FALSE)
spx <- spx["2014::2015"]
cpts <- techchart::find.major.trends(spx)
summary(cpts)
```

    ## change points:
    ## [1] 411
    ## segments length summary:
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    93.0   172.5   252.0   252.0   331.5   411.0 
    ## segments returns summary:
    ## 0.1041298 0.2329813
    ## segments offset summary:
    ## 0 -0.09676756

``` r
quantmod::chart_Series(spx)
```

<img src="unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

``` r
quantmod::add_TA(cpts$segments[[1]],on=1,lty=3, col="red")
```

<img src="unnamed-chunk-3-2.png" style="display: block; margin: auto;" />

``` r
quantmod::add_TA(cpts$segments[[2]],on=1,lty=3, col="red")
```

<img src="unnamed-chunk-3-3.png" style="display: block; margin: auto;" />

Supports/ Resistance
--------------------

Supports and resistance levels are very popular tools for technical analysis. The function `find.pivots` implements a couple of ways to identify supports and resistance levels for a price series. Using the option `FIB` will produce a set of Fibonacci levels around the most recent price point. The option `SR` will run an algorithm to find co-linear points along x-axis (horizontal line) to find levels most tested in recent times. A set of levels as well as xts representation of the lines defined by them are returned

``` r
spx <- quantmod::getSymbols("^GSPC", auto.assign = FALSE)
spx <- spx["2014::2015"]
sups <- techchart::find.pivots(spx, type = "FIB")
summary(sups)
```

    ## supports and resistance:
    ## next 3 supports:1982.249 1936.355 1890.461
    ## next 3 resistance:2130.82

``` r
sups <- techchart::find.pivots(spx, type = "SR", strength = 5)
summary(sups)
```

    ## supports and resistance:
    ## next 3 supports:2043.688 1992.551 1895.028
    ## next 3 resistance:2070.407 2111.588

Price Envelop Identification
----------------------------

Price envelopes features are an integral part of technical analysis. For example technical analysts look for features like trending channel, or ascending triangles etc to identify continuation or breakout from current price actions. The function `find.tchannel` identifies the most recent such envelopes using an implementation of the popular Hough transform algorithm in image processing, along with some heuristics. The wrapper function `find.trend.channel` returns the best-fit such envelope.

``` r
spx <- quantmod::getSymbols("^GSPC", auto.assign = FALSE)
spx <- spx["2016-01-01::2016-09-30"]
tchannel <- techchart::find.trend.channel(spx)
tchannel
```

    ## name: megaphone
    ## direction: 1
    ## upper limit: 2247.467
    ## lower limit: 2145.405
    ## duration ratio: 0.497
    ## aesthetics - aspect ratio: 1.45 score: 0 fit: 1 strength: 3

``` r
quantmod::chart_Series(spx)
```

<img src="unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

``` r
quantmod::add_TA(tchannel$xlines$maxlines[[1]],on=1, lty=3, col="brown")
```

<img src="unnamed-chunk-5-2.png" style="display: block; margin: auto;" />

``` r
quantmod::add_TA(tchannel$xlines$minlines[[1]],on=1, lty=3, col="brown")
```

<img src="unnamed-chunk-5-3.png" style="display: block; margin: auto;" />

The function returns an object with parameters of the envelopes found (if any), as well as the xts representation of the envelopes lines

Technical Pattern Identification
--------------------------------

Technical pattern identification implementation is based on PIPs. See [here](http://web.mit.edu/people/wangj/pap/LoMamayskyWang00.pdf) for a discussion on the basic aproach. Note we here replace the kernel smoothing based algorithm with PIPs, which appears more robust. The function `find.tpattern` identifies all or most recent pattern as defined in the pattern definition function `pattern.db`. The wrapper function `find.pattern` calls this for identifying the latest (both completed and forming) patterns and suitable for a sweep call over a period to identify.

``` r
x <- quantmod::getSymbols("^NSEI",auto.assign = F)
x <- na.omit(x)
x <- x["2014-12-03::2015-12-03"]
tpattern <- techchart::find.pattern(x)
quantmod::chart_Series(x)
```

<img src="unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

``` r
quantmod::add_TA(tpattern$matches[[1]]$data,on=1,col=adjustcolor("red",alpha.f = 0.5), lwd=5)
```

<img src="unnamed-chunk-6-2.png" style="display: block; margin: auto;" />

``` r
tpattern
```

    ## ------pattern matched on: 2015-12-02 --------
    ## name: Head and shoulder
    ## type: forming
    ## move: -2.42 (percentage annualized)
    ## threshold: 7728.75
    ## duration: 71 (days)

These functions returns a list of objects of class patterns, which has a set of quantitative and descriptive parameters of the patterns found.
