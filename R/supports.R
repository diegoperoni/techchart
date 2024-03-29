# support resistance and important points functions
#'@importFrom Rcpp evalCpp
#'@importFrom graphics lines plot points
#'@useDynLib techchart
#'
#'@export
find.imppoints_pt = function (x, tolerance_pt = 1) {
  z1 <- find.minima_maxima_pt(x, tolerance_pt, minima=TRUE) # min
  z2 <- find.minima_maxima_pt(x, tolerance_pt, minima=FALSE) # max
  z <- rbind(z1, z2)
  z <- z[order(z$pos), ]
  for (i in 1:5) {
    if (!checkoptimapos(as.numeric(z$pos))) {
      sign_x <- sortoptimaposition(as.numeric(z$pos), as.numeric(z$sign), 
                                   as.numeric(z$value))
      z$sign <- sign_x
      z <- z[which(z$sign != 0), ]
    }
    if (!checkoptimasign(as.numeric(z$sign))) {
      sign_x <- sortoptimasign(as.numeric(z$pos), as.numeric(z$sign), 
                               as.numeric(z$value))
      z$sign <- sign_x
      z <- z[which(z$sign != 0), ]
    }
  }
  pts = NULL
  if (NROW(z)>0) {
    rownames(z) <- seq(1:NROW(z))
    pts <- list()
    pts$data <- x
    
    #se ha trovato un minimo (massimo) allora il massimo (minimo) è max (min) alla sinistra
    subset = x[1:z[1, ]$pos]
    if (z[1, ]$sign==-1) {
      if (quantmod::is.OHLC(subset))
        subset = as.numeric(quantmod::Hi(subset))
      z = rbind(data.frame(pos=which.max(subset), sign=1, value=max(subset)), z)
    } else {
      if (quantmod::is.OHLC(subset))
        subset = as.numeric(quantmod::Lo(subset))
      z = rbind(data.frame(pos=which.min(subset), sign=-1, value=min(subset)), z)
    }
  
    # se ha trovato un minimo (massimo) e il abs delta massimo (minimo) 
    # successivo > threshold allora il massimo (minimo) è max (min) alla destra
    subset = x[z[NROW(z), ]$pos:NROW(x)]
    if (z[NROW(z), ]$sign==-1) {
      if (quantmod::is.OHLC(subset))
        subset = as.numeric(quantmod::Hi(subset))
      if (abs(z[NROW(z), ]$value - max(subset)) > tolerance_pt)
        z = rbind(z, data.frame(pos=(z[NROW(z), ]$pos+which.max(subset)-1), sign=1, value=max(subset)))
    } else {
      if (quantmod::is.OHLC(subset))
        subset = as.numeric(quantmod::Lo(subset))
      if (abs(z[NROW(z), ]$value - min(subset)) > tolerance_pt)
        z = rbind(z, data.frame(pos=(z[NROW(z), ]$pos+which.min(subset)-1), sign=-1, value=min(subset)))
    }
    
    if (xts::is.xts(x)) {
      z <- xts::as.xts(z, zoo::index(x)[z$pos])
      data <- data.frame(pos = z$pos[which(z$sign == 1)], value = z$value[which(z$sign == 1)])
      maxima <- xts::as.xts(data, zoo::index(x)[z$pos[which(z$sign == 1)]])
      data <- data.frame(pos = z$pos[which(z$sign == -1)], value = z$value[which(z$sign == -1)])
      minima <- xts::as.xts(data, zoo::index(x)[z$pos[which(z$sign == -1)]])
    }
    else {
      maxima <- data.frame(pos = z$pos[which(z$sign == 1)], 
                           value = z$value[which(z$sign == 1)])
      minima <- data.frame(pos = z$pos[which(z$sign == -1)], 
                           value = z$value[which(z$sign == -1)])
    }
    pts$results <- z
    pts$maxima <- maxima
    pts$minima <- minima
    class(pts) <- "imppoints"
  }
  
  return(pts)
}

#'@export
find.imppoints_perc = function (x, tolerance_perc = 0.01, true.close.colname="true.close") {
  
  if (true.close.colname %in% colnames(x)) {
    x$truecl = x[, true.close.colname]
    x = x[, !colnames(x) %in% true.close.colname]
  } else
    x$truecl = x$close
  
  z1 <- find.minima_maxima_perc(x, tolerance_perc, minima=TRUE) # min
  z2 <- find.minima_maxima_perc(x, tolerance_perc, minima=FALSE) # max
  z <- rbind(z1, z2)
  z <- z[order(z$pos), ]
  for (i in 1:5) {
    if (!checkoptimapos(as.numeric(z$pos))) {
      sign_x <- sortoptimaposition(as.numeric(z$pos), as.numeric(z$sign), 
                                   as.numeric(z$value))
      z$sign <- sign_x
      z <- z[which(z$sign != 0), ]
    }
    if (!checkoptimasign(as.numeric(z$sign))) {
      sign_x <- sortoptimasign(as.numeric(z$pos), as.numeric(z$sign), 
                               as.numeric(z$value))
      z$sign <- sign_x
      z <- z[which(z$sign != 0), ]
    }
  }
  pts = NULL
  if (NROW(z)>0) {
    rownames(z) <- seq(1:NROW(z))
    pts <- list()
    pts$data <- x
    
    #se ha trovato un minimo (massimo) allora il massimo (minimo) è max (min) alla sinistra
    subset = x[1:z[1, ]$pos]
    if (z[1, ]$sign==-1) {
      if (quantmod::is.OHLC(subset))
        subset = as.numeric(quantmod::Hi(subset))
      z = rbind(data.frame(pos=which.max(subset), sign=1, value=max(subset)), z)
    } else {
      if (quantmod::is.OHLC(subset))
        subset = as.numeric(quantmod::Lo(subset))
      z = rbind(data.frame(pos=which.min(subset), sign=-1, value=min(subset)), z)
    }
    
    # se ha trovato un minimo (massimo) e il abs delta massimo (minimo) 
    # successivo > threshold allora il massimo (minimo) è max (min) alla destra
    last_threshold_pt <- as.numeric(x$truecl[z[NROW(z), ]$pos]) * tolerance_perc
    subset = x[z[NROW(z), ]$pos:NROW(x)]
    if (z[NROW(z), ]$sign==-1) {
      if (quantmod::is.OHLC(subset))
        subset = as.numeric(quantmod::Hi(subset))
      if (abs(z[NROW(z), ]$value - max(subset)) > last_threshold_pt)
        z = rbind(z, data.frame(pos=(z[NROW(z), ]$pos+which.max(subset)-1), sign=1, value=max(subset)))
    } else {
      if (quantmod::is.OHLC(subset))
        subset = as.numeric(quantmod::Lo(subset))
      if (abs(z[NROW(z), ]$value - min(subset)) > last_threshold_pt)
        z = rbind(z, data.frame(pos=(z[NROW(z), ]$pos+which.min(subset)-1), sign=-1, value=min(subset)))
    }
    
    if (xts::is.xts(x)) {
      z <- xts::as.xts(z, zoo::index(x)[z$pos])
      data <- data.frame(pos = z$pos[which(z$sign == 1)], value = z$value[which(z$sign == 1)])
      maxima <- xts::as.xts(data, zoo::index(x)[z$pos[which(z$sign == 1)]])
      data <- data.frame(pos = z$pos[which(z$sign == -1)], value = z$value[which(z$sign == -1)])
      minima <- xts::as.xts(data, zoo::index(x)[z$pos[which(z$sign == -1)]])
    }
    else {
      maxima <- data.frame(pos = z$pos[which(z$sign == 1)], 
                           value = z$value[which(z$sign == 1)])
      minima <- data.frame(pos = z$pos[which(z$sign == -1)], 
                           value = z$value[which(z$sign == -1)])
    }
    pts$results <- z
    pts$maxima <- maxima
    pts$minima <- minima
    class(pts) <- "imppoints"
  }
  
  return(pts)
}

#'@export
find.minima_maxima_pt = function(x, tolerance_pt, minima=TRUE) {
    n <- NROW(x)
    y <- data.frame(rep(1,n),rep(0,n), rep(0,n))
    
    threshold <- rep(tolerance_pt, NROW(x))
    if(quantmod::is.OHLC(x)) {
        #x.min <- apply(merge(quantmod::Cl(x),quantmod::Op(x)),1,min)
        #x.max <- apply(merge(quantmod::Cl(x),quantmod::Op(x)),1,max)
        #x.min <- pmin(as.numeric(quantmod::Cl(x)), as.numeric(quantmod::Op(x)))
        #x.max <- pmax(as.numeric(quantmod::Cl(x)), as.numeric(quantmod::Op(x)))
        x.min <- quantmod::Lo(x) # uses low
        x.max <- quantmod::Hi(x) # uses high
        x.exceed <- (x.max - x.min) >= tolerance_pt
        x.min[x.exceed] <- quantmod::Cl(x)[x.exceed]
        x.max[x.exceed] <- quantmod::Cl(x)[x.exceed]

    } else
        x.min <- x.max <- zoo::coredata(x) # molto piu veloce
        #x.min <- x.max <- as.matrix(x)[,1]
    
    if (minima) {
        y <- data.frame(techchart::findminima_pt(as.numeric(x.min),as.numeric(x.max),threshold))
        output = x.min
    } else {
        y <- data.frame(techchart::findmaxima_pt(as.numeric(x.min),as.numeric(x.max),threshold))
        output = x.max
    }
    y <- y[which(y[, 2]!=0), ]
    colnames(y) <- c("pos", "sign")
    y$pos <- y$pos+1
    y$value <- as.numeric(output)[y$pos]
    return(y)
}

#'@export
find.minima_maxima_perc = function(x, tolerance_perc, minima=TRUE) {
  n <- NROW(x)
  y <- data.frame(rep(1,n),rep(0,n), rep(0,n))
  
  threshold <- x$truecl * tolerance_perc
  
  if(quantmod::is.OHLC(x)) {
    x.min <- quantmod::Lo(x) # uses low
    x.max <- quantmod::Hi(x) # uses high
    x.exceed <- (x.max - x.min) >= threshold
    x.min[x.exceed] <- quantmod::Cl(x)[x.exceed]
    x.max[x.exceed] <- quantmod::Cl(x)[x.exceed]
    
  } else
    x.min <- x.max <- zoo::coredata(x) # molto piu veloce
  #x.min <- x.max <- as.matrix(x)[,1]
  
  if (minima) {
    y <- data.frame(techchart::findminima_pt(as.numeric(x.min),as.numeric(x.max),threshold))
    output = x.min
  } else {
    y <- data.frame(techchart::findmaxima_pt(as.numeric(x.min),as.numeric(x.max),threshold))
    output = x.max
  }
  y <- y[which(y[, 2]!=0), ]
  colnames(y) <- c("pos", "sign")
  y$pos <- y$pos+1
  y$value <- as.numeric(output)[y$pos]
  return(y)
}

#'@export
print.imppoints <- function(x,...){
  print(x$results, ...)
}

#'@export
summary.imppoints <- function(object, ...){
  x <- object
  maxima <- x$maxima
  minima <- x$minima
  ret <- list(maxima=maxima, minima=minima)
  class(ret) <- "summary.imppoints"
  return(ret)
}

#'@export
print.summary.imppoints <- function(x,...){
  cat("extrement points:\n")
  print(paste("maxima:", NROW(x$maxima),"minima:",NROW(x$minima)))
  cat("Highs summary:\n")
  print(summary(x$maxima$value))
  cat("Lows summary:\n")
  print(summary(x$minima$value))
}

#'@export
plot.imppoints <- function(x, maxcol="green", mincol="red", ...){
  if(xts::is.xts(x$data)){
    if(quantmod::is.OHLC(x$data)){
      plot(quantmod::Cl(x$data), ...)
    }else{
      plot(x$data[,1], ...)
    }
    max.xy <- xts::as.xts(x$maxima[,2],zoo::index(x$data)[x$maxima[,1]])
    min.xy <- xts::as.xts(x$minima[,2],zoo::index(x$data)[x$minima[,1]])
    points(max.xy, col="black", pch = 24, bg=maxcol)
    points(min.xy, col="black", pch = 25, bg=mincol)
  } else{
    plot(x$data, type ="n", ...)
    lines(x$data)
    points(x$maxima[,1],x$maxima[,2], col="black", pch = 24, bg=maxcol)
    points(x$minima[,1],x$minima[,2], col="black", pch = 25, bg=mincol)
  }
  Sys.sleep(0)
}

#'@export
`[.imppoints` <- function(x,condition){
  if(!xts::is.xts(x$data))stop("subsetting not allowed on non-xts input")
  imppts <- list()
  imppts$data <- x$data[condition,]
  imppts$results <- x$results[condition,]
  imppts$maxima <- x$maxima[condition,]
  imppts$minima <- x$minima[condition,]
  class(imppts) <- "imppoints"
  return(imppts)
}

find.minima <- function(x, tolerance, lookback=20){
  n <- NROW(x)
  y <- data.frame(rep(1,n),rep(0,n), rep(0,n))
  i.min <- i.y <- 1

  if(tolerance > 1 & quantmod::is.OHLC(x)){
    threshold <- TTR::ATR(quantmod::HLC(x),n=lookback)$atr/quantmod::Cl(x)
    threshold[is.na(threshold)] <- na.omit(threshold)[1]
    threshold <- tolerance*threshold
  } else if(tolerance > 1 & !(quantmod::is.OHLC(x))){
    threshold <- zoo::rollapply(TTR::ROC(x[,1]),lookback, sd)
    threshold[is.na(threshold)] <- na.omit(threshold)[1]
    threshold <- tolerance*threshold
  } else if(tolerance < 1){
    threshold <- rep(tolerance,NROW(x))
  } else{
    stop("tolerance is not valid for data")
  }

  threshold <- (1+threshold)
  if(quantmod::is.OHLC(x)){
    x.min <- apply(merge(quantmod::Cl(x),quantmod::Op(x)),1,min)
    x.max <- apply(merge(quantmod::Cl(x),quantmod::Op(x)),1,max)
  } else{
    x.min <- x.max <- as.matrix(x)[,1]
  }

  y <- data.frame(findminima(as.numeric(x.min),as.numeric(x.max),threshold))
  y <- y[which(y[,2]!=0),]

  colnames(y) <- c("pos","sign")
  y$pos <- y$pos+1
  y$value <- as.numeric(x.min)[y$pos]
  return(y)
}

find.maxima <- function(x, tolerance, lookback=20){
  n <- NROW(x)
  y <- data.frame(rep(1,n),rep(0,n), rep(0,n))
  i.min <- i.y <- 1

  if(tolerance > 1 & quantmod::is.OHLC(x)){
    threshold <- TTR::ATR(quantmod::HLC(x),n=lookback)$atr/quantmod::Cl(x)
    threshold[is.na(threshold)] <- na.omit(threshold)[1]
    threshold <- tolerance*threshold
  } else if(tolerance > 1 & !(quantmod::is.OHLC(x))){
    threshold <- zoo::rollapply(TTR::ROC(x[,1]),lookback, sd)
    threshold[is.na(threshold)] <- na.omit(threshold)[1]
    threshold <- tolerance*threshold
  } else if(tolerance < 1){
    threshold <- rep(tolerance,NROW(x))
  } else{
    stop("tolerance is not valid for data. Variable tolerance allowed only for xts input")
  }

  threshold <- (1+threshold)
  if(quantmod::is.OHLC(x)){
    x.min <- apply(merge(quantmod::Cl(x),quantmod::Op(x)),1,min)
    x.max <- apply(merge(quantmod::Cl(x),quantmod::Op(x)),1,max)
  } else{
    x.min <- x.max <- as.matrix(x)[,1]
  }

  y <- data.frame(findmaxima(as.numeric(x.min),as.numeric(x.max),threshold))
  y <- y[which(y[,2]!=0),]
  colnames(y) <- c("pos","sign")
  y$pos <- y$pos+1
  y$value <- as.numeric(x.max)[y$pos]
  return(y)
}

#' time series extrema using important point algorithm
#' @param x xts object or vector, representing a time series
#' @param tolerance threshold for percentage change or vol multiple for extreme points
#' @param lookback Used for volatility dependent adaptive threshold
#' @return important points data object (object of class imppoints)
#' @examples
#' x <- quantmod::getSymbols("^GSPC", auto.assign = FALSE)
#' x <- x["2015/"]
#' imppts <- find.imppoints(x,2)
#' quantmod::chart_Series(x)
#' points(as.numeric(imppts$maxima$pos),as.numeric(imppts$maxima$value),bg="green",pch=24,cex=1.25)
#' points(as.numeric(imppts$minima$pos),as.numeric(imppts$minima$value),bg="red",pch=25,cex=1.25)
#' @export
find.imppoints <- function(x, tolerance=0.02, lookback=20){
  z1 <- find.minima(x,tolerance, lookback)
  z2 <- find.maxima(x, tolerance, lookback)
  z <- rbind(z1,z2)
  z <- z[order(z$pos),]

  for(i in 1:5){
    if(!checkoptimapos(as.numeric(z$pos))){
      sign_x <- sortoptimaposition(as.numeric(z$pos),as.numeric(z$sign),
                                   as.numeric(z$value))
      z$sign <- sign_x
      z <- z[which(z$sign!=0),]
    }
    if(!checkoptimasign(as.numeric(z$sign))){
      sign_x <- sortoptimasign(as.numeric(z$pos),as.numeric(z$sign),
                               as.numeric(z$value))
      z$sign <- sign_x
      z <- z[which(z$sign!=0),]
    }
  }
  rownames(z) <- seq(1:NROW(z))
  pts <- list()
  pts$data <- x

  if(xts::is.xts(x)){
    z <- xts::as.xts(z,zoo::index(x)[z$pos])
    data <- data.frame(pos=z$pos[which(z$sign==1)],value=z$value[which(z$sign==1)])
    maxima <- xts::as.xts(data,zoo::index(x)[z$pos[which(z$sign==1)]])
    data <- data.frame(pos=z$pos[which(z$sign==-1)],value=z$value[which(z$sign==-1)])
    minima <- xts::as.xts(data,zoo::index(x)[z$pos[which(z$sign==-1)]])
  } else{
    maxima <- data.frame(pos=z$pos[which(z$sign==1)],value=z$value[which(z$sign==1)])
    minima <- data.frame(pos=z$pos[which(z$sign==-1)],value=z$value[which(z$sign==-1)])
  }
  pts$results <- z
  pts$maxima <- maxima
  pts$minima <- minima
  class(pts) <- "imppoints"
  return(pts)
}

#'@export
print.supports <- function(x,...){
  print(x$results, ...)
}

#'@export
summary.supports <- function(object, ...){
  x <- object
  resist<- x$results$value[which(x$results$value > x$lastpoint)]
  sups<- x$results$value[which(x$results$value < x$lastpoint)]
  resist <- sort(resist)
  sups <- rev(sort(sups))
  ret <- list(supports=sups, resistance=resist)
  class(ret) <- "summary.supports"
  return(ret)
}

#' @export
print.summary.supports <- function(x, n=3, ...){
  cat("supports and resistance:\n")
  n.s <- NROW(x$supports); n.r <- NROW(x$resistance)
  if(n.s <1){
    cat("no supports at curret levels")
  } else{
    cat(paste("next",n,"supports:"))
    if(n.s >n){
      cat(x$supports[1:n])
    } else{
      cat(x$supports)
    }
  }
  cat("\n")
  if(n.r <1){
    cat("no resistance at curret levels")
  } else{
    cat(paste("next",n,"resistance:"))
    if(n.r >n){
      cat(x$resistance[1:n])
    } else{
      cat(x$resistance)
    }
  }
}

#'@export
plot.supports <- function(x, ...){
  n <- NROW(x$results)
  if(xts::is.xts(x$data)){
    if(quantmod::is.OHLC(x$data)){
      plot(quantmod::Cl(x$data), main = "supports and resistance", xlab="x", ylab="y")
    }else{
      plot(x$data[,1], main = "supports and resistance", xlab="x", ylab="y")
    }
    for(i in 1:n){
      lines(xts::as.xts(x$lines[[i]]), ...)
    }
  } else{
    plot(x$data, type = "n", main="supports and resistance", xlab="x", ylab="y")
    lines(x$data)
    for(i in 1:n){
      lines(x$lines[[i]], ...)
    }
  }
  Sys.sleep(0)
}

merge_levels <- function(levels, clusters, tolerance=0.01, strength=3){
  n <- length(levels)
  supports <- data.frame(rep(0,n),rep(0,n))
  sups <- levels
  k <- 1
  for(i in 1:n){
    cut <- cutree(clusters,i)
    for(j in 1:i){
      s <- sups[which(cut==j)]
      s <- na.omit(s)
      if(length(s)<1){
        next
      }
      error <- (max(s) - min(s))/mean(s)
      if(error < tolerance){
        supports[k,1] <- mean(s)
        supports[k,2] <- length(s)
        sups[which(cut==j)] <- NA
        k <- k+1
      }
    }
  }
  for(i in 1:n){
    if(!is.na(sups[i])){
      supports[k,1] <- sups[i]
      supports[k,2] <- 1
      k <- k+1
    }
  }
  supports <- supports[supports[,2] >= strength,]
  colnames(supports) <- c("value","strength")
  return(supports)
}

find.supports <- function(x, tolerance=0.02, strength=3, maxline=10,lookback=20){
  optima <- find.imppoints(x, tolerance = tolerance, lookback = lookback)
  clusters <- stats::hclust(dist(optima$results$value))
  sups <- merge_levels(optima$results$value,clusters,tolerance,strength)
  if(NROW(sups)<1)stop("no supports found, try reducing strength parameter")

  if(xts::is.xts(x)){
    if(quantmod::is.OHLC(x)){
      lastval <-as.numeric(quantmod::Cl(x)[NROW(x)])
    }else{
      lastval <-as.numeric(x[NROW(x),1])
    }
  }else{
    lastval <- as.matrix(x)[NROW(x),1]
  }

  sups$dist <- abs(sups$value - lastval)
  if(NROW(sups) > maxline){
    sups <- sups[order(sups$dist),]
    sups <- sups[1:maxline,]
  }
  sups <- sups[,1:2]
  rownames(sups) <- seq(1:NROW(sups))

  suplines <- list()
  for(i in 1:NROW(sups)){
    if(xts::is.xts(x)){
      suplines[[i]] <- xts::as.xts(rep(sups$value[i],NROW(x)),zoo::index(x))
    } else{
      suplines[[i]] <- data.frame(x=zoo::index(x), y=rep(sups$value[i],NROW(x)))
    }
  }

  supports <- list()
  supports$lastpoint <- lastval
  supports$data <- x
  supports$results <- sups
  supports$lines <- suplines
  class(supports) <- "supports"

  return(supports)
}

#' Find supports and resitance for a time series
#' @param x xts object, or vector, representing a time series
#' @param type either FIB (Fibonacci) or SR. SR is based on best fit lines of multiple peaks and troughs
#' @param tolerance threshold for percentage change or vol multiple for extreme points
#' @param strength minimum number of extreme points defining a support
#' @param maxline maximum number of support/ resistance lines to return
#' @param lookback Used for volatility dependent adaptive threshold
#' @return support/ resistance object (object of class supports)
#' @examples
#' x <- quantmod::getSymbols("^GSPC", auto.assign = FALSE)
#' x <- x["2015/"]
#' sups <- find.pivots(x, type = "FIB")
#' summary(sups)
#' sups <- find.pivots(x, type = "SR", strength = 5)
#' summary(sups)
#' quantmod::chart_Series(x)
#' quantmod::add_TA(sups$lines[[1]],on=1, lty=2)
#' @export
find.pivots <- function(x, type=c("SR","FIB"), tolerance=0.02, strength=3, maxline=10, lookback=20){
  if(type=="SR"){
    return(find.supports(x,tolerance,strength,maxline))
  }

  lastval <- 0
  if(xts::is.xts(x)){
    if(quantmod::is.OHLC(x)){
      lastval <-as.numeric(quantmod::Cl(x)[NROW(x)])
    }else{
      lastval <-as.numeric(x[NROW(x),1])
    }
  }else{
    lastval <- as.matrix(x)[NROW(x),1]
  }

  imppts <- find.imppoints(x,tolerance)
  xmax <- max(lastval,imppts$results$value)
  xmin <- min(lastval,imppts$results$value)
  xrange <- xmax - xmin
  levels <- rep(0,6)

  levels[1]<-xmin; levels[2]<-levels[1]+0.236*xrange; levels[3]<-levels[1]+0.382*xrange;
  levels[4]<-levels[1]+0.5*xrange; levels[5]<-levels[1]+0.618*xrange; levels[6]<-xmax;

  sups <- data.frame(levels, rep(1,6))
  colnames(sups) <- c("value","strength")

  suplines <- list()
  for(i in 1:6){
    if(xts::is.xts(x)){
      suplines[[i]] <- xts::as.xts(rep(levels[i],NROW(x)),zoo::index(x))
    } else{
      suplines[[i]] <- data.frame(x=zoo::index(x), y=rep(levels[i],NROW(x)))
    }
  }

  supports <- list()
  supports$lastpoint <- lastval
  supports$data <- x
  supports$results <- sups
  supports$lines <- suplines
  class(supports) <- "supports"

  return(supports)
}


