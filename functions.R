#Read in data
read.in.data <-function(x){
  read.csv(x,header=TRUE,fill=TRUE,skip=45)
  }

block.pres<-function(data){
  x<-data[match('Data Block 0',data$c1):match('Data Block 1',data$c1),1:2]
  y<-x[9:nrow(x)-1,]
 }

block.temp<-function(data){
  x<-data[match('Data Block 1',data$c1):match('Data Block 2',data$c1),1:2]
  y<-x[9:nrow(x)-1,]
 }

block.xyz<-function(data){
  x<-data[match('Data Block 2',data$c1):match('Fast Log 1',data$c1),1:2]
  y<-x[9:nrow(x)-1,]
  }

datetime<-function(x){
  x$c1<-as.POSIXct(strptime(x$c1,format="%d/%m/%y %H:%M:%S"),tz="GMT")
}

newdatetime<-function(data){
  data$Date.Time<-as.POSIXct(strptime(data$Date.Time,format="%Y-%m-%d %H:%M:%S"),tz="GMT")
}

merger<-function(x,y){
  merge(x,y,by = "Date.Time",all.x=TRUE)
}

ma <- function(x,n=5){
  filter(x,rep(1/n,n), sides=2)
}

tsoutliers <- function(x,plot=FALSE)
{
  x <- as.ts(x)
  if(frequency(x)>1)
    resid <- stl(x,s.window="periodic",robust=TRUE)$time.series[,3]
  else
  {
    tt <- 1:length(x)
    resid <- residuals(loess(x ~ tt))
  }
  resid.q <- quantile(resid,prob=c(0.25,0.75))
  iqr <- diff(resid.q)
  limits <- resid.q + 1.5*iqr*c(-1,1)
  score <- abs(pmin((resid-limits[1])/iqr,0) + pmax((resid - limits[2])/iqr,0))
  if(plot)
  {
    plot(x)
    x2 <- ts(rep(NA,length(x)))
    x2[score>0] <- x[score>0]
    tsp(x2) <- tsp(x)
    points(x2,pch=19,col="red")
    return(invisible(score))
  }
  else
    return(score)
}

outlierMAD <- function (x, k)#
{#
  #   x:  vector or time series#
  #   k:  window [x_(i-k),...,x_i,...,x_(i+k)]#
  n   <- length(x)#
  y   <- x         # corrected x vector#
  ind <- c()       # indices of outliers#
  #
  L  <- 1.4826     # constants for normal distributions#
  t0 <- 3          # Pearson's 3 sigma edit rule#
  #
  # we don't look at outliers at the end parts of x !#
  for ( i in (k+1):(n-k) ) {#
    x0 <- median( x[(i-k):(i+k)] )#
    S0 <- L * median( abs(x[(i-k):(i+k)] - x0) )#
    if ( abs(x[i]-x0) > t0 * S0 ) {#
      y[i] <- x0#
      ind  <- c(ind, i)#
    }#
  }#
  # return a list with 2 components#
  list(y=y, ind=ind)#
}

HampelFilter <- function (x, k,t0=3){
  n <- length(x)
  y <- x
  ind <- c()
  L <- 1.4826
  for (i in (k + 1):(n - k)) {
    x0 <- median(x[(i - k):(i + k)])
    S0 <- L * median(abs(x[(i - k):(i + k)] - x0))
    if (abs(x[i] - x0) > t0 * S0) {
      y[i] <- x0
      ind <- c(ind, i)
    }
  }
  list(y = y, ind = ind)
}

##Functions for fastlogs##
data.sub <- function(i, start, end, data)
  data[start[i]:end[i]]


filename.logs <- function(filename)
  paste0(filename, "_logs.rds")

## Will be fragile with respect to stray commas.
read.data <- function(x) {
  header.length <- grep("^Data points available", x)
  x <- x[-seq_len(header.length)]
  if (length(x) == 0)
    return(NULL)
  x.split <- strsplit(x, ",", fixed=TRUE)
  m <- data.frame(do.call(rbind, x.split[-1]), stringsAsFactors=FALSE)
  #names(m) <- x.split[[1]]
  m[-1] <- lapply(m[-1], as.numeric)
  m
}

load.logs <- function(filename, data, regenerate=FALSE) {
  f.logs <- filename.logs(filename)
  if (!regenerate && file.exists(f.logs))
    return(readRDS(f.logs))
  
  logs <- 
    lapply(idx.log[1:n], function(i)
      read.data(data.sub(i, block.start, block.end, data)))
  # Drop empty logs:
  logs <- logs[sapply(logs, is.data.frame)]
  # Drop logs that are too short:
  logs <- logs[sapply(logs, nrow) > 200]
  
  saveRDS(logs, filename.logs(filename))  
  
  logs
}

##Make a data frame for fast logs
df.for.fl <- function(x) {
  d<-c(logs1)
  d<-rbindlist(d)
  setnames(d,c("d.time","x","y","z"))
  }

##Make a data frame for lists
l.to.df <- function(x) {
  c1<-contents.data[[1]]
  names(c1)<-c("d.time","depth")
  c2<-contents.data[[2]]
  names(c2)<-c("d.time","temp")
  c3<-contents.data[[3]]
  names(c3)<-c("d.time","x","y","z")
  content<-merger(c1,c2,"d.time")
  contents<-merger(content,c3,"d.time")
  contents$Date.Time<-as.POSIXct(strptime(contents$d.time,format="%d/%m/%Y %H:%M:%S"),tz="GMT")
  ###Fill in Temp#########
  contents<-transform(contents,temp=na.locf(temp))
}

is.data.norm<-function(data){
  par(mfrow=c(2,2))
  par(mar=c(2.5,2.5,2.5,2.5))
  hist(data$x,xlab="",main="X",ylab="")
  hist(data$y,xlab="",main="Y",ylab="")
  hist(data$z,xlab="",main="Z",ylab="")
  hist(data$depth,xlab="",main="Depth",ylab="")
}

xyzd.plot<-function(data){
  par(mfrow=c(4,1))
  par(mar=c(1.5,2,.5,.25))
  plot(data$x,type="l",col="blue",axes=FALSE)
  axis(2,pos=0)
  mtext("x",side=2,line=0,at=0)
  plot(data$y,type="l",col="orange",
       axes=FALSE,yaxp=c(-4,4,5))
  axis(2,pos=0)
  mtext("y",side=2,line=0,at=0)
  plot(data$z,type="l",col="dark green",
       axes=FALSE,yaxp=c(-4,4,5))
  axis(2,pos=0)
  mtext("z",side=2,line=0,at=0)
  plot(data$Date.Time,data$depth,type="l",
       lwd=0.5,xlab="",ylab="Depth")
  dev.copy(pdf,"figs\\A09804_xyzd.pdf")
  dev.off()
}
