getStooq <- function(asset, useReadCsv=F) {
    as.url <- paste0("http://stooq.com/q/d/l/?s=", asset, "&i=d")
    as.dir <- file.path(".","tempData")
    if(!dir.exists(as.dir)) { 
      dir.create(as.dir)
    }
    as.fileName <- paste0(asset, ".csv")
    as.file<- file.path(as.dir, as.fileName)
    if(file.exists(as.file) & file.size(as.file) >0) {
      sprintf("file exist %s", as.file)
      ret <- read.csv(file=as.file, strip.white = T)
      ret$Date <- as.Date(ret$Date)
    }else {
      if(useReadCsv) {
        ret <- read.csv(as.url)
        ret$Date <- as.Date(ret$Date)
      } else {
        u <- getURI(as.url, ssl.verifypeer=0L, followlocation=1L)
        ret <- read.csv(text=u)
        ret$Date <- as.Date(ret$Date)
      }
      write.csv(ret, file = as.file)
    } 
    #download.file(url=as.url, destfile = as.file)
    #ret$Month <- format(ret$Date, "%Y-%m")
    ret$Month <- format(ret$Date, "%Y-%m")
    ret$Date.POSIXct <- as.POSIXct(ret$Date)
    return (ret)
}

addNRow <- function(df) {
  df$NRow <- 1:nrow(df);
  return(df);
}


inner_join_stooq <- function (x, y,  colSelect =c("Date","Close")) {
  ret <- inner_join(x[, colSelect], y[,colSelect], by = colSelect[1] ) %>%
    select (Date, x = Close.x, y = Close.y) %>% 
    mutate(ratio = x / y) %>%
    mutate(ratio.rev = 1 / ratio)

  return (ret)
}

hist2 <- function(x, selector="ratio", name, length=40, adjust=1, ...) {
  val <- x[[selector]]
  par(mfrow = c(2, 1), pty = "m") 
  plot(x$Date, val,  type='l', ylab = name, xlab = "Data")
  prop <- pnorm(last(val), mean(val), sd(val))
  propText <- paste("P(", name, "<",signif(last(val),digits = 3),") =", signif(prop, digits=3))
  mtext(propText,3)
  
  
  h <- hist(val, xlab=name, ylab = "Występowanie" )
  xfit<-seq(min(val),max(val),length=length) 
  yfit<-dnorm(xfit,mean=mean(val),sd=sd(val)) 
  yfit <- yfit*diff(h$mids[1:2])*length(val) 
  lines(xfit, yfit, col="blue", lwd=2)
  abline(v = last(val), col = "red",lwd = 3)
  rug(val )
  
  q <- quantile(val, probs = c(.1,.9))
  qText <- paste("Quantile 10% =", signif(q[1], digits=3), " 90% =", signif(q[2], digits=3) )
  mtext(qText,3)
  abline(v = q[1], col = "green")
  abline(v = q[2], col = "green")
  
  
  
} 


