getStooq <- function(asset) {
  as.url <- paste("http://stooq.com/q/d/l/?s=", asset, "&i=d", sep="")
  as <- read.csv(as.url)
  as$Date <- as.Date(as$Date)
  as$Month <- format(as$Date, "%Y-%m")
  return (as)
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

hist2 <- function(x, length=40, adjust=1, ...) {
  h <- hist(x, ...,)
  xfit<-seq(min(x),max(x),length=length) 
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, col="blue", lwd=2)
  abline(v = last(x), col = "red",lwd = 2)
  rug(x)
} 



