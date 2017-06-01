getStooq <- function(asset, useReadCsv=T) {  
as.url <- paste0("http://stooq.com/q/d/l/?s=", asset, "&i=d")
    as.dir <- file.path(".","tempData")
    if(!dir.exists(as.dir))    
    {      
    dir.create(as.dir)   
    }    
    as.fileName <- paste0(asset, ".csv")
    as.file<- file.path(as.dir,as.fileName)
    if(file.exists(as.file) & file.size(as.file) >0)     {
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
    ret$Date.POSIXct  <- as.POSIXct(ret$Date)  
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

hist2 <- function(x, length=40, adjust=1, ...) {
  h <- hist(x, ...,)
  xfit<-seq(min(x),max(x),length=length) 
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, col="blue", lwd=2)
  abline(v = last(x), col = "red",lwd = 2)
  rug(x)
} 



