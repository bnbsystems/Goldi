libs <- c("dplyr", "xts","ggplot2", "quantmod", "XLConnect", "XML", "RCurl", "magrittr") 
#lapply(libs, function(x) {print(x); install.packages(x)})

lapply(libs, function(x) library(x, character.only = T))
