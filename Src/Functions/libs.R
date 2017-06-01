libs <- c("dplyr", "xts","ggplot2", "quantmod", "XLConnect", "XML", "RCurl", "magrittr") 
#lapply(libs, function(x) {print(x); install.packages(x)})

lapply(libs, function(x) require(x, character.only = T))
