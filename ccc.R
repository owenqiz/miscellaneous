library(quantmod)
from <- c("CAD", "CNY")
to <- c("CNY", "CAD")
currency <- getQuote(paste0(from, to, "=X"))$Open

ccc <- function(val, currency){
  
  gst <- 0.05
  qst <- 0.0975
  
  if(currency == 'CAD'){
    er <- quantmod::getQuote(paste0('CAD', 'CNY', "=X"))$Open
    return(val * (1 + gst + qst) * er)
  } else {
    er <- quantmod::getQuote(paste0('CNY', 'CAD', "=X"))$Open
    return(val*er/(1 + gst + qst))
  }
}
