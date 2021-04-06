library(curl)
library(microbenchmark)
library(ggplot2)

urls <- c("http://127.0.0.1:4000/SearchRTE-JW/?searchString=socredo",
          "http://127.0.0.1:4000/SearchRTE-Like/?searchString=socredo")

appelAPI <- function(url) {
  req <- curl_fetch_memory(url)
  req
}

mbm <- microbenchmark("JW"=appelAPI(urls[1]),
                      "SQL-Like"=appelAPI(urls[2]),
                      times=100)
autoplot(mbm)



