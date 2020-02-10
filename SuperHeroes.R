library("jsonlite")
library(tibble)

superheroes_data = fromJSON("https://superheroapi.com/api/863227500790928/1")
for (x in c(2:731)) {
  url_req <- paste("https://superheroapi.com/api/863227500790928/",as.character(x),sep="")
  superheroes_data <- rbind(superheroes_data,fromJSON(url_req))
}

