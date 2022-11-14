
library(rmarkdown)
channelIDs <- list("lifestyle", "entertainment", "bus", "socmed", "tech", "world")
output_file <- paste0(channelIDs, ".html")
parameters = lapply(channelIDs, FUN = function(x){list(channel = x)})
reports <- tibble(output_file, parameters)
apply(reports, MARGIN = 1,
      FUN = function(x){
        render(input = "./project-3.Rmd", output_file = x[[1]], params = x[[2]])
      })
