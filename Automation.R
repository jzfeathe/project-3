library(rmarkdown)

channelIDs <- c("lifestyle", "entertainment", "bus", "socmed", "tech", "world")
output_file <- paste0(channelIDs, ".md")
params = lapply(channelIDs, FUN = function(x){list(channel = x)})
reports <- tibble(output_file, params)

apply(reports, MARGIN = 1,
      FUN = function(x){
        render(input = "./project-3.Rmd", output_file = x[[1]], params = x[[2]])
      })