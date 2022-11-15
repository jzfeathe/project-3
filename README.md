The analysis for [Bus articles is available here](bus.html)  
The analysis for [Entertainment articles is available here](entertainment.html)  
The analysis for [Lifestyle articles is available here](lifestyle.html)  
The analysis for [Socmed articles is available here](socmed.html)  
The analysis for [Tech articles is available here](tech.html)  
The analysis for [World articles is available here](world.html)  

library(rmarkdown)  
channelIDs <- c("lifestyle", "entertainment", "bus", "socmed", "tech", "world")  
output_file <- paste0(channelIDs, ".md")  
params = lapply(channelIDs, FUN = function(x){list(channel = x)})  
reports <- tibble(output_file, params)  
apply(reports, MARGIN = 1,  
      FUN = function(x){  
        render(input = "./project-3.Rmd", output_file = x[[1]], params = x[[2]])  
      })  
