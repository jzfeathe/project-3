This repo is for analyzing an [online news popularity dataset](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity)
and trying to predict the response variable "shares" using multiple linear regression and random forest models.  

The R packages used in the analyses were:
* tidyverse  
* ggplot2
* lattice
* corrplot  
* caret  
* shiny  
* rmarkdown  

Links to the analyses:  
The analysis for [Bus articles is available here](bus.html)  
The analysis for [Entertainment articles is available here](entertainment.html)  
The analysis for [Lifestyle articles is available here](lifestyle.html)  
The analysis for [Socmed articles is available here](socmed.html)  
The analysis for [Tech articles is available here](tech.html)  
The analysis for [World articles is available here](world.html)  

Finally, here is the code used to automate the knitting of documents for the six different data channels:  

library(rmarkdown)  
channelIDs <- c("lifestyle", "entertainment", "bus", "socmed", "tech", "world")  
output_file <- paste0(channelIDs, ".md")  
params = lapply(channelIDs, FUN = function(x){list(channel = x)})  
reports <- tibble(output_file, params)  
apply(reports, MARGIN = 1,  
      FUN = function(x){  
        render(input = "./project-3.Rmd", output_file = x[[1]], params = x[[2]])  
      })  
 
