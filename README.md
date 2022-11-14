# project-3
---
title: "project-3"
author: "Justin Feathers"
date: '2022-11-01'
output: rmarkdown::github_document
params:
    channel: "tech"
---
 
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE)
```

```{r}
library(rmarkdown)
channelIDs <- list("lifestyle", "entertainment", "bus", "socmed", "tech", "world")
output_file <- paste0(channelIDs, ".html")
parameters = lapply(channelIDs, FUN = function(x){list(channel = x)})
reports <- tibble(output_file, parameters)
apply(reports, MARGIN = 1,
      FUN = function(x){
        render(input = "./project-3.Rmd", output_file = x[[1]], params = x[[2]])
      })
```
