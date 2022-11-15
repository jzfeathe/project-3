project-3
================
Justin Feathers
2022-11-01

-   <a href="#introduction" id="toc-introduction">Introduction</a>
-   <a href="#data" id="toc-data">Data</a>
-   <a href="#summarizations" id="toc-summarizations">Summarizations</a>
-   <a href="#modeling" id="toc-modeling">Modeling</a>
    -   <a href="#multiple-linear-regression"
        id="toc-multiple-linear-regression">Multiple Linear Regression</a>
    -   <a href="#random-forest" id="toc-random-forest">Random Forest</a>
-   <a href="#comparison" id="toc-comparison">Comparison</a>

# Introduction

A good way of starting is by checking how strongly all variables are
correlated to the response variable of interest. I created a correlation
matrix using the `cor` function and sorted the absolute values of the
output to get a convenient tibble of descending correlation values.

From here, we can look at a correlation plot of the chosen variables to
see if multicollinearity exists between any of the variables. Using
`corrplot`, we can see that the variables `n_unique_tokens` and
`n_tokens_content` have a strong negative correlation of -0.73. There is
an extremely strong positive correlation of 0.93 between
`n_unique_tokens` and `n_non_stop_unique_tokens` – we will try dropping
`n_unique_tokens` from the dataset and reassessing. We can see in the
new `corrplot` that multicollinearity has been minimized as desired.

Describes the data and variables we want to use. Target is `shares`

``` r
library(tidyverse)
library(corrplot)
library(caret)
```

# Data

``` r
newsData <- read_csv(file = "./OnlineNewsPopularity.csv")
data <- newsData %>% 
            filter(get(paste0("data_channel_is_", params$channel)) == 1) %>%
              select(-url, -timedelta)
```

# Summarizations

We can analyze a few of the variables by plotting them against `shares`.
If we create a scatter plot of `num_imgs` by `shares`, we can see an
outlier when `num_imgs` = 1. Let’s remove that.

``` r
dataCor <- cor(data$shares, data) %>%
        as.tibble() %>%
        abs() %>%
        sort(decreasing = TRUE)
dataCor
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["shares"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["kw_avg_avg"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["kw_max_avg"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["kw_max_min"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["kw_avg_min"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["self_reference_avg_sharess"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["self_reference_min_shares"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["self_reference_max_shares"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["num_hrefs"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["num_keywords"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["num_imgs"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["global_subjectivity"],"name":[12],"type":["dbl"],"align":["right"]},{"label":["kw_avg_max"],"name":[13],"type":["dbl"],"align":["right"]},{"label":["LDA_03"],"name":[14],"type":["dbl"],"align":["right"]},{"label":["is_weekend"],"name":[15],"type":["dbl"],"align":["right"]},{"label":["LDA_02"],"name":[16],"type":["dbl"],"align":["right"]},{"label":["weekday_is_sunday"],"name":[17],"type":["dbl"],"align":["right"]},{"label":["LDA_00"],"name":[18],"type":["dbl"],"align":["right"]},{"label":["kw_min_avg"],"name":[19],"type":["dbl"],"align":["right"]},{"label":["title_subjectivity"],"name":[20],"type":["dbl"],"align":["right"]},{"label":["abs_title_sentiment_polarity"],"name":[21],"type":["dbl"],"align":["right"]},{"label":["avg_positive_polarity"],"name":[22],"type":["dbl"],"align":["right"]},{"label":["min_positive_polarity"],"name":[23],"type":["dbl"],"align":["right"]},{"label":["global_sentiment_polarity"],"name":[24],"type":["dbl"],"align":["right"]},{"label":["global_rate_positive_words"],"name":[25],"type":["dbl"],"align":["right"]},{"label":["LDA_04"],"name":[26],"type":["dbl"],"align":["right"]},{"label":["weekday_is_tuesday"],"name":[27],"type":["dbl"],"align":["right"]},{"label":["global_rate_negative_words"],"name":[28],"type":["dbl"],"align":["right"]},{"label":["weekday_is_saturday"],"name":[29],"type":["dbl"],"align":["right"]},{"label":["kw_max_max"],"name":[30],"type":["dbl"],"align":["right"]},{"label":["max_negative_polarity"],"name":[31],"type":["dbl"],"align":["right"]},{"label":["num_self_hrefs"],"name":[32],"type":["dbl"],"align":["right"]},{"label":["n_tokens_content"],"name":[33],"type":["dbl"],"align":["right"]},{"label":["avg_negative_polarity"],"name":[34],"type":["dbl"],"align":["right"]},{"label":["average_token_length"],"name":[35],"type":["dbl"],"align":["right"]},{"label":["LDA_01"],"name":[36],"type":["dbl"],"align":["right"]},{"label":["n_tokens_title"],"name":[37],"type":["dbl"],"align":["right"]},{"label":["weekday_is_wednesday"],"name":[38],"type":["dbl"],"align":["right"]},{"label":["max_positive_polarity"],"name":[39],"type":["dbl"],"align":["right"]},{"label":["rate_negative_words"],"name":[40],"type":["dbl"],"align":["right"]},{"label":["min_negative_polarity"],"name":[41],"type":["dbl"],"align":["right"]},{"label":["kw_min_max"],"name":[42],"type":["dbl"],"align":["right"]},{"label":["rate_positive_words"],"name":[43],"type":["dbl"],"align":["right"]},{"label":["weekday_is_thursday"],"name":[44],"type":["dbl"],"align":["right"]},{"label":["n_unique_tokens"],"name":[45],"type":["dbl"],"align":["right"]},{"label":["n_non_stop_words"],"name":[46],"type":["dbl"],"align":["right"]},{"label":["n_non_stop_unique_tokens"],"name":[47],"type":["dbl"],"align":["right"]},{"label":["abs_title_subjectivity"],"name":[48],"type":["dbl"],"align":["right"]},{"label":["kw_min_min"],"name":[49],"type":["dbl"],"align":["right"]},{"label":["title_sentiment_polarity"],"name":[50],"type":["dbl"],"align":["right"]},{"label":["weekday_is_monday"],"name":[51],"type":["dbl"],"align":["right"]},{"label":["weekday_is_friday"],"name":[52],"type":["dbl"],"align":["right"]},{"label":["num_videos"],"name":[53],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.1742764","3":"0.1596356","4":"0.1398467","5":"0.1198875","6":"0.09533648","7":"0.07864837","8":"0.0742809","9":"0.04676477","10":"0.0397448","11":"0.03809298","12":"0.03751081","13":"0.03649163","14":"0.03394801","15":"0.03326525","16":"0.03081855","17":"0.03066592","18":"0.03064968","19":"0.02827869","20":"0.02520492","21":"0.02513925","22":"0.0240712","23":"0.02379888","24":"0.01954282","25":"0.01844167","26":"0.0170663","27":"0.0157599","28":"0.01360618","29":"0.01353825","30":"0.01352521","31":"0.01203021","32":"0.01146973","33":"0.01122183","34":"0.01057653","35":"0.009991654","36":"0.009480464","37":"0.009065995","38":"0.006990723","39":"0.006940211","40":"0.006778685","41":"0.005900553","42":"0.005658058","43":"0.005400506","44":"0.005164032","45":"0.004551485","46":"0.004438222","47":"0.004268375","48":"0.003476565","49":"0.003088106","50":"0.002798214","51":"0.002450865","52":"0.001549303","53":"0.0005863698"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
data <- data %>% 
            select(num_hrefs, n_tokens_content, n_unique_tokens, kw_avg_avg,
                   n_non_stop_unique_tokens, num_videos, min_negative_polarity,
                   rate_positive_words, min_positive_polarity, shares)

correlation <- cor(data)
corrplot(correlation, type = "upper", tl.pos = "lt")
corrplot(correlation, type = "lower", method = "number",
         add = TRUE, diag = FALSE, tl.pos = "n")
```

![](entertainment_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
data <- data %>%
            select(-n_unique_tokens)

correlation <- cor(data)
corrplot(correlation, type = "upper", tl.pos = "lt")
corrplot(correlation, type = "lower", method = "number",
         add = TRUE, diag = FALSE, tl.pos = "n")
```

![](entertainment_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

We can see the outlier is a single point. We can find the value by using
a `summary` statement. After filtering out the outlier, we can see the
scatter plot looks much more reasonable. Based on this plot, it looks
like articles with 0 or 1 images tend to get the most shares with a
quadratic decline until hitting the local minimum at 5 images where it
changes to a positive upswing until 11 images. It looks as though
`shares` continues on a negative linear trend after that. Inspecting the
plot for `shares` vs. `n_tokens_content` seems to suggest shares tend to
decrease after articles go beyond 250-500 words. Next, the plot of
`shares` vs. `rate_positive_words` suggests that articles are far more
likely to be shared as the rate of positive words increases. Finally, we
can see quartiles and means for the variables using the `summary`
function and standard deviations with the `sd` function.

``` r
summary(data$shares)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      47     833    1200    2970    2100  210300

``` r
noOutlier <- data %>%
             filter(shares != 663600)

g <- ggplot(noOutlier, aes(y = shares))
g + geom_point(aes(x = num_hrefs))
```

![](entertainment_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
g + geom_point(aes(x = n_tokens_content))
```

![](entertainment_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
g + geom_point(aes(x = rate_positive_words))
```

![](entertainment_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
summary(data)
```

    ##    num_hrefs      n_tokens_content   kw_avg_avg    n_non_stop_unique_tokens   num_videos     min_negative_polarity
    ##  Min.   :  0.00   Min.   :   0.0   Min.   :    0   Min.   :  0.0000         Min.   : 0.000   Min.   :-1.0000      
    ##  1st Qu.:  4.00   1st Qu.: 255.0   1st Qu.: 2551   1st Qu.:  0.6234         1st Qu.: 0.000   1st Qu.:-0.8000      
    ##  Median :  7.00   Median : 433.0   Median : 2967   Median :  0.6917         Median : 1.000   Median :-0.6000      
    ##  Mean   : 10.69   Mean   : 607.5   Mean   : 3156   Mean   :  0.7632         Mean   : 2.546   Mean   :-0.5877      
    ##  3rd Qu.: 13.00   3rd Qu.: 805.0   3rd Qu.: 3553   3rd Qu.:  0.7597         3rd Qu.: 1.000   3rd Qu.:-0.4000      
    ##  Max.   :304.00   Max.   :6505.0   Max.   :36023   Max.   :650.0000         Max.   :74.000   Max.   : 0.0000      
    ##  rate_positive_words min_positive_polarity     shares      
    ##  Min.   :0.0000      Min.   :0.00000       Min.   :    47  
    ##  1st Qu.:0.5789      1st Qu.:0.05000       1st Qu.:   833  
    ##  Median :0.6875      Median :0.10000       Median :  1200  
    ##  Mean   :0.6663      Mean   :0.09351       Mean   :  2970  
    ##  3rd Qu.:0.7843      3rd Qu.:0.10000       3rd Qu.:  2100  
    ##  Max.   :1.0000      Max.   :1.00000       Max.   :210300

``` r
data %>%
  sapply(sd)
```

    ##                num_hrefs         n_tokens_content               kw_avg_avg n_non_stop_unique_tokens 
    ##             1.292069e+01             5.289519e+02             1.099092e+03             7.731159e+00 
    ##               num_videos    min_negative_polarity      rate_positive_words    min_positive_polarity 
    ##             6.225034e+00             2.972439e-01             1.850866e-01             6.393063e-02 
    ##                   shares 
    ##             7.858134e+03

# Modeling

``` r
set.seed(250)
index <- createDataPartition(data$shares, p = 0.70, list = FALSE)
train <- data[index, ]
test <- data[-index, ]
```

## Multiple Linear Regression

Fitting a multiple regression model on all variables in the `data`
dataset, we can see from the `summary` function that this model is not a
very good fit with an adjusted R^2 value of 0.01 – this means only 1% of
the variance in the data is explained by the model. We need to explore
better options.

``` r
mlrFit <- train(shares ~ ., data = train,
                preProcess = c("center", "scale"),
                method = "lm",
                trControl = trainControl(method = "cv", number = 5))
mlrPredict <- predict(mlrFit, newdata = test)
temp <- postResample(mlrPredict, test$shares)
mlrRsquare <- temp[2]
mlrRsquare
```

    ##    Rsquared 
    ## 0.006112606

## Random Forest

``` r
forest <- train(shares ~ ., data = train,
                method = "rf",
                preProcess = c("center", "scale"),
                tuneGrid = data.frame(mtry = ncol(train)/3),
                trControl = trainControl(method = "cv", number = 5))
forestPredict <- predict(forest, newdata = test)
temp <- postResample(forestPredict, test$shares)
forestRsquare <- temp[2]
forestRsquare
```

    ##   Rsquared 
    ## 0.01228135

# Comparison

To compare the models, we will use a simple comparison of R^2 and choose
the one with the highest value. We will use this method since R^2 can be
interpreted as how much of the variance in the data can be explained by
the model, i.e., how well the model fits.

``` r
if (mlrRsquare > forestRsquare) {
  paste0("Multiple linear regression is the preferred model for data channel = ", params$channel)
} else {
  paste0("Random forest is the preferred model for data channel = ", params$channel)
} 
```

    ## [1] "Random forest is the preferred model for data channel = entertainment"
