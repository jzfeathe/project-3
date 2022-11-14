project-3
================
Justin Feathers
2022-11-01

# Introduction

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

    ## # A tibble: 1 x 53
    ##   shares LDA_00 self_reference_avg_sha~ kw_avg_avg min_negative_po~ self_reference_~ n_unique_tokens LDA_02 min_positive_po~
    ##    <dbl>  <dbl>                   <dbl>      <dbl>            <dbl>            <dbl>           <dbl>  <dbl>            <dbl>
    ## 1      1  0.107                  0.0982     0.0964           0.0961           0.0918          0.0839 0.0810           0.0770
    ## # ... with 44 more variables: self_reference_max_shares <dbl>, n_non_stop_unique_tokens <dbl>, kw_min_avg <dbl>,
    ## #   LDA_01 <dbl>, kw_max_avg <dbl>, global_sentiment_polarity <dbl>, abs_title_sentiment_polarity <dbl>,
    ## #   avg_negative_polarity <dbl>, rate_negative_words <dbl>, weekday_is_thursday <dbl>, n_tokens_content <dbl>,
    ## #   title_subjectivity <dbl>, kw_min_max <dbl>, global_rate_negative_words <dbl>, kw_avg_min <dbl>,
    ## #   rate_positive_words <dbl>, weekday_is_sunday <dbl>, avg_positive_polarity <dbl>, num_keywords <dbl>, kw_max_min <dbl>,
    ## #   num_imgs <dbl>, kw_min_min <dbl>, LDA_03 <dbl>, kw_avg_max <dbl>, weekday_is_monday <dbl>, weekday_is_friday <dbl>,
    ## #   kw_max_max <dbl>, num_self_hrefs <dbl>, is_weekend <dbl>, LDA_04 <dbl>, num_hrefs <dbl>, ...

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

![](socmed_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
data <- data %>%
            select(-n_unique_tokens)

correlation <- cor(data)
corrplot(correlation, type = "upper", tl.pos = "lt")
corrplot(correlation, type = "lower", method = "number",
         add = TRUE, diag = FALSE, tl.pos = "n")
```

![](socmed_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

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
    ##       5    1400    2100    3629    3800  122800

``` r
noOutlier <- data %>%
             filter(shares != 663600)

g <- ggplot(noOutlier, aes(y = shares))
g + geom_point(aes(x = num_hrefs))
```

![](socmed_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
g + geom_point(aes(x = n_tokens_content))
```

![](socmed_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
g + geom_point(aes(x = rate_positive_words))
```

![](socmed_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
summary(data)
```

    ##    num_hrefs      n_tokens_content   kw_avg_avg    n_non_stop_unique_tokens   num_videos     min_negative_polarity
    ##  Min.   :  0.00   Min.   :   0.0   Min.   :    0   Min.   :0.0000           Min.   : 0.000   Min.   :-1.0000      
    ##  1st Qu.:  5.00   1st Qu.: 253.0   1st Qu.: 2656   1st Qu.:0.6170           1st Qu.: 0.000   1st Qu.:-0.8000      
    ##  Median :  8.00   Median : 434.0   Median : 3168   Median :0.6839           Median : 0.000   Median :-0.5000      
    ##  Mean   : 13.18   Mean   : 609.6   Mean   : 3224   Mean   :0.6823           Mean   : 1.118   Mean   :-0.5213      
    ##  3rd Qu.: 15.00   3rd Qu.: 761.5   3rd Qu.: 3619   3rd Qu.:0.7553           3rd Qu.: 1.000   3rd Qu.:-0.3000      
    ##  Max.   :171.00   Max.   :4878.0   Max.   :36717   Max.   :1.0000           Max.   :73.000   Max.   : 0.0000      
    ##  rate_positive_words min_positive_polarity     shares      
    ##  Min.   :0.0000      Min.   :0.00000       Min.   :     5  
    ##  1st Qu.:0.6667      1st Qu.:0.03333       1st Qu.:  1400  
    ##  Median :0.7500      Median :0.05000       Median :  2100  
    ##  Mean   :0.7441      Mean   :0.07817       Mean   :  3629  
    ##  3rd Qu.:0.8333      3rd Qu.:0.10000       3rd Qu.:  3800  
    ##  Max.   :1.0000      Max.   :0.60000       Max.   :122800

``` r
data %>%
  sapply(sd)
```

    ##                num_hrefs         n_tokens_content               kw_avg_avg n_non_stop_unique_tokens 
    ##             1.552699e+01             5.539522e+02             1.329443e+03             1.158418e-01 
    ##               num_videos    min_negative_polarity      rate_positive_words    min_positive_polarity 
    ##             3.731452e+00             2.885506e-01             1.431667e-01             7.320644e-02 
    ##                   shares 
    ##             5.524167e+03

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

    ##   Rsquared 
    ## 0.04163527

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
    ## 0.02434815

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

    ## [1] "Multiple linear regression is the preferred model for data channel = socmed"
