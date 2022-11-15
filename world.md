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

The dataset we’ll be exploring is a collection of online news articles
published by Mashable in a period of two years. By analyzing a subset of
the variables from the dataset, we hope to predict the response
variable, `shares`. Using correlation plots, I have decided to use
`num_hrefs` (number of links in the article), `n_tokens_content` (number
of words in the article), `kw_avg_avg` (mean shares for the average
number of keywords), `n_non_stop_unique_tokens` (the rate of unique
non-stop words in the content), `num_videos` (the number of videos),
`min_negative_polarity` (minimum polarity of negative words),
`rate_positive_words` (rate of positive words among non-neutral tokens),
and `min_positive_polarity` (minimum polarity of positive words) as our
predictive variables. I will go into more detail with the variable
selection process later.

We will explore these variables through a number of summary statistics
and plots and finally use multiple linear regression and random forest
models to make predictions for the `shares` variable.

# Data

The first step in any analysis is to call the required packages and read
in the dataset. Here, we use the `get` function to tell R to use the
character string as a variable object – in this case, the appropriate
`params` value which corresponds to one of six data channels in the
dataset that we will use to subset the rows. By using a logical
statement we are able to choose only the rows corresponding to our news
channel. Because `url` and `timedelta` are not predictive variables,
they have been dropped from the dataset.

``` r
library(tidyverse)
library(corrplot)
library(caret)
library(shiny)
newsData <- read_csv(file = "./OnlineNewsPopularity.csv")
data <- newsData %>% 
            filter(get(paste0("data_channel_is_", params$channel)) == 1) %>%
              select(-url, -timedelta)
```

# Summarizations

Once the dataset is read in, a good way to start is by checking how
strongly all variables are correlated to the response variable of
interest. This is the beginning of the variable selection process. Since
we are interested in predicting `shares`, I created a correlation matrix
using the `cor` function and sorted the absolute values of the output to
get a convenient tibble of descending correlation values; this was
originally done using the `tech` data channel to set the ground work. I
decided to choose the 9 variables with the highest correlation with
`shares`.

``` r
dataCor <- cor(data$shares, data) %>%
        as.tibble() %>%
        abs() %>%
        sort(decreasing = TRUE)
dataCor
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["shares"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["LDA_02"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["LDA_03"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["num_imgs"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["kw_avg_avg"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["average_token_length"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["rate_negative_words"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["kw_max_avg"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["n_tokens_title"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["n_non_stop_words"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["abs_title_sentiment_polarity"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["LDA_04"],"name":[12],"type":["dbl"],"align":["right"]},{"label":["title_sentiment_polarity"],"name":[13],"type":["dbl"],"align":["right"]},{"label":["num_videos"],"name":[14],"type":["dbl"],"align":["right"]},{"label":["num_hrefs"],"name":[15],"type":["dbl"],"align":["right"]},{"label":["weekday_is_wednesday"],"name":[16],"type":["dbl"],"align":["right"]},{"label":["num_keywords"],"name":[17],"type":["dbl"],"align":["right"]},{"label":["title_subjectivity"],"name":[18],"type":["dbl"],"align":["right"]},{"label":["max_negative_polarity"],"name":[19],"type":["dbl"],"align":["right"]},{"label":["self_reference_avg_sharess"],"name":[20],"type":["dbl"],"align":["right"]},{"label":["self_reference_max_shares"],"name":[21],"type":["dbl"],"align":["right"]},{"label":["n_non_stop_unique_tokens"],"name":[22],"type":["dbl"],"align":["right"]},{"label":["global_sentiment_polarity"],"name":[23],"type":["dbl"],"align":["right"]},{"label":["global_rate_negative_words"],"name":[24],"type":["dbl"],"align":["right"]},{"label":["is_weekend"],"name":[25],"type":["dbl"],"align":["right"]},{"label":["abs_title_subjectivity"],"name":[26],"type":["dbl"],"align":["right"]},{"label":["global_subjectivity"],"name":[27],"type":["dbl"],"align":["right"]},{"label":["LDA_01"],"name":[28],"type":["dbl"],"align":["right"]},{"label":["global_rate_positive_words"],"name":[29],"type":["dbl"],"align":["right"]},{"label":["self_reference_min_shares"],"name":[30],"type":["dbl"],"align":["right"]},{"label":["n_tokens_content"],"name":[31],"type":["dbl"],"align":["right"]},{"label":["weekday_is_saturday"],"name":[32],"type":["dbl"],"align":["right"]},{"label":["LDA_00"],"name":[33],"type":["dbl"],"align":["right"]},{"label":["n_unique_tokens"],"name":[34],"type":["dbl"],"align":["right"]},{"label":["kw_max_min"],"name":[35],"type":["dbl"],"align":["right"]},{"label":["weekday_is_sunday"],"name":[36],"type":["dbl"],"align":["right"]},{"label":["kw_avg_max"],"name":[37],"type":["dbl"],"align":["right"]},{"label":["kw_avg_min"],"name":[38],"type":["dbl"],"align":["right"]},{"label":["weekday_is_monday"],"name":[39],"type":["dbl"],"align":["right"]},{"label":["kw_min_avg"],"name":[40],"type":["dbl"],"align":["right"]},{"label":["avg_negative_polarity"],"name":[41],"type":["dbl"],"align":["right"]},{"label":["min_negative_polarity"],"name":[42],"type":["dbl"],"align":["right"]},{"label":["weekday_is_thursday"],"name":[43],"type":["dbl"],"align":["right"]},{"label":["rate_positive_words"],"name":[44],"type":["dbl"],"align":["right"]},{"label":["kw_max_max"],"name":[45],"type":["dbl"],"align":["right"]},{"label":["avg_positive_polarity"],"name":[46],"type":["dbl"],"align":["right"]},{"label":["weekday_is_tuesday"],"name":[47],"type":["dbl"],"align":["right"]},{"label":["weekday_is_friday"],"name":[48],"type":["dbl"],"align":["right"]},{"label":["kw_min_min"],"name":[49],"type":["dbl"],"align":["right"]},{"label":["kw_min_max"],"name":[50],"type":["dbl"],"align":["right"]},{"label":["max_positive_polarity"],"name":[51],"type":["dbl"],"align":["right"]},{"label":["min_positive_polarity"],"name":[52],"type":["dbl"],"align":["right"]},{"label":["num_self_hrefs"],"name":[53],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.1011866","3":"0.09656465","4":"0.08035302","5":"0.07661761","6":"0.06051878","7":"0.05194207","8":"0.04343202","9":"0.04212992","10":"0.04028168","11":"0.03915758","12":"0.03823958","13":"0.03701552","14":"0.03300678","15":"0.03199908","16":"0.03199387","17":"0.03125003","18":"0.03054614","19":"0.03048359","20":"0.02973602","21":"0.02955617","22":"0.02908938","23":"0.02825679","24":"0.02628946","25":"0.02474066","26":"0.02336134","27":"0.02232124","28":"0.02171243","29":"0.02152962","30":"0.02044572","31":"0.02011013","32":"0.01987719","33":"0.01868835","34":"0.01796884","35":"0.01461261","36":"0.01401512","37":"0.01231014","38":"0.01213934","39":"0.01210477","40":"0.01087195","41":"0.009829636","42":"0.008520382","43":"0.008347813","44":"0.007910551","45":"0.006912074","46":"0.006768011","47":"0.005262046","48":"0.004170195","49":"0.00331743","50":"0.001052058","51":"0.0003420754","52":"0.0001780103","53":"6.694316e-05"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
data <- data %>% 
            select(num_hrefs, n_tokens_content, n_unique_tokens, kw_avg_avg,
                   n_non_stop_unique_tokens, num_videos, min_negative_polarity,
                   rate_positive_words, min_positive_polarity, shares)
```

From here, we can look at a correlation plot of the chosen variables to
see if multicollinearity exists between any of them. Using `corrplot`,
we can check the relationships (correlations) between the chosen
variables. Though subjective in nature, a general rule of thumb is that
any correlation between variables (not involving the response) greater
than 0.70 or less than -0.70 is considered a problematic level of
multicollinearity. In the original assessment, `n_unique_tokens` had a
high correlation with 2 different variables, so it was dropped from the
dataset.

``` r
correlation <- cor(data)
corrplot(correlation, type = "upper", tl.pos = "lt")
corrplot(correlation, type = "lower", method = "number",
         add = TRUE, diag = FALSE, tl.pos = "n")
```

![](world_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
data <- data %>%
            select(-n_unique_tokens)

correlation <- cor(data)
corrplot(correlation, type = "upper", tl.pos = "lt")
corrplot(correlation, type = "lower", method = "number",
         add = TRUE, diag = FALSE, tl.pos = "n")
```

![](world_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

Next, we should look at some summary data with `summary()` to get a feel
for the ranges of the predictors and response.

``` r
summary <- summary(data)
summary  
```

    ##    num_hrefs     n_tokens_content   kw_avg_avg    n_non_stop_unique_tokens   num_videos      min_negative_polarity
    ##  Min.   :  0.0   Min.   :   0.0   Min.   :    0   Min.   :0.0000           Min.   : 0.0000   Min.   :-1.0000      
    ##  1st Qu.:  5.0   1st Qu.: 332.0   1st Qu.: 2070   1st Qu.:0.6280           1st Qu.: 0.0000   1st Qu.:-0.8000      
    ##  Median :  8.0   Median : 509.0   Median : 2404   Median :0.6837           Median : 0.0000   Median :-0.5000      
    ##  Mean   : 10.2   Mean   : 597.3   Mean   : 2525   Mean   :0.6653           Mean   : 0.5495   Mean   :-0.5607      
    ##  3rd Qu.: 13.0   3rd Qu.: 768.0   3rd Qu.: 2785   3rd Qu.:0.7350           3rd Qu.: 1.0000   3rd Qu.:-0.4000      
    ##  Max.   :161.0   Max.   :7081.0   Max.   :17839   Max.   :1.0000           Max.   :51.0000   Max.   : 0.0000      
    ##  rate_positive_words min_positive_polarity     shares      
    ##  Min.   :0.0000      Min.   :0.00000       Min.   :    35  
    ##  1st Qu.:0.5357      1st Qu.:0.05000       1st Qu.:   827  
    ##  Median :0.6429      Median :0.10000       Median :  1100  
    ##  Mean   :0.6234      Mean   :0.08849       Mean   :  2288  
    ##  3rd Qu.:0.7417      3rd Qu.:0.10000       3rd Qu.:  1900  
    ##  Max.   :1.0000      Max.   :0.80000       Max.   :284700

``` r
mean <- mean(data$shares)
paste0("The mean of shares is ", summary[4, 9])
```

    ## [1] "The mean of shares is Mean   :  2288  "

``` r
paste0("The 3rd Quartile of shares is ", summary[5, 9])
```

    ## [1] "The 3rd Quartile of shares is 3rd Qu.:  1900  "

``` r
sd <- sd(data$shares)
paste0("The standard deviation of shares is ", sd)
```

    ## [1] "The standard deviation of shares is 6089.66947588667"

``` r
paste0("The maximum value of shares is ", summary[6, 9])
```

    ## [1] "The maximum value of shares is Max.   :284700  "

``` r
interval <- 3*sd(data$shares) + mean(data$shares)
g <- ggplot(data, aes(x = shares))
g + geom_histogram()
```

![](world_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

We can see from the summary statistics and histogram that `shares` has a
very large max value, but a small mean, q3, and standard deviation
comparatively. This indicates that the distribution is strongly skewed
left. We can fix this by temporarily removing outliers to make data
visualization more meaningful. Using the mean and standard deviation for
`shares`, we can calculate an interval that gives us 3 standard
deviations above the mean, which means we will encompass 99.7% of the
data while removing outliers. Because we can’t have negative shares, we
will have values between 0 and 2.0556742^{4}.

First, we’ll take a look at a plot without the adjustment for
comparison’s sake.

``` r
g <- ggplot(data, aes(y = shares))
g + geom_point(aes(x = num_hrefs))
```

![](world_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
temp <- data %>%
          filter(shares < interval)
g <- ggplot(temp, aes(y = shares))
g + geom_point(aes(x = num_hrefs))
```

![](world_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

Clearly the adjustment does, indeed, make it easier to observe trends in
the data. Now we can inspect some scatter plots with our improved
`shares` variable.  
The above is a scatter plot of `num_hrefs` by `shares`. A positive trend
would indicate that shares should increase as the number of links in the
article increases. If we observe a negative trend, the number of shares
should decrease as the number of links increases.

Next is a scatter plot of `n_tokens_content` by `shares`. A positive
trend would indicate that shares should increase as the number of words
in the article increases; likewise, the number of shares should decrease
as word count increases if we observe a negative trend.

``` r
g + geom_point(aes(x = n_tokens_content))
```

![](world_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

And finally, we have a scatter plot of `rate_positive_words` by
`shares`. Again, a positive trend would indicate that we should observe
an increase in shares as as the proportion of positive words to
non-neutral words increases while a negative trend would indicate a
decrease in shares as the proportion of positive words to non-neutral
words increases.

``` r
g + geom_point(aes(x = rate_positive_words))
```

![](world_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Modeling

Now that we have some rudimentary understanding of the data, we can
explore predictions using different models. The first step in this
process is to split the data into a training and test set. We will use
the `createDataPartition` function to create an index on which to split
the data, using 70% for the training set and the remaining 30% for the
test set.

``` r
set.seed(250)
index <- createDataPartition(data$shares, p = 0.70, list = FALSE)
train <- data[index, ]
test <- data[-index, ]
```

## Multiple Linear Regression

A linear regression model attempts to use one or more predictor
variables to explain (or predict) a single response variable. In this
particular case, we will fit a multiple linear regression model, which
means we will use a linear combination of more than one variable to
explain our response variable, `shares`.

Using the `train` function, we will train the data using the `train`
object we created, making sure to preprocess the data by centering and
scaling it. Notice for a linear regression model, we use method `lm`; we
are also specifying the `trControl` option to run cross-validation with
five folds.

We can use the `predict` function to utilize the trained data for making
predictions on the test set.

``` r
mlrFit <- train(shares ~ ., data = train,
                preProcess = c("center", "scale"),
                method = "lm",
                trControl = trainControl(method = "cv", number = 5))
paste0("The results for the model fit on the training set are:")  
```

    ## [1] "The results for the model fit on the training set are:"

``` r
mlrFit$results
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["intercept"],"name":[1],"type":["lgl"],"align":["right"]},{"label":["RMSE"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Rsquared"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["MAE"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["RMSESD"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["RsquaredSD"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["MAESD"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"TRUE","2":"4962.06","3":"0.0163814","4":"1889.865","5":"1061.63","6":"0.00795676","7":"94.29995","_rn_":"1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
mlrPredict <- predict(mlrFit, newdata = test)
mlrPredictResults <- postResample(mlrPredict, test$shares)
paste0("The results for the model fit on the test set are:")
```

    ## [1] "The results for the model fit on the test set are:"

``` r
mlrPredictResults
```

    ##         RMSE     Rsquared          MAE 
    ## 7.945231e+03 2.594642e-03 1.963890e+03

``` r
mlrRsquare <- mlrPredictResults[2]
```

`mlrFit` gives us an R<sup>2</sup> value of **0.0163814 for the trained
data**, while `mlrRsquare` gives us an R<sup>2</sup> value of
**0.0025946 for the test set**. If the R<sup>2</sup> value for the
training set is low, the model is underfitting the data and more
exploration needs to be done in order to find a better model, better
predictor variables, or both.

If the training and test set R<sup>2</sup> values are similar, then we
can say the model generalizes well. If the training set R<sup>2</sup> is
much higher than the test set R<sup>2</sup>, then we can say the model
is overfitting the data, meaning the model does not generalize well to
data outside the training set.

## Random Forest

Now we will fit a more robust model – a random forest. This model is one
of many tree-based methods that splits up the predictor space into
regions with different predictions for each region. A random forest
utilizes a method known as “bootstrap aggregation”. This process takes a
single sample and then takes repeated samples of size n with replacement
(where n = the total number of observations in the original sample) to
create a distribution of bootstrapped samples which greatly reduces
variance. What I have described so far is known as a “bagged tree”
method; a random forest takes the idea of a bagged tree one step further
by also taking a random subset of predictor variables for each bootstrap
sample. If a very strong predictor exists, every bootstrap sample will
likely use it for the first split. This is problematic because it will
cause the trees produced to have more strongly correlated predictions
which, in turn, diminishes the variance reduction from the bootstrap
aggregation process described earlier. Choosing a random subset of
predictors mitigates this problem by greatly increasing the likelihood
that any given tree does not select the dominating predictor variable.

Now that we have an understanding of how the method works, we can set up
a random forest model for our data. Just like the linear regression
model, we can use the `train` function, but this time with method `rf`.
Although preprocessing is not necessary for tree-based methods, I still
like to include it in my models as a best practice, lest I forget to use
it when it is necessary. The `tuneGrid` option is where we can tell the
model how many predictor variables to grab per bootstrap sample. The
default is (number of predictors)/3 for regression, but I still like to
do this as best practice again. The `trainControl` function within the
`trControl` option is how we set up options like the cross-validation we
will use here. Finally, as with the multiple linear regression above, we
can use the trained data to make predictions on the test set.

``` r
forest <- train(shares ~ ., data = train,
                method = "rf",
                preProcess = c("center", "scale"),
                tuneGrid = data.frame(mtry = ncol(train)/3),
                trControl = trainControl(method = "cv", number = 5))
paste0("The results for the model fit on the training set are:")  
```

    ## [1] "The results for the model fit on the training set are:"

``` r
forest$results
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["mtry"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["RMSE"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Rsquared"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["MAE"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["RMSESD"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["RsquaredSD"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["MAESD"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"3","2":"5054.269","3":"0.01460181","4":"2009.329","5":"1032.328","6":"0.007034915","7":"117.2443","_rn_":"1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
forestPredict <- predict(forest, newdata = test)
forestPredictResults <- postResample(forestPredict, test$shares)
paste0("The results for the model fit on the test set are:")
```

    ## [1] "The results for the model fit on the test set are:"

``` r
forestPredictResults
```

    ##         RMSE     Rsquared          MAE 
    ## 8.004234e+03 3.748422e-03 2.096752e+03

``` r
forestRsquare <- forestPredictResults[2]
```

`forest` gives us an R<sup>2</sup> value of **0.0146018 for the trained
data**, while `forestRsquare` gives us an R<sup>2</sup> value of
**0.0037484 for the test set**.

# Comparison

To compare the models, we will use a simple comparison of R<sup>2</sup>
and choose the one with the highest value. We will use this method since
R<sup>2</sup> can be interpreted as how much of the variance in the data
can be explained by the model, i.e., how well the model fits.

``` r
if (mlrRsquare > forestRsquare) {
  paste0("Multiple linear regression is the preferred model for data channel = ", params$channel)
} else {
  paste0("Random forest is the preferred model for data channel = ", params$channel)
} 
```

    ## [1] "Random forest is the preferred model for data channel = world"
