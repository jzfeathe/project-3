project-3
================
Justin Feathers
2022-11-01

-   <a href="#introduction" id="toc-introduction">Introduction</a>
-   <a href="#data" id="toc-data">Data</a>
-   <a href="#summarizations" id="toc-summarizations">Summarizations</a>
    -   <a href="#correlation" id="toc-correlation">Correlation</a>
    -   <a href="#plots" id="toc-plots">Plots</a>
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

Now that we have the data read in, what do we do with it? We should
perform an exploratory data analysis to get an idea of what we’re
working with and give some summarizations in the form of tables and
plots.

## Correlation

A good way to start is by checking how strongly all variables are
correlated to the response variable of interest. This is the beginning
of the variable selection process. Since we are interested in predicting
`shares`, I created a correlation matrix using the `cor` function and
sorted the absolute values of the output to get a convenient tibble of
descending correlation values; this was originally done using the `tech`
data channel to set the ground work. I decided to choose the 9 variables
with the highest correlation with `shares`.

``` r
dataCor <- cor(data$shares, data) %>%
        as.tibble() %>%
        abs() %>%
        sort(decreasing = TRUE)
dataCor
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["shares"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["num_hrefs"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["n_tokens_content"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["n_unique_tokens"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["kw_avg_avg"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["n_non_stop_unique_tokens"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["num_videos"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["min_negative_polarity"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["rate_positive_words"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["min_positive_polarity"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["is_weekend"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["rate_negative_words"],"name":[12],"type":["dbl"],"align":["right"]},{"label":["avg_negative_polarity"],"name":[13],"type":["dbl"],"align":["right"]},{"label":["kw_min_avg"],"name":[14],"type":["dbl"],"align":["right"]},{"label":["kw_max_avg"],"name":[15],"type":["dbl"],"align":["right"]},{"label":["max_negative_polarity"],"name":[16],"type":["dbl"],"align":["right"]},{"label":["title_sentiment_polarity"],"name":[17],"type":["dbl"],"align":["right"]},{"label":["global_sentiment_polarity"],"name":[18],"type":["dbl"],"align":["right"]},{"label":["abs_title_sentiment_polarity"],"name":[19],"type":["dbl"],"align":["right"]},{"label":["weekday_is_sunday"],"name":[20],"type":["dbl"],"align":["right"]},{"label":["LDA_00"],"name":[21],"type":["dbl"],"align":["right"]},{"label":["global_rate_negative_words"],"name":[22],"type":["dbl"],"align":["right"]},{"label":["max_positive_polarity"],"name":[23],"type":["dbl"],"align":["right"]},{"label":["global_rate_positive_words"],"name":[24],"type":["dbl"],"align":["right"]},{"label":["num_keywords"],"name":[25],"type":["dbl"],"align":["right"]},{"label":["weekday_is_thursday"],"name":[26],"type":["dbl"],"align":["right"]},{"label":["weekday_is_saturday"],"name":[27],"type":["dbl"],"align":["right"]},{"label":["title_subjectivity"],"name":[28],"type":["dbl"],"align":["right"]},{"label":["weekday_is_wednesday"],"name":[29],"type":["dbl"],"align":["right"]},{"label":["abs_title_subjectivity"],"name":[30],"type":["dbl"],"align":["right"]},{"label":["kw_min_min"],"name":[31],"type":["dbl"],"align":["right"]},{"label":["self_reference_avg_sharess"],"name":[32],"type":["dbl"],"align":["right"]},{"label":["LDA_04"],"name":[33],"type":["dbl"],"align":["right"]},{"label":["weekday_is_monday"],"name":[34],"type":["dbl"],"align":["right"]},{"label":["kw_avg_max"],"name":[35],"type":["dbl"],"align":["right"]},{"label":["self_reference_max_shares"],"name":[36],"type":["dbl"],"align":["right"]},{"label":["self_reference_min_shares"],"name":[37],"type":["dbl"],"align":["right"]},{"label":["weekday_is_tuesday"],"name":[38],"type":["dbl"],"align":["right"]},{"label":["LDA_01"],"name":[39],"type":["dbl"],"align":["right"]},{"label":["LDA_03"],"name":[40],"type":["dbl"],"align":["right"]},{"label":["global_subjectivity"],"name":[41],"type":["dbl"],"align":["right"]},{"label":["num_imgs"],"name":[42],"type":["dbl"],"align":["right"]},{"label":["n_non_stop_words"],"name":[43],"type":["dbl"],"align":["right"]},{"label":["kw_min_max"],"name":[44],"type":["dbl"],"align":["right"]},{"label":["n_tokens_title"],"name":[45],"type":["dbl"],"align":["right"]},{"label":["avg_positive_polarity"],"name":[46],"type":["dbl"],"align":["right"]},{"label":["num_self_hrefs"],"name":[47],"type":["dbl"],"align":["right"]},{"label":["kw_max_max"],"name":[48],"type":["dbl"],"align":["right"]},{"label":["average_token_length"],"name":[49],"type":["dbl"],"align":["right"]},{"label":["weekday_is_friday"],"name":[50],"type":["dbl"],"align":["right"]},{"label":["LDA_02"],"name":[51],"type":["dbl"],"align":["right"]},{"label":["kw_avg_min"],"name":[52],"type":["dbl"],"align":["right"]},{"label":["kw_max_min"],"name":[53],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"0.0779286","3":"0.07136418","4":"0.05378062","5":"0.05215783","6":"0.04263356","7":"0.03393982","8":"0.03032356","9":"0.02937818","10":"0.02931108","11":"0.02856703","12":"0.02821046","13":"0.02583319","14":"0.02452682","15":"0.02441971","16":"0.02416909","17":"0.0239414","18":"0.02369962","19":"0.0236575","20":"0.02283931","21":"0.02196288","22":"0.02153411","23":"0.02131067","24":"0.01934008","25":"0.01894544","26":"0.01692019","27":"0.01669957","28":"0.01664752","29":"0.01573821","30":"0.01536497","31":"0.01503377","32":"0.0144814","33":"0.01267426","34":"0.01249449","35":"0.0114642","36":"0.01098863","37":"0.01082528","38":"0.01048675","39":"0.009769622","40":"0.009607673","41":"0.009474031","42":"0.008224127","43":"0.005783596","44":"0.004178993","45":"0.003284668","46":"0.003141542","47":"0.002185302","48":"0.002105939","49":"0.001188936","50":"0.0009384798","51":"0.0006896233","52":"0.0005002068","53":"0.0003494592"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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

![](tech_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
data <- data %>%
            select(-n_unique_tokens)

correlation <- cor(data)
corrplot(correlation, type = "upper", tl.pos = "lt")
corrplot(correlation, type = "lower", method = "number",
         add = TRUE, diag = FALSE, tl.pos = "n")
```

![](tech_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

Next, we should look at some summary data with `summary()` to get a feel
for the ranges of the predictors and response.

``` r
summary <- summary(data)
summary  
```

    ##    num_hrefs       n_tokens_content   kw_avg_avg    n_non_stop_unique_tokens   num_videos      min_negative_polarity
    ##  Min.   :  0.000   Min.   :   0.0   Min.   :    0   Min.   :0.0000           Min.   : 0.0000   Min.   :-1.0000      
    ##  1st Qu.:  5.000   1st Qu.: 256.0   1st Qu.: 2346   1st Qu.:0.6165           1st Qu.: 0.0000   1st Qu.:-0.6000      
    ##  Median :  7.000   Median : 405.0   Median : 2698   Median :0.6897           Median : 0.0000   Median :-0.4000      
    ##  Mean   :  9.417   Mean   : 571.6   Mean   : 2746   Mean   :0.6829           Mean   : 0.4472   Mean   :-0.4513      
    ##  3rd Qu.: 11.000   3rd Qu.: 728.0   3rd Qu.: 3079   3rd Qu.:0.7568           3rd Qu.: 1.0000   3rd Qu.:-0.2500      
    ##  Max.   :120.000   Max.   :5530.0   Max.   :19429   Max.   :1.0000           Max.   :73.0000   Max.   : 0.0000      
    ##  rate_positive_words min_positive_polarity     shares      
    ##  Min.   :0.0000      Min.   :0.00000       Min.   :    36  
    ##  1st Qu.:0.6667      1st Qu.:0.05000       1st Qu.:  1100  
    ##  Median :0.7524      Median :0.10000       Median :  1700  
    ##  Mean   :0.7466      Mean   :0.09917       Mean   :  3072  
    ##  3rd Qu.:0.8333      3rd Qu.:0.10000       3rd Qu.:  3000  
    ##  Max.   :1.0000      Max.   :1.00000       Max.   :663600

``` r
mean <- mean(data$shares)
paste0("The mean of shares is ", summary[4, 9])
```

    ## [1] "The mean of shares is Mean   :  3072  "

``` r
paste0("The 3rd Quartile of shares is ", summary[5, 9])
```

    ## [1] "The 3rd Quartile of shares is 3rd Qu.:  3000  "

``` r
sd <- sd(data$shares)
paste0("The standard deviation of shares is ", sd)
```

    ## [1] "The standard deviation of shares is 9024.34380284528"

``` r
paste0("The maximum value of shares is ", summary[6, 9])
```

    ## [1] "The maximum value of shares is Max.   :663600  "

``` r
interval <- 3*sd(data$shares) + mean(data$shares)
g <- ggplot(data, aes(x = shares))
g + geom_histogram()
```

![](tech_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

We can see from the summary statistics and histogram that `shares` has a
very large max value, but a small mean, q3, and standard deviation
comparatively. This indicates that the distribution is strongly skewed
left. We can fix this by temporarily removing outliers to make data
visualization more meaningful. Using the mean and standard deviation for
`shares`, we can calculate an interval that gives us 3 standard
deviations above the mean, which means we will encompass 99.7% of the
data while removing outliers. Because we can’t have negative shares, we
will have values between 0 and 3.0145315^{4}.

## Plots

First, we’ll take a look at a plot without the adjustment for
comparison’s sake.

``` r
g <- ggplot(data, aes(y = shares))
g + geom_point(aes(x = num_hrefs), color = "darkblue") +
    labs(x = "Links", 
         y = "Shares",
         title = "Shares by Links in Article") +
         theme(plot.title = element_text(hjust = 0.5))
```

![](tech_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
temp <- data %>%
          filter(shares < interval)
g <- ggplot(temp, aes(y = shares))
g + geom_point(aes(x = num_hrefs), color = "darkblue") +
    labs(x = "Links",
         y = "Shares",
         title = "Shares by Links in Article") +
         theme(plot.title = element_text(hjust = 0.5))
```

![](tech_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

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
g + geom_point(aes(x = n_tokens_content), color = "#355E3B") +
    labs(x = "Words",
         y = "Shares",
         title = "Shares by Words in Article") +
         theme(plot.title = element_text(hjust = 0.5))
```

![](tech_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

And finally, we have a scatter plot of `rate_positive_words` by
`shares`. Again, a positive trend would indicate that we should observe
an increase in shares as as the proportion of positive words to
non-neutral words increases while a negative trend would indicate a
decrease in shares as the proportion of positive words to non-neutral
words increases.

``` r
g + geom_point(aes(x = rate_positive_words), color = "#593876") +
    labs(x = "Proportion of Positive Words",
         y = "Shares",
         title = "Shares by Proportion of Positive Words to Non-Neutral Words") +
         theme(plot.title = element_text(hjust = 0.5))
```

![](tech_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

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
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["intercept"],"name":[1],"type":["lgl"],"align":["right"]},{"label":["RMSE"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Rsquared"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["MAE"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["RMSESD"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["RsquaredSD"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["MAESD"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"TRUE","2":"4661.786","3":"0.02937314","4":"2199.596","5":"747.0569","6":"0.02020692","7":"76.93005","_rn_":"1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
    ## 1.475628e+04 4.651945e-03 2.492095e+03

``` r
mlrRsquare <- mlrPredictResults[2]
```

`mlrFit` gives us an R<sup>2</sup> value of **0.0293731 for the trained
data**, while `mlrRsquare` gives us an R<sup>2</sup> value of
**0.0046519 for the test set**. If the R<sup>2</sup> value for the
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
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["mtry"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["RMSE"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Rsquared"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["MAE"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["RMSESD"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["RsquaredSD"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["MAESD"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"3","2":"4748.288","3":"0.03671395","4":"2299.692","5":"635.7111","6":"0.02398351","7":"65.37647","_rn_":"1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
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
    ## 1.474842e+04 5.736350e-03 2.564333e+03

``` r
forestRsquare <- forestPredictResults[2]
```

`forest` gives us an R<sup>2</sup> value of **0.036714 for the trained
data**, while `forestRsquare` gives us an R<sup>2</sup> value of
**0.0057363 for the test set**.

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

    ## [1] "Random forest is the preferred model for data channel = tech"
