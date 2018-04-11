Bootstrap is basically used to check the accuracy of the model. It can be applied in almost all situations. Bootstrap analysis in R has two steps, 1. Create a function that computes statistic of interest. 2. use `boot()` function to perform bootstrap by repeatedly sampling observations from the dataset with replacement.

Estimating the Accuracy of a Linear Regression Model The bootstarp approach can be used to assess the variability of the coefficient estimates and predictions from a statistical learning method. In this example we'll use bootstrap approach to assess the variability of the estimates for `beta-zero`(intercept) and `beta-one`(slope) terms for the linear regression model that uses `horsepower` to predict `mpg` in the `Auto` dataset.

First create a function, `boot.fn` which returns the intercept and slope for the linear regression model.

``` r
options(warn = -1)
library(ISLR)
library(boot)
attach(Auto)
```

``` r
boot.fn = function(input_data, index){
  return(coef(lm(mpg ~ horsepower, data = input_data, subset = index)))
}
```

now use `boot()` function to compue standard errors 1000 bootstrap estimates for the intercept and slope terms. Here

``` r
set.seed(1)
boot(Auto, boot.fn, 1000)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = Auto, statistic = boot.fn, R = 1000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##       original        bias    std. error
    ## t1* 39.9358610  0.0269563085 0.859851825
    ## t2* -0.1578447 -0.0002906457 0.007402954

boostrap estimate for SE(beta-zero) is 0.86 and SE(beta-one) is 0.0074 Now using standard formulas to compute the SE for regression coeff of linear model

``` r
summary(lm(mpg ~ horsepower, data = Auto))$coef
```

    ##               Estimate  Std. Error   t value      Pr(>|t|)
    ## (Intercept) 39.9358610 0.717498656  55.65984 1.220362e-187
    ## horsepower  -0.1578447 0.006445501 -24.48914  7.031989e-81

here SE for beta-zero is 0.71 and beta-one is 0.0064 which is different from above. Standard formulas rely on certain assumptions. 1. They depend on an unknown parameter `noise variance`. We estimate this variance using RSS. For Auto dataset there is a non-linear relationship in data. So the residulas fit will be inflated and so will be variance. 2. all x are fixed and and all the variablity comes from the variation in the errors.

The bootstrap doesn't rely on these assumptions ans hence provide acccurate SE than `summary` function.

``` r
detach(Auto)
```
