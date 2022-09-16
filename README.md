readme
================
Santiago Franco Valencia
14/9/2022

# Summary of GLM and LM

Se resumen las funciones utilizadas en los capítulos 3, 4, 5 y 6 del
texto guía de modelos jerarquícos:

## Linear regression

![linear regression](https://i.stack.imgur.com/83Jog.png)

``` r
data(cars)
cars.lm <- lm(dist ~ speed, data = cars)
summary(cars.lm)
```

    ## 
    ## Call:
    ## lm(formula = dist ~ speed, data = cars)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -29.069  -9.525  -2.272   9.215  43.201 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -17.5791     6.7584  -2.601   0.0123 *  
    ## speed         3.9324     0.4155   9.464 1.49e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.38 on 48 degrees of freedom
    ## Multiple R-squared:  0.6511, Adjusted R-squared:  0.6438 
    ## F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12

``` r
new.dat <- data.frame(speed=30)
#Confidence intervals and predictions
predict(cars.lm, newdata = new.dat, interval = 'confidence', level = 0.95)
```

    ##        fit      lwr      upr
    ## 1 100.3932 87.43543 113.3509

``` r
##See predict.lm() for more documentation
```

## Logistic regression

![Logit function](https://i.stack.imgur.com/WY61Z.png)

### functions

``` r
logit <- function(x){
  return(log((x)/(1-x)))
}

inverse_logit <- function(x){
  return(exp(x)/(1+exp(x)))
}
```

### Fitting logistic regression models:

``` r
formula = ""
#logit_model <- glm(formula = formula, family=binomial(link='logit'))
```

### Interpretation of logistic regression coeficients:

-   Constant term can be interpreted as the estimated probability when
    other variables has the value of 0. (The “Weight” of the constant
    term).

-   The pendient terms can be interpreted as the estimated probability
    per unit by deriving the predictor inside the linf formula.

### Graphic logistic regretion:

``` r
#Graph logit func:
## Plot x and y values
#plot(x,y)
## Plot curve of the model using the formula
#curve()
```

## Generalized linear models

Allos the answer Y to be normal, binomial, poisson, negative-binomial,
gamma, and inverse gaussian.

The variable Y is not modeled insted the mu parameter of the variable Y
is modeled.

``` r
#Adjust model
#glm(formula, data=data, family=linkfunction())
#Predict
#link = predict(nb1, type = "link"),
#fit = predict(nb1, type = "response"),
```

The link function allows to map the predictor variable values inside the
correct parameter of the distribution assumed for Y.

It is important to know that te parameter *family* of the glm package
allows to assume certain distribution for the Y variable.
