library(testthat)
library(Regression)


test_that("Checking if the output is same",{
  
  expect_that(as.numeric(linreg(formula = as.formula(Petal.Length ~ Petal.Width), data = iris)$beta),
              equals(as.numeric(lm(formula =as.formula(Petal.Length ~ Petal.Width), data = iris)$coefficients)))
  
  expect_that(as.numeric(linreg(formula =as.formula(Petal.Length ~ Petal.Width), data =iris)$resd),
              equals(as.numeric(lm(formula =as.formula(Petal.Length ~ Petal.Width), data = iris)$residuals)))
  
  expect_that(as.numeric(linreg(formula =as.formula(Petal.Length ~ Petal.Width), data =iris)$fitted),
              equals(as.numeric(lm(formula =as.formula(Petal.Length ~ Petal.Width), data = iris)$fitted.values)))
  
  expect_that(linreg(formula =as.formula(Petal.Length ~ Petal.Width), data = iris)$df,
              equals(148))
  
  
}
          )