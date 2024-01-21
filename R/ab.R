#' ab for R
#'
#' @param a quadratic coefficient
#' @param b linear coefficient
#' @param c constant
#' @return root
#' @export
aa <- function(a){
  cat("	
library(AFR)
Gauss-Markov theorem

2 tests for detecting heteroskedasticity:

Breusch-Pagan Test
bp(model)

Goldfeld-Quandt Test
gq(model)

3 tests for detecting multicollinearity and autocorrelation:

VIF test
vif_reg(model)

Durbin Watson Test
dwtest(model)

Breusch-Godfrey Test
bg(model)

4 tests for detecting normality:

Shapiro-Wilk test
Kolmogorov-Smirnov test
Cramer-Von Mises test
Anderson test

vyzyvautsya odnoy functie
ols_test_normality(model)
"
     )
}
ab <- function(a){
  cat("
library(lmtest)
library(car)
library(AFR)

y <- c()

x1 <- c()

x2 <- c()

x3 <- c()

x4 <- c()

# try different x for reduce error and increase R
model <- lm(y~x1+x2+x3+x4)
summary(model)

#mean aproximation
predicted<-model$fitted.values
MAE <- function (Obs, Prd, dgt=3){
  ErrorSq = sum(abs(Prd - Obs)/Prd)/length(Obs) * 100
  return(ErrorSq)
}
MAE(y, predicted)
"
     )
}
