#' abc for R
#'
#' @param a quadratic coefficient
#' @param b linear coefficient
#' @param c constant
#' @return root
#' @export
abc <- function(a){
  print(" 
    #gde ' tam dve '
    # source('Ci.R')      # Подключаем файл Ci.R данной командой 
      #-------------------------------------------------------------------------------------
      # Доверительные интервалы
      # ciE(Rez,0.95)       # Вычисляет доверительный интервал для E по выборке Rez
      # ciD(Rez,0.95)       # Вычисляет доверительный интервал для D по выборке Rez
      
      ciE <- function(X, Gamma) {
        n <- length(X)
        ciE <- mean(X, na.rm = TRUE)-sd(X, na.rm = TRUE)/sqrt(n)*qt((1+c(Gamma,0,-Gamma))/2,n-1)
        Interval <- cbind(E.Left = ciE[1],E.Right = ciE[3])
        Res <- list(Eo = ciE[2], Interval = Interval)
        return(Res)
      }
      #--------------------------
      # Доверительный интервал для генеральной дисперсии (или для истинной дисперсии) по выборке
      ciD <- function(X, Gamma) {
        n <- length(X)
        ciD <- sd(X, na.rm = TRUE)^2*(n-1)/qchisq((1+c(Gamma,0,-Gamma))/2,n-1)
        Interval <- cbind(D.Left = ciD[1], D.Right = ciD[3])
        Res <- list(Do = var(X), Interval = Interval)
        return(Res)
      }
      #-----------------------------------------------------------------------------------------
      # Доверительный интервал для генеральной доли (или для истинной вероятности) по выборке
      ciP <- function(k,n,N,Gamma) {
        if (N == Inf) {ciP <- k/n-sqrt(k/n*(1-k/n)/n)*qnorm((1+c(Gamma,0,-Gamma))/2)} 
        else  {ciP <- k/n-sqrt(k/n*(1-k/n)/n)*sqrt(1-n/N)*qnorm((1+c(Gamma,0,-Gamma))/2)}
        Interval <- cbind(P.Left = max(0,ciP[1]), P.Right = min(1,ciP[3]))
        Res <- list(Po = ciP[2], Interval = Interval)
        return(Res)
      }
      ciP(10,21,Inf,0.95)  # Для повторной выборки
      ciP(10,21,100,0.95)  # Для бесповторной выборки с указанием объема генеральной совокупности
      #-------------------------------------------------------------------------------------
      # Непрерывная с.в. X с произвольно заданной функцией плотности f(x) на [a, b], включая b = inf.
      # ВАЖНО! Необходимая Функция плотности f <- function(x) {x*exp(-x)} должна быть объявлена заранее
      
      # is.fun(f, a = 0, b = Inf)        # Проверяет нормировку функции плотности f 
      # rfun(n = 300, f, a = 0, b = 4)   # Наблюдения n раз случайной величины X ~ f(x), локализованной на [a, b]
      # pfun(q = 2, f, a = 0, b = 3)     # Вероятность P[X ≤ q] для X ~ f(x), локализованной на [a, b]
      # dfun(x = 3, f, a = -5, b = Inf)  # Плотность f(x) для X ~ f(x), локализованной на [a, b]
      # qfun(p = 0.5, f, a = 4, b = 100) # Квантиль уровня p для X ~ f(x), локализованной на [a, b]
      
      #--------------------------
      # Проверяет условие нормировки функции плотности вероятности f на области локализации [a, b] 
      
      is.fun <- function(f, a, b) {
        if (abs(integrate(f,a,b)$value - 1) > 10^(-8)) {return(paste(integrate(f,a,b)$value,' Opps...! f is not density!'))}
        else { return('OK!')}
      }
      
      #--------------------------
      # Разыгрывает n раз случайную величину X c плотностью вероятности f, локализованную на [a, b]
      # P.S. Параметр step - шаг по x при вычислении интегрального уравнения, по умолчанию 0.01
      
      rfun <- function(n, f, a, b, step = 0.01) {
        X <- rep(a, n) # Назначаем X в начале каждого i-го опыта из n самому левому значению: a
        Y <- runif(n)    # n раз генерируем равномерную с.в. Y на (0,1)
        for (i in 1:n) {
          I <- 0
          while(I < Y[i]) { # Этот цикл решает интегральное уравнение для генерации X по Y
            X[i] <- X[i] + step
            I <- integrate(f, a, X[i])$value
          }
          X[i] <- X[i] - step
        }
        return(X)
      }
      
      #--------------------------
      # Вычисляет вероятность P(X ≤ q) для X ~ f(x), локализованной на [a, b].
      
      pfun <- function(q, f, a, b) {
        if (q < a) {return(0)} else 
          if (q > b) {return(1)} else {
            P <- sapply(q, function(q) {integrate(f, a, q)$value})
            return(P) 
          }
      }
      
      #--------------------------
      # Вычисляет значение плотности вероятности f в точке x для X ~ f(x), локализованной на [a, b] .
      
      dfun <- function(x, f, a, b) {
        return(f(x)) 
      }  
      
      #--------------------------
      # Вычисляет квантиль уровня p для X ~ f(x), локализованной на [a, b] .
      # P.S. Параметр step - шаг по x при вычислении интегрального уравнения, по умолчанию 0.001
      
      qfun <- function(p, f, a, b, step = 0.01) {
        q <- a
        I <- 0
        while(I < p) { # Этот цикл решает интегральное уравнение
          q <- q + step
          I <- integrate(f, a, q)$value
        }
        return(q - step) 
      }
      #---------Проверка равенства долей--------------------
      EqP <- function(k1, k2, n1, n2) {
        p1 <- k1/n1; p2 <- k2/n2; p <- (k1+k2)/(n1+n2)
        Z <- (p1-p2-0.5/(n1+n2))/(p*(1-p)*(1/n1+1/n2))^0.5
        #  by(X, Y, mean)
        Res <- paste('Pvalue = ', round(2*(1-pnorm(abs(Z))), 5))
        return(Res)
      })
  }")
}
