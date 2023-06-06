#' ab for R
#'
#' @param a quadratic coefficient
#' @param b linear coefficient
#' @param c constant
#' @return root
#' @export
ab <- function(a){
  cat(" 1) Opisatelnaya statistika
       Var(X)	
       33. n	SChET(F:F)
       34. s^2	DISP.V(F:F)
       35. xi2 lev	KhI2.OBR(I37/2;I33-1)
       36. khi2 prav	KhI2.OBR(1-I37/2;I33-1)
      Test 1
      N0: r=0	statisticheski neznachim
      H1: rne=0	statisticheski znachim
      PV < 0,005 => N0 otklonyaetsya koef korrel statisticheski znachim
      dlya 3 kompaniy po stolbtsu logdokhodnosti
      cor.test(X$V1, Y$V1) 
      cor.test(X$V1, Y$V1)$p.value
      cor.test(X$V1, Y$V1)$p.value<0.005

      Test 2
      𝐻0:𝐸(𝑋𝑖)=0		alfa=0,05
      𝐻1:𝐸(𝑋𝑖)≠0
      t.test(X$V1, mu=0)  $p.value   	
      t.test(X$V1, mu=0)  $p.value <0.05 
      bolshe alfa	ne otklonyaetsya
      menshe alfa	otklonyaetsya

      Test 3
      𝐻0:𝐸(𝑋𝑖)=𝐸(𝑋𝑗)		
      𝐻1:𝐸(𝑋𝑖)≠𝐸(𝑋𝑗)		
      alfa=0,05
      t.test(X$V1, Y$V1)$p.value 		
      t.test(X$V1, Y$V1)$p.value <0.05 		

      Test 4
      N0: Var(X)=Var(Y)	
      H1: Var(X)ne=Var(Y)	
      alfa=0,05	
      var.test(X$V1, Y$V1)$p.value 		
      var.test(X$V1, Y$V1)$p.value <0.05 		

      Test 5
      lillie.test(X$V1) 		
      lillie.test(X$V1)$p.value 		
      lillie.test(X$V1)$p.value < 0.001 		

    #gde ' tam dve '
    # source('Ci.R')      # Podklyuchaem fayl Ci.R dannoy komandoy 
      #-------------------------------------------------------------------------------------
      # Doveritelnye intervaly
      # ciE(Rez,0.95)       # Vychislyaet doveritelnyy interval dlya E po vyborke Rez
      # ciD(Rez,0.95)       # Vychislyaet doveritelnyy interval dlya D po vyborke Rez
      
      ciE <- function(X, Gamma) {
        n <- length(X)
        ciE <- mean(X, na.rm = TRUE)-sd(X, na.rm = TRUE)/sqrt(n)*qt((1+c(Gamma,0,-Gamma))/2,n-1)
        Interval <- cbind(E.Left = ciE[1],E.Right = ciE[3])
        Res <- list(Eo = ciE[2], Interval = Interval)
        return(Res)
      }
      #--------------------------
      # Doveritelnyy interval dlya generalnoy dispersii (ili dlya istinnoy dispersii) po vyborke
      ciD <- function(X, Gamma) {
        n <- length(X)
        ciD <- sd(X, na.rm = TRUE)^2*(n-1)/qchisq((1+c(Gamma,0,-Gamma))/2,n-1)
        Interval <- cbind(D.Left = ciD[1], D.Right = ciD[3])
        Res <- list(Do = var(X), Interval = Interval)
        return(Res)
      }
      #-----------------------------------------------------------------------------------------
      # Doveritelnyy interval dlya generalnoy doli (ili dlya istinnoy veroyatnosti) po vyborke
      ciP <- function(k,n,N,Gamma) {
        if (N == Inf) {ciP <- k/n-sqrt(k/n*(1-k/n)/n)*qnorm((1+c(Gamma,0,-Gamma))/2)} 
        else  {ciP <- k/n-sqrt(k/n*(1-k/n)/n)*sqrt(1-n/N)*qnorm((1+c(Gamma,0,-Gamma))/2)}
        Interval <- cbind(P.Left = max(0,ciP[1]), P.Right = min(1,ciP[3]))
        Res <- list(Po = ciP[2], Interval = Interval)
        return(Res)
      }
      ciP(10,21,Inf,0.95)  # Dlya povtornoy vyborki
      ciP(10,21,100,0.95)  # Dlya bespovtornoy vyborki s ukazaniem obema generalnoy sovokupnosti
      #-------------------------------------------------------------------------------------
      # Nepreryvnaya s.v. X s proizvolno zadannoy funktsiey plotnosti f(x) na [a, b], vklyuchaya b = inf.
      # VAZhNO! Neobkhodimaya Funktsiya plotnosti f <- function(x) {x*exp(-x)} dolzhna byt obyavlena zaranee
      
      # is.fun(f, a = 0, b = Inf)        # Proveryaet normirovku funktsii plotnosti f 
      # rfun(n = 300, f, a = 0, b = 4)   # Nablyudeniya n raz sluchaynoy velichiny X ~ f(x), lokalizovannoy na [a, b]
      # pfun(q = 2, f, a = 0, b = 3)     # Veroyatnost P[X ≤ q] dlya X ~ f(x), lokalizovannoy na [a, b]
      # dfun(x = 3, f, a = -5, b = Inf)  # Plotnost f(x) dlya X ~ f(x), lokalizovannoy na [a, b]
      # qfun(p = 0.5, f, a = 4, b = 100) # Kvantil urovnya p dlya X ~ f(x), lokalizovannoy na [a, b]
      
      #--------------------------
      # Proveryaet uslovie normirovki funktsii plotnosti veroyatnosti f na oblasti lokalizatsii [a, b] 
      
      is.fun <- function(f, a, b) {
        if (abs(integrate(f,a,b)$value - 1) > 10^(-8)) {return(paste(integrate(f,a,b)$value,' Opps...! f is not density!'))}
        else { return('OK!')}
      }
      
      #--------------------------
      # Razygryvaet n raz sluchaynuyu velichinu X c plotnostyu veroyatnosti f, lokalizovannuyu na [a, b]
      # P.S. Parametr step - shag po x pri vychislenii integralnogo uravneniya, po umolchaniyu 0.01
      
      rfun <- function(n, f, a, b, step = 0.01) {
        X <- rep(a, n) # Naznachaem X v nachale kazhdogo i-go opyta iz n samomu levomu znacheniyu: a
        Y <- runif(n)    # n raz generiruem ravnomernuyu s.v. Y na (0,1)
        for (i in 1:n) {
          I <- 0
          while(I < Y[i]) { # Etot tsikl reshaet integralnoe uravnenie dlya generatsii X po Y
            X[i] <- X[i] + step
            I <- integrate(f, a, X[i])$value
          }
          X[i] <- X[i] - step
        }
        return(X)
      }
      
      #--------------------------
      # Vychislyaet veroyatnost P(X ≤ q) dlya X ~ f(x), lokalizovannoy na [a, b].
      
      pfun <- function(q, f, a, b) {
        if (q < a) {return(0)} else 
          if (q > b) {return(1)} else {
            P <- sapply(q, function(q) {integrate(f, a, q)$value})
            return(P) 
          }
      }
      
      #--------------------------
      # Vychislyaet znachenie plotnosti veroyatnosti f v tochke x dlya X ~ f(x), lokalizovannoy na [a, b] .
      
      dfun <- function(x, f, a, b) {
        return(f(x)) 
      }  
      
      #--------------------------
      # Vychislyaet kvantil urovnya p dlya X ~ f(x), lokalizovannoy na [a, b] .
      # P.S. Parametr step - shag po x pri vychislenii integralnogo uravneniya, po umolchaniyu 0.001
      
      qfun <- function(p, f, a, b, step = 0.01) {
        q <- a
        I <- 0
        while(I < p) { # Etot tsikl reshaet integralnoe uravnenie
          q <- q + step
          I <- integrate(f, a, q)$value
        }
        return(q - step) 
      }
      #---------Proverka ravenstva doley--------------------
      EqP <- function(k1, k2, n1, n2) {
        p1 <- k1/n1; p2 <- k2/n2; p <- (k1+k2)/(n1+n2)
        Z <- (p1-p2-0.5/(n1+n2))/(p*(1-p)*(1/n1+1/n2))^0.5
        #  by(X, Y, mean)
        Res <- paste('Pvalue = ', round(2*(1-pnorm(abs(Z))), 5))
        return(Res)
      })
  }
  library(psych)
  library(e1071)
  data <- c(1.46, -0.62, -0.21, -0.71, 0.11, -0.24, 0.37, 0.44, -0.64, -0.77, -0.06, 0.03, 0.25, 0.44, -1.07, 0.49, 0.4, -0.44, 0.02, 0.31, 0.67, 1.22, -1.12, 1.56, 0.9, -0.8, 0.04, 0.92, -0.47, -0.54, -1.68, 0.3, 0.94, 0.9, -0.24, -1.02, 1.22, -0.31, 1.14, -0.9, -0.21, 0.98, -1.07, 0.12, -2.18, -0.63, -0.66, 0.05, 0.34, 0.03, -0.1, 0.15, -0.34, -1.36, 0.31, 0.99, -0.65, -0.52, -0.12, -0.44, 0.21, 0.33, 0.78, -0.55, -1.04, -1.02, -2.29, 0.14, -0.84, 1.65, 1.82, 0.16, 1.27, 1.42, -0.8, -0.31, 0.26, -0.32, -1.2, -1.34, 1.13, -0.66, 1.37, -2.03, -0.22, 1.72, 0.05, -0.01, -0.08, 2.21, 0.52, 1.41, 0.83, -0.76, -0.86, -0.77, -0.14, -1.4, 0.3, -0.81, 0.03, 1.27, -0.25, 0.21, -0.32, -2.11, -0.42, -0.66, -1.17, -0.79, -0.51, -2.18, -0.69, 0.35, -0.74, 0.23, -1.1, 0.09, 0.89, 0.11, 0.06, -0.1, -0.29, 0.75, -0.6, 0.71, 0.91, -1.18, -0.38, -0.49, -0.21, -0.49, 0.67, 0.4, -0.68, -1.57, 2.44, -0.71, 1.11, 0.24, -0.76, -1.09, -0.49, 0.52, 0.94, 0.49, 0.86, -0.09, -0.97, -0.37, 0.6, -2.64, -0.4, -1.42, -0.91, 0.36, -0.48, 0.22, 0.52, 1.46, -0.21, 2.52, 3.06, -1.29, 0.51, -1.31, -1.05, -0.79, 2.3, -2.08, -0.85, -1.4, -0.91, -0.58, -0.27, 0.4, 0.3, 1.22, 0.23, 0.47, 0.75, -0.12, -1.02, -1.52, 0.47, -1.36, 0.12, -0.1, 0.19, -0.23, -0.75, -2.36, -1.2, -2.08, 0.22, 0.33, 0.07, 1.05, 0.61, 0.2, -1.67)
  length(data)
  summary(data)
  describe(data)
  IQR(data) # Mezhkvartilnyy razmakh
  quantile(data, probs = 0.38) # Kvantil urovnya
  var(data, y = NULL, na.rm = FALSE) #Dispersiya vyborki (nesmeshchennaya)
  skew(data)#	Asimmetrichnost (nesmeshchennaya otsenka)
  kurtosi(data)#	Ekstsess (nesmeshchennaya otsenka)
  psych::describe(data)
  IQR <- IQR(data)
  Q1 <- quantile(data, .25)#	Nizhnyaya granitsa normy
  lower_b <- Q1 - 1.5*IQR
  lower_b
  #sprosit
  Q3 <- quantile(data, .75)#Verkhnyaya granitsa normy
  upper_b <- Q3 + 1.5*IQR
  upper_b
  extra <- data[data<lower_b | data > upper_b]
  extra
  no_outliers <- subset(data, data> (Q1 - 1.5*IQR) & data< (Q3 + 1.5*IQR))
  length(data) - length(no_outliers)
  #1.2
  var(no_outliers, y = NULL, na.rm = FALSE) #Dispersiya vyborki (nesmeshchennaya)
  # Doveritelnye intervaly

  ciE <- function(X, Gamma) {
    n <- length(X)
    ciE <- mean(X, na.rm = TRUE)-sd(X, na.rm = TRUE)/sqrt(n)*qt((1+c(Gamma,0,-Gamma))/2,n-1)
    Interval <- cbind(E.Left = ciE[1],E.Right = ciE[3])
    Res <- list(Eo = ciE[2], Interval = Interval)
    return(Res)
  }
  #--------------------------
  # Doveritelnyy interval dlya generalnoy dispersii (ili dlya istinnoy dispersii) po vyborke
  ciD <- function(X, Gamma) {
    n <- length(X)
    ciD <- sd(X, na.rm = TRUE)^2*(n-1)/qchisq((1+c(Gamma,0,-Gamma))/2,n-1)
    Interval <- cbind(D.Left = ciD[1], D.Right = ciD[3])
    Res <- list(Do = var(X), Interval = Interval)
    return(Res)
  }
  ciE(no_outliers,0.96)
  ciD(no_outliers,0.96)
  #Gistogramma intervalnykh chastot
  hist(data) 
  #Diagramma razmakha ('yashchik s usami')
  boxplot(data,ylab='data')

  #Chast 2
  library(stringr)
  data_2 <- c(0)
  data_2 = str_replace_all(data_2, '[;]', ',')
  data_2 = str_replace_all(data_2, '[()]', '')
  data_2
  data_2 <- c()
  n <- matrix(data_2, nrow = 2)
  n = t(n)
  dim(n)
  n
  X = n[, 1]
  Y = n[, 2]
  X
  Y
  #1.2
  cor(X, Y)
  mean(X)
  mean(Y)
  var(X, y = NULL, na.rm = FALSE) #Dispersiya (nesmeshchennaya)
  var(Y, y = NULL, na.rm = FALSE) #Dispersiya  (nesmeshchennaya)
  #2.1
  #H0:E(X)=E(Y)
  #H1:E(X)<E(Y)
  t.test(Y,X, alternative='greater',paired = TRUE, var.equal = FALSE)$p.value
  #H0:E(X)=E(Y)
  #H1:E(X)!=E(Y)
  t.test(Y,X, alternative='two.sided', paired = TRUE, var.equal = FALSE)$p.value
  #H0:E(X)=E(Y)
  #H1:E(X)>E(Y)
  t.test(Y,X, alternative='less', paired = TRUE)$p.value
  #2.2
  #H0:Var(X)=Var(Y)
  #H1:Var(X)≠Var(Y)
  var.test(Y,X, alternative='two.sided')$p.value
  #H0:Var(X)=Var(Y)
  #H1:Var(X)>Var(Y)
  var.test(Y,X, alternative='less')$p.value
  #H0:Var(X)=Var(Y)
  #H1:Var(X)<Var(Y)
  var.test(Y,X, alternative='greater')$p.value

  #Chast 3
  data3 = c()
  data3 = str_replace_all(data3, '[;]', ',')
  data3 = str_replace_all(data3, 'NA', '')
  data3 = str_replace_all(data3, ', ,', ',')
  data3 = str_replace_all(data3, ', ,', ',')
  data3 = c()
  length(data3)
  table(data3)
  summary(data3)
  sort(data3)
  #input sample size and sample proportion
  n <- 154
  p <- 0.3441558
  #calculate margin of error
  margin <- qnorm(0.95)*sqrt(p*(1-p)/n)
  #calculate lower and upper bounds of confidence interval
  low <- p - margin
  low
  high <- p + margin
  high
  library(dplyr)
  table(t)
  barplot(table(data3))
  #3.2
  chisq.test(table(data3), simulate.p.value = TRUE)
  qchisq(0.02,table(data3))
  #Teoriya
  #nizhnyaya granitsa mat ozhid P{E(X)> mean(X) - t_n-1;y * s/korenn} = gamma kak y
  #verkhnyaya granitsa mat ozhid P{E(X)> mean(X) - t_n-1;1-y * s/korenn} = gamma kak y
  #verkhnyaya granitsa dispresii p{q^2 < (n-1)s^2 / x^2_n-1;1-y} =  gamma kak y
  # a1=a1 : Z= (mean(1) - mean(2))/ coren(q^1_1 / n1 + q^2_2/n2) : criticheskaya oblast prinatya H1 Z>z_1-a ili Z>-z_1-a ili |Z| < z 1-a/2
  #dolzhno vapolnyatsya N(0,1) pri H0
  #q^2_1 = q^2_2 : F_n1-1, n2-1 = S^2_1/ S^2_2 gde vnizy (s^2_1>S^2_2)
  #dolzhno vapolnyatsya  F_n1-1, n2-1 pri H0
  #q^2_1>q^2_2 Fa1 - 1 a2-1 > f_n1-1; n2-1; 1-a
  #q^2_1!=q^2_2 Fn1 - 1 n2-1 > f_n1-1; n2-1; 1-a/2
  "
     )
}
