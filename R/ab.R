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


Printsipy spetsifikatsii: 
1.Pervyy printsip i strukturnaya forma:
Printsip: model voznikaet v itoge zapisi matematicheskim yazykom vzaimosvyazi iskhodnykh dannykh i iskomykh neizvestnykh v zadache. 
Pri etom, pri sostavlenii vzaimosvyazi privlechem lineynye algebraicheskie funktsii (kak samye prosteyshie).
2.Vtoroy printsip spetsifikatsii modeli.
Printsip: V modeli (strukturirovannoy forme) dolzhno byt stolko ekzogennykh peremennykh, skolko endogennykh (neizvestnykh peremennykh). 
Vtoroy printsip neobkhodim dlya transformatsii modeli k takoy forme, v kotoroy kazhdaya endogennaya (iskomaya) peremennaya vyrazhena tolko cherez ekzogennye (izvestnye) peremennye. 
Pri etom vse endogennye peremennye nakhodyatsya v levoy chasti, a pravye chasti soderzhat tolko predopredelennye peremennye i sluchaynye sostavlyayushchie.
3.Tretiy printsip spetsifikatsii modeli
Printsip: dannyy printsip pomogaet otrazit v modeli faktor vremeni. 
Faktor vremeni chasto prisutstvuet vo vzaimosvyazi iskhodnykh dannykh i iskomykh neizvestnykh iskomogo obekta, to est privyazany ko vremeni (dinamicheskaya model)
	4.Chetvertyy printsip spetsifikatsii modeli
Printsip: dannyy printsip pomogaet otrazit v spetsifikatsii modeli vozdeystvie na tekushchie endogennye peremennye neuchtennykh faktorov.
+postroit grafiki dlya opredeleniya znakov

F, soglasno, RStudio = 123,8 , PValue pri etom ravno 3,316e-12
neravenstvo F>Fkrit spravedlivo, gipoteza o nevernom vybore spetsifikatsii otvergaetsya i spetsifikatsiya modeli priznaetsya udovletvoritelnoy, 
tak kak PValue < 0,05. (Fkrit-chislo, ktr ne prevyshaet F s zadannoy doveritelnoy veroyatnostyu pri spravedlivoy gipoteze.)
PREDPOSYLKI:

library(stats)
library(lmtest)
library(tseries)

NULEVAYa  Cond0
Obyasnyayushchie peremennye 𝑥1𝑡, 𝑥2𝑡…𝑥𝑘𝑡 lineyno nezavisimye;
Nelzya ni odnu iz obyasnyayushchikh peremennykh (napr. kh2) vyrazit cherez kh1; esli predposylka ne vypolnyaetsya protsedury MNK ne sushchestvuet 
sond0 <-lm(data=LnY, LnK~LnL+Lnp)
summary(cond0)
 Tak kak koeffitsient determinatsii R2 = 0,21, to est menshe 
edinitsy, to predposylka 0, o lineĭnoĭ nezavisimosti obyasnyayushchikh 
peremennykh, spravedliva. 


PERVAYa resettest Test Remzi
Ozhidaemye/srednie znacheniya sluchaynogo vozmushcheniya tozhdestvenny 0
resettest(NmodYx)
V protokole testa obrashchaem vnimanie na velichinu PValue=0,1728, kotoraya bolshe, chem 0,05 => predposylka 1, 
o tozhdestvennosti  srednikh znacheniĭ sluchaĭnogo vozmushcheniya, interpretiruetsya kak spravedlivaya. 

VTORAYa gqtest Test Goldfelda-Kvandta 
gqtest(NmodYx, fraction = 0.33)
V protokole testa obrashchaem vnimanie na velichinu PValue=0,9168, kotoraya bolshe, chem 0,05 => na urovne znachimosti 5% predposylka 2, 
o sokhranenii dispersii sluchaĭnogo vozmushcheniya neizmennoĭ pri lyubykh znacheniyakh obyasnyayushchikh peremennykh, 
prinimaetsya i ee sluchaĭnoe vozmushchenie interpretiruetsya kak gomoskedastichnoe. 

TRETYa dwtest Test Darbina-Uotsona
Sluchaynye vozmushcheniya ne zavisyat drug ot druga (u velichiny ut (sluchaynoe vozmushchenie) otsutstvuet avtokorrelyatsiya) 
dwtest(NmodYx)
Tak kak PValue=0,7103, chto bolshe, chem 0,05 => predposylka 3, o nezavisimosti sluchaynykh vozmushcheniĭ, interpretiruetsya kak spravedlivaya. 

O NORMALNOSTI
res <-residuals.lm(NmodYx)
res
jarque.bera.test(res)
V rassmatrivaemom sluchaet Rvalue = 0,8593 > 0,05, 
sledovatelno sluchaĭnoe vozmushchenie raspredeleno  po normalnomu zakonu i postroenie doveritelnogo intervala korrektno. 


T-TEST ZNAChIMOSTI 
Takim obrazom model poluchilas neaddekvatnoy, my mozhem proverit, nuzhen li nam koef-t ko s pomoshchyu tj statistiki
Vvedem gipotezu H0: k0=0 ↔ k0 dlya togo, chtoby proverit, yavlyaetsya li 
ona znachashchey peremennoy i neset li v sebe informatsiyu ob obyasnyaemoy
peremennoy y. Dlya proverki gipotezy vospolzuemsya statistikoy tj. Esli 
|𝑡j | ≤ 𝑡 to gipoteza H0 prinimaetsya. 
t𝑗  v nashem sluchae = 3,81
𝑡krit opredelim s pomoshchyu funktsii Excel STYuDENT.OBR.2Kh(0,05;m): Veroyatnost = (1-α) = 0,05; Stepeni svobody: m=n-K
Tak kak neravenstvo |3,8| ≤ 2,1  ne vypolnyaetsya, gipoteza H0 o tom, 
chto ko-neznachashchaya peremennaya ne prinimaetsya, to est ko yavlyaetsya znachashchey i ee isklyuchenie iz modeli budet netselesoobrazno. 


TRAKTOVKA R^2  KOEF.DETERMINATsII
Znachit, chto obyasnyayushchaya peremennaya(sprava) «Khuy» na R^2% obyasnyaet peremennuyu sleva. 
Blagodarya R^2 my mozhet vyyasnit, pravilno li my vybrali spetsifikatsiyu modeli i kakim kachestvom ona obladaet (blizhe k 1-vysokoe kachestvo) (Ostavshiysya protsent pri etom-neuchtennye faktory)

Primer: V tretyey stroke pervogo stolbtsa poschitano znachenie koeffitsienta determinatsii R2  , 
kotoroe ravno 0,62 chto govorit nam o tom, chto obyasnyayushchie peremennye vybrany pravilno, 
to est mozhno sdelat vyvod o tom, chto spetsifikatsiya modeli obladaet khoroshim kachestvom, 
tak kak peremennaya Y na 62% obyasnyaet peremennuyu C, ostavshiysya protsent pri etom-neuchtennye faktory 

TRAKTOVKA R^2ADJ SKORREKT. KOEF. DETERMINATsII
Skorrektirovannyy koeffitsient determinatsii nakhoditsya v 
predposledney strochke posle «Adjusted R-squared» - 𝑅2. Etot koeffitsient vozrastaet tolko togda, 
kogda v otvet na dobavlennuyu obyasnyayushchuyu peremennuyu umenshaetsya standartnaya oshibka modeli. 
S pomoshchyu etogo koef my mozhem opredelit tselesoobraznost dobavleniya peremennykh v model.




O TsELESOOBRAZNOSTI VVODA:

Skorrektirovannyy koeffitsient determinatsii i t-test kak instrument modifikatsii. 
 Skorrektirovannyy koeffitsient determinatsii nakhoditsya v 
predposledney strochke posle «Adjusted R-squared» - 𝑅2. Etot koeffitsient vozrastaet tolko togda, 
kogda v otvet na dobavlennuyu obyasnyayushchuyu peremennuyu umenshaetsya standartnaya oshibka modeli. 
Znachenie 𝑅2 dlya iznachalnoy modeli= 0,4108, dlya modifitsirovannoy modeli obmennogo 𝑅2 =0,4108.
Vidim, chto skorrektirovannyĕ koeffitsient determinatsii izmenilsya 
v otvet na dobavlenie peremennoy kh1 (umenshilsya s 0,4408 do 0,4108), 
sledovatelno dobavlenie kh1 yavlyaetsya ne tselesoobraznym. Standartnaya oshibka pri etom uvelichilas s 1,284 do 1,318. 

#Bez dobavleniya x1 
Yx23
Yx23
modYx23 <-lm(data = Yx23, y~x2+x3) 
summary(modYx23)
#Posle dobavleniya
Yx123
modYx123 <-lm(data = Yx123, y~x1+x2+x3) 
summary(modYx123)

ADEKVATNOST
KhUY s volnoy Prognoz po lineyn schitaem
Delta Khuy s volnoy Istinnaya oshibka=realnoe znachenie-prognoz
Sigma Khuy s volnoy Otnositelnaya oshibka=ABS(IO/P)*100
Vyvody po povodu otnositelnykh oshibok: tak kak otnositelnaya oshibka prognoza na daty kontroliruyushchey̆ vyborki v 2021godu prevyshaet 15%, 
modifitsirovannaya model ne mozhet interpretirovatsya kak adekvatnaya. 

cdata<-data.frame(t=1992, LnKt=7.2907, LnLt=4.2777, Lnpt=3.1416)
cdata
predict.lm(modv23, newdata = cdata, interval = prediction, level = 0.95)
 
VYBROSY
Vybrosami budem schitat znacheniya na grafike, kotorye bolshe vsego otkloneny ot zadannoy linii trenda 

Zamechanie!
•	Ne zabyt Logarifmirovat 3 tip 
•	Datirovat peremennye tolko togda, kogda est stolbets dat
•	Dlya sostavleniya spetsifikatsii nelineynoy modeli znaki koeffitsientov (A, alpha, beta, delta) nuzhno proveryat postroeniem grafika
•	Esli otnositelnaya oshibka modeli okazhetsya >15%, a doveritelnyy interval (postroennyy komandoy predictlm()) 
nakryvaet prognoznoe znachenie, schitaetsya li model v takom sluchae neadekvatnoy?
(v takom sluchae ona adekvatnaya)
•	bqtest(Mmodel) - test Broysha -Pagana testirovaniya gomoskedastichnost  


"
     )
}
