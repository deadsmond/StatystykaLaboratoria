# Zaj�cia 11 ####

load(url("http://ls.home.amu.edu.pl/data_sets/liver_data.RData"))

# zadanie 1 ---------------------------------------------------------------
# W jednym badaniu klinicznym oceniono wp�yw poziom�w enzymu LDH i zmian
# poziom�w bilirubiny na zdrowie pacjent�w z przewlek�ym zapaleniem w�troby. Uzyskane
# wyniki s� zawarte w pliku liver _data.RData. Zmienne to:
# bilirubin - zmiana st�enia bilirubiny we krwi, 
# ldh - st�enie enzymu LDH w ciele pacjenta, 
# condition - zmiana stanu pacjenta ( Yes - pogorszenie, No - brak pogorszenia). 

# 1. Dopasuj model regresji logistycznej do tych danych. Jakie s� warto�ci estymator�w
# wsp�czynnik�w regresji? 

model_1 <- glm(
  formula=condition ~ bilirubin + ldh, 
  family = "binomial", 
  data=liver_data
)
model_1

# 2. Kt�re wsp�czynniki s� statystycznie istotne w skontruowanym modelu? Jakie jest
# dopasowanie modelu?

summary(model_1)

# 3. Czy model ten mo�e by� zredukowany za pomoc� regresji krokowej? 
step(model_1)

# 4. Zinterpretuj wsp�czynniki modelu. 
bilirubin <- exp(coef(model_1[1]))[2]

ldh <- exp(coef(model_1[1]))[3]

# 5. Narysuj krzyw� ROC i oblicz AUC dla modelu. 
library(ROCR)
pred_1 <- prediction(
  model_1$fitted.values, 
  liver_data$condition
)

plot(
  performance(
    pred_1, 
    'tpr', 
    'fpr', 
    main="Model 1"
  )
)

# auc
performance(pred_1, 'auc')@y.values

# 6. Dokonaj przedykcji zmiennej condition dla trzech pacjent�w scharakteryzonych
# nast�puj�co: ( bilirubin , 1dh ) = (0.9, 100), (2.1, 200), (3.4, 300). 
# Zilustruj wyniki na wykresie. 

liver_data_new <- data.frame(
  bilirubin = c(0.9, 2.1, 3.4),
  ldh = c(100, 200, 300)
)

predict_glm <- predict(
  model_1, 
  liver_data_new,
  type = 'response'
)

model_1_hat <- coef(model_1)[1] + coef(model_1)[2] * liver_data$bilirubin + coef(model_1)[3] * liver_data$ldh
model_1_temp <- seq(min(model_1_hat) - 1, max(model_1_hat) + 2.5, length.out = 100)
condition_temp <- exp(model_1_temp) / (1 + exp(model_1_temp))

plot(
  model_1_temp, 
  condition_temp, 
  type = "l", 
  xlab = "X beta", 
  ylab = "condition", 
  xlim = c(-6, 9), 
  ylim = c(-0.1, 1.1)
)

# g�rne czarne kropki
points(
  model_1_hat, 
  liver_data$condition, 
  pch = 16
)

# czerwone kropki
points(
  coef(model_1)[1] + coef(model_1)[2] * liver_data_new$bilirubin + coef(model_1)[3] * liver_data_new$ldh, 
  predict_glm, 
  pch = 16, 
  col = "red"
)

# 7. Powy�szy wykres pokazuje, �e istniej� dwie obserwacje odstaj�ce dla pacjent�w z
# pogorszeniem i jedna obserwacja odstaj�ca dla pacjent�w bez pogorszenia. Zidentyfikuj
# je i wykonaj powy�sz� analiz� dla danych bez tych trzech warto�ci odstaj�cych. Jak
# zmieniaj� si� wyniki? 

# 7.1
# 7.2
# 7.3
# 7.4
# 7.5
# 7.6

# zadanie 2 ---------------------------------------------------------------
# U�yj modelu regresji Poissona do zestawu danych moths (wp�yw siedliska na
# liczb� moli) z pakietu DAAG . U�yj zlogarytmowanej zmiennej meters jako zmiennej
# obja�niaj�cej, a liczby moli A jako zmiennej obja�nianej. 

# 1. Dopasuj model regresji Poissona do tych danych. Jakie s� warto�ci estymator�w
# wsp�czynnik�w regresji?

# 2.

# 3.

# 4.

# 5.
# 5.1
# 5.2
# 5.3
# 5.4
