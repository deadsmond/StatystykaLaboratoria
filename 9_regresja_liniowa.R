# Zajêcia 9 ####


# zadanie 1 ---------------------------------------------------------------
rok <- c(1995,1996,1997,1998,1999,2000,2001,2002)
przypadki <- c(39.7,38.2,34.7,33.1,30.1,28.4,26.3,24.7)

data_set <- data.frame(rok=rok, liczba_przypadkow=przypadki)
plot(data_set, main= "Wykres rozrzutu", pch=20)


#2)
model <- lm(przypadki~rok, data=data_set)

plot(data_set, main= "Wykres rozrzutu", pch=20)
abline(model, col="red", lwd=2)
coef(model)
confint(model)


#3)
summary(model)
#odrzucamy hipoteze zerow¹ bo Pr < 0.05 | czyli s¹ statystycznie ISTOTNE


#4)
fitted(model)
residuals(model)


#5)
#przedzia³y ufnoœci
temp_pred <- data.frame(rok= seq(min(data_set$rok)-10, max(data_set$rok)+10, length=100))
pred <- predict(model, temp_pred, interval="prediction")

plot(data_set, main="Wykres rozrzutu", pch=16)
abline(model, col="red", lwd=2)
lines(temp_pred$rok, pred[,2], lty=2, col="red")
lines(temp_pred$rok, pred[,3], lty=2, col="red")


#6)
#predykcja dla lat 2003-2007
temp_pred <- data.frame(rok = seq(1994, 2008, length = 100))
pred <- predict(model, temp_pred, interval="prediction")

plot(data_set, main="Wykres rozrzutu", pch=16, xlim = c(1995, 2007), ylim = c(10, 40))

pred_2003_2007 <- predict(model, data.frame(rok = 2003:2007), interval = 'prediction')
points(2003:2007, pred_2003_2007[, 1], col = "blue", pch = 16)

abline(model, col="red", lwd=2)
lines(temp_pred$rok, pred[,2], lty=2, col="red")
lines(temp_pred$rok, pred[,3], lty=2, col="red")


#7)


# zadanie 2 ---------------------------------------------------------------
#1)
load("j:/Desktop/R/braking.RData")

data_set <- data.frame(speed = braking$speed, distance = braking$distance)
head(data_set)

plot(braking, main= "Wykres rozrzutu", pch=20)

#2)
model <- lm(distance~speed, data=data_set)

plot(braking, main= "Wykres rozrzutu", pch=20)
abline(model, col="red", lwd=2)
coef(model)
confint(model)

#3)
summary(model)
#odrzucamy hipoteze zerow¹ bo Pr < 0.05 | czyli s¹ statystycznie ISTOTNE

#4)
fitted(model)
residuals(model)


#5)
#przedzia³y ufnoœci
temp_pred <- data.frame(speed = seq(-5, 30, length = 51))
pred <- predict(model, temp_pred, interval="prediction")

plot(data_set, main="Wykres rozrzutu", pch=16, xlim = c(0, 25), ylim = c(-50, 200))

abline(model, col="red", lwd=2)
lines(temp_pred$speed, pred[,2], lty=2, col="red")
lines(temp_pred$speed, pred[,3], lty=2, col="red")


#6)
#predykcja dla prêdkoœci 30 - 50
temp_pred <- data.frame(speed = seq(-5, 51), length = 10)
pred <- predict(model, temp_pred, interval="prediction")

plot(data_set, main="Wykres rozrzutu", pch=16, xlim = c(0, 50), ylim = c(-50, 200))

pred_30_50 <- predict(model, data.frame(speed = 30:50), interval = 'prediction')
points(30:50, pred_30_50[, 1], col = "blue", pch = 16)

abline(model, col="red", lwd=2)
lines(temp_pred$speed, pred[,2], lty=2, col="red")
lines(temp_pred$speed, pred[,3], lty=2, col="red")

