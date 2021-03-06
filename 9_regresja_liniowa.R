# Zaj�cia 9 ####


# zadanie 1 ---------------------------------------------------------------
# Poni�sza tabela przedstawia liczb� przypadk�w gru�licy uk�adu oddechowego w
# latach 1995-2002. Podano liczb� przypadk�w na 100.000 ludno�ci. Zak�adaj�c liniow�
# zale�no�� mi�dzy rokiem a liczb� przypadk�w, wykonaj kompleksow� analiz� regresji. 

rok <- c(1995,1996,1997,1998,1999,2000,2001,2002)
przypadki <- c(39.7,38.2,34.7,33.1,30.1,28.4,26.3,24.7)

data_set <- data.frame(rok=rok, liczba_przypadkow=przypadki)

# 1. Przedstaw dane na wykresie rozrzutu. Czy model regresji liniowej wydaje si� adekwatny? 
plot(data_set, main= "Wykres rozrzutu", pch=20)


# 2. Dopasuj model regresji liniowej do tych danych. Jakie s� warto�ci estymator�w
# wsp�czynnik�w regresji i przedzia�y ufno�ci? Narysuj uzyskan� prost� regresji na
# schemacie punktowym. 

model <- lm(przypadki~rok, data=data_set)

plot(data_set, main= "Wykres rozrzutu", pch=20)
abline(model, col="red", lwd=2)
coef(model)
confint(model)


# 3. Kt�re wsp�czynniki s� istotne statystycznie w skonstruowanym? 
# Jakie jest dopasowanie modelu? 

summary(model)
#odrzucamy hipoteze zerow� bo Pr < 0.05 | czyli s� statystycznie ISTOTNE

# 4. Oblicz warto�ci dopasowane przez model, a tak�e reszty. 
fitted(model)
residuals(model)


# 5. Na wykresie rozrzutu przedstaw granice przedzia�u prognozy 95%.

#przedzia�y ufno�ci
temp_pred <- data.frame(rok= seq(min(data_set$rok)-10, max(data_set$rok)+10, length=100))
pred <- predict(model, temp_pred, interval="prediction")

plot(data_set, main="Wykres rozrzutu", pch=16)
abline(model, col="red", lwd=2)
lines(temp_pred$rok, pred[,2], lty=2, col="red")
lines(temp_pred$rok, pred[,3], lty=2, col="red")


# 6. Dokonaj predykcji liczby przypadk�w gru�licy uk�adu oddechowego w latach 2003-2007.
# Zilustruj wyniki na wykresie rozrzutu. 

#predykcja dla lat 2003-2007
temp_pred <- data.frame(rok = seq(1994, 2008, length = 100))
pred <- predict(model, temp_pred, interval="prediction")

plot(data_set, main="Wykres rozrzutu", pch=16, xlim = c(1995, 2007), ylim = c(10, 40))

pred_2003_2007 <- predict(model, data.frame(rok = 2003:2007), interval = 'prediction')
points(2003:2007, pred_2003_2007[, 1], col = "blue", pch = 16)

abline(model, col="red", lwd=2)
lines(temp_pred$rok, pred[,2], lty=2, col="red")
lines(temp_pred$rok, pred[,3], lty=2, col="red")


# 7. Czy mia�oby sens usuni�cie wyrazu wolnego z modelu? Je�li tak, wykonaj powy�sze
# polecenia dla modelu regresji liniowej bez wyrazu losowego. 


# zadanie 2 ---------------------------------------------------------------
# Zbi�r danych zawarty w pliku braking.RData zawiera informacje o d�ugo�ci drogi
# hamowania przy danej pr�dko�ci okre�lonego modelu samochodu. W tym zbiorze danych
# wyst�puje obserwacja odstaj�ca. Zidentyfikuj j� za pomoc� wykresu rozrzutu. Korzystaj�c z
# modelu regresji liniowej, opisz zwi�zek mi�dzy d�ugo�ci� drogi hamowania a pr�dko�ci� przy
# u�yciu pe�nych danych i danych bez obserwacji odstaj�cej. Jakie s� wyniki dla obu modeli?
#   Kt�ry model jest lepszy? Dok�adniej, wykonaj polecenia 2-7 Zadania 1 dla ka�dego modelu
# osobno. W punkcie 6 przeprowad� predykcj� d�ugo�ci drogi hamowania dla pr�dko�ci 30, 31, ..., 50. 


# 1.
load("j:/Desktop/R/braking.RData")

data_set <- data.frame(speed = braking$speed, distance = braking$distance)
head(data_set)

plot(braking, main= "Wykres rozrzutu", pch=20)

# 2.
model <- lm(distance~speed, data=data_set)

plot(braking, main= "Wykres rozrzutu", pch=20)
abline(model, col="red", lwd=2)
coef(model)
confint(model)

# 3.
summary(model)
# odrzucamy hipoteze zerow� bo Pr < 0.05 | czyli s� statystycznie ISTOTNE

# 4.
fitted(model)
residuals(model)


#5.
 #przedzia�y ufno�ci
temp_pred <- data.frame(speed = seq(-5, 30, length = 51))
pred <- predict(model, temp_pred, interval="prediction")

plot(data_set, main="Wykres rozrzutu", pch=16, xlim = c(0, 25), ylim = c(-50, 200))

abline(model, col="red", lwd=2)
lines(temp_pred$speed, pred[,2], lty=2, col="red")
lines(temp_pred$speed, pred[,3], lty=2, col="red")


# 6.
# predykcja dla pr�dko�ci 30 - 50
temp_pred <- data.frame(speed = seq(-5, 51), length = 10)
pred <- predict(model, temp_pred, interval="prediction")

plot(data_set, main="Wykres rozrzutu", pch=16, xlim = c(0, 50), ylim = c(-50, 200))

pred_30_50 <- predict(model, data.frame(speed = 30:50), interval = 'prediction')
points(30:50, pred_30_50[, 1], col = "blue", pch = 16)

abline(model, col="red", lwd=2)
lines(temp_pred$speed, pred[,2], lty=2, col="red")
lines(temp_pred$speed, pred[,3], lty=2, col="red")

