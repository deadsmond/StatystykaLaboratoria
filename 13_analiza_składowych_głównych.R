#ANALIZA SKŁADOWYCH GŁÓWNYCH

# zadanie 1 ---------------------------------------------------------------

dane <- USArrests
dane <- dane[,-3]

#1.1
#Przygotowanie danych do analizy
var(dane)
#Skalowanie
dane_scale <- scale(dane)

var(dane_scale)
#Analiza składowych głównych
pca <- prcomp(dane, scale=TRUE)

#1.2
summary(pca)
#procent wariancji - drugi wiersz

#1.3
#współrzędne obserwacji
head(pca$x)

#1.4
#interpretacja ładunków
pca$rotation
#wykres
#???

#1.5
#wykres osypiska
plot(pca)

#1.6
biplot(pca)

#1.7
#drzewo rozpinające
library(ape)
plot(mst(dist(dane_scale)), x1 = pca$x[, 1], x2 = pca$x[, 2])

# zadanie 2 ---------------------------------------------------------------
#??

dane <- mtcars
mtcars_sel <- mtcars[, c(1, 3:7)]

#Analiza składowych głównych
(pca_2 <- prcomp(mtcars_sel, scale = TRUE))
dane_scale <- scale(mtcars_sel)

#procent wariancji - drugi wiersz
summary(pca_2)

#współrzędne obserwacji
head(pca_2$x)

#interpretacja ładunków
pca_2$rotation

#wykres osypiska
plot(pca_2)

biplot(pca_2)

#drzewo rozpinające
library(ape)
plot(mst(dist(dane_scale)), x1 = pca_2$x[, 1], x2 = pca_2$x[, 2])

#bez skalowania
#Analiza składowych głównych
(pca_3 <- prcomp(mtcars_sel, scale = FALSE, center = FALSE))
dane_scale <- scale(mtcars_sel)

#procent wariancji - drugi wiersz
summary(pca_3)

#współrzędne obserwacji
head(pca_3$x)

#interpretacja ładunków
pca_3$rotation

#wykres osypiska
plot(pca_3)

biplot(pca_3)

#drzewo rozpinające
library(ape)
plot(mst(dist(dane_scale)), x1 = pca_3$x[, 1], x2 = pca_3$x[, 2])
