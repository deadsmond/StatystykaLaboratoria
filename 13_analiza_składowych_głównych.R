# ANALIZA SK�ADOWYCH G��WNYCH

# zadanie 1 ---------------------------------------------------------------
# W powy�szym przyk�adzie do analizy sk�adowych g��wnych zosta�y wykorzystane
# wszystkie zmienne. Jednak jedna z nich jest bardzo s�abo skorelowana z pozosta�ymi. Ustal t�
# zmienn�, a nast�pnie wykonaj poni�sze polecenia bez jej uzwgl�dnienia: 

dane <- USArrests
dane <- dane[,-3]

# 1. Dokonaj analizy sk�adowych g��wnych.

# Przygotowanie danych do analizy
var(dane)

# Skalowanie
dane_scale <- scale(dane)
var(dane_scale)

# Analiza sk�adowych g��wnych
pca <- prcomp(dane, scale=TRUE)

# 2. Jaki procent wariancji ttumaczony jest przez poszczeg�lne sk�adowe? 

summary(pca)
# procent wariancji - drugi wiersz

# 3. Wyznacz wsp�rz�dne obserwacji w nowym uk�adzie wsp�rz�dnych utworzonym przez
# sk�adowe g��wne. 

# wsp�rz�dne obserwacji
head(pca$x)

# 4. Dokonaj interpretacji �adunk�w i zilustruj je na wykresie. 

#interpretacja �adunk�w
pca$rotation

# wykres
# ???

# 5. Narysuj wykres osypiska i zaproponuj optymaln� liczb� sk�adowych g��wnych w oparciu o
# trzy kryteria. 

# wykres osypiska
plot(pca)

# 6. Przedstaw stany w uk�adzie dw�ch pierwszych sk�adowych g��wnych (dok�adniej narysuj
# biplot i dokonaj jego interpretacji).

biplot(pca)

# 7. Przedstaw stany za pomoc� minimalnego drzewa rozpinaj�cego. 

#drzewo rozpinaj�ce
library(ape)
plot(mst(dist(dane_scale)), x1 = pca$x[, 1], x2 = pca$x[, 2])

# zadanie 2 ---------------------------------------------------------------
# Zbi�r danych mtcars zawiera informacje na temat 32 samochod�w z roku 1974. 

dane <- mtcars

# 1. Dokonaj analizy sk�adowych g��wnych bior�c pod uwag� cechy: 
# mpg, disp, hp, drat, wt, qsec. 
mtcars_sel <- mtcars[, c(1, 3:7)]

# Analiza sk�adowych g��wnych
(pca_2 <- prcomp(mtcars_sel, scale = TRUE))
dane_scale <- scale(mtcars_sel)

# 2. Jaki procent wariancji t�umaczony jest przez poszczeg�lne sk�adowe? 

# procent wariancji - drugi wiersz
summary(pca_2)

# 3. Wyznacz wsp�rz�dne obserwacji w nowym uk�adzie wsp�rz�dnych utworzonym przez
# sk�adowe g��wne. 

head(pca_2$x)

# 4. Dokonaj interpretacji �adunk�w i zilustruj je na wykresie.

pca_2$rotation

# 5. Narysuj wykres osypiska i zaproponuj optymaln� liczb� sk�adowych g��wnych w oparciu o trzy kryteria. 
# ???

# 6. Przedstaw samochody w uk�adzie dw�ch pierwszych sk�adowych g��wnych (dok�adniej
# narysuj biplot i dokonaj jego interpretacji). 

plot(pca_2)
biplot(pca_2)

# 7. Przedstaw samochody za pomoc� minimalnego drzewa rozpinaj�cego.
library(ape)
plot(mst(dist(dane_scale)), x1 = pca_2$x[, 1], x2 = pca_2$x[, 2])


# 8. Jak bardzo b�d� r�ni�y si� wyniki, je�li nie wykonamy skalowania danych? 

# Analiza sk�adowych g��wnych
(pca_3 <- prcomp(mtcars_sel, scale = FALSE, center = FALSE))
dane_scale <- scale(mtcars_sel)

# procent wariancji - drugi wiersz
summary(pca_3)

# wsp�rz�dne obserwacji
head(pca_3$x)

# interpretacji �adunk�w
pca_3$rotation

# wykres osypiska
plot(pca_3)

biplot(pca_3)

# drzewo rozpinaj�ce
library(ape)
plot(mst(dist(dane_scale)), x1 = pca_3$x[, 1], x2 = pca_3$x[, 2])
