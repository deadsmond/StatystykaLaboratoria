# ANALIZA SK£ADOWYCH G£ÓWNYCH

# zadanie 1 ---------------------------------------------------------------
# W powy¿szym przyk³adzie do analizy sk³adowych g³ównych zosta³y wykorzystane
# wszystkie zmienne. Jednak jedna z nich jest bardzo s³abo skorelowana z pozosta³ymi. Ustal tê
# zmienn¹, a nastêpnie wykonaj poni¿sze polecenia bez jej uzwglêdnienia: 

dane <- USArrests
dane <- dane[,-3]

# 1. Dokonaj analizy sk³adowych g³ównych.

# Przygotowanie danych do analizy
var(dane)

# Skalowanie
dane_scale <- scale(dane)
var(dane_scale)

# Analiza sk³adowych g³ównych
pca <- prcomp(dane, scale=TRUE)

# 2. Jaki procent wariancji ttumaczony jest przez poszczególne sk³adowe? 

summary(pca)
# procent wariancji - drugi wiersz

# 3. Wyznacz wspó³rzêdne obserwacji w nowym uk³adzie wspó³rzêdnych utworzonym przez
# sk³adowe g³ówne. 

# wspó³rzêdne obserwacji
head(pca$x)

# 4. Dokonaj interpretacji ³adunków i zilustruj je na wykresie. 

#interpretacja ³adunków
pca$rotation

# wykres
# ???

# 5. Narysuj wykres osypiska i zaproponuj optymaln¹ liczbê sk³adowych g³ównych w oparciu o
# trzy kryteria. 

# wykres osypiska
plot(pca)

# 6. Przedstaw stany w uk³adzie dwóch pierwszych sk³adowych g³ównych (dok³adniej narysuj
# biplot i dokonaj jego interpretacji).

biplot(pca)

# 7. Przedstaw stany za pomoc¹ minimalnego drzewa rozpinaj¹cego. 

#drzewo rozpinaj¹ce
library(ape)
plot(mst(dist(dane_scale)), x1 = pca$x[, 1], x2 = pca$x[, 2])

# zadanie 2 ---------------------------------------------------------------
# Zbiór danych mtcars zawiera informacje na temat 32 samochodów z roku 1974. 

dane <- mtcars

# 1. Dokonaj analizy sk³adowych g³ównych bior¹c pod uwagê cechy: 
# mpg, disp, hp, drat, wt, qsec. 
mtcars_sel <- mtcars[, c(1, 3:7)]

# Analiza sk³adowych g³ównych
(pca_2 <- prcomp(mtcars_sel, scale = TRUE))
dane_scale <- scale(mtcars_sel)

# 2. Jaki procent wariancji t³umaczony jest przez poszczególne sk³adowe? 

# procent wariancji - drugi wiersz
summary(pca_2)

# 3. Wyznacz wspó³rzêdne obserwacji w nowym uk³adzie wspó³rzêdnych utworzonym przez
# sk³adowe g³ówne. 

head(pca_2$x)

# 4. Dokonaj interpretacji ³adunków i zilustruj je na wykresie.

pca_2$rotation

# 5. Narysuj wykres osypiska i zaproponuj optymaln¹ liczbê sk³adowych g³ównych w oparciu o trzy kryteria. 
# ???

# 6. Przedstaw samochody w uk³adzie dwóch pierwszych sk³adowych g³ównych (dok³adniej
# narysuj biplot i dokonaj jego interpretacji). 

plot(pca_2)
biplot(pca_2)

# 7. Przedstaw samochody za pomoc¹ minimalnego drzewa rozpinaj¹cego.
library(ape)
plot(mst(dist(dane_scale)), x1 = pca_2$x[, 1], x2 = pca_2$x[, 2])


# 8. Jak bardzo bêd¹ ró¿ni³y siê wyniki, jeœli nie wykonamy skalowania danych? 

# Analiza sk³adowych g³ównych
(pca_3 <- prcomp(mtcars_sel, scale = FALSE, center = FALSE))
dane_scale <- scale(mtcars_sel)

# procent wariancji - drugi wiersz
summary(pca_3)

# wspó³rzêdne obserwacji
head(pca_3$x)

# interpretacji ³adunków
pca_3$rotation

# wykres osypiska
plot(pca_3)

biplot(pca_3)

# drzewo rozpinaj¹ce
library(ape)
plot(mst(dist(dane_scale)), x1 = pca_3$x[, 1], x2 = pca_3$x[, 2])
