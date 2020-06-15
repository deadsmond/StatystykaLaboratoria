# Zajêcia 5 ####

# Zadanie 1 ---------------------------------------------------------------
# 1. Niech X = (X;,..., X,)' bêdzie prób¹ prost¹ z populacji o rozk³adzie
# jednostajnym U (a, b). 

# 1. 

# 2. Oblicz wartoœci tych estymatorów dla danych z przyk³adu dotycz¹cego czasu oczekiwania
# na tramwaj. 

# 3. Zilustruj otrzymane teoretyczne funkcje gêstoœci korzystaj¹ce z ENW i EMM na histogramie. 


# Zadanie 2 ---------------------------------------------------------------
# Przebadano 200 losowo wybranych 5-sekundowych okresów pracy centrali
# telefonicznej. Rejestrowano liczbê zg³oszeñ. Wyniki s¹ zawarte w pliku Centrala.RData. 

load("Centrala.RData")

# 1. Zasugeruj rozk³ad teoretyczny badanej zmiennej. 
rozklad_centrali = prop.table(table(Centrala))

# wykres s³upkowy
barplot(
  rozklad_centrali,
  xlab = "Liczba b³êdów", 
  ylab = "Prawdopodobieñstwo",
  main = "Rozk³ad empiryczny liczby b³êdów"
)

# sugerujê rozklad Poissona
rozklad_poissona = dpois(
  x = 0:5, 
  lambda = mean(Centrala$Liczba)
)

barplot(
  rozklad_poissona, 
  names.arg = 0:5,
  xlab = "k", 
  ylab = "P(X=k)", 
  main = "Funkcja prawdopodobieñstwa"
)

both <- rbind(rozklad_centrali, rozklad_poissona)
barplot(both,beside=T)
 
# 2. Oblicz wartoœæ estymatora parametru rozk³adu teoretycznego.

# wartoœæ estymatora
v = as.vector(Centrala[1])
p_est = colMeans(v) / length(v)

p_est <- mean(Centrala$Liczba)

# 3. Porównaj empiryczne prawdopodobieñstwa wyst¹pienia poszczególnych wartoœci liczby
# zg³oszeñ w próbie z wartoœciami teoretycznymi uzyskanymi na podstawie rozk³adu
# teoretycznego. 

probs <- dpois(x = sort(unique(Centrala$Liczba)), lambda = p_est)
sum(probs)

counts <- matrix(
  c(rozklad_centrali, probs), 
  nrow = 2, 
  byrow = TRUE
)
rownames(counts) <- c("empiryczny", "teoretyczny")
colnames(counts) <- sort(unique(Centrala$Liczba))
counts

barplot(
  counts, 
  xlab = "Liczba b³êdów", ylab = "Prawdopodobieñstwo",
  main = "Rozk³ady empiryczny i teoretyczny liczby b³êdów",
  col = c("red", "blue"), 
  legend = rownames(counts), 
  beside = TRUE
)

# 4. SprawdŸ dopasowanie rozk³adu teoretycznego za pomoc¹ wykresy kwantyl-kwantyl. 

# wykres kwantyl-kwantyl
qq_1 = matrix(
  c(rozklad_centrali), 
  nrow = 1, 
  byrow = TRUE
)

qq_2 = matrix(
  c(probs), 
  nrow = 1, 
  byrow = TRUE
)

qqplot(qq_1, qq_2)
qqline(qq_1, distribution = function(qq_2) { qpois(qq_2, lambda = mean(Centrala$Liczba)) })

# 5. Czy na podstawie powy¿szych rozwa¿añ rozk³ad teoretyczny wydaje siê odpowiedni? 


# 6. Oblicz prawdopodobieñstwo empiryczne i teoretyczne, ¿e liczba zg³oszeñ jest mniejsza ni¿ 4. 

# empirycznie
mean(Centrala$Liczba < 4)

# teoretycznie: X ~ U(a_est, b_est) oraz P(X > 10) = 1 - P(X <= 10) = 1 - F(10)
ppois(3, lambda = mean(Centrala$Liczba))

# Zadanie 3 ---------------------------------------------------------------
# Niech X = (Xq,... „X, bêdzie prób¹ prost¹ z rozk³adu Rayleigha o gêstoœci danej w æwiczeniu

# Poka¿, ¿e ENW parametru A jest postaci danej w æwiczeniu 

# 1. Poka¿, ¿e funkcja wiarogodnoœci wynosi dane w æwiczeniu

# 2. WprowadŸ pomocnicz¹ funkcjê dan¹ w æwiczeniu

# 3. Wyznacz pochodn¹ funkcji / wzglêdem A

# 4. Przyrównaj powy¿sz¹ pochodn¹ do zera i rozwi¹¿ otrzymane równanie. 


# Zadanie 4 ---------------------------------------------------------------
# Notowano pomiary œredniej szybkoœci wiatru w odstêpach 15 minutowych wokó³
# nowo powstaj¹cej elektrowni wiatrowej. Wyniki s¹ nastêpuj¹ce: 

wiatr = c(0.9, 6.2, 2.1, 4.1, 7.3, 1.0, 4.6, 6.4, 3.8, 5.0, 2.7, 9.2, 5.9, 7.4, 3.0, 4.9, 8.2, 5.0, 1.2, 10.1, 12.2, 2.8, 5.9, 8.2, 0.5)

# 1. Zasugeruj rozk³ad teoretyczny badanej zmiennej. 
# sugerujê rozk³ad Rayleigha

# 2. Oblicz wartoœæ ENW parametru rozk³adu teoretycznego. 

# estymator najwiêkszej wiarygodnoœci 
ENW = mean(wiatr^2)

lambda = ENW

# 3, Porównaj rozk³ad empiryczny wyst¹pienia poszczególnych wartoœci œredniej szybkoœci
# wiatru w próbie z wartoœciami teoretycznymi uzyskanymi na podstawie rozk³adu
# teoretycznego. 

# histogram z estymatorem j¹drowym gêstoœci
hist(
  wiatr, 
  xlab = "Czas oczekiwania na tramwaj", 
  main = "Rozk³ad empiryczny czasu oczekiwania na tramwaj",
  probability = TRUE, 
  col = "lightgreen"
)

# empiryczny
lines(
  density(wiatr), 
  col = "red", 
  lwd = 2
)

# teoretyczny
lambda <- ENW
curve(
  VGAM::drayleigh(x, sqrt(lambda / 2)), 
  col = "blue", 
  add = TRUE, 
  lwd = 2
)

# 4. SprawdŸ dopasowanie rozk³adu teoretycznego za pomoc¹ wykresy kwantyl-kwantyl.
# wykres kwantyl-kwantyl
EnvStats::qqPlot(
  wiatr, 
  distribution = "unif", 
  param.list = list(
    min = min(wiatr), 
    max = max(wiatr)
  ),
  add.line = TRUE
)

# 5. Czy na podstawie powy¿szych rozwa¿añ rozk³ad teoretyczny wydaje siê odpowiedni? 
# dlaczego nie, kogo to interesuje

# 6. Oblicz empiryczne i teoretyczne prawdopodobieñstwo, ¿e œrednia szybkoœæ wiatru 
# jest zawarta w przedziale [4, 8]

# estymatory: min i max
min_w = min(wiatr)
max_w = max(wiatr)

# empirycznie
mean(c(wiatr >= 4 & wiatr <= 8))

# teoretycznie
more_than_4 = VGAM::prayleigh(3)
more_than_8 = VGAM::prayleigh(8)

between_4_and_8 = more_than_4 - more_than_8 # TODO REPAIR

# 7. Oblicz wartoœæ ENW dla wartoœci oczekiwanej i wariancji rozk³adu teoretycznego. 

## [1] 5.274353
## [1] 7.601197

