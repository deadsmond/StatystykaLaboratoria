# Zaj�cia 5 ####

# Zadanie 1 ---------------------------------------------------------------
# 1. Niech X = (X;,..., X,)' b�dzie pr�b� prost� z populacji o rozk�adzie
# jednostajnym U (a, b). 

# 1. 

# 2. Oblicz warto�ci tych estymator�w dla danych z przyk�adu dotycz�cego czasu oczekiwania
# na tramwaj. 

# 3. Zilustruj otrzymane teoretyczne funkcje g�sto�ci korzystaj�ce z ENW i EMM na histogramie. 


# Zadanie 2 ---------------------------------------------------------------
# Przebadano 200 losowo wybranych 5-sekundowych okres�w pracy centrali
# telefonicznej. Rejestrowano liczb� zg�osze�. Wyniki s� zawarte w pliku Centrala.RData. 

load("Centrala.RData")

# 1. Zasugeruj rozk�ad teoretyczny badanej zmiennej. 
rozklad_centrali = prop.table(table(Centrala))

# wykres s�upkowy
barplot(
  rozklad_centrali,
  xlab = "Liczba b��d�w", 
  ylab = "Prawdopodobie�stwo",
  main = "Rozk�ad empiryczny liczby b��d�w"
)

# sugeruj� rozklad Poissona
rozklad_poissona = dpois(
  x = 0:5, 
  lambda = mean(Centrala$Liczba)
)

barplot(
  rozklad_poissona, 
  names.arg = 0:5,
  xlab = "k", 
  ylab = "P(X=k)", 
  main = "Funkcja prawdopodobie�stwa"
)

both <- rbind(rozklad_centrali, rozklad_poissona)
barplot(both,beside=T)
 
# 2. Oblicz warto�� estymatora parametru rozk�adu teoretycznego.

# warto�� estymatora
v = as.vector(Centrala[1])
p_est = colMeans(v) / length(v)

p_est <- mean(Centrala$Liczba)

# 3. Por�wnaj empiryczne prawdopodobie�stwa wyst�pienia poszczeg�lnych warto�ci liczby
# zg�osze� w pr�bie z warto�ciami teoretycznymi uzyskanymi na podstawie rozk�adu
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
  xlab = "Liczba b��d�w", ylab = "Prawdopodobie�stwo",
  main = "Rozk�ady empiryczny i teoretyczny liczby b��d�w",
  col = c("red", "blue"), 
  legend = rownames(counts), 
  beside = TRUE
)

# 4. Sprawd� dopasowanie rozk�adu teoretycznego za pomoc� wykresy kwantyl-kwantyl. 

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

# 5. Czy na podstawie powy�szych rozwa�a� rozk�ad teoretyczny wydaje si� odpowiedni? 


# 6. Oblicz prawdopodobie�stwo empiryczne i teoretyczne, �e liczba zg�osze� jest mniejsza ni� 4. 

# empirycznie
mean(Centrala$Liczba < 4)

# teoretycznie: X ~ U(a_est, b_est) oraz P(X > 10) = 1 - P(X <= 10) = 1 - F(10)
ppois(3, lambda = mean(Centrala$Liczba))

# Zadanie 3 ---------------------------------------------------------------
# Niech X = (Xq,... �X, b�dzie pr�b� prost� z rozk�adu Rayleigha o g�sto�ci danej w �wiczeniu

# Poka�, �e ENW parametru A jest postaci danej w �wiczeniu 

# 1. Poka�, �e funkcja wiarogodno�ci wynosi dane w �wiczeniu

# 2. Wprowad� pomocnicz� funkcj� dan� w �wiczeniu

# 3. Wyznacz pochodn� funkcji / wzgl�dem A

# 4. Przyr�wnaj powy�sz� pochodn� do zera i rozwi�� otrzymane r�wnanie. 


# Zadanie 4 ---------------------------------------------------------------
# Notowano pomiary �redniej szybko�ci wiatru w odst�pach 15 minutowych wok�
# nowo powstaj�cej elektrowni wiatrowej. Wyniki s� nast�puj�ce: 

wiatr = c(0.9, 6.2, 2.1, 4.1, 7.3, 1.0, 4.6, 6.4, 3.8, 5.0, 2.7, 9.2, 5.9, 7.4, 3.0, 4.9, 8.2, 5.0, 1.2, 10.1, 12.2, 2.8, 5.9, 8.2, 0.5)

# 1. Zasugeruj rozk�ad teoretyczny badanej zmiennej. 
# sugeruj� rozk�ad Rayleigha

# 2. Oblicz warto�� ENW parametru rozk�adu teoretycznego. 

# estymator najwi�kszej wiarygodno�ci 
ENW = mean(wiatr^2)

lambda = ENW

# 3, Por�wnaj rozk�ad empiryczny wyst�pienia poszczeg�lnych warto�ci �redniej szybko�ci
# wiatru w pr�bie z warto�ciami teoretycznymi uzyskanymi na podstawie rozk�adu
# teoretycznego. 

# histogram z estymatorem j�drowym g�sto�ci
hist(
  wiatr, 
  xlab = "Czas oczekiwania na tramwaj", 
  main = "Rozk�ad empiryczny czasu oczekiwania na tramwaj",
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

# 4. Sprawd� dopasowanie rozk�adu teoretycznego za pomoc� wykresy kwantyl-kwantyl.
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

# 5. Czy na podstawie powy�szych rozwa�a� rozk�ad teoretyczny wydaje si� odpowiedni? 
# dlaczego nie, kogo to interesuje

# 6. Oblicz empiryczne i teoretyczne prawdopodobie�stwo, �e �rednia szybko�� wiatru 
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

# 7. Oblicz warto�� ENW dla warto�ci oczekiwanej i wariancji rozk�adu teoretycznego. 

## [1] 5.274353
## [1] 7.601197

