# Zaj�cia 6 ####

# Zadanie 1 ---------------------------------------------------------------
# Przebadano 200 losowo wybranych 5-sekundowych okres�w pracy centrali
# telefonicznej. Rejestrowano liczb� zg�osze�. Wyniki s� zawarte w pliku Centrala.RData.
# Wykorzystuj�c przyj�ty wcze�niej model statystyczny dla tych danych, wyznacz (trzema
# metodami) przedzia� ufno�ci dla parametru rozk�adu teoretycznego. 


load(url("http://ls.home.amu.edu.pl/data_sets/Centrala.RData"))

# sugeruj� rozk�ad Poissona, zatem funkcja to epois

library(EnvStats)

b_conf_int <- function(x, conf_level = 0.95) {
  u <- epois(
    x, 
    ci = TRUE, 
    ci.method = "pearson",
    conf.level = conf_level
  )$interval$limits
  
  return(c(u))
}

d_conf_int <- function(x, conf_level = 0.95) {
  u <- epois(
    x, 
    ci = TRUE, 
    ci.method = "pearson.hartley.approx",
    conf.level = conf_level
  )$interval$limits
  
  return(c(u))
}


c_conf_int <- function(x, conf_level = 0.95) {
  u <- epois(
    x, 
    ci = TRUE, 
    ci.method = "normal.approx",
    conf.level = conf_level
  )$interval$limits
  
  return(c(u))
}

b_conf_int(Centrala$Liczba)
d_conf_int(Centrala$Liczba)
c_conf_int(Centrala$Liczba)

# Zadanie 2 ---------------------------------------------------------------
# Zmienna w pliku awarie.txt opisuje wyniki 50 pomiar�w czasu bezawaryjnej pracy
# danego urz�dzenia (w godzinach). Wykorzystuj�c przyj�ty na wyk�adzie model statystyczny
# dla tych danych wyznacz granice przedzia�u ufno�ci dla warto�ci oczekiwanej i wariancji
# rozk�adu teoretycznego.


Awaria = as.numeric(unlist(read.table("awarie.txt")))

# sugeruj� rozk�ad wyk�adniczy

# zgodnie z tabel� https://pl.wikipedia.org/wiki/Rozk%C5%82ad_wyk%C5%82adniczy
# wariancja to parametr^-2, warto�� oczekiwana to par^-1

wariancja <- function(x, conf_level = 0.95) {
  
  u <- eexp(
    x,
    ci = TRUE, 
    ci.method = "exact",
    conf.level = conf_level
  )
  
  temp = u$interval$limits ** -2
  
  return(temp)
}

expected_value <- function(x, conf_level = 0.95) {
  
  u <- eexp(
    x,
    ci = TRUE, 
    ci.method = "exact",
    conf.level = conf_level
  )
  
  temp = u$interval$limits ** -1
  
  return(temp)
}

wariancja(Awaria)
expected_value(Awaria)

# Zadanie 3 ---------------------------------------------------------------
# Niech X =(Xy,...,X�) b�dzie pr�b� prost� z populacji o rozk�adzie
# Rayleigha R(A), A > 0. 

# 1. Napisz funkcj� median_cint() , kt�ra implementuje nast�puj�cy przybli�ony przedzia�
# ufno�ci dla mediany sqrt( A In 2 ) tego rozk�adu: 

print.confint <- function(x) {
  cat(x$conf_level * 100, "percent confidence interval:", "\n")
  cat(x$l, " ", x$r, "\n")
}

summary.confint <- function(x) {
  cat("\n", "Confidence interval of", x$title, "\n", "\n")
  cat(x$conf_level * 100, "percent confidence interval:", "\n")
  cat(x$l, " ", x$r, "\n")
  cat("sample estimate", "\n")
  cat(x$est, "\n")
}

median_cint <- function(x, conf_level = 0.95){
  
  n = length(x)
  z = qnorm(1 - (1 - conf_level) / 2, 0, 1)
  LCL = sqrt(ln(2) * mean(x ** 2) * (1 - z / sqrt(n)))
  UCL = sqrt(ln(2) * mean(x ** 2) * (1 + z / sqrt(n)))
  ENW = mean(c(LCL, UCL))
  
  w = list(
    title = "mediana", 
    est = ENW, 
    l = LCL, 
    r = UCL, 
    conf_level = conf_level
  )
  class(w) <- "confint"
  
  return(w)
}

# 2. Nast�puj�ce dane to pomiary �redniej szybko�ci wiatru w odst�pach 15 minutowych
# odnotowane wok� nowo powstaj�cej elektrowni wiatrowej: 
  
wiatr = c(0.9, 6.2, 2.1, 4.1, 7.3, 1.0, 4.6, 6.4, 3.8, 5.0, 2.7, 9.2, 5.9, 7.4, 3.0, 4.9, 8.2, 5.0, 1.2, 10.1, 12.2, 2.8, 5.9, 8.2, 0.5)

# Teoretyczny rozk�ad �redniej szybko�ci wiatru to rozk�ad Rayleigha R(N), A>0.
# U�ywaj�c funkcji median_cint() , oblicz warto�� ENW i kra�ce 95% przedzia�u ufno�ci
# dla mediany �redniej szybko�ci wiatru. Wskaz�wka: Przed wywo�aniem funkcji
# median_cint(), najpierw za�aduj nast�puj�ce funkcje przeci��one print() oraz summary():

rez = median_cint(wiatr)
print(rez)
summary(rez)

# Zadanie 4 ---------------------------------------------------------------
# Dla danego wektora obserwacji i poziomu ufno�ci napisz funkcj� okre�laj�c�
# granice przedzia�u ufno�ci na poziomie ufno�cil-a,a � (0, 1) dla warto�ci oczekiwanej w
# rozk�adzie normalnym. Domy�lny poziom ufno�ci powinien wynosi� 0,95. Nast�pnie
# przeprowad� symulacje (z liczb� powt�rze� nr = 1088 ) sprawdzaj�c prawdopodobie�stwo
# pokrycia tego przedzia�u ufno�ci (tj. prawdopodobie�stwo, �e ten przedzia� ufno�ci zawiera
# warto�� oczekiwan�) dla rozk�ad�w N(1,3), x*(3) i Ex(3) osobno. Rozwa� liczby
# obserwacji n = 10, 50, 100. Zinterpretuj wyniki. Wskaz�wka: Symulacja powinna przebiega�
# wed�ug nast�puj�cych krok�w:

#   1. Przyjmij poziom istotno�ci, n, nr , rozk�ad generowanych danych oraz temp = 0.
#   2. Wygeneruj n obserwacji z zadanego rozk�adu. 
#   3. Wyznacz granice przedzia�u ufno�ci dla danych wygenerowanych w kroku 2.
#   4. Je�li teoretyczna warto�� oczekiwana nale�y do przedzia�u otrzymanego w kroku 3,
#      zwi�ksz temp o jeden. 
#   5. Powt�rz kroki 2-4 nr razy.
#   6. Wyznacz temp / nr. 


# https://stats.idre.ucla.edu/r/modules/probabilities-and-distributions/
granica_przedzia�u_ufno�ci_wartosci_oczekiwanej_rozk�adu_normalnego <- function(x, conf_level = 0.95) {
  n = length(x)
  
  enorm(x, conf.level = conf_level)
  
  LCL = 
  
  LCL = max(x) / nthroot(1 - conf_level/2, n)
  UCL = max(x) / nthroot(conf_level/2, n)
  
  return(c(LCL, UCL))
}

granica_przedzia�u_ufno�ci_wartosci_oczekiwanej_rozk�adu_normalnego <- function(x, conf_level = 0.95) {
  
  u <- enorm(
    x,
    conf.level = conf_level
  )

  return(u)
}

norm <- rnorm(10)
norm <- rnorm(50)
norm <- rnorm(100)

granica_przedzia�u_ufno�ci_wartosci_oczekiwanej_rozk�adu_normalnego(wiatr, 0.95)
