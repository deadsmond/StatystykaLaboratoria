# Zajêcia 6 ####

# Zadanie 1 ---------------------------------------------------------------
# Przebadano 200 losowo wybranych 5-sekundowych okresów pracy centrali
# telefonicznej. Rejestrowano liczbê zg³oszeñ. Wyniki s¹ zawarte w pliku Centrala.RData.
# Wykorzystuj¹c przyjêty wczeœniej model statystyczny dla tych danych, wyznacz (trzema
# metodami) przedzia³ ufnoœci dla parametru rozk³adu teoretycznego. 


load(url("http://ls.home.amu.edu.pl/data_sets/Centrala.RData"))

# sugerujê rozk³ad Poissona, zatem funkcja to epois

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
# Zmienna w pliku awarie.txt opisuje wyniki 50 pomiarów czasu bezawaryjnej pracy
# danego urz¹dzenia (w godzinach). Wykorzystuj¹c przyjêty na wyk³adzie model statystyczny
# dla tych danych wyznacz granice przedzia³u ufnoœci dla wartoœci oczekiwanej i wariancji
# rozk³adu teoretycznego.


Awaria = as.numeric(unlist(read.table("awarie.txt")))

# sugerujê rozk³ad wyk³adniczy

# zgodnie z tabel¹ https://pl.wikipedia.org/wiki/Rozk%C5%82ad_wyk%C5%82adniczy
# wariancja to parametr^-2, wartoœæ oczekiwana to par^-1

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
# Niech X =(Xy,...,X„) bêdzie prób¹ prost¹ z populacji o rozk³adzie
# Rayleigha R(A), A > 0. 

# 1. Napisz funkcjê median_cint() , która implementuje nastêpuj¹cy przybli¿ony przedzia³
# ufnoœci dla mediany sqrt( A In 2 ) tego rozk³adu: 

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

# 2. Nastêpuj¹ce dane to pomiary œredniej szybkoœci wiatru w odstêpach 15 minutowych
# odnotowane wokó³ nowo powstaj¹cej elektrowni wiatrowej: 
  
wiatr = c(0.9, 6.2, 2.1, 4.1, 7.3, 1.0, 4.6, 6.4, 3.8, 5.0, 2.7, 9.2, 5.9, 7.4, 3.0, 4.9, 8.2, 5.0, 1.2, 10.1, 12.2, 2.8, 5.9, 8.2, 0.5)

# Teoretyczny rozk³ad œredniej szybkoœci wiatru to rozk³ad Rayleigha R(N), A>0.
# U¿ywaj¹c funkcji median_cint() , oblicz wartoœæ ENW i krañce 95% przedzia³u ufnoœci
# dla mediany œredniej szybkoœci wiatru. Wskazówka: Przed wywo³aniem funkcji
# median_cint(), najpierw za³aduj nastêpuj¹ce funkcje przeci¹¿one print() oraz summary():

rez = median_cint(wiatr)
print(rez)
summary(rez)

# Zadanie 4 ---------------------------------------------------------------
# Dla danego wektora obserwacji i poziomu ufnoœci napisz funkcjê okreœlaj¹c¹
# granice przedzia³u ufnoœci na poziomie ufnoœcil-a,a € (0, 1) dla wartoœci oczekiwanej w
# rozk³adzie normalnym. Domyœlny poziom ufnoœci powinien wynosiæ 0,95. Nastêpnie
# przeprowadŸ symulacje (z liczb¹ powtórzeñ nr = 1088 ) sprawdzaj¹c prawdopodobieñstwo
# pokrycia tego przedzia³u ufnoœci (tj. prawdopodobieñstwo, ¿e ten przedzia³ ufnoœci zawiera
# wartoœæ oczekiwan¹) dla rozk³adów N(1,3), x*(3) i Ex(3) osobno. Rozwa¿ liczby
# obserwacji n = 10, 50, 100. Zinterpretuj wyniki. Wskazówka: Symulacja powinna przebiegaæ
# wed³ug nastêpuj¹cych kroków:

#   1. Przyjmij poziom istotnoœci, n, nr , rozk³ad generowanych danych oraz temp = 0.
#   2. Wygeneruj n obserwacji z zadanego rozk³adu. 
#   3. Wyznacz granice przedzia³u ufnoœci dla danych wygenerowanych w kroku 2.
#   4. Jeœli teoretyczna wartoœæ oczekiwana nale¿y do przedzia³u otrzymanego w kroku 3,
#      zwiêksz temp o jeden. 
#   5. Powtórz kroki 2-4 nr razy.
#   6. Wyznacz temp / nr. 


# https://stats.idre.ucla.edu/r/modules/probabilities-and-distributions/
granica_przedzia³u_ufnoœci_wartosci_oczekiwanej_rozk³adu_normalnego <- function(x, conf_level = 0.95) {
  n = length(x)
  
  enorm(x, conf.level = conf_level)
  
  LCL = 
  
  LCL = max(x) / nthroot(1 - conf_level/2, n)
  UCL = max(x) / nthroot(conf_level/2, n)
  
  return(c(LCL, UCL))
}

granica_przedzia³u_ufnoœci_wartosci_oczekiwanej_rozk³adu_normalnego <- function(x, conf_level = 0.95) {
  
  u <- enorm(
    x,
    conf.level = conf_level
  )

  return(u)
}

norm <- rnorm(10)
norm <- rnorm(50)
norm <- rnorm(100)

granica_przedzia³u_ufnoœci_wartosci_oczekiwanej_rozk³adu_normalnego(wiatr, 0.95)
