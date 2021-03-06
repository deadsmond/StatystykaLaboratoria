# Zaj�cia 4 ####

# Zadanie 1 ---------------------------------------------------------------
# Zmienna wynik w pliku ankieta.txt opisuje wyniki badania dzia�alno�ci prezydenta
# pewnego miasta. Wybrano losowo 100 mieszka�c�w miasta i zadano im nast�puj�ce pytanie:
# Jak oceniasz dzia�alno�� prezydenta miasta? Dost�pne by�y nast�puj�ce odpowiedzi:
# zdecydowanie dobrze ( a ), dobrze ( b ), �le ( c ), zdecydowanie �le ( d ), nie mam zdania
# ( e ). Jakiego typu jest ta zmienna? Jakie s� mo�liwe warto�ci tej zmiennej? 
  
# 1. Zaimportuj dane z pliku ankieta.txt do zmiennej ankieta .
ankieta <- read.table("http://ls.home.amu.edu.pl/data_sets/ankieta.txt", header = TRUE)

# 2. Przedstaw rozk�ad empiryczny zmiennej wynik za pomoc� szeregu rozdzielczego.
rozklad_1 = data.frame(
  cbind(
    liczebnosc = table(ankieta$wynik),
    procent = prop.table(
      table(ankieta$wynik)
    )
  )
)

# 3. Przedstaw rozk�ad empiryczny zmiennej wynik tylko dla os�b z wykszta�ceniem
# podstawowym za pomoc� szeregu rozdzielczego. 
temp = ankieta[ankieta$szkola == "p", ]

rozklad_2 = data.frame(
  cbind(
    liczebnosc = table(temp$wynik),
    procent = prop.table(
      table(temp$wynik)
    )
  )
)

# 4. Zilustruj wyniki ankiety za pomoc� wykresu s�upkowego i ko�owego. 
barplot(
  table(ankieta$wynik),
  col=terrain.colors(5),
  xlab = "Odpowiedzi", 
  ylab = "Liczebno��",
  main = "Rozk�ad empiryczny liczby b��d�w"
)

barplot(
  table(ankieta$wynik),
  col=terrain.colors(5),
  xlab = "Odpowiedzi", 
  ylab = "Liczebno��",
  main = "Rozk�ad empiryczny zmiennej wynik"
)

pie(table(ankieta$wynik))

barplot(
  prop.table(
    table(
      ankieta$wynik
    )
  ),
  col = terrain.colors(5),
  main = "Rozk�ad empiryczny zmiennej wynik",
  ylab = "Prawdopodobie�stwo",
  xlab = "Odpowiedzi"
)

# 5. Zilustruj wyniki ankiety za pomoc� wykresu s�upkowego z podzia�em na kobiety i m�czyzn. 
men = ankieta[ankieta$plec == "m", ]
women = ankieta[ankieta$plec == "k", ]

barplot(
  cbind(
    k=table(women$wynik),
    m=table(men$wynik)
  ),
  beside=T,
  col=terrain.colors(5), 
  legend.text = c("a", "b", "c", "d", "e")
)

# 6. Zinterpretuj powy�sze wyniki (tabelaryczne i graficzne).


# Zadanie 2 ---------------------------------------------------------------
# 1. Zaimportuj dane z pliku Centrala.RData. 
load("Centrala.RData")

# 2. Przedstaw rozk�ad empiryczny liczby zg�osze� za pomoc� szeregu rozdzielczego. 
data.frame(
  cbind(
    liczebnosc = table(Centrala$Liczba),
    procent = prop.table(
      table(Centrala$Liczba)
    )
  )
)

# 3. Zilustruj liczb� zg�osze� za pomoc� wykresu s�upkowego i ko�owego.
barplot(table(Centrala$Liczba),
        col= c("black", "red", "green","blue", "yellow"),
        main = "Rozk�ad empiryczny zmiennej wynik",
        ylab = "liczebno��",
        xlab = "Odpowiedzi")

barplot(prop.table(table(Centrala$Liczba)),
        col= c("black", "red", "green","blue", "yellow"),
        main = "Rozk�ad empiryczny zmiennej wynik",
        ylab = "liczebno��",
        xlab = "Odpowiedzi")

pie(table(Centrala$Liczba))

# 4. Obliczy� �redni� z liczby zg�osze�, median� liczby zg�osze�, odchylenie standardowe
# liczby zg�osze� i wsp�czynnik zmienno�ci liczby zg�osze�. 

# �rednia
mean(Centrala$Liczba)

# mediana
median(Centrala$Liczba)

# odchylenie standardowe
sd(Centrala$Liczba)

# wsp�czynnik zmienno�ci
sd(Centrala$Liczba) / mean(Centrala$Liczba) * 100

# 5, Zinterpretuj powy�sze wyniki (tabelaryczne, graficzne i liczbowe). 


# Zadanie 3 ---------------------------------------------------------------
# Notowano pomiary �redniej szybko�ci wiatru w odst�pach 15 minutowych wok�
# nowo powstaj�cej elektrowni wiatrowej. Jakiego typu jest ta zmienna? Jakie s� mo�liwe warto�ci tej zmiennej? 

# 1. Przedstaw rozk�ad empiryczny badanej zmiennej za pomoc� szeregu rozdzielczego. 
vec = c(0.9, 6.2, 2.1, 4.1, 7.3, 1.0, 6.4, 3.8, 5.0, 2.7, 9.2, 5.9, 7.4, 3.0, 4.9, 5.0, 1.2, 10.1, 12.2, 2.8, 5.9, 8.2, 0.5)

mat <- cbind(c(0.9,1.0,2.7,4.9,12.2),c(6.2,4.6,9.2,8.2,2.8),c(2.1,6.4,5.9,5.0,5.9),c(4.1,3.8,7.4,1.2,8.2),c(7.3,5.0,3.0,10.1,0.5))
dane = matrix(mat, nrow = 5, ncol = 5)

centrala = data.frame(
  cbind(
    liczebnosc=table(
      cut(
        dane, 
        breaks = seq(0,14,2)
      )
    ),
    procent = prop.table(
      table(
        cut(
          dane, 
          breaks = seq(0,14,2)
        )
      )
    )
  )
)

# 2. Zilustruj rozk�ad empiryczny �redniej szybko�ci wiatru za pomoc� histogramu i wykresu
# pude�kowego. Jakie s� zalety i wady tych wykres�w?

# histogram
hist(
  dane, 
  xlab = "Czas oczekiwania na tramwaj", 
  main = "Rozk�ad empiryczny �redniej szybko�ci wiatru"
)

# histogram z estymatorem j�drowym g�sto�ci
hist(
  dane, 
  xlab = "Czas oczekiwania na tramwaj", 
  main = "Rozk�ad empiryczny czasu oczekiwania na tramwaj",
  probability = TRUE, 
  col = "lightgreen"
)

lines(
  density(dane), 
  col = "red", 
  lwd = 2
)

# wykres ramkowy
boxplot(
  c(mat), 
  ylab = "Czas oczekiwania na tramwaj", 
  main = "Rozk�ad empiryczny czasu oczekiwania na tramwaj"
)

# 3. Obliczy� �redni�, median�, odchylenie standardowe, wsp�czynnik asymetrii i kurtoz� �redniej szybko�ci wiatru. 

# �rednia
mean(dane)

# mediana
median(dane)

# odchylenie standardowe
sd(dane)

# wsp�czynnik zmienno�ci
sd(dane) / mean(dane) * 100
library(e1071)

# wsp�czynnik asymetrii
skewness(dane)

# kurtoza
kurtosis(dane)

# 5. Zinterpretuj powy�sze wyniki (tabelaryczne, graficzne i liczbowe). 


# Zadanie 4 ---------------------------------------------------------------
# Napisz funkcj� wspolczynnik_zmiennosci() , kt�ra oblicza warto�� wsp�czynnika
# zmienno�ci dla danego wektora obserwacji. Funkcja powinna mie� dwa argumenty:
#   * x -wektor zawieraj�cy dane,
#   e na.rm - warto�� logiczna (domy�lnie FALSE ), 
# kt�ra wskazuje czy braki danych (obiekty NA ) maj� by� zignorowane.
# Funkcja zwraca warto�� wsp�czynnika zmienno�ci wyra�on� w procentach. Ponadto funkcja
# Sprawdza, czy wektor x jest wektorem numerycznym. W przeciwnym razie zostanie
# zwr�cony b��d z nast�puj�cym komunikatem: �argument nie jest liczb��. 


wspolczynnik_zmiennosci <- function(x, na.rm = FALSE) {
  if(is.numeric(x) == TRUE) {
    
    if(na.rm == TRUE) {
      
      x <- x[!is.na(x)]
      print
      sd <- sd(x)
      avg <- mean(x)
      V <- sd/avg
      return(V* 100)
      
    } else {
      return(NA)
    }
  } else {
    stop("argument nie jest liczb�")
  }
}

x <- c(1, NA, 3)

wspolczynnik_zmiennosci(x)
## [1] NA

wspolczynnik_zmiennosci(x, na.rm = TRUE)
## [1] 70.71068

wspolczynnik_zmiennosci()
## Error in wspolczynnik_zmiennosci() : 
##   argument "x" is missing, with no default

wspolczynnik_zmiennosci(c("x", "y"))
## Error in wspolczynnik_zmiennosci(c("x", "y")) : argument nie jest liczb�