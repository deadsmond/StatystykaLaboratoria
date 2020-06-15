# Zajêcia 4 ####

# Zadanie 1 ---------------------------------------------------------------
# Zmienna wynik w pliku ankieta.txt opisuje wyniki badania dzia³alnoœci prezydenta
# pewnego miasta. Wybrano losowo 100 mieszkañców miasta i zadano im nastêpuj¹ce pytanie:
# Jak oceniasz dzia³alnoœæ prezydenta miasta? Dostêpne by³y nastêpuj¹ce odpowiedzi:
# zdecydowanie dobrze ( a ), dobrze ( b ), Ÿle ( c ), zdecydowanie Ÿle ( d ), nie mam zdania
# ( e ). Jakiego typu jest ta zmienna? Jakie s¹ mo¿liwe wartoœci tej zmiennej? 
  
# 1. Zaimportuj dane z pliku ankieta.txt do zmiennej ankieta .
ankieta <- read.table("http://ls.home.amu.edu.pl/data_sets/ankieta.txt", header = TRUE)

# 2. Przedstaw rozk³ad empiryczny zmiennej wynik za pomoc¹ szeregu rozdzielczego.
rozklad_1 = data.frame(
  cbind(
    liczebnosc = table(ankieta$wynik),
    procent = prop.table(
      table(ankieta$wynik)
    )
  )
)

# 3. Przedstaw rozk³ad empiryczny zmiennej wynik tylko dla osób z wykszta³ceniem
# podstawowym za pomoc¹ szeregu rozdzielczego. 
temp = ankieta[ankieta$szkola == "p", ]

rozklad_2 = data.frame(
  cbind(
    liczebnosc = table(temp$wynik),
    procent = prop.table(
      table(temp$wynik)
    )
  )
)

# 4. Zilustruj wyniki ankiety za pomoc¹ wykresu s³upkowego i ko³owego. 
barplot(
  table(ankieta$wynik),
  col=terrain.colors(5),
  xlab = "Odpowiedzi", 
  ylab = "Liczebnoœæ",
  main = "Rozk³ad empiryczny liczby b³êdów"
)

barplot(
  table(ankieta$wynik),
  col=terrain.colors(5),
  xlab = "Odpowiedzi", 
  ylab = "Liczebnoœæ",
  main = "Rozk³ad empiryczny zmiennej wynik"
)

pie(table(ankieta$wynik))

barplot(
  prop.table(
    table(
      ankieta$wynik
    )
  ),
  col = terrain.colors(5),
  main = "Rozk³ad empiryczny zmiennej wynik",
  ylab = "Prawdopodobieñstwo",
  xlab = "Odpowiedzi"
)

# 5. Zilustruj wyniki ankiety za pomoc¹ wykresu s³upkowego z podzia³em na kobiety i mê¿czyzn. 
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

# 6. Zinterpretuj powy¿sze wyniki (tabelaryczne i graficzne).


# Zadanie 2 ---------------------------------------------------------------
# 1. Zaimportuj dane z pliku Centrala.RData. 
load("Centrala.RData")

# 2. Przedstaw rozk³ad empiryczny liczby zg³oszeñ za pomoc¹ szeregu rozdzielczego. 
data.frame(
  cbind(
    liczebnosc = table(Centrala$Liczba),
    procent = prop.table(
      table(Centrala$Liczba)
    )
  )
)

# 3. Zilustruj liczbê zg³oszeñ za pomoc¹ wykresu s³upkowego i ko³owego.
barplot(table(Centrala$Liczba),
        col= c("black", "red", "green","blue", "yellow"),
        main = "Rozk³ad empiryczny zmiennej wynik",
        ylab = "liczebnoœæ",
        xlab = "Odpowiedzi")

barplot(prop.table(table(Centrala$Liczba)),
        col= c("black", "red", "green","blue", "yellow"),
        main = "Rozk³ad empiryczny zmiennej wynik",
        ylab = "liczebnoœæ",
        xlab = "Odpowiedzi")

pie(table(Centrala$Liczba))

# 4. Obliczyæ œredni¹ z liczby zg³oszeñ, medianê liczby zg³oszeñ, odchylenie standardowe
# liczby zg³oszeñ i wspó³czynnik zmiennoœci liczby zg³oszeñ. 

# œrednia
mean(Centrala$Liczba)

# mediana
median(Centrala$Liczba)

# odchylenie standardowe
sd(Centrala$Liczba)

# wspó³czynnik zmiennoœci
sd(Centrala$Liczba) / mean(Centrala$Liczba) * 100

# 5, Zinterpretuj powy¿sze wyniki (tabelaryczne, graficzne i liczbowe). 


# Zadanie 3 ---------------------------------------------------------------
# Notowano pomiary œredniej szybkoœci wiatru w odstêpach 15 minutowych wokó³
# nowo powstaj¹cej elektrowni wiatrowej. Jakiego typu jest ta zmienna? Jakie s¹ mo¿liwe wartoœci tej zmiennej? 

# 1. Przedstaw rozk³ad empiryczny badanej zmiennej za pomoc¹ szeregu rozdzielczego. 
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

# 2. Zilustruj rozk³ad empiryczny œredniej szybkoœci wiatru za pomoc¹ histogramu i wykresu
# pude³kowego. Jakie s¹ zalety i wady tych wykresów?

# histogram
hist(
  dane, 
  xlab = "Czas oczekiwania na tramwaj", 
  main = "Rozk³ad empiryczny œredniej szybkoœci wiatru"
)

# histogram z estymatorem j¹drowym gêstoœci
hist(
  dane, 
  xlab = "Czas oczekiwania na tramwaj", 
  main = "Rozk³ad empiryczny czasu oczekiwania na tramwaj",
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
  main = "Rozk³ad empiryczny czasu oczekiwania na tramwaj"
)

# 3. Obliczyæ œredni¹, medianê, odchylenie standardowe, wspó³czynnik asymetrii i kurtozê œredniej szybkoœci wiatru. 

# œrednia
mean(dane)

# mediana
median(dane)

# odchylenie standardowe
sd(dane)

# wspó³czynnik zmiennoœci
sd(dane) / mean(dane) * 100
library(e1071)

# wspó³czynnik asymetrii
skewness(dane)

# kurtoza
kurtosis(dane)

# 5. Zinterpretuj powy¿sze wyniki (tabelaryczne, graficzne i liczbowe). 


# Zadanie 4 ---------------------------------------------------------------
# Napisz funkcjê wspolczynnik_zmiennosci() , która oblicza wartoœæ wspó³czynnika
# zmiennoœci dla danego wektora obserwacji. Funkcja powinna mieæ dwa argumenty:
#   * x -wektor zawieraj¹cy dane,
#   e na.rm - wartoœæ logiczna (domyœlnie FALSE ), 
# która wskazuje czy braki danych (obiekty NA ) maj¹ byæ zignorowane.
# Funkcja zwraca wartoœæ wspó³czynnika zmiennoœci wyra¿on¹ w procentach. Ponadto funkcja
# Sprawdza, czy wektor x jest wektorem numerycznym. W przeciwnym razie zostanie
# zwrócony b³¹d z nastêpuj¹cym komunikatem: „argument nie jest liczb¹”. 


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
    stop("argument nie jest liczb¹")
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
## Error in wspolczynnik_zmiennosci(c("x", "y")) : argument nie jest liczb¹