# Zajêcia 4 ####

# Zadanie 1 ---------------------------------------------------------------
# 1. 
ankieta <- read.table("http://ls.home.amu.edu.pl/data_sets/ankieta.txt", header = TRUE)

# 2.
# rozk³ad empiryczny opisany za pomoc¹ szeregu rozdzielczego
rozklad_1 = data.frame(
  cbind(
    liczebnosc = table(ankieta$wynik),
    procent = prop.table(
      table(ankieta$wynik)
    )
  )
)

# 3.
temp = ankieta[ankieta$szkola == "p", ]

rozklad_2 = data.frame(
  cbind(
    liczebnosc = table(temp$wynik),
    procent = prop.table(
      table(temp$wynik)
    )
  )
)

# 4.
# wykres s³upkowy
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

# 5
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

# Zadanie 2 ---------------------------------------------------------------
# 1. 
load("Centrala.RData")

# 2
data.frame(
  cbind(
    liczebnosc = table(Centrala$Liczba),
    procent = prop.table(
      table(Centrala$Liczba)
    )
  )
)

# 3
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

# 4
# œrednia
mean(Centrala$Liczba)

# mediana
median(Centrala$Liczba)

# odchylenie standardowe
sd(Centrala$Liczba)

# wspó³czynnik zmiennoœci
sd(Centrala$Liczba) / mean(Centrala$Liczba) * 100

# Zadanie 3 ---------------------------------------------------------------
# 1.
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

# 2
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

# 3
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

# Zadanie 4 ---------------------------------------------------------------
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