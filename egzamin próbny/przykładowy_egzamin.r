
computers = read.csv("j:/Desktop/R/egzamin pr�bny/computers.csv")
heights = read.csv("j:/Desktop/R/egzamin pr�bny/weight-height.csv")
spotify = read.csv("j:/Desktop/R/egzamin pr�bny/spotify.csv")

# Zadanie - zbi�r danych "computers", rozk�ad RAM ----
# Plik computers.csv zawiera dane dotycz�ce cen komputer�w
# Zmienna ram zawiera informacje o pami�ci RAM w MB,
# a zmienna screen o przek�tnej ekranu w calach.
# Wyznacz rozk�ad zmiennej ram dla komputer�w z 14-calowym ekranem. 

comp_screen_14 = computers[computers$screen==14, ]

prop.table(
  table(
    comp_screen_14$ram  # wa�ne - wyb�r kolumny RAM
  )
)

# Zadanie - zbi�r danych spotify, sk�adowe g��wne --
# PLik spotify.csv zawiera dane piosenek wraz z ich
# cechami muzycznymi pochodz�cymi z API Spotify.
# Przeprowadzono analiz� sk��dowych g��wnych ze skalowaniem,
# wykorzystuj�c wyl�cznie zmienne numeryczne (kolumny 1-9).
# Ile procent zmienno�ci uk�adu wyja�niaj� 
# sumarycznie trzy pierwsze sk�adowe g��wne?

spotify_num = spotify[, 1:9]

var(spotfy_num)

p_cal = prcomp(spotify_num, scale = TRUE)

# suma po drugim wierszu pierwszych trzech kolumn?
summary(p_cal) 

# Zadanie - zbi�r danych spotify, regresja wieloliniowa --
# PLik spotify.csv zawiera dane piosenek wraz z ich
# cechami muzycznymi pochodz�cymi z API Spotify.
# Zmienna ci�g�a valence, mierzona w przedziale 0-1.0,
# opisuje pozytywny nastr�j utworu (0 - ponure, 1 - radosne)
# Dopasowano model regresji wielokrotnej dla zmiennej zale�nej
# valence i pozostalych zmiennych jako niezale�nych.
# Nast�pnie model pe�ny zredukowano za pomoc� regresji krokowej w ty�.
# Zakladaj�c model zredukowany, o ile wzro�nie zmienna
# valence, je�eli zmienna danceability wzros�aby o jedn� jednostk�,
# a pozostale wsp�czynniki pozosta�yby bez zmian?

spotify_num = spotify[, 1:9]

model_1 = lm(valence ~ ., data = spotify_num)

summary(model_1)

model_zredukowany = step(model_1)

summary(model_zredukowany) 


# Zadanie - zbi�r danych "heights", t-student ----
# Plik weight-height.csv zawiera dane wzrostu i wagi 10000 os�b
# z podzialem na p�e�. Zmienna Gender wskazuje na p�e�
# i przyjmuje wartosci Male i Female.
# Na poziomie istotno�ci postanowiono zweryfikowa� hipotez�
# �e m�czy�ni s� istotnie wy�si od kobiet. 
# Wyznacz Warto�� statystyki testowej.

heights_male = heights[heights$Gender == "Male", "Height"]
heights_female = heights[heights$Gender == "Female", "Height"]

shapiro.test(heights_male)
shapiro.test(heights_female)

t.test(
  heights_female, 
  heights_male, 
  alternative = "greater", 
  var.equal = FALSE
)

# Zadanie - test istotno�ci dla wariancji ---
# Napisz funkcj� realizujac� test istotno�ci dla
# wariancji w modelu normalnym. Zwracany wynik
# ma by� klasy htest.

war.test = function(
  x, 
  war0, 
  alt = c('two.sided', 'less', 'greater')
){
  statistic = (length(x)-1)*var(x)/war0
  d = length(x) - 1
  alt = match.arg(alt)
  p.value = pchisq(statistic, d)
  p.value = switch (
    alt, 
    'two.sided' = 2*min(p.value, 1-p.value), 
    'greater' = 1 - p.value, 
    'less' = p.value
  )
  names(statistic) = 'T'
  names(d) = 'num df'
  wynik = list(
    statistic = statistic, 
    parameter = d, 
    p.value = p.value, 
    alternative = alt, 
    method = 'Test istotno?ci dla wariacji w modelu normalnym', 
    data.name = deparse(substitute(x))
  ) #paste(deparse(substitute(x))))
  
  class(wynik) = 'htest'
  return(wynik)
}
