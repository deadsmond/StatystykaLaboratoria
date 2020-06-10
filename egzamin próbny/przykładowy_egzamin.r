
computers = read.csv("j:/Desktop/R/egzamin próbny/computers.csv")
heights = read.csv("j:/Desktop/R/egzamin próbny/weight-height.csv")
spotify = read.csv("j:/Desktop/R/egzamin próbny/spotify.csv")

# Zadanie - zbiór danych "computers", rozk³ad RAM ----
# Plik computers.csv zawiera dane dotycz¹ce cen komputerów
# Zmienna ram zawiera informacje o pamiêci RAM w MB,
# a zmienna screen o przek¹tnej ekranu w calach.
# Wyznacz rozk³ad zmiennej ram dla komputerów z 14-calowym ekranem. 

comp_screen_14 = computers[computers$screen==14, ]

prop.table(
  table(
    comp_screen_14$ram  # wa¿ne - wybór kolumny RAM
  )
)

# Zadanie - zbiór danych spotify, sk³adowe g³ówne --
# PLik spotify.csv zawiera dane piosenek wraz z ich
# cechami muzycznymi pochodz¹cymi z API Spotify.
# Przeprowadzono analizê sk³¹dowych g³ównych ze skalowaniem,
# wykorzystuj¹c wyl¹cznie zmienne numeryczne (kolumny 1-9).
# Ile procent zmiennoœci uk³adu wyjaœniaj¹ 
# sumarycznie trzy pierwsze sk³adowe g³ówne?

spotify_num = spotify[, 1:9]

var(spotfy_num)

p_cal = prcomp(spotify_num, scale = TRUE)

# suma po drugim wierszu pierwszych trzech kolumn?
summary(p_cal) 

# Zadanie - zbiór danych spotify, regresja wieloliniowa --
# PLik spotify.csv zawiera dane piosenek wraz z ich
# cechami muzycznymi pochodz¹cymi z API Spotify.
# Zmienna ci¹g³a valence, mierzona w przedziale 0-1.0,
# opisuje pozytywny nastrój utworu (0 - ponure, 1 - radosne)
# Dopasowano model regresji wielokrotnej dla zmiennej zale¿nej
# valence i pozostalych zmiennych jako niezale¿nych.
# Nastêpnie model pe³ny zredukowano za pomoc¹ regresji krokowej w ty³.
# Zakladaj¹c model zredukowany, o ile wzroœnie zmienna
# valence, je¿eli zmienna danceability wzros³aby o jedn¹ jednostkê,
# a pozostale wspó³czynniki pozosta³yby bez zmian?

spotify_num = spotify[, 1:9]

model_1 = lm(valence ~ ., data = spotify_num)

summary(model_1)

model_zredukowany = step(model_1)

summary(model_zredukowany) 


# Zadanie - zbiór danych "heights", t-student ----
# Plik weight-height.csv zawiera dane wzrostu i wagi 10000 osób
# z podzialem na p³eæ. Zmienna Gender wskazuje na p³eæ
# i przyjmuje wartosci Male i Female.
# Na poziomie istotnoœci postanowiono zweryfikowaæ hipotezê
# ¿e mê¿czyŸni s¹ istotnie wy¿si od kobiet. 
# Wyznacz Wartoœæ statystyki testowej.

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

# Zadanie - test istotnoœci dla wariancji ---
# Napisz funkcjê realizujac¹ test istotnoœci dla
# wariancji w modelu normalnym. Zwracany wynik
# ma byæ klasy htest.

war.test = function(
  x, 
  war0, 
  alt = c('two.sided', 'less', 'greater')
){
  d = length(x) - 1
  statistic = d * var(x) / war0
  alt = match.arg(alt)
  p.value = pchisq(statistic, d)
  p.value = switch(
    alt, 
    'two.sided' = 2 * min(p.value, 1-p.value),
    'greater' = 1 - xxx,
    'less' = xxx
  )
  names(statistic) = 'T'
  names(d) = 'num df'
  wynik = list(
    statistic = statistic,
    parameter = d,
    p.value = p.value,
    alternative = xxx
  )
  class(wynik) = 'htest'
  return(wynik)
}
