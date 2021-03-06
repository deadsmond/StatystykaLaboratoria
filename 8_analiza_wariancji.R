# Zaj�cia 8 ####


# Zadanie 1 ---------------------------------------------------------------
# Zadanie to zosta�o opracowane na podstawie eksperymentu Smitha (1979).
# G��wnym jego celem by�o pokazanie, �e bycie w tym samym kontek�cie psychicznym w
# czasie nauki i podczas jej sprawdzania (test, egzamin) daje lepsze wyniki ni� bycie w
# odmiennych kontekstach. Podczas fazy ucz�cej uczniowie uczyli si� 80 s��w w pokoju
# pomalowanym na pomara�czowo, ozdobionym plakatami, obrazami i du�� ilo�ci�
# dodatkowych akcesori�w. Pierwszy sprawdzian pami�ci zosta� przeprowadzony aby da�
# uczniom wra�enie, �e eksperyment si� zako�czy�. Nast�pnego dnia, uczniowie zostali
# niespodziewanie poddani testowi ponownie. Mieli napisa� wszystkie s�owa, kt�re zapami�tali.
# Test zosta� przeprowadzony w 5 r�nych warunkach. 50 uczni�w zosta�o losowo podzielonych
# na 5 r�wnolicznych grup:
#   * "Same context" - test odbywa� si� w tym samym pokoju, w kt�rym si� uczyli.
#   * "Different context" - test odbywa� si� w bardzo odmiennym pomieszczeniu, w innej cz�ci
# kampusu, pomalowanym na szaro i wygl�daj�cym bardzo surowo.
#   * "Imaginary context' - test odbywa� si� w tym samym pomieszczeniu, co w punkcie
# poprzednim. Dodatkowo, uczniowie mieli przypomnie� sobie pok�j, w kt�rym si� uczyli.
# Aby im w tym pom�c badacz zadawa� dodatkowe pytania o pokoju i jego wyposa�eniu.
#   * "Photographed context� - test odbywa� si� w tych samych warunkach, co w punkcie 
# poprzednim. Dodatkowo pokazano im zdj�cie pokoju, w kt�rym si� uczyli.
#   * "Placebo context" - test odbywa� si� w tym samych warunkach co grupy �Different
# context'. Dodatkowo uczniowie wykonali �wiczenia ,,rozgrzewaj�ce� (przypominanie sobie swojego salonu).
# Liczba zapami�tanych s��w zosta�a zawarta w poni�szej tabeli.


dane <- read.table("http://ls.home.amu.edu.pl/data_sets/kontekst.txt")

## 1. Wyznacz �rednie liczb zapami�tanych s��w w grupach. Ponadto, przedstaw otrzymane
# dane za pomoc� wykresu ramkowego dla ka�dej grupy z osobna. 

colnames(dane) <- c("number", "context")

aggregate(
  dane$number, 
  list(CONTEXT = dane$context), 
  FUN = mean
)

boxplot(
  number ~ context,
  data = dane
)

## 2. Wykonaj test analizy wariancji w celu sprawdzenia, czy liczba zapami�tanych s��w zale�y
# od kontekstu sprawdzania wiedzy. 

summary(
  aov(
    number ~ context,
    data = dane
  )
)

## 3. Sprawd� za�o�enia modelu jednoczynnikowej analizy wariancji. 

shapiro.test(
  lm(
    number ~ context,
    data = dane
  )
  $residuals
)$p.value

qqnorm(dane$number)
qqline(dane$number)

bartlett.test(number ~ context, data = dane)$p.value

fligner.test(number ~ context, data = dane)$

library(car)
leveneTest(number ~ context, data = dane)$Pr

leveneTest(number ~ context, data = dane, center = "mean")$Pr

## 4. Wykonaj testy post hoc w celu sprawdzenia, kt�re konteksty sprawdzania wiedzy r�ni�
# si� mi�dzy sob�.

# testy post hoc
attach(dane)
pairwise.t.test(number, context, data = dane)

model_aov <- aov(number ~ context, data = dane)
TukeyHSD(model_aov)

plot(TukeyHSD(model_aov))

library(agricolae)
HSD.test(model_aov, "context", console = TRUE)

SNK.test(model_aov, "context", console = TRUE)

LSD.test(model_aov, "context", p.adj = "holm", console = TRUE)

scheffe.test(model_aov, "context", console = TRUE)

## 5. Chcemy przetestowa� nast�puj�ce hipotezy szczeg�owe:
# no nie wiem czy chcemy


# Zadanie 2 ---------------------------------------------------------------

## 1.
dane <- read.table("http://ls.home.amu.edu.pl/data_sets/Eysenck.txt", header=TRUE)[,c(2,3)]
colnames(dane) <- c("wynik", "instrukcja")

## 2.
aggregate(
  dane$wynik, 
  list(Instrukcja = dane$instrukcja), 
  FUN = mean
)

boxplot(
  wynik ~ instrukcja,
  data = dane
)

## 3.
summary(
  aov(
    wynik ~ instrukcja,
    data = dane
  )
)

## 4.
shapiro.test(
  lm(
    wynik ~ instrukcja,
    data = dane
  )
  $residuals
)$p.value

qqnorm(dane$wynik)
qqline(dane$wynik)

bartlett.test(wynik ~ instrukcja, data = dane)$p.value

fligner.test(wynik ~ instrukcja, data = dane)$p.value
  
library(car)
leveneTest(wynik ~ instrukcja, data = dane)$Pr

leveneTest(wynik ~ instrukcja, data = dane, center = "mean")$Pr

## 5.
# testy post hoc
attach(dane)
pairwise.t.test(wynik, instrukcja, data = dane)

model_aov <- aov(wynik ~ instrukcja, data = dane)
TukeyHSD(model_aov)

plot(TukeyHSD(model_aov))

library(agricolae)
HSD.test(model_aov, "instrukcja", console = TRUE)

SNK.test(model_aov, "instrukcja", console = TRUE)

LSD.test(model_aov, "instrukcja", p.adj = "holm", console = TRUE)

scheffe.test(model_aov, "instrukcja", console = TRUE)


## 6.
