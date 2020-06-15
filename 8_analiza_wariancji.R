# Zajêcia 8 ####


# Zadanie 1 ---------------------------------------------------------------
# Zadanie to zosta³o opracowane na podstawie eksperymentu Smitha (1979).
# G³ównym jego celem by³o pokazanie, ¿e bycie w tym samym kontekœcie psychicznym w
# czasie nauki i podczas jej sprawdzania (test, egzamin) daje lepsze wyniki ni¿ bycie w
# odmiennych kontekstach. Podczas fazy ucz¹cej uczniowie uczyli siê 80 s³ów w pokoju
# pomalowanym na pomarañczowo, ozdobionym plakatami, obrazami i du¿¹ iloœci¹
# dodatkowych akcesoriów. Pierwszy sprawdzian pamiêci zosta³ przeprowadzony aby daæ
# uczniom wra¿enie, ¿e eksperyment siê zakoñczy³. Nastêpnego dnia, uczniowie zostali
# niespodziewanie poddani testowi ponownie. Mieli napisaæ wszystkie s³owa, które zapamiêtali.
# Test zosta³ przeprowadzony w 5 ró¿nych warunkach. 50 uczniów zosta³o losowo podzielonych
# na 5 równolicznych grup:
#   * "Same context" - test odbywa³ siê w tym samym pokoju, w którym siê uczyli.
#   * "Different context" - test odbywa³ siê w bardzo odmiennym pomieszczeniu, w innej czêœci
# kampusu, pomalowanym na szaro i wygl¹daj¹cym bardzo surowo.
#   * "Imaginary context' - test odbywa³ siê w tym samym pomieszczeniu, co w punkcie
# poprzednim. Dodatkowo, uczniowie mieli przypomnieæ sobie pokój, w którym siê uczyli.
# Aby im w tym pomóc badacz zadawa³ dodatkowe pytania o pokoju i jego wyposa¿eniu.
#   * "Photographed context” - test odbywa³ siê w tych samych warunkach, co w punkcie 
# poprzednim. Dodatkowo pokazano im zdjêcie pokoju, w którym siê uczyli.
#   * "Placebo context" - test odbywa³ siê w tym samych warunkach co grupy „Different
# context'. Dodatkowo uczniowie wykonali æwiczenia ,,rozgrzewaj¹ce” (przypominanie sobie swojego salonu).
# Liczba zapamiêtanych s³ów zosta³a zawarta w poni¿szej tabeli.


dane <- read.table("http://ls.home.amu.edu.pl/data_sets/kontekst.txt")

## 1. Wyznacz œrednie liczb zapamiêtanych s³ów w grupach. Ponadto, przedstaw otrzymane
# dane za pomoc¹ wykresu ramkowego dla ka¿dej grupy z osobna. 

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

## 2. Wykonaj test analizy wariancji w celu sprawdzenia, czy liczba zapamiêtanych s³ów zale¿y
# od kontekstu sprawdzania wiedzy. 

summary(
  aov(
    number ~ context,
    data = dane
  )
)

## 3. SprawdŸ za³o¿enia modelu jednoczynnikowej analizy wariancji. 

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

## 4. Wykonaj testy post hoc w celu sprawdzenia, które konteksty sprawdzania wiedzy ró¿ni¹
# siê miêdzy sob¹.

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

## 5. Chcemy przetestowaæ nastêpuj¹ce hipotezy szczegó³owe:
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
