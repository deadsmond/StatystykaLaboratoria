# Zajêcia 8 ####


# Zadanie 1 ---------------------------------------------------------------
dane <- read.table("http://ls.home.amu.edu.pl/data_sets/kontekst.txt")

## 1.
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

## 2.
summary(
  aov(
    number ~ context,
    data = dane
  )
)

## 3.
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

## 4.
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

## 5.


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
