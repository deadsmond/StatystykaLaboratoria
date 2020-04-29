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

## 2.

## 3.

## 4.

## 5.

## 6.
