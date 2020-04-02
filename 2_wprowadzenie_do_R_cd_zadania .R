# Zajêcia 2 ####

# Zadanie 1 ---------------------------------------------------------------
moja_lista = list(c("Karol", "Marcinkowski"), pi, sqrt, seq(0.1, 1, by = 0.1))

moja_lista[c(1, 3)] = NULL

lapply(moja_lista, gamma)

# Zadanie 2 ---------------------------------------------------------------
x = cbind(c(1, 2, 1), c(5, 0, 2), c(3, 5, 1))
library(Matrix)

rankMatrix(x)
det(x)
eigen(x)

rowSums(x)
rowMeans(x)

colSums(x)
colMeans(x)

x %*% solve(x)


# Zadanie 3 ---------------------------------------------------------------
v = seq(1, 100) * seq(1, 100)
table(factor(v%%10))


# Zadanie 4 ---------------------------------------------------------------
outer(seq(1, 5, by = 1), seq(1, 5, by = 1), FUN = "*")


# Zadanie 5 ---------------------------------------------------------------
file = read.csv("http://ls.home.amu.edu.pl/data_sets/dane1.csv", header=TRUE)
data.frame(file) # TODO ####


# Zadanie 6 ---------------------------------------------------------------
ramka = data.frame(
  month = format(ISOdate(2004,1:12,1),"%B"),
  NY_F = c(32, 33, 41, 52, 62, 72, 77, 75, 68, 58, 47, 35)
)

library(weathermetrics)
ramka = cbind(ramka[1:2, ], NY_C = fahrenheit.to.celsius(ramka$NY_F, round = 2))

ramka$NY_F = NULL

save(ramka, file = "NY_temp.RData")
